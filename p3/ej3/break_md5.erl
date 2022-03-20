-module(break_md5).
-define(PASS_LEN, 6).
-define(UPDATE_BAR_GAP, 2600).
-define(BAR_SIZE, 40).
-define(MAX_TIME, 1000000).
-define(NUM_PROCESS, 6).

-export([break_md5s/1,
         pass_to_num/1,
         num_to_pass/1,
         num_to_hex_string/1,
         hex_string_to_num/1
        ]).
-export([break/6]).
-export([start_process/10]).
-export([progress_loop/2]).
-export([done_receive/5]).
-export([cont_seg/1]).
-export([wait/1]).


% Base ^ Exp

pow_aux(_Base, Pow, 0) ->
    Pow;
pow_aux(Base, Pow, Exp) when Exp rem 2 == 0 ->
    pow_aux(Base*Base, Pow, Exp div 2);
pow_aux(Base, Pow, Exp) ->
    pow_aux(Base, Base * Pow, Exp - 1).

pow(Base, Exp) -> pow_aux(Base, 1, Exp).


%% Number to password and back conversion
num_to_pass_aux(_N, 0, Pass) -> Pass;
num_to_pass_aux(N, Digit, Pass) ->
    num_to_pass_aux(N div 26, Digit - 1, [$a + N rem 26 | Pass]).

num_to_pass(N) -> num_to_pass_aux(N, ?PASS_LEN, []).

pass_to_num(Pass) ->
    lists:foldl(fun (C, Num) -> Num * 26 + C - $a end, 0, Pass).


%% Hex string to Number
hex_char_to_int(N) ->
    if (N >= $0) and (N =< $9) -> N - $0;
       (N >= $a) and (N =< $f) -> N - $a + 10;
       (N >= $A) and (N =< $F) -> N - $A + 10;
       true                    -> throw({not_hex, [N]})
    end.

int_to_hex_char(N) ->
    if (N >= 0)  and (N < 10) -> $0 + N;
       (N >= 10) and (N < 16) -> $A + (N - 10);
       true                   -> throw({out_of_range, N})
    end.

%
hex_string_to_num(Hex_Str) ->
    lists:foldl(fun(Hex, Num) -> Num*16 + hex_char_to_int(Hex) end, 0, Hex_Str).

num_to_hex_string_aux(0, Str) -> Str;
num_to_hex_string_aux(N, Str) ->
    num_to_hex_string_aux(N div 16,
                          [int_to_hex_char(N rem 16) | Str]).

num_to_hex_string(0) -> "0";
num_to_hex_string(N) -> num_to_hex_string_aux(N, []).



%% Progress bar runs in its own process
%% Barra del proceso
%% Recibira un flag, cuando done_receive/5 intente matar a Progress_Pid
progress_loop(N, Bound) ->
    process_flag(trap_exit, true),
    receive     % it is used to allow processes to wait for messages from other processes
        {'EXIT', From, terminate} ->
            From ! {terminated, self()};
        {progress_report, Checked} ->
            N2 = N + Checked,
            Full_N = N2 * ?BAR_SIZE div Bound,
            Full = lists:duplicate(Full_N, $=), %Returns a list containing N(Full_N) copies of term Elem($=).
            Empty = lists:duplicate(?BAR_SIZE - Full_N, $-),
            io:format("\r[~s~s] ~.2f%\t\t", [Full, Empty, N2/Bound*100]),
            progress_loop(N2, Bound)
    end.


    
%% Recibe los casos procesados por el proceso que este ejecutando break_md5/7
%% Recibira un flag, cuando done_receive/5 intente matar a Contador_pid
cont_seg(T1)->
    process_flag(trap_exit, true),
    receive 
        {'EXIT', From, terminate} ->
            From ! {terminated, self()}
    after 
        1000 ->
            receive
                {cont, N} ->
                    T2 = erlang:monotonic_time(microsecond),
                    Time = 
                        case ((T2 div ?MAX_TIME) - (T1 div ?MAX_TIME)) of 
                            0 ->
                                1;
                            M -> 
                                M
                        end,
                    Media = N div Time,
                    io:format("Casos: ~p", [Media]),
                    cont_seg(T1)
            end
    end.



%% break_md5/7 iterates checking the possible passwords
%% Envia mensajes a la barraDeProceso y al contador, tambien ejecuta notify
break_md5(Pid_Padre, [], _N, _Bound, _Progress_Pid, _Contador_Pid,_Process_List) ->
    Pid_Padre ! done;
break_md5(Pid_Padre, Num_Hashes, N, N, _Progress_Pid, _Contador_Pid,_Process_List) ->
    Pid_Padre ! {not_found, Num_Hashes};  % Checked every possible password
break_md5(Pid_Padre, Num_Hashes, N, Bound, Progress_Pid, Contador_Pid,Process_List) ->
    receive
        {notify_hash, Hashes} ->
            break_md5(Pid_Padre,Hashes, N, Bound, Progress_Pid, Contador_Pid,Process_List)
    after 
        0 ->
            if N rem ?UPDATE_BAR_GAP == 0 ->   %rem =  módulo
                Contador_Pid ! {cont, N},
                Progress_Pid ! {progress_report, ?UPDATE_BAR_GAP};
            true ->
                    ok
            end,
            Pass = num_to_pass(N),
            Hash = crypto:hash(md5, Pass),
            Num_Hash = binary:decode_unsigned(Hash),
            case lists:member(Num_Hash, Num_Hashes) of
                true -> 
                    notify(lists:delete(self(), Process_List), lists:delete(Num_Hash, Num_Hashes)),
                    io:format("\e[2K\r~.16B: ~s~n", [Num_Hash, Pass]),
                    break_md5(Pid_Padre, lists:delete(Num_Hash, Num_Hashes), N+1, Bound, Progress_Pid, Contador_Pid,Process_List);
                false ->
                    break_md5(Pid_Padre, Num_Hashes, N+1, Bound, Progress_Pid, Contador_Pid,Process_List)
            end
    end.


%% funcion asignada a cada proceso
%% cuando reciba {process_list, Process_List_M} ejecutara break_md5/7
break(Pid_Padre, Num_Hashes,First,Last,Progress_Pid, Contador_Pid) ->
    receive
        {process_list, Process_List_M} ->
            break_md5(Pid_Padre, Num_Hashes,First,Last,Progress_Pid, Contador_Pid,Process_List_M)
    end.


%% Crea n procesos asignandole a cada uno un rango
%% Cuando Num_Process = 0 la Process_List estara completa,entonces se envia un mensaje{process_list, Process_List} a cada proceso, 
%% y cada uno se encargara de ejecutar break/6 que a su vez ejecutara break_md5/7
start_process(0, _,_,_,_,_,_,Process_List,_,_) ->
    Fun = fun(N) -> N ! {process_list, Process_List} end,
    lists:foreach(Fun, Process_List),
    Process_List;
start_process(Num_Process, Pid_Padre, Num_Hashes,First,Last,Progress_Pid, Contador_Pid,Process_List,Bound,Rango) ->
    if
        Last+Rango >= Bound ->
            Pid = spawn(?MODULE, break,[ Pid_Padre, Num_Hashes, First, Bound, Progress_Pid, Contador_Pid]),
            start_process(Num_Process-1,Pid_Padre, Num_Hashes, First, Bound, Progress_Pid, Contador_Pid,[Pid|Process_List],Bound, Rango);

        true ->
            Pid = spawn(?MODULE, break,[ Pid_Padre, Num_Hashes, First, Last, Progress_Pid, Contador_Pid]),
            start_process(Num_Process-1,Pid_Padre, Num_Hashes, First+Rango,Last+Rango, Progress_Pid, Contador_Pid,[Pid|Process_List],Bound, Rango)
    end.



%% El Pid_Padre envia a todos los procesos que ejecuten break_md5/7 la nueva lista de hashes que faltan por encontrar
notify([], _) -> ok;
notify([H|T], Num_Hashes) ->
    H ! {notify_hash, Num_Hashes},
    notify(T, Num_Hashes).


%% Funcion auxiliar de done_receive
%% Procesa los mensajes de la linea 201
wait([]) ->
    ok;
wait([Msg | Rest]) ->
    receive
        Msg ->
            wait(Rest)
    end.

%% Realizada por el Pid_Padre
%% Si el mensaje es 'done' entonces se matan a todos los procesos generados para ejecutas 
%% el break_md5/6 envia flags para matar a Progess_Pid y Contador_Pid
%% Si el mensaje es not_found entonces devuelve la lista con los hashes no encontrados
done_receive(0, _List_Processes, _Progress_Pid, _Contador_Pid, NotFoundHashes) ->
    NotFoundHashes;
done_receive(N, List_Processes, Progress_Pid, Contador_Pid, _NotFoundHashes) ->
    receive
        done->
            lists:foreach(fun (P) -> exit(P, terminate) end, List_Processes),
            exit(Progress_Pid, terminate),
            exit(Contador_Pid, terminate),
            wait([{terminated, Progress_Pid}, {terminated, Contador_Pid}]),                
            [];
        {not_found, Hashes} ->
            if
                (N == 1) ->
                    lists:foreach(fun (P) -> exit(P, terminate) end, List_Processes),
                    exit(Progress_Pid, terminate),
                    exit(Contador_Pid, terminate),
                    Hashes;
                true ->
                    done_receive(N-1, List_Processes, Progress_Pid, Contador_Pid, Hashes)
            end
    end.


%% limpia el buffer del proceso padre, que ejecuta el shell
clean_queue() ->
    receive
        _ -> 
            clean_queue()
    after
        0 ->
            ok
    end.


break_md5s(Hashes) ->
    Bound = pow(26, ?PASS_LEN),
    Progress_Pid = spawn(?MODULE, progress_loop, [0, Bound]),  %[0, Bound] son los argumentos para progess_loop
    Contador_Pid = spawn(?MODULE, cont_seg, [erlang:monotonic_time(microsecond)]),
    Num_Hashes = lists:map(fun hex_string_to_num/1, Hashes),
    Pid_Padre = self(),
    Rango = (Bound div ?NUM_PROCESS),
    List_Processes = start_process(?NUM_PROCESS, Pid_Padre, Num_Hashes, 0,Rango,Progress_Pid, Contador_Pid, [], Bound,Rango),
    NotFoundHashes = done_receive(?NUM_PROCESS, List_Processes, Progress_Pid, Contador_Pid, []),
    io:format("\rHashes no encontrados: ~p\t\t\t\t\t\t\t\t\t\t~n",[NotFoundHashes]),
    clean_queue().

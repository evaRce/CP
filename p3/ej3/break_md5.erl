-module(break_md5).
-define(PASS_LEN, 6).
-define(UPDATE_BAR_GAP, 2600).
-define(BAR_SIZE, 40).
-define(MAX_TIME, 1000000).
-define(NUM_THREADDS, 6).

-export([break_md5s/1,
         pass_to_num/1,
         num_to_pass/1,
         num_to_hex_string/1,
         hex_string_to_num/1
        ]).

-export([progress_loop/2]).

-export([cont_seg/1]).


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
progress_loop(N, Bound) ->
    receive     % it is used to allow processes to wait for messages from other processes
        stop -> ok;
        {progress_report, Checked} ->
            N2 = N + Checked,
            Full_N = N2 * ?BAR_SIZE div Bound,
            Full = lists:duplicate(Full_N, $=), %Returns a list containing N(Full_N) copies of term Elem($=).
            Empty = lists:duplicate(?BAR_SIZE - Full_N, $-),
            io:format("\r[~s~s] ~.2f%\t\t", [Full, Empty, N2/Bound*100]),
            progress_loop(N2, Bound)
    end.


    
%% Recibe los casos procesados por el Progress_Pid   
cont_seg(T1)->
    receive 
        done ->
            exit(normal)
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



%% break_md5/2 iterates checking the possible passwords
%% Envia mensajes a la barraDeProceso y al contador
break_md5(_Pid_Padre, [], _N, _Bound, _Progress_Pid, _Contador_Pid) -> 
    ok;
break_md5(Pid_Padre, Num_Hashes, N, N, __Progress_Pid, __Contador_Pid) -> 
    {not_found, Num_Hashes};  % Checked every possible password
break_md5(Pid_Padre, Num_Hashes, N, Bound, Progress_Pid, Contador_Pid) ->
    receive
        {notify_hash, Hash} -> 
            break_md5(Pid_Padre, lists:delete(Hash, Num_Hashes), N, Bound, Progress_Pid, Contador_Pid)
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
                    Pid_Padre ! {hash_found, Num_Hash, self()},
                    io:format("\e[2K\r~.16B: ~s~n", [Num_Hash, Pass]),
                    break_md5(Pid_Padre, lists:delete(Num_Hash, Num_Hashes), N+1, Bound, Progress_Pid, Contador_Pid);
                false ->
                    break_md5(Pid_Padre, Num_Hashes, N+1, Bound, Progress_Pid, Contador_Pid)
            end
    end.



start_threads(0, _, _, Process_List, _, _) -> Process_List;
start_threads(Num_Process, Fun, [Args], Process_List, Rango, Resto_Rango) ->
    Pid = spawn(?MODULE, Fun, Args),
    start_threads(Num_Process-1, Fun, Args, [Pid|Process_List], Rango, Rango).


%% EL Pid_Padre avisa a todos los procesos la contraseña(Hash) que fue encontrada
notify([], _) -> ok;
notify([H|T], Hash) ->
    H ! {notify_hash, Hash},
    notify(T, Hash).


%% Realizada por el Pid_Padre
loop_receive(Pid_List, []) ->
    Fun = fun(Pid) -> exit(Pid, kill) end,
    lists:foreach(Fun, Pid_List);
loop_receive(Pid_List, Num_Hashes) ->
    receive
        {hash_found, Hash, Pid} ->
            notify(lists:delete(Pid, Pid_List), Hash),
            New_Num_Hashes = lists:delete(Hash, Num_Hashes),
            loop_receive(Pid_List, New_Num_Hashes)
    end.



    

%ej1
break_md5s(Hashes) ->
    Bound = pow(26, ?PASS_LEN),
    Progress_Pid = spawn(?MODULE, progress_loop, [0, Bound]),  %[0, Bound] son los argumentos para progess_loop
    Contador_Pid = spawn(?MODULE, cont_seg, [erlang:monotonic_time(microsecond)]),
    Num_Hashes = lists:map(fun hex_string_to_num/1, Hashes),
    Pid_Padre = self(),
    %%rangos
    Rango = (Bound div ?NUM_THREADDS),
    Resto_Rango = Bound rem ?NUM_THREADDS,
    Pid_List = start_threads(?NUM_THREADDS, break_md5, [Pid_Padre, Num_Hashes, 0, Bound, Progress_Pid, Contador_Pid], [], Rango, Resto_Rango),
    loop_receive(Pid_List, Num_Hashes),
    Contador_Pid ! done,
    Progress_Pid ! stop.


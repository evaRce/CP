-module(break_md5).
-define(PASS_LEN, 6).
-define(UPDATE_BAR_GAP, 1000).
-define(BAR_SIZE, 40).
-define(MAX_TIME, 1000000).

-export([break_md5s/1,
         pass_to_num/1,
         num_to_pass/1,
         num_to_hex_string/1,
         hex_string_to_num/1
        ]).

-export([progress_loop/2]).

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

progress_loop(N, Bound) ->
    receive     % it is used to allow processes to wait for messages from other processes
        stop -> ok;
        {progress_report, Checked} ->
            N2 = N + Checked,
            Full_N = N2 * ?BAR_SIZE div Bound,
            Full = lists:duplicate(Full_N, $=), %Returns a list containing N(Full_N) copies of term Elem($=).
            Empty = lists:duplicate(?BAR_SIZE - Full_N, $-),
            io:format("\r[~s~s] ~.2f%", [Full, Empty, N2/Bound*100]),
            progress_loop(N2, Bound)
    end.


contador(N) ->
    receive 
        stop ->
            ok;
        inicial ->
            contador(0);
        incrementar ->
            contador(N+1);
        imprimir ->
            io:format("Casos ~w", [N])
    end.


total(T1, T2)->
    T2-T1.


%% break_md5/2 iterates checking the possible passwords
break_md5([], _, _, Progress_Pid, Contador_Pid) ->
    Contador_Pid ! stop, 
    Progress_Pid ! stop;
break_md5(Num_Hashes, N, N, _, _) -> {not_found, Num_Hashes};  % Checked every possible password
break_md5(Num_Hashes, N, Bound, Progress_Pid, Contador_Pid) ->
    if (N == 0) or (total(T1, T2) >= ?MAX_TIME)->
            T1 = erlang:monotonic_time(microsecond),
            Contador_Pid ! imprimir,
            Contador_Pid ! inicial;
        true -> 
            ok
    end,
    if N rem ?UPDATE_BAR_GAP == 0 ->   %rem =  módulo
            Progress_Pid ! {progress_report, ?UPDATE_BAR_GAP};
       true ->
            ok
    end,
    Pass = num_to_pass(N),
    Hash = crypto:hash(md5, Pass),
    Num_Hash = binary:decode_unsigned(Hash),
    T2 = erlang:monotonic_time(microsecond),
    total(T1, T2),
    Contador_Pid ! incrementar,
    case lists:member(Num_Hash, Num_Hashes) of
        true -> 
            io:format("\e[2K\r~.16B: ~s~n", [Num_Hash, Pass]),
            break_md5(lists:delete(Num_Hash, Num_Hashes), N+1, Bound, Progress_Pid);
        false ->
            break_md5(Num_Hashes, N+1, Bound, Progress_Pid)
    end.


%% Break a hash
%break_md5(Hash) ->
%%    Bound = pow(26, ?PASS_LEN),
%%    Progress_Pid = spawn(?MODULE, progress_loop, [0, Bound]),  %[0, Bound] son los argumentos para progess_loop
%%    Num_Hash = hex_string_to_num(Hash),
%%    Res = break_md5(Num_Hash, 0, Bound, Progress_Pid),
%%    Progress_Pid ! stop,
%%    Res.


%ej1
break_md5s(Hashes)->
    Bound = pow(26, ?PASS_LEN),
    Progress_Pid = spawn(?MODULE, progress_loop, [0, Bound]),  %[0, Bound] son los argumentos para progess_loop
    Contador_Pid = spawn(?MODULE, contador, 0),
    Num_Hashes = lists:map(fun hex_string_to_num/1, Hashes),
    break_md5(Num_Hashes, 0, Bound, Progress_Pid, Contador_Pid).


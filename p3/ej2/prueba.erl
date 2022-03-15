-module(prueba).

-export([start/0, do_progress/2]).

start() ->
    List = [a,b,c,d,e,f,g,h,i],
    Pid = spawn(?MODULE, do_progress, [0, erlang:monotonic_time(microsecond)]),
    loop(Pid, List, 0).

loop(Pid, [], N) ->
    Pid ! done;
loop(Pid, [_H| Rest], N) ->
    timer:sleep(rand:uniform(10)*1000),
    Pid ! {num, rand:uniform(10)},
    loop(Pid, Rest, N).

do_progress(N, T0) ->
    receive
        done ->
            exit(normal);
        {num, Num} ->
            T = erlang:monotonic_time(microsecond),
            Val = (Num * 10000000) div (T - T0),
            io:format(" ~p ~p ~p ~p ~n",[Num, Val, T, T0]),
            do_progress(Num, T0)
    after 1000 ->
        T = erlang:monotonic_time(microsecond),
        Val = (N * 10000000) div (T - T0),
        io:format(" ~p ~p ~p ~p ~n",[N * 10000000, Val, T, T0]),
        do_progress(N, T0)
    end.

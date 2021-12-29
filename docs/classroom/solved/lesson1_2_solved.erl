-module(lesson1_2_solved).
-export([test/0]).

display_all([]) -> 
    io:format("~n");

display_all([First|Rest]) ->
    io:format("~p ",[First]),
    display_all(Rest).

sum([]) -> 0;
sum([First|Rest]) ->
    First + sum(Rest).

average(List) when length(List) == 0 -> undefined;
average(List) -> sum(List) / length(List).

process([display|Rest]) ->
    io:format("~p~n",[Rest]);
process([sum|Rest]) ->
    sum(Rest);
process([get|Rest]) ->
    Rest;
process(_List) ->
    invalid.

test() ->
    L1 = [2,4,6,8,10],
    L2 = [0|L1],
    [F1, F2|_] = L2,
    io:format("First=~p Second=~p ~n",[F1,F2]),
    display_all(L2),
    30 = sum(L2),
    undefined = average([]),
    25.0 = average([10,20,30,40]),
    process([display,"Bob","Tim","Sue"]),
    15 = process([sum,1,2,3,4,5]),
    [1,2,3,4,5] = process([get,1,2,3,4,5]),
    invalid = process([guess,1,2,3,4,5]),

    L3 = lists:seq(1,20),
    io:format("~p~n",[L3]),
    L4 = [3*X || X <- L3],
    io:format("~p~n",[L4]),
    L5 = [3*X || X <- L3, X rem 5 == 0],
    io:format("~p~n",[L5]),

    
    pass.
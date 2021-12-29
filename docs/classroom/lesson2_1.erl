-module(lesson2_1).
-export([test/0]).



test() ->
    L1 = prepend([],1),
    L2 = prepend(L1,2),
    L3 = append(L2,3),
    L4 = append([],4),
    L5 = prepend(L4,5),
    L6 = prepend(L2,6),
    L7 = prepend(L3,7),
    L8 = append(L7,8),
    L9 = append(L8,9),
    L10 = prepend(L8,10),
    io:format("~p~n",[[L1,L2,L3,L4,L5,L6,L7,L8,L9,L10]]),
    pass.
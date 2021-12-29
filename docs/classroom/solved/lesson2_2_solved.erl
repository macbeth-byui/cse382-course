-module(lesson2_2_solved).
-export([test/0]).

remove_first([]) -> [];
remove_first([_First|Rest]) -> Rest.

insert_at(List, _Value, Index) when (Index < 0) or (Index >= length(List)) -> List;
insert_at(List, Value, Index) when Index == 0 -> [Value | List];
insert_at([First|Rest], Value, Index) -> [First | insert_at(Rest, Value, Index-1)].

test() ->
    [] = remove_first([]),
    
    L1 = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
    L2 = remove_first(L1),
    L2 = insert_at(L2, 11, -1),
    L2 = insert_at(L2, 12, 10),
    L3 = insert_at(L2, 13, 3),
    L4 = insert_at(L3, 14, 5),
    L5 = insert_at(L4, 15, 0),
    L6 = insert_at(L5, 16, 13),
    io:format("~p~n",[L6]),
    
    pass.
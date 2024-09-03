% CSE 382 Prove 6 - Problem Set 3 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit

-module(prove06_3).
-export([create/0, push/2, len/1, value/1, pop/1, bind/3]).

% Problem 3.1
% Most of the list monad code is provided below.  Implement the pop and bind functions.
create() -> {nil, 0}.

push(Value, List) -> {{Value, List},1}.

len({_List, Length}) -> Length.

value({List, _Length}) -> List.



% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

list_test() ->
        L1 = create(),
    nil = value(L1),
    0 = len(L1),

    L2 = bind(L1, fun push/2, [2]),
    {2,nil} = value(L2),
    1 = len(L2),

    L3 = bind(L2, fun push/2, [4]),
    {4,{2,nil}} = value(L3),
    2 = len(L3),

    L4 = bind(L3, fun push/2, [6]),
    {6,{4,{2,nil}}} = value(L4),
    3 = len(L4),
    
    L5 = bind(L4, fun pop/1, []),
    {4,{2,nil}} = value(L5),
    2 = len(L5),
    
    L6 = bind(L5, fun push/2, [8]),
    {8,{4,{2,nil}}} = value(L6),
    3 = len(L6),

    L7 = bind(L6, fun pop/1, []),
    {4,{2,nil}} = value(L7),
    2 = len(L7),

    L8 = bind(L7, fun pop/1, []),
    {2,nil} = value(L8),
    1 = len(L8),

    L9 = bind(L8, fun pop/1, []),
    nil = value(L9),
    0 = len(L9).

-endif.

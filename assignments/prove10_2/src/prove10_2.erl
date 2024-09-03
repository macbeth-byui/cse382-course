% CSE 382 Prove 10 - Problem Set 2 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit

-module(prove10_2).
-export([prepend/2, merge/2, count/1, lookup/2, lookup_in_tree/2, update/3, update_in_tree/3]).

% Problem 1.1
% We will need the prepend, merge, and count functions that you wrote in problem set 1.
% Copy and paste the code below:



% Problem 2.1



% Problem 2.2



% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

lookup_test() ->
    RAL1 = prepend(100, []),
    RAL2 = prepend(200, RAL1),
    RAL3 = prepend(300, RAL2),
    RAL4 = prepend(400, RAL3),
    RAL5 = prepend(500, RAL4),
    RAL6 = prepend(600, RAL5),
    RAL7 = prepend(700, RAL6),
    nil = lookup(7,RAL7),  % Invalid Index
    nil = lookup(-1,RAL7), % Invalid Index
    700 = lookup(0,RAL7),
    600 = lookup(1,RAL7),
    500 = lookup(2,RAL7),
    400 = lookup(3,RAL7),
    300 = lookup(4,RAL7),
    200 = lookup(5,RAL7),
    100 = lookup(6,RAL7).

update_test() ->
        RAL1 = prepend(100, []),
    RAL2 = prepend(200, RAL1),
    RAL3 = prepend(300, RAL2),
    RAL4 = prepend(400, RAL3),
    RAL5 = prepend(500, RAL4),
    RAL6 = prepend(600, RAL5),
    RAL7 = prepend(700, RAL6),
    RAL7 = update(7,1000,RAL7),  % Invalid Index
    RAL7 = update(-1,1000,RAL7), % Invalid Index
    RAL8 = update(0,888,RAL7),
    [{leaf,888},
     {node,2,{leaf,600},{leaf,500}},
     {node,4,{node,2,{leaf,400},{leaf,300}},{node,2,{leaf,200},{leaf,100}}}
    ] = RAL8,

    RAL9 = update(4,333,RAL8),
    [{leaf,888},
     {node,2,{leaf,600},{leaf,500}},
     {node,4,{node,2,{leaf,400},{leaf,333}},{node,2,{leaf,200},{leaf,100}}}
    ] = RAL9,

    RAL10 = update(6,111,RAL9),
    [{leaf,888},
     {node,2,{leaf,600},{leaf,500}},
     {node,4,{node,2,{leaf,400},{leaf,333}},{node,2,{leaf,200},{leaf,111}}}
    ] = RAL10.

-endif.

% CSE 382 Prove 10 - Problem Set 1 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit

-module(prove10_1).
-export([prepend/2, merge/2, count/1]).

% Problem 1.1



% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

prepend_test() ->
    RAL1 = prepend(100, []),
    [{leaf,100}] = RAL1,

    RAL2 = prepend(200, RAL1),
    [nil,
     {node,2,{leaf,200},{leaf,100}}
    ] = RAL2,
    
    RAL3 = prepend(300, RAL2),
    [{leaf,300},
     {node,2,{leaf,200},{leaf,100}}
    ] = RAL3,

    RAL4 = prepend(400, RAL3),
    [nil,
     nil,
     {node,4,{node,2,{leaf,400},{leaf,300}},{node,2,{leaf,200},{leaf,100}}}
    ] = RAL4,

    RAL5 = prepend(500, RAL4),
    [{leaf,500},
     nil,
     {node,4,{node,2,{leaf,400},{leaf,300}},{node,2,{leaf,200},{leaf,100}}}
    ] = RAL5,
    
    RAL6 = prepend(600, RAL5),
    [nil,
     {node,2,{leaf,600},{leaf,500}},
     {node,4,{node,2,{leaf,400},{leaf,300}},{node,2,{leaf,200},{leaf,100}}}
    ] = RAL6,
    
    RAL7 = prepend(700, RAL6),
    [{leaf,700},
     {node,2,{leaf,600},{leaf,500}},
     {node,4,{node,2,{leaf,400},{leaf,300}},{node,2,{leaf,200},{leaf,100}}}
    ] = RAL7,
    
    RAL8 = prepend(800, RAL7),
    [nil,
     nil,
     nil,
     {node,8,{node,4,{node,2,{leaf,800},{leaf,700}},{node,2,{leaf,600},{leaf,500}}},{node,4,{node,2,{leaf,400},{leaf,300}},{node,2,{leaf,200},{leaf,100}}}}
    ] = RAL8.



-endif.

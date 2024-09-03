% CSE 382 Prove 03 - Problem Set 1 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit
% Delete object files and logs: rebar3 clean

-module(prove03_1).
-export([prepend/2, append/2, head/1, tail/1]).

% Problem 1.1


% Problem 1.2


% Problem 1.3


% Problem 1.4



% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

prepend_test() ->
    [0,1,2,3,4] = prepend([1,2,3,4],0),
    [0] = prepend([],0).

append_test() ->
    [1,2,3,4,5] = append([1,2,3,4],5),
    [5] = append([],5).

head_test() ->
    1 = head([1,2,3,4]),
    nil = head([]).

tail_test() ->
    4 = tail([1,2,3,4]),
    nil = tail([]).

-endif.
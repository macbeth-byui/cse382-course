% CSE 382 Prove 3 - Problem Set 2 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit

-module(prove03_2).
-export([remove_first/1, insert_at/3, remove_last/1, remove_at/2]).

% Problem 2.1


% Problem 2.2


% Problem 2.3


% Problem 2.4



% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

remove_first_test() ->
    [2,3,4] = remove_first([1,2,3,4]),
    [] = remove_first([]).

insert_at_test() ->
    [0,1,2,3,4] = insert_at([1,2,3,4], 0, 0),
    [1,0,2,3,4] = insert_at([1,2,3,4], 0, 1),
    [1,2,3,0,4] = insert_at([1,2,3,4], 0, 3),
    [1,2,3,4,0] = insert_at([1,2,3,4], 0, 4),
    [1,2,3,4] = insert_at([1,2,3,4], 0, 5),
    [1,2,3,4] = insert_at([1,2,3,4], 0, -1),
    [0] = insert_at([], 0, 0).

remove_last_test() ->
    [1,2,3] = remove_last([1,2,3,4]),
    [] = remove_last([]).

remove_at_test() ->
    [2,3,4] = remove_at([1,2,3,4], 0),
    [1,3,4] = remove_at([1,2,3,4], 1),
    [1,2,3] = remove_at([1,2,3,4], 3),
    [1,2,3,4] = remove_at([1,2,3,4], 4),
    [1,2,3,4] = remove_at([1,2,3,4], -1).

-endif.

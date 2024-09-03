% CSE 382 Prove 5 - Problem Set 3 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit

-module(prove05_3).
-export([unfold/3, range/3]).

% Problem 3.1



% Problem 3.3



% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

unfold_1_test() ->
    [5, 10, 15, 20, 25, 30] = unfold(6, 5, fun(Value)->Value+5 end).

unfold_2_test() ->
    % Problem 3.2
    % Call the unfold function with a lambda that will generate a 
    % geometric series as described in the instructions.
    [1, 0.5, 0.25, 0.125, 0.0625, 0.03125] = todo.

range_test() ->
    [3,7,11,15,19] = range(3,5,4),
    [10,20,30,40,50,60] = range(10, 6, 10).

-endif.

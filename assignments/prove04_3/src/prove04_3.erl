% CSE 382 Prove 4 - Problem Set 3 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit

-module(prove04_3).
-export([]).

% NOTE: No new functions need to be written.  All code changes are in the test function below.

% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

map_composition_test() ->
    % Problem 3.1
    % Call the built-in map function within Erlang to demonstrate
    % that both sides of the composition property:
    %   map(g(h), [a]) = map(g, map(h, [a]))
    Values = [1, 2, 3, 4],

    G = fun(Value) -> 2 * Value end,
    H = fun(Value) -> (Value * Value) - 1 end,

    % Left Side of Composition Property
    [0,6,16,30] = todo,

    % Right Side of Composition Property
    [0,6,16,30] = todo.

-endif.

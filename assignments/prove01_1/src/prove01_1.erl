% CSE 382 Prove 1 - Problem Set 1 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit

-module(prove01_1).
-export([add/2, multiply/1, multiply/2, multiply/3, water/1, fib/1, sum/1, plot/1]).

% Problem 1.1


% Problem 1.2


% Problem 1.3


% Problem 1.4


% Problem 1.5


% The plot function is written for you and will be used in the plot_test function below.
plot(Lambda) ->
    X = lists:seq(-3,3),
    Y = lists:map(Lambda, lists:seq(-3,3)),
    lists:zip(X,Y).

% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

hello_test() ->
    ?debugMsg("Hello CSE 382 Student!").

add_test() ->
    5 = add(2,3).

multiply_test() ->
    4 = multiply(4),
    12 = multiply(4,3),
    60 = multiply(4,3,5).

water_test() ->
    "Frozen" = water(31),
    "Liquid" = water(33),
    "Gas" = water(212).

fib_test() ->
    1 = fib(1),
    1 = fib(2),
    2 = fib(3),
    3 = fib(4),
    5 = fib(5),
    8 = fib(6).

sum_test() ->
    0 = sum(0),
    55 = sum(10).

plot_test() ->
    % Problem 1.6
    % Add a lambda to each plot function call below
    % per the instructions.
    [{-3,9},{-2,4},{-1,1},{0,0},{1,1},{2,4},{3,9}] = plot(todo),
    [{-3,-4},{-2,-3},{-1,-2},{0,-1},{1,0},{2,1},{3,2}] = plot(todo),
    [{-3,3},{-2,2},{-1,1},{0,0},{1,1},{2,2},{3,3}] = plot(todo),
    [{-3,-1.0},{-2,-1.0},{-1,-1.0},{0,+0.0},{1,+0.0},{2,+0.0},{3,1.0}] = plot(todo).

-endif.

% CSE 382 Prove 1 - Problem Set 2 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit

-module(prove01_2).
-export([stack_push/2, stack_pop/1, quicksort/1]).

% Problem 2.1


% Problem 2.2


% Problem 2.3
% Modify the code below to add your list comprehensions
quicksort([]) -> [];
quicksort([First|Rest]) -> 
    quicksort(todo) 
    ++ [First] ++ 
    quicksort(todo).

% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

stack_push_test() ->
    S1 = stack_push([],10),
    S2 = stack_push(S1,20),
    S3 = stack_push(S2,30),
    [30,20,10] = S3.

stack_pop_test() ->
    S1 = stack_push([],10),
    S2 = stack_push(S1,20),
    S3 = stack_push(S2,30),
    [30|_] = S3,
    S4 = stack_pop(S3),
    [20|_] = S4,
	S5 = stack_push(S4,40),
	[40|_] = S5,
    S6 = stack_pop(S5),
    [40|_] = S5,
    S7 = stack_pop(S6),
    [10] = S7,
	S8 = stack_pop(S7),
	[] = S8.

quicksort_test() ->
    [] = quicksort([]),
    [1,2,3,4,5] = quicksort([3,1,5,4,2]).

-endif.

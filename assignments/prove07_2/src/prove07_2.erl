% CSE 382 Prove 7 - Problem Set 2 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit

-module(prove07_2).
-export([range/3, iter/1, next/1, iter_sum/1]).

% Problem 1.1
% We will need the range function that you wrote in problem set 1.
% Copy and paste the code below:



% Problem 2.1
% The iter and next functions for the fixed_iterator Monad are written below.
% Complete the iter_sum function.
iter(Stream) -> {undefined, Stream}.

next({_,done}) -> {undefined, done};
next({_,Lambda}) -> Lambda().



% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

iter_sum_1_test() ->
    55 = iter_sum(iter(range(1,10,1))).

iter_sum_2_test() ->
    25 = iter_sum(iter(range(1,10,2))).

iter_sum_3_test() ->
    30 = iter_sum(iter(range(10,1,-2))).

-endif.

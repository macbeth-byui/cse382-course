% CSE 382 Prove 3 - Problem Set 3 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit

-module(prove03_3).
-export([backwards/1]).

% Problem 3.1



% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

backwards_test() ->
    [5,4,3,2,1] = backwards([1,2,3,4,5]),
    [] = backwards([]).

-endif.

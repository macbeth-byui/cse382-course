% CSE 382 Prove 5 - Problem Set 2 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit

-module(prove05_2).
-export([foldr/3]).

% Problem 2.1


% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

foldr_test() ->
    % Problem 2.1
    % Use the same Concat lambda that you wrote from Problem 1.2 below
    Concat = todo,
    "DCBA" = foldr(Concat, "", ["A","B","C","D"]),
    "FourThreeTwoOne" = foldr(Concat, "", ["One","Two","Three","Four"]),
    "lamb little a had Mary" = foldr(Concat, "", ["Mary"," ","had"," ","a"," ","little"," ","lamb"]).

-endif.

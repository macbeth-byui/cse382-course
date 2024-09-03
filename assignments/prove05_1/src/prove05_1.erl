% CSE 382 Prove 5 - Problem Set 1 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit

-module(prove05_1).
-export([fold/3]).

% Problem 1.1


% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

fold_1_test() ->
    Sum = fun (A, Acc) -> Acc + A end,
    55 = fold(Sum, 0, [1,2,3,4,5,6,7,8,9,10]).

fold_2_test() ->
    % Problem 1.2
    % Write the Concat lambda to combine the previous String and the new String.  This 
    % lambda will then be used by the fold function.
    Concat = todo,
    "ABCD" = fold(Concat, "", ["A","B","C","D"]),
    "OneTwoThreeFour" = fold(Concat, "", ["One","Two","Three","Four"]),
    "Mary had a little lamb" = fold(Concat, "", ["Mary"," ","had"," ","a"," ","little"," ","lamb"]).

fold_3_test() ->
    % Problem 1.3
    % Write the Count lambda to count the items in the list using the fold function
    Count = todo,
    8 = fold(Count, 0, [4,2,7,9,10,25,-3,0]).

fold_4_test() ->
    % Problem 1.4
    % Write the Reverse lambda to reverse a list using the fold function
    Reverse = todo,
    [7,5,3,1] = fold(Reverse, [], [1,3,5,7]).

-endif.

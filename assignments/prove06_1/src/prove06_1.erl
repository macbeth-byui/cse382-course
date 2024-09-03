% CSE 382 Prove 6 - Problem Set 1 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit

-module(prove06_1).
-export([combine_accounts/2, combine_functions/2]).

% Problem 1.1


% Problem 1.3


% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

combine_accounts_1_test() ->
    A1 = {"Bob", "1234", 100},
    A2 = {"Tim", "2424", 600},
    A3 = {"Sue", "5851", 500},
    {"Tim", "2424", 1200} = combine_accounts(combine_accounts(A1, A2), A3),
    {"Tim", "2424", 1200} = combine_accounts(A1,combine_accounts(A2, A3)).

combine_accounts_2_test() ->
    % Problem 1.2
    % Using the built-in foldl function in Erlang, combine all 5 accounts below
    A1 = {"Bob", "1234", 100},
    A2 = {"Tim", "2424", 600},
    A3 = {"Sue", "5851", 500},
    A4 = {"Edward", "8879", 3000},
    A5 = {"George", "5546", 50},
    {"Edward", "8879", 4250} = todo.

combine_functions_1_test() ->
    F1 = fun string:to_upper/1,
    F2 = fun string:trim/1,
    F3 = fun string:reverse/1,
    Combined_F1 = combine_functions(combine_functions(F1, F2), F3),
    Combined_F2 = combine_functions(F1, combine_functions(F2, F3)),
    "FEDCBA" = Combined_F1("   abCDef  "),
    "FEDCBA" = Combined_F2("   abCDef  ").

combine_functions_2_test() ->
    % Problem 1.4
    % Using the built-in foldl function in Erlang, combine all 3 functions below
    F1 = fun string:to_upper/1,
    F2 = fun string:trim/1,
    F3 = fun string:reverse/1,
    All_Combined = todo,
    "FEDCBA" = All_Combined("   abCDef  ").

-endif.

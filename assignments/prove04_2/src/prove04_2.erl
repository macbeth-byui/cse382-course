% CSE 382 Prove 4 - Problem Set 2 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit

-module(prove04_2).
-export([filter/2, filter_listcomp/2]).

% Problem 2.1


% Problem 2.2



% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

filter_1_test() ->
    Numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
    Even_Lambda = fun (X) -> X rem 2 == 0 end,
    Even_Numbers = filter(Even_Lambda, Numbers),
    [2, 4, 6, 8, 10] = Even_Numbers.

filter_listcomp_test() ->
    Numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
    Even_Lambda = fun (X) -> X rem 2 == 0 end,
    Even_Numbers = filter_listcomp(Even_Lambda, Numbers),
    [2, 4, 6, 8, 10] = Even_Numbers.

filter_2_test() ->
    % Problem 2.3
    % Call the filter function to get the temperatures in Celsius
    % that are in liquid form.
    Temps = [-10, 20, -15, 110, 80],
    [20, 80] = todo.

filter_3_test() ->
    % Problem 2.4
    % Call the filter function to get the ERROR messages only
    Messages = ["INFO: Reading the data",
        "INFO: Validating the data",
        "WARNING: Missing meta data",
        "ERROR: Column 2 Row 5 expected number",
        "WARNING: Row 12 unexpected line terminator",
        "INFO: Validation complete",
        "INFO: Processing data",
        "ERROR: Command 7 unknown",
        "INFO: Processing complete"],
    
    ["ERROR: Column 2 Row 5 expected number", "ERROR: Command 7 unknown"] = todo.

-endif.

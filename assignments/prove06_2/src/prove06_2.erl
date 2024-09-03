% CSE 382 Prove 6 - Problem Set 2 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit

-module(prove06_2).
-export([cut_half/1, maybe_unit/1, maybe_bind/2, check_mixed_case/1, 
    check_number_exists/1, check_length/1, result_unit/0, result_bind/3]).

% Problem 2.1



% Problem 2.2
% The following 3 check functions already return the Result Monad type as described in the instructions.
% Implement the result_unit and result_bind functions.  
check_mixed_case(Password) -> 
    UpperExists = lists:foldl(fun (Letter, Result) -> Result or ((Letter >= 65) and (Letter =< 90)) end, false, Password),
    LowerExists = lists:foldl(fun (Letter, Result) -> Result or ((Letter >= 97) and (Letter =< 172)) end, false, Password),
    if
        UpperExists and LowerExists -> {ok};
        UpperExists -> {error,["Must have at least one lower case letter."]};
        LowerExists -> {error,["Must have at least one upper case letter."]};
        true -> {error,["Must have at least one upper case and one lower case letter."]}
    end.

check_number_exists(Password) -> 
    NumberExists = lists:foldl(fun (Letter, Result) -> Result or ((Letter >= 48) and (Letter =< 57)) end, false, Password),
    if
        NumberExists -> {ok};
        true -> {error,["Must have at least one number."]}
    end.

check_length(Password) -> 
    if
        length(Password) < 8 -> {error,["Must be at least 8 characters long."]};
        true -> {ok}
    end.




% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

maybe_test() ->
    {ok,4} = cut_half(8),
    {fail} = cut_half(5),
    {fail} = cut_half(-1),
    {ok,50} = maybe_bind(maybe_bind(maybe_unit(200), fun cut_half/1), fun cut_half/1).

result_1_test() ->
    Password1 = "simple",
    Result1 = result_bind(result_bind(result_bind(result_unit(), Password1, fun check_mixed_case/1), Password1, fun check_number_exists/1), Password1, fun check_length/1),
    {error, ["Must be at least 8 characters long.", "Must have at least one number.", "Must have at least one upper case letter."]} = Result1,

    Password2 = "mostlygood23",
    Result2 = result_bind(result_bind(result_bind(result_unit(), Password2, fun check_mixed_case/1), Password2, fun check_number_exists/1), Password2, fun check_length/1),
    {error, ["Must have at least one upper case letter."]} = Result2,

    Password3 = "GoodPassword42",
    Result3 = result_bind(result_bind(result_bind(result_unit(), Password3, fun check_mixed_case/1), Password3, fun check_number_exists/1), Password3, fun check_length/1),
    {ok} = Result3.
    

result_2_test() ->
    % Problem 2.3
    % Rewrite the expressions assigned to Result1, Result2, and Result3 from the test case above so that you using 
    % the built-in foldr function in Erlang to call the bind functions.
    Password1 = "simple",
    Result1 = todo,
    {error, ["Must be at least 8 characters long.", "Must have at least one number.", "Must have at least one upper case letter."]} = Result1,

    Password2 = "mostlygood23",
    Result2 = todo,
    {error, ["Must have at least one upper case letter."]} = Result2,

    Password3 = "GoodPassword42",
    Result3 = todo,
    {ok} = Result3.

-endif.

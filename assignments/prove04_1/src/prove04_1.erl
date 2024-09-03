% CSE 382 Prove 4 - Problem Set 1 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit

-module(prove04_1).
-export([map/2, map_listcomp/2]).

% Problem 1.1


% Problem 1.3



% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

map_1_test() ->
    Data_In = [1, 2, 3, 4, 5],
    In_to_Cm_Lambda = fun (X) -> X * 2.54 end,
    Data_Cm = map(In_to_Cm_Lambda, Data_In),
    [2.54, 5.08, 7.62, 10.16, 12.7] = Data_Cm.

map_2_test() ->
    % Problem 1.2
    % Update Cipher_Lambda as described in the instructions
    Password = "PASSWORD",
    Cipher_Lambda = todo,
    Encrypted = map(Cipher_Lambda, Password),
    "QBTTXPSE" = Encrypted.

map_listcomp_test() ->
    Data_In = [1, 2, 3, 4, 5],
    In_to_Cm_Lambda = fun (X) -> X * 2.54 end,
    Data_Cm = map_listcomp(In_to_Cm_Lambda, Data_In),
    [2.54, 5.08, 7.62, 10.16, 12.7] = Data_Cm.


-endif.

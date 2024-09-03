% CSE 382 Prove 11 - Problem Set 1 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit

-module(prove11_1).
-export([add/2]).

% Problem 1.1


% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

add_test() ->
    T1 = add("day",nil),
    #{100 := #{97 := #{121 := #{done := nil}}}} = T1,

    T2 = add("date",T1),
    #{100 := #{97 := #{116 := #{101 := #{done := nil}},121 := #{done := nil}}}} = T2,

    T3 = add("days",T2),
    #{100 := #{97 := #{116 := #{101 := #{done := nil}}, 121 := #{115 := #{done := nil},done := nil}}}} = T3,

    T4 = add("",T3),
    #{100 := #{97 := #{116 := #{101 := #{done := nil}}, 121 := #{115 := #{done := nil},done := nil}}}, done := nil} = T4,

    T5 = add("cow",T4),
    #{99 := #{111 := #{119 := #{done := nil}}}, 100 := #{97 := #{116 := #{101 := #{done := nil}}, 121 := #{115 := #{done := nil},done := nil}}}, done := nil} = T5,

    T6 = add("cold",T5),
    #{99 := #{111 := #{108 := #{100 := #{done := nil}},119 := #{done := nil}}}, 100 := #{97 := #{116 := #{101 := #{done := nil}}, 121 := #{115 := #{done := nil},done := nil}}}, done := nil} = T6,

    T7 = add("dog",T6),
    #{99 := #{111 := #{108 := #{100 := #{done := nil}},119 := #{done := nil}}}, 100 := #{97 := #{116 := #{101 := #{done := nil}}, 121 := #{115 := #{done := nil},done := nil}}, 111 := #{103 := #{done := nil}}}, done := nil} = T7.


-endif.

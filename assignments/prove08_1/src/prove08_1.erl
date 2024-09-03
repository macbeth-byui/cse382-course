% CSE 382 Prove 8 - Problem Set 1 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit

-module(prove08_1).
-export([add/2, contains/2]).

% Problem 1.1


% Problem 1.2


% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

add_test() ->
    L1 = add(5, nil),
    {5,nil,nil} = L1,

    L2 = add(3, L1),
    {5,{3,nil,nil},nil} = L2,

    L3 = add(7, L2),
    {5,{3,nil,nil},{7,nil,nil}} = L3,

    L4 = add(4, L3),
    {5,{3,nil,{4,nil,nil}},{7,nil,nil}} = L4,

    L5 = add(2, L4),
    {5,{3,{2,nil,nil},{4,nil,nil}},{7,nil,nil}} = L5,

    L6 = add(6, L5),
    {5,{3,{2,nil,nil},{4,nil,nil}},{7,{6,nil,nil},nil}} = L6,

    L7 = add(8, L6),
    {5,{3,{2,nil,nil},{4,nil,nil}},{7,{6,nil,nil},{8,nil,nil}}} = L7,

    L8 = add(5, L7), % Check a duplicate value
    {5,{3,{2,nil,nil},{4,nil,nil}},{7,{6,nil,nil},{8,nil,nil}}} = L8.

contains_test() ->
    L1 = add(5, nil),
    {5,nil,nil} = L1,

    L2 = add(3, L1),
    {5,{3,nil,nil},nil} = L2,

    L3 = add(7, L2),
    {5,{3,nil,nil},{7,nil,nil}} = L3,

    L4 = add(4, L3),
    {5,{3,nil,{4,nil,nil}},{7,nil,nil}} = L4,

    L5 = add(2, L4),
    {5,{3,{2,nil,nil},{4,nil,nil}},{7,nil,nil}} = L5,

    L6 = add(6, L5),
    {5,{3,{2,nil,nil},{4,nil,nil}},{7,{6,nil,nil},nil}} = L6,

    L7 = add(8, L6),
    {5,{3,{2,nil,nil},{4,nil,nil}},{7,{6,nil,nil},{8,nil,nil}}} = L7,

    false = contains(1, L7),
    true = contains(2, L7),
    true = contains(3, L7),
    true = contains(4, L7),
    true = contains(5, L7),
    true = contains(6, L7),
    true = contains(7, L7),
    true = contains(8, L7),
    false = contains(9, L7).

-endif.

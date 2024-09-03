% CSE 382 Prove 8 - Problem Set 2 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit

-module(prove08_2).
-export([add_rbt/2, balance/1, contains_rbt/2]).

% Problem 2.1
% Finish the add_rbt by implementing add_rbt_ and the remaining scenarios
% for the balance function per the instructions.
add_rbt(New_Value, Tree) ->
    {_Color, Value, Left, Right} = add_rbt_(New_Value, Tree),
    {black, Value, Left, Right}.  % Change the root so its always black

add_rbt_(New_Value, Node) -> todo.

balance({black,Z,{red,X,A,{red,Y,B,C}},D}) -> {red,Y,{black,X,A,B},{black,Z,C,D}};

balance(Node) -> Node.

% Problem 2.2


% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

add_rbt_test() ->
    L1 = add_rbt(5, nil),
    {black,5,nil,nil} = L1,

    L2 = add_rbt(3, L1),
    {black,5,{red,3,nil,nil},nil} = L2,

    L3 = add_rbt(7, L2),
    {black,5,{red,3,nil,nil},{red,7,nil,nil}} = L3,

    L4 = add_rbt(4, L3),
    {black,4,{black,3,nil,nil},{black,5,nil,{red,7,nil,nil}}} = L4,
    
    L5 = add_rbt(2, L4),
    {black,4,{black,3,{red,2,nil,nil},nil},{black,5,nil,{red,7,nil,nil}}} = L5,

    L6 = add_rbt(6, L5),
    {black,4,{black,3,{red,2,nil,nil},nil},{red,6,{black,5,nil,nil},{black,7,nil,nil}}} = L6,

    L7 = add_rbt(8, L6),
    {black,4,{black,3,{red,2,nil,nil},nil},{red,6,{black,5,nil,nil},{black,7,nil,{red,8,nil,nil}}}} = L7,

    L8 = add_rbt(10, L7), 
    {black,6,{black,4,{black,3,{red,2,nil,nil},nil},{black,5,nil,nil}},{black,8,{black,7,nil,nil},{black,10,nil,nil}}} = L8,

    L9 = add_rbt(1, L8), 
    {black,6,{black,4,{red,2,{black,1,nil,nil},{black,3,nil,nil}},{black,5,nil,nil}},{black,8,{black,7,nil,nil},{black,10,nil,nil}}} = L9,

    L10 = add_rbt(0, L9), 
    {black,6,{black,4,{red,2,{black,1,{red,0,nil,nil},nil},{black,3,nil,nil}},{black,5,nil,nil}},{black,8,{black,7,nil,nil},{black,10,nil,nil}}} = L10.

contains_rbt_test() ->
    L1 = add_rbt(5, nil),
    {black,5,nil,nil} = L1,

    L2 = add_rbt(3, L1),
    {black,5,{red,3,nil,nil},nil} = L2,

    L3 = add_rbt(7, L2),
    {black,5,{red,3,nil,nil},{red,7,nil,nil}} = L3,

    L4 = add_rbt(4, L3),
    {black,4,{black,3,nil,nil},{black,5,nil,{red,7,nil,nil}}} = L4,
    
    L5 = add_rbt(2, L4),
    {black,4,{black,3,{red,2,nil,nil},nil},{black,5,nil,{red,7,nil,nil}}} = L5,

    L6 = add_rbt(6, L5),
    {black,4,{black,3,{red,2,nil,nil},nil},{red,6,{black,5,nil,nil},{black,7,nil,nil}}} = L6,

    L7 = add_rbt(8, L6),
    {black,4,{black,3,{red,2,nil,nil},nil},{red,6,{black,5,nil,nil},{black,7,nil,{red,8,nil,nil}}}} = L7,

    L8 = add_rbt(10, L7), 
    {black,6,{black,4,{black,3,{red,2,nil,nil},nil},{black,5,nil,nil}},{black,8,{black,7,nil,nil},{black,10,nil,nil}}} = L8,

    L9 = add_rbt(1, L8), 
    {black,6,{black,4,{red,2,{black,1,nil,nil},{black,3,nil,nil}},{black,5,nil,nil}},{black,8,{black,7,nil,nil},{black,10,nil,nil}}} = L9,

    L10 = add_rbt(0, L9), 
    {black,6,{black,4,{red,2,{black,1,{red,0,nil,nil},nil},{black,3,nil,nil}},{black,5,nil,nil}},{black,8,{black,7,nil,nil},{black,10,nil,nil}}} = L10,

    false = contains_rbt(-1, L10),
    true = contains_rbt(2, L10),
    true = contains_rbt(3, L10),
    true = contains_rbt(4, L10),
    true = contains_rbt(5, L10),
    true = contains_rbt(6, L10),
    true = contains_rbt(7, L10),
    true = contains_rbt(8, L10),
    false = contains_rbt(9, L10).

-endif.

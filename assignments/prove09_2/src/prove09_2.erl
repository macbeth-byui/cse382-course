% CSE 382 Prove 9 - Problem Set 2 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit

-module(prove09_2).
-export([rank/1, make/3, merge/2, remove_min/1, insert/2]).

% Problem 2.1
% Modify the make function below to perform the swap per the instructions.  Continue
% to use the rank function provided.
rank(nil) -> 0;
rank({Rank, _, _, _}) -> Rank.

make(Value, Left, Right) ->
    {rank(Right) + 1, Value, Left, Right}.

% Problem 2.2
 

% The functions remove_min and insert have been implemented already.  Note that the insert
% function is simplified from the previous problem set to use the merge function.

remove_min(nil) -> nil;
remove_min({_,_,Left,Right}) -> merge(Left, Right).

insert(New_Value, Node) -> merge(make(New_Value, nil, nil),Node).

% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

insert_test() ->
    H = lists:foldl(fun insert/2, nil, [10,15,20,5,12,17,19,20,21,13,8,1]),
    {1,1,{2,5,{2,10,{1,15,nil,nil},{1,20,nil,nil}},{1,8,{2,12,{2,19,{1,20,nil,nil},{1,21,nil,nil}},{1,13,{1,17,nil,nil},nil}},nil}},nil} = H.

remove_min_test() ->
    H1 = lists:foldl(fun insert/2, nil, [10,15,20,5,12,17,19]),    
    {3,5,{2,10,{1,15,nil,nil},{1,20,nil,nil}},{2,12,{1,17,nil,nil},{1,19,nil,nil}}} = H1,

    H2 = remove_min(H1),
    {2,10,{2,12,{1,17,nil,nil},{1,19,{1,20,nil,nil},nil}},{1,15,nil,nil}} = H2,

    H3 = remove_min(H2),
    {2,12,{1,17,nil,nil},{1,15,{1,19,{1,20,nil,nil},nil},nil}} = H3,

    H4 = remove_min(H3),
    {2,15,{1,19,{1,20,nil,nil},nil},{1,17,nil,nil}} = H4,

    H5 = remove_min(H4),
    {1,17,{1,19,{1,20,nil,nil},nil},nil} = H5,

    H6 = remove_min(H5),
    {1,19,{1,20,nil,nil},nil} = H6,

    H7 = remove_min(H6),
    {1,20,nil,nil} = H7,

    H8 = remove_min(H7),
    nil = H8.

-endif.

% CSE 382 Prove 9 - Problem Set 1 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit

-module(prove09_1).
-export([rank/1, make/3, insert/2]).

% Problem 1.1
% Using the rank and make functions below, implement the insert function
% per the instructions.  
rank(nil) -> 0;
rank({Rank, _, _, _}) -> Rank.

make(Value, Left, Right) -> 
    {rank(Right) + 1, Value, Left, Right}.  



% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

insert_test() ->
    H = lists:foldl(fun insert/2, nil, [10,15,20,5,12,17,19,20,21,13,8,1]),
    {12,1,nil,
     {11,5,nil,
      {10,8,nil,
       {9,10,nil,
        {8,12,nil,
         {7,13,nil,
          {6,15,nil,
           {5,17,nil,
            {4,19,nil,
             {3,20,nil,
              {2,20,nil,
               {1,21,nil,nil}}}}}}}}}}}} = H.

-endif.

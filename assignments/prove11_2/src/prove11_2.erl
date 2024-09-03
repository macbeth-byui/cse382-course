% CSE 382 Prove 11 - Problem Set 2 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit

-module(prove11_2).
-export([add/2, search/2, count/1]).

% Problem 1.1
% We will need the add function that you wrote in problem set 1.
% Copy and paste the code below:



% Problem 2.1


% Problem 2.2



% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

search_test() ->
    Trie = lists:foldl(fun add/2, nil, ["day","date","days","","cow","cold","dog"]),
    true = search("day",Trie),
    true = search("date",Trie),
    true = search("days",Trie),
    false = search("da",Trie),
    false = search("dates",Trie),
    true = search("",Trie),
    true = search("cow",Trie),
    true = search("cold",Trie),
    true = search("dog",Trie),
    false = search("colt",Trie),
    false = search("pig",Trie),
    false = search("bob",nil). % Test with an empty Trie

count_test() ->
    Trie = lists:foldl(fun add/2, nil, ["day","date","days","","cow","cold","dog"]),
    7 = count(Trie),
    0 = count(nil).

-endif.

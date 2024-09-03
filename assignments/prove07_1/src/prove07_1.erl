% CSE 382 Prove 7 - Problem Set 1 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit

-module(prove07_1).
-export([range/3, first_word/1, words/1]).

% Problem 1.1
% Modify the code below to add the Step parameter per the instructions.
range(Start, Stop) ->
    fun () when Start =< Stop -> {Start, range(Start+1, Stop)};
        () -> {undefined, done} end.

% Problem 1.2
% Implement the words stream function using the first_word function provided below
first_word(Text) -> 
    Result = string:split(Text," "),
    case Result of
        [Word,Rest] -> {Word, Rest};
        [Word] -> {Word, ""}
    end.



% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

range_test() ->
    Stream1 = range(1,10,3),
    {1, Stream2} = Stream1(),
    {4, Stream3} = Stream2(),
    {7, Stream4} = Stream3(),
    {10, Stream5} = Stream4(),
    {undefined, done} = Stream5(),

    Stream6 = range(10,1,-4),
    {10, Stream7} = Stream6(),
    {6, Stream8} = Stream7(),
    {2, Stream9} = Stream8(),
    {undefined, done} = Stream9(),

    Stream10 = range(1,1,0),
    {undefined, done} = Stream10().

words_test() ->
    Stream11 = words("The cow jumped over the moon"),
    {"The", Stream12} = Stream11(),
    {"cow", Stream13} = Stream12(),
    {"jumped", Stream14} = Stream13(),
    {"over", Stream15} = Stream14(),
    {"the", Stream16} = Stream15(),
    {"moon", Stream17} = Stream16(),
    {undefined, done} = Stream17(),

    Stream18 = words("Happy"),
    {"Happy", Stream19} = Stream18(),
    {undefined, done} = Stream19(),

    Stream20 = words(""),
    {undefined, done} = Stream20().

-endif.

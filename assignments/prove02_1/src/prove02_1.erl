% CSE 382 Prove 2 - Problem Set 1 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit
% To run the Start function after compiling and testing:
%    rebar3 shell
%    prove02_1:start().
% To exit the shell when done:
%    q().

-module(prove02_1).
-export([select_magic_number/2, check_guess/2, process_guess/5, play_game/4, start/0]).

% Problem 1.1


% Problem 1.2


% Problem 1.4


% Problem 1.5
% Update the play_game function to do the next thing if an invalid guess is 
% made (e.g. a string instead of a integer) or if a valid guess is made.
play_game(Magic, Curr_Guesses, Max_Guesses, History) when Curr_Guesses < Max_Guesses ->
    io:format("~nGuess #~p~n",[Curr_Guesses+1]),
    case io:fread("> ","~d") of
        {error, _} -> 
            io:format("Invalid entry.  Try again.~n"),
            todo;
        {ok, [Guess]} -> todo
    end;
play_game(Magic, _, _, History) -> 
    io:format("All out of Guesses.  The answer was ~p~n",[Magic]),
    io:format("Here were your guesses (last to first): ~p~n",[History]).


% The start function is written for you to play the game.  The game
% is setup to pick the correct number between 1 and 100 within
% ten guesses.
start() ->
    io:format("Welcome to the Number Guessing Game!~n"),
    io:format("You will pick a number between 1 and 100.~n"),
    io:format("You have 10 guesses ... Let's Play!~n"),
    Magic = select_magic_number(1, 100),
    play_game(Magic, 0, 10, []).

% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

select_magic_number_test() ->
    Magic1 = select_magic_number(100,110),
    true = (Magic1 >= 100) and (Magic1 =< 110),
    Magic2 = select_magic_number(-110,-100),
    true = (Magic2 >= -110) and (Magic2 =< -100),
    42 = select_magic_number(42,42).

check_guess_test() ->
    % Problem 2.3
    % Write 3 tests in this function to verify that
    % check_guess works properly.  The 3 tests should
    % cover all the possible values returned from
    % the function.
    todo.



-endif.

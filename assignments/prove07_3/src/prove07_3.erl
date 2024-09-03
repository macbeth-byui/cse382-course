% CSE 382 Prove 7 - Problem Set 3 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit

-module(prove07_3).
-export([handle_server/0, start_server/0, handle_running_avg_server/2, start_running_avg_server/0, send_to_server/3]).

% Problem 3.1
% Modify the handle_server as described in the instructions
handle_server() ->
    receive
        {Client_PID, echo, {Text}} -> Client_PID ! {Text};
        {Client_PID, add, {X, Y}} -> Client_PID ! {X+Y}
    end,
    handle_server().

% The start_server function is written for you
start_server() ->
    spawn(prove07_3, handle_server, []).

% Problem 3.2

    
% The start_running_avg_server is written for you
start_running_avg_server() ->
    spawn(prove07_3, handle_running_avg_server, [0,0]).

% The following function is used to send
% commands to your servers for problems 3.1 and 3.2
send_to_server(Server_PID, Command, Params) ->
    Server_PID ! {self(), Command, Params},
    receive
        {Response} -> Response
    end.


% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

server_test() ->
    Server1_PID = start_server(),
    "Hello" = send_to_server(Server1_PID, echo, {"Hello"}),
    21 = send_to_server(Server1_PID, add, {13, 8}),
    25.0 = send_to_server(Server1_PID, avg, {[10,20,30,40]}).

avg_server_test() ->
    Server2_PID = start_running_avg_server(),
    10.0 = send_to_server(Server2_PID, add, {10}),    % 10
    15.0 = send_to_server(Server2_PID, add, {20}),    % 10, 20
    20.0 = send_to_server(Server2_PID, add, {30}),    % 10, 20, 30
    25.0 = send_to_server(Server2_PID, remove, {10}), % 20, 30
    30.0 = send_to_server(Server2_PID, add, {40}),    % 20, 30, 40
    30.0 = send_to_server(Server2_PID, remove, {30}), % 20, 40
    40.0 = send_to_server(Server2_PID, add, {60}),    % 20, 40, 60
    42.5 = send_to_server(Server2_PID, add, {50}),    % 20, 40, 60, 50
    42.5 = send_to_server(Server2_PID, display, {}).


-endif.

% CSE 382 Prove 07

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed in a public file sharing site.

% Instructions: Use this template file for this prove assignment.
% The details of what to do for each problem are found in 
% the reading. IMPORTANT: Comment out code that is not 
% running properly.  The `test_ps#` functions should return `pass`.

-module(prove07).
-export([test_ps1/0, test_ps2/0, test_ps3/0, handle_server/0]).

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


% Problem 2.1
% The iter, next, value, and lambda functions for the
% fixed_iterator Monad is written below.
% Complete the collect function by providing the
% implementaiton of the arity 2 function.
iter(Stream) -> {undefined, Stream}.

next({_,done}) -> {undefined, done};
next({_,Lambda}) -> Lambda().

value({Value,_Lambda}) -> Value.
lambda({_Value,Lambda}) -> Lambda.

collect(Stream) -> collect(Stream, []).
collect(Stream, Result) -> add_your_code_here.


% Problem 3.1
% Modify the handle_server as described in the instructions
handle_server() ->
    receive
        {Client_PID, echo, {Text}} -> Client_PID ! {Text};
        {Client_PID, add, {X, Y}} -> Client_PID ! {X+Y}
    end,
    handle_server().

start_server() ->
    spawn(prove07, handle_server, []).

% Problem 3.2


% The following function is used to send
% commands to your servers for problems 3.1 and 3.2
send_to_server(Server_PID, Command, Params) ->
    Server_PID ! {self(), Command, Params},
    receive
        {Response} -> Response
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test Code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test code for problem set 1
test_ps1() ->

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 1.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    %Stream1 = range(1,10,3),
    %{1, Stream2} = Stream1(),
    %{4, Stream3} = Stream2(),
    %{7, Stream4} = Stream3(),
    %{10, Stream5} = Stream4(),
    %{undefined, done} = Stream5(),

    %Stream6 = range(10,1,-4),
    %{10, Stream7} = Stream6(),
    %{6, Stream8} = Stream7(),
    %{2, Stream9} = Stream8(),
    %{undefined, done} = Stream9(), 

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 1.2
    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    %Stream10 = words("The cow jumped over the moon"),
    %{"The", Stream11} = Stream10(),
    %{"cow", Stream12} = Stream11(),
    %{"jumped", Stream13} = Stream12(),
    %{"over", Stream14} = Stream13(),
    %{"the", Stream15} = Stream14(),
    %{"moon", Stream16} = Stream15(),
    %{undefined, done} = Stream16(),

    %Stream17 = words("Happy"),
    %{"Happy", Stream18} = Stream17(),
    %{undefined, done} = Stream18(),

    %Stream19 = words(""),
    %{undefined, done} = Stream19(),

    pass.

% Test code for problem set 2
test_ps2() ->

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 2.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    %[2,4,6,8,10] = collect(iter(range(2,10,2))),
    
    %["The","cow","jumped","over","the","moon"] = collect(iter(words("The cow jumped over the moon"))),

    pass.

% Test code for problem set 3
test_ps3() ->

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 3.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    %Server1_PID = start_server(),
    %"Hello" = send_to_server(Server1_PID, echo, {"Hello"}),
    %21 = send_to_server(Server1_PID, add, {13, 8}),
    %25.0 = send_to_server(Server1_PID, avg, {[10,20,30,40]}),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 3.2
    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    %Server2_PID = start_running_avg_server(),
    %10.0 = send_to_server(Server2_PID, add, {10}),    % 10
    %15.0 = send_to_server(Server2_PID, add, {20}),    % 10, 20
    %20.0 = send_to_server(Server2_PID, add, {30}),    % 10, 20, 30
    %25.0 = send_to_server(Server2_PID, remove, {10}), % 20, 30
    %30.0 = send_to_server(Server2_PID, add, {40}),    % 20, 30, 40
    %30.0 = send_to_server(Server2_PID, remove, {30}), % 20, 40
    %40.0 = send_to_server(Server2_PID, add, {60}),    % 20, 40, 60
    %42.5 = send_to_server(Server2_PID, add, {50}),    % 20, 40, 60, 50

    pass.
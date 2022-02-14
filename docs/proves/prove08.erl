% CSE 382 Prove 08

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed in a public file sharing site.

% Instructions: Use this template file for this prove assignment.
% The details of what to do for each problem are found in 
% the reading. IMPORTANT: Comment out code that is not 
% running properly.  The `test_ps#` functions should return `pass`.

-module(prove08).
-export([test_ps1/0, test_ps2/0, test_ps3/0]).

% Problem 1.1


% Problem 1.2


% Problem 2.1
% Finish the add_rbt by implementing add_rbt_ and the remaining scenarios
% for the balance function per the instructions.
add_rbt(Tree, New_Value) -> 
    {_Color, Value, Left, Right} = add_rbt_(Tree, New_Value),
    {black, Value, Left, Right}.  % Change the root so its always black

add_rbt_(_Tree, _New_Value) -> implement_this_function.

balance({black,Z,{red,X,A,{red,Y,B,C}},D}) -> {red,Y,{black,X,A,B},{black,Z,C,D}};

balance(Node) -> Node.

% Problem 2.2


% The following functions are fully implemented for use by the Problem 3.1 test
% code.
start_perf() ->
    eprof:start(),
    eprof:start_profiling([self()]).

stop_perf(Title) ->
    io:format("Perf (~p): ~n",[Title]),
    eprof:stop_profiling(),
    eprof:analyze(total),
    eprof:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test Code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test code for problem set 1
test_ps1() ->

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 1.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    %L1 = add(nil, 5),
    %{5,nil,nil} = L1,

    %L2 = add(L1, 3),
    %{5,{3,nil,nil},nil} = L2,

    %L3 = add(L2, 7),
    %{5,{3,nil,nil},{7,nil,nil}} = L3,

    %L4 = add(L3, 4),
    %{5,{3,nil,{4,nil,nil}},{7,nil,nil}} = L4,

    %L5 = add(L4, 2),
    %{5,{3,{2,nil,nil},{4,nil,nil}},{7,nil,nil}} = L5,

    %L6 = add(L5, 6),
    %{5,{3,{2,nil,nil},{4,nil,nil}},{7,{6,nil,nil},nil}} = L6,

    %L7 = add(L6,8),
    %{5,{3,{2,nil,nil},{4,nil,nil}},{7,{6,nil,nil},{8,nil,nil}}} = L7,

    %L8 = add(L7, 5), % Check a duplicate value
    %{5,{3,{2,nil,nil},{4,nil,nil}},{7,{6,nil,nil},{8,nil,nil}}} = L8,

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 1.2
    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    %false = contains(L8, 1),
    %true = contains(L8, 2),
    %true = contains(L8, 3),
    %true = contains(L8, 4),
    %true = contains(L8, 5),
    %true = contains(L8, 6),
    %true = contains(L8, 7),
    %true = contains(L8, 8),
    %false = contains(L8, 9),

    pass.

% Test code for problem set 2
test_ps2() ->

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 2.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    %L1 = add_rbt(nil, 5),
    %{black,5,nil,nil} = L1,

    %L2 = add_rbt(L1, 3),
    %{black,5,{red,3,nil,nil},nil} = L2,

    %L3 = add_rbt(L2, 7),
    %{black,5,{red,3,nil,nil},{red,7,nil,nil}} = L3,

    %L4 = add_rbt(L3, 4),
    %{black,4,{black,3,nil,nil},{black,5,nil,{red,7,nil,nil}}} = L4,
    
    %L5 = add_rbt(L4, 2),
    %{black,4,{black,3,{red,2,nil,nil},nil},{black,5,nil,{red,7,nil,nil}}} = L5,

    %L6 = add_rbt(L5, 6),
    %{black,4,{black,3,{red,2,nil,nil},nil},{red,6,{black,5,nil,nil},{black,7,nil,nil}}} = L6,

    %L7 = add_rbt(L6,8),
    %{black,4,{black,3,{red,2,nil,nil},nil},{red,6,{black,5,nil,nil},{black,7,nil,{red,8,nil,nil}}}} = L7,

    %L8 = add_rbt(L7, 10), 
    %{black,6,{black,4,{black,3,{red,2,nil,nil},nil},{black,5,nil,nil}},{black,8,{black,7,nil,nil},{black,10,nil,nil}}} = L8,

    %L9 = add_rbt(L8, 1), 
    %{black,6,{black,4,{red,2,{black,1,nil,nil},{black,3,nil,nil}},{black,5,nil,nil}},{black,8,{black,7,nil,nil},{black,10,nil,nil}}} = L9,

    %L10 = add_rbt(L9, 0), 
    %{black,6,{black,4,{red,2,{black,1,{red,0,nil,nil},nil},{black,3,nil,nil}},{black,5,nil,nil}},{black,8,{black,7,nil,nil},{black,10,nil,nil}}} = L10,

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 2.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    %false = contains_rbt(L8, 1),
    %true = contains_rbt(L8, 2),
    %true = contains_rbt(L8, 3),
    %true = contains_rbt(L8, 4),
    %true = contains_rbt(L8, 5),
    %true = contains_rbt(L8, 6),
    %true = contains_rbt(L8, 7),
    %true = contains_rbt(L8, 8),
    %false = contains_rbt(L8, 9),

    pass.

% Test code for problem set 3
test_ps3() ->

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 3.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Write test code to compare performane of the binary search tree
    % and the red black tree per the instructions.
    
    
    % Report your analysis of the performance results (as code comments) below:

    pass.
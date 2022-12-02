% CSE 382 Prove 08

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site.

% Instructions: Use this template file for this prove assignment.
% The details of what to do for each problem are found in 
% the reading. IMPORTANT: Comment out code that is not 
% running properly.  The `test_ps#` functions should return `ok`.
% When writing tests use the `expected_result` = `actual result` format.

-module(prove08).
-export([test_ps1/0, test_ps2/0, test_ps3/0]).

% Problem 1.1


% Problem 1.2


% Problem 2.1
% Finish the add_rbt by implementing add_rbt_ and the remaining scenarios
% for the balance function (scenario 1 is already implemented) per the instructions.
add_rbt(New_Value, Tree) -> 
    {_Color, Value, Left, Right} = add_rbt_(New_Value, Tree),
    {black, Value, Left, Right}.  % Change the root so its always black

add_rbt_(New_Value, Node) -> implement_code_here.

balance({black,Z,{red,X,A,{red,Y,B,C}},D}) -> {red,Y,{black,X,A,B},{black,Z,C,D}};

balance(Node) -> Node.

% Problem 2.2


% The following functions are fully implemented for use by the Problem 3.1 test
% code.
start_perf() ->
    eprof:start_profiling([self()]).

stop_perf(Title) ->
    io:format("Perf (~p): ~n",[Title]),
    eprof:stop_profiling(),
    eprof:analyze(total).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test Code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test code for problem set 1
test_ps1() ->

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 1.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    %L1 = add(5, nil),
    %{5,nil,nil} = L1,

    %L2 = add(3, L1),
    %{5,{3,nil,nil},nil} = L2,

    %L3 = add(7, L2),
    %{5,{3,nil,nil},{7,nil,nil}} = L3,

    %L4 = add(4, L3),
    %{5,{3,nil,{4,nil,nil}},{7,nil,nil}} = L4,

    %L5 = add(2, L4),
    %{5,{3,{2,nil,nil},{4,nil,nil}},{7,nil,nil}} = L5,

    %L6 = add(6, L5),
    %{5,{3,{2,nil,nil},{4,nil,nil}},{7,{6,nil,nil},nil}} = L6,

    %L7 = add(8, L6),
    %{5,{3,{2,nil,nil},{4,nil,nil}},{7,{6,nil,nil},{8,nil,nil}}} = L7,

    %L8 = add(5, L7), % Check a duplicate value
    %{5,{3,{2,nil,nil},{4,nil,nil}},{7,{6,nil,nil},{8,nil,nil}}} = L8,

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 1.2
    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    %false = contains(1, L8),
    %true = contains(2, L8),
    %true = contains(3, L8),
    %true = contains(4, L8),
    %true = contains(5, L8),
    %true = contains(6, L8),
    %true = contains(7, L8),
    %true = contains(8, L8),
    %false = contains(9, L8),

    ok.

% Test code for problem set 2
test_ps2() ->

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 2.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    %L1 = add_rbt(5, nil),
    %{black,5,nil,nil} = L1,

    %L2 = add_rbt(3, L1),
    %{black,5,{red,3,nil,nil},nil} = L2,

    %L3 = add_rbt(7, L2),
    %{black,5,{red,3,nil,nil},{red,7,nil,nil}} = L3,

    %L4 = add_rbt(4, L3),
    %{black,4,{black,3,nil,nil},{black,5,nil,{red,7,nil,nil}}} = L4,
    
    %L5 = add_rbt(2, L4),
    %{black,4,{black,3,{red,2,nil,nil},nil},{black,5,nil,{red,7,nil,nil}}} = L5,

    %L6 = add_rbt(6, L5),
    %{black,4,{black,3,{red,2,nil,nil},nil},{red,6,{black,5,nil,nil},{black,7,nil,nil}}} = L6,

    %L7 = add_rbt(8, L6),
    %{black,4,{black,3,{red,2,nil,nil},nil},{red,6,{black,5,nil,nil},{black,7,nil,{red,8,nil,nil}}}} = L7,

    %L8 = add_rbt(10, L7), 
    %{black,6,{black,4,{black,3,{red,2,nil,nil},nil},{black,5,nil,nil}},{black,8,{black,7,nil,nil},{black,10,nil,nil}}} = L8,

    %L9 = add_rbt(1, L8), 
    %{black,6,{black,4,{red,2,{black,1,nil,nil},{black,3,nil,nil}},{black,5,nil,nil}},{black,8,{black,7,nil,nil},{black,10,nil,nil}}} = L9,

    %L10 = add_rbt(0, L9), 
    %{black,6,{black,4,{red,2,{black,1,{red,0,nil,nil},nil},{black,3,nil,nil}},{black,5,nil,nil}},{black,8,{black,7,nil,nil},{black,10,nil,nil}}} = L10,

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 2.2
    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    %false = contains_rbt(-1, L10),
    %true = contains_rbt(2, L10),
    %true = contains_rbt(3, L10),
    %true = contains_rbt(4, L10),
    %true = contains_rbt(5, L10),
    %true = contains_rbt(6, L10),
    %true = contains_rbt(7, L10),
    %true = contains_rbt(8, L10),
    %false = contains_rbt(9, L10),

    ok.

% Test code for problem set 3
test_ps3() ->

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 3.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Write test code to compare performance of the binary search tree
    % and the red black tree per the instructions.  You can use the List
    % variable below when performing the foldl functions.  Add your code
	% in between the start_perf and stop_perf function calls.
    
    %List = lists:seq(1,10000),
    %start_perf(),
    
    %stop_perf("add"),

    %start_perf(),
    
    %stop_perf("add_rbt"),

    %start_perf(),
    
    %stop_perf("contains"),

    %start_perf(),
    
    %stop_perf("contains_rbt"),

    % Observations (see instructions):  

    ok.
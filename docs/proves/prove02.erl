% CSE 382 Prove 02

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed in a public file sharing site.

% Instructions: Use this template file for this prove assignment.
% The details of what to do for each problem are found in 
% the reading. IMPORTANT: Comment out code that is not 
% running properly.  The `test_ps#` functions should return `pass`.

-module(prove02).
-export([test_ps1/0, test_ps2/0, test_ps3/0]).

% Problem 1.1


% Problem 1.2


% Problem 1.3


% Problem 1.4


% Problem 2.1


% Problem 2.2


% Problem 2.3


% Problem 2.4


% Problem 3.1
% Write the specifications and definitions below using comments:
%
% head:
%
% tail:
%
% removeLast:
%
% removeAt:
%

% Problem 3.2


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test Code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test code for problem set 1
test_ps1() ->

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 1.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    %[0,1,2,3,4] = prepend([1,2,3,4],0),
    %[0] = prepend([],0),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 1.2
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    %[1,2,3,4,5] = append([1,2,3,4],5),
    %[5] = append([],5),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 1.3
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    %1 = head([1,2,3,4]),
    %nil = head([]),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 1.4
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    %4 = tail([1,2,3,4]),
    %nil = tail([]),

    ok.

% Test code for problem set 2
test_ps2() ->

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 2.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    %[2,3,4] = remove_first([1,2,3,4]),
    %[] = remove_first([]),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 2.2
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    %[0,1,2,3,4] = insert_at([1,2,3,4], 0, 0),
    %[1,0,2,3,4] = insert_at([1,2,3,4], 0, 1),
    %[1,2,3,0,4] = insert_at([1,2,3,4], 0, 3),
    %[1,2,3,4,0] = insert_at([1,2,3,4], 0, 4),
    %[1,2,3,4] = insert_at([1,2,3,4], 0, 5),
    %[1,2,3,4] = insert_at([1,2,3,4], 0, -1),
    %[0] = insert_at([], 0, 0),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 2.3
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    %[1,2,3] = remove_last([1,2,3,4]),
    %[] = remove_last([]),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 2.4
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    %[2,3,4] = remove_at([1,2,3,4], 0),
    %[1,3,4] = remove_at([1,2,3,4], 1),
    %[1,2,3] = remove_at([1,2,3,4], 3),
    %[1,2,3,4] = remove_at([1,2,3,4], 4),
    %[1,2,3,4] = remove_at([1,2,3,4], -1),

    ok.

% Test code for problem set 3
test_ps3() ->

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 3.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    % There is no test code for problem 3.1.  

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 3.2
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    %[5,4,3,2,1] = backwards([1,2,3,4,5]),
    %[] = backwards([]),

    ok.


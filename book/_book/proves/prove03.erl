% CSE 382 Prove 03

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site.

% Instructions: Use this template file for this prove assignment.
% The details of what to do for each problem are found in 
% the reading. IMPORTANT: Comment out code that is not 
% running properly.  The `test_ps#` functions should return `ok`.
% When writing tests use the `expected_result` = `actual result` format.

-module(prove03).
-export([test_ps1/0, test_ps2/0, test_ps3/0]).

% Problem 1.1


% Problem 1.3


% Problem 2.1


% Problem 2.2



%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test Code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test code for problem set 1
test_ps1() ->

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 1.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    % Modify the test code below to convert a list of measurements
    %Data_In = [1, 2, 3, 4, 5],
    %In_to_Cm_Lambda = put_your_lambda_function_here,
    %Data_Cm = use_map_function_here,
    %[2.54, 5.08, 7.62, 10.16, 12.7] = Data_Cm,

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 1.2
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    % Modify the test code below to implement a simple cipher using map
	%Password = "PASSWORD",
    %Cipher_Lambda = put_your_lambda_function_here,
    %Encrypted = use_map_function_here,
    %"QBTTXPSE" = Encrypted,

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 1.3
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    % Write test code below to implement a simple cipher using map2



    ok.

% Test code for problem set 2
test_ps2() ->

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 2.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    % Modify the test code below to get the even numbers from a list using filter
    %Numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
    %Even_Lambda = put_your_lambda_function_here,
    %Even_Numbers = use_filter_function_here
    %[2, 4, 6, 8, 10] = Even_Numbers,

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 2.2
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    % Write test code below to get the even numbers from a list using filter2



    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 2.3
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    % Modify the test code to get liquid temperatures from a list using filter
	%[20, 80] = filter(put_your_lambda_function_here, [-10, 20, -15, 110, 80]),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 2.4
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    Messages = ["INFO: Reading the data",
        "INFO: Validating the data",
        "WARNING: Missing meta data",
        "ERROR: Column 2 Row 5 expected number",
        "WARNING: Row 12 unexpected line terminator",
        "INFO: Validation complete",
        "INFO: Processing data",
        "ERROR: Command 7 unknown",
        "INFO: Processing complete"],

    % Write test code using the Messages variable above to get only
    % ERROR messages using filter.


    
    ok.

% Test code for problem set 3
test_ps3() ->

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 3.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    % List of values to use in the demonstration
    Values = [1, 2, 3, 4],

    % Create the lambda functions
    G = put_your_lambda_function_here,
    H = put_your_lambda_function_here,

    % Left Side of Composition Property

    %[0,6,16,30] = put_your_code_here,

    % Right Side of Composition Property

    %[0,6,16,30] = put_your_code_here,

    ok.
% CSE 382 Prove 06

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed in a public file sharing site.

% Instructions: Use this template file for this prove assignment.
% The details of what to do for each problem are found in 
% the reading. IMPORTANT: Comment out code that is not 
% running properly.  The `test_ps#` functions should return `pass`.

-module(prove06).
-export([test_ps1/0, test_ps2/0, test_ps3/0]).

% Problem 1.1


% Problem 1.3


% Problem 2.1


% Problem 2.2
% The following 3 check functions already return the Result Monad type as described in the instructions.
% Implement the result_unit and result_bind functions.  
check_mixed_case(Password) -> 
    UpperExists = lists:foldl(fun (Letter, Result) -> Result or ((Letter >= 65) and (Letter =< 90)) end, false, Password),
    LowerExists = lists:foldl(fun (Letter, Result) -> Result or ((Letter >= 97) and (Letter =< 172)) end, false, Password),
    if
        UpperExists and LowerExists -> {ok};
        UpperExists -> {error,["Must have at least one lower case letter."]};
        LowerExists -> {error,["Must have at least one upper case letter."]};
        true -> {error,["Must have at least one upper case and one lower case letter."]}
    end.

check_number_exists(Password) -> 
    NumberExists = lists:foldl(fun (Letter, Result) -> Result or ((Letter >= 48) and (Letter =< 57)) end, false, Password),
    if
        NumberExists -> {ok};
        true -> {error,["Must have at least one number."]}
    end.

check_length(Password) -> 
    if
        length(Password) < 8 -> {error,["Must be at least 8 characters long."]};
        true -> {ok}
    end.


% Problem 3.1
% Most of the list monad code is provided below.  Implement the pop and bind functions.
create() -> {nil, 0}.

push(List, Value) -> {{Value, List},1}.

len({_List, Length}) -> Length.

value({List, _Length}) -> List.



% Problem 3.2
% Complete the second clause of the concat function per the instructions.
concat(List2, nil) -> {List2, 0};
concat(List2, {First, Rest}) -> 
    put_your_code_here.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test Code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test code for problem set 1
test_ps1() ->

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 1.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    A1 = {"Bob", "1234", 100},
    A2 = {"Tim", "2424", 200},
    A3 = {"Sue", "5851", 500},

    % Write test code to demonstrate the associative property with combineAccounts using the 3 accounts above


    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 1.2
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    A4 = {"Edward", "8879", 300},
    A5 = {"George", "5546", 50},

    % Write test code using a fold to combine all 5 accounts into one account for Bob (A1)



    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 1.3
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    F1 = fun string:to_upper/1,
    F2 = fun string:trim/1,
    F3 = fun string:reverse/1,

    % Write test code to demonstrate the associate property with combineFunctions using the 3 functions above



    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 1.4
    %%%%%%%%%%%%%%%%%%%%%%%%%%%



    pass.

% Test code for problem set 2
test_ps2() ->

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 2.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    %{ok,4} = cut_half(8),
    %{fail} = cut_half(5),
    %{fail} = cut_half(-1),
    %{ok,50} = maybe_bind(maybe_bind(maybe_unit(200), fun cut_half/1), fun cut_half/1),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 2.2
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    %Password1 = "simple",
    %Result1 = result_bind(result_bind(result_bind(result_unit(), Password1, fun check_mixed_case/1), Password1, fun check_number_exists/1), Password1, fun check_length/1),
    %io:format("Result1 = ~p~n",[Result1]), % Should show error with all three errors in the list

    %Password2 = "mostlygood23",
    %Result2 = result_bind(result_bind(result_bind(result_unit(), Password2, fun check_mixed_case/1), Password2, fun check_number_exists/1), Password2, fun check_length/1),
    %io:format("Result2 = ~p~n",[Result2]), % Should show error with one error about missing an upper case letter

    %Password3 = "GoodPassword42",
    %Result3 = result_bind(result_bind(result_bind(result_unit(), Password3, fun check_mixed_case/1), Password3, fun check_number_exists/1), Password3, fun check_length/1),
    %io:format("Result3 = ~p~n",[Result3]), % Should show ok 

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 2.3
    %%%%%%%%%%%%%%%%%%%%%%%%%%%    

    % Modify the test code below to replace `put_your_fold_here` with a foldl to do the same chaining as the previous problem
    Result1_With_Fold = put_your_fold_here,
    io:format("Result1_With_Fold = ~p~n",[Result1_With_Fold]),

    Result2_With_Fold = put_your_fold_here,
    io:format("Result2_With_Fold = ~p~n",[Result2_With_Fold]),

    Result3_With_Fold = put_your_fold_here,
    io:format("Result3_With_Fold = ~p~n",[Result3_With_Fold]),

    pass.

% Test code for problem set 3
test_ps3() ->

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 3.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    %L1 = create(),
    %nil = value(L1),
    %0 = len(L1),

    %L2 = bind(L1, fun push/2, [2]),
    %{2,nil} = value(L2),
    %1 = len(L2),

    %L3 = bind(L2, fun push/2, [4]),
    %{4,{2,nil}} = value(L3),
    %2 = len(L3),

    %L4 = bind(L3, fun push/2, [6]),
    %{6,{4,{2,nil}}} = value(L4),
    %3 = len(L4),
    
    %L5 = bind(L4, fun pop/1, []),
    %{4,{2,nil}} = value(L5),
    %2 = len(L5),
    
    %L6 = bind(L5, fun push/2, [8]),
    %{8,{4,{2,nil}}} = value(L6),
    %3 = len(L6),

    %L7 = bind(L6, fun pop/1, []),
    %{4,{2,nil}} = value(L7),
    %2 = len(L7),

    %L8 = bind(L7, fun pop/1, []),
    %{2,nil} = value(L8),
    %1 = len(L8),

    %L9 = bind(L8, fun pop/1, []),
    %nil = value(L9),
    %0 = len(L9),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 3.2
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    %L10 = {{1,{2,{3,nil}}},3},
    %L11 = {{4,{5,{6,nil}}},3},
    %L12 = bind(L11, fun concat/2, [value(L10)]),
    %{1,{2,{3,{4,{5,{6,nil}}}}}} = value(L12),
    %6 = len(L12),

    % To show associative property, both sides are demonstrated:
    %    L14: L10++(L11++L13)
    %    L15: (L10++L11)++L13  

    %L13 = {{7,{8,nil}},2},
    %L14 = bind(bind(L13, fun concat/2, [value(L11)]), fun concat/2, [value(L10)]),
    %L15 = bind(L13, fun concat/2, [value(bind(L11, fun concat/2, [value(L10)]))]),

    %{1,{2,{3,{4,{5,{6,{7,{8,nil}}}}}}}} = value(L14),
    %8 = len(L14),
    %{1,{2,{3,{4,{5,{6,{7,{8,nil}}}}}}}} = value(L15),
    %8 = len(L15),

    pass.
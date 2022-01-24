% CSE 382 Prove 05

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed in a public file sharing site.

% Instructions: Use this template file for this prove assignment.
% The details of what to do for each problem are found in 
% the reading. IMPORTANT: Comment out code that is not 
% running properly.  The `test_ps#` functions should return `pass`.

-module(prove05).
-export([test_ps1/0, test_ps2/0, test_ps3/0]).

% Problem 1.2


% Problem 1.3
% Provide specification and definition (as comments) along with the code
 

% Problem 2.1


% Problem 3.1


% Problem 3.2


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% The following functions are fully implemented for you to use in the problem sets.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Display a formatted alert message with three parts.
alert(Location, Category, Message) -> io:format("~p for ~p : ~p~n", [Category, Location, Message]).

% Return True if the Value is within range
range_check(Low, High, Value) -> (Value >= Low) and (Value =< High).

% Calculate the average of list of numbers.
list_average(List) -> 
    lists:foldl(fun (Item, Total) -> Item + Total end, 0, List) / length(List).

% Create a one arity function that counts all items in a list that have 
% the specified Term in the string.
list_text_count(Term) ->
    fun(List) -> lists:foldl(fun (Item, Total) -> 
        case string:find(Item,Term) of
            nomatch -> Total;
            _Else -> Total + 1
        end
    end, 0, List) end.

% Open the file, skip the header row, and begin reading
% each row one at a time to produce a list of lists.
read_csv_file(Filename) -> 
    {ok, FileHandle} = file:open(Filename, read),
    file:read_line(FileHandle), % Skip header row
    read_csv_file(FileHandle, []).

% Read each line of the CSV file and split the line by the comma
% delimeter.  Each line will be represented by a list and the file
% will be represented as a list of lists.
read_csv_file(FileHandle, Lines) ->
    Result = file:read_line(FileHandle),
    case Result of
        {ok, Line} -> read_csv_file(FileHandle, [string:split(Line,",",all)|Lines]);
        eof -> Lines
    end.

% Extract the specified column as text
extract_column_array(text, Array, ColumnId) ->
    ExtractColumn = fun(Row) -> lists:nth(ColumnId, Row) end,
    lists:map(ExtractColumn, Array);

% Extract the specified column as integers
extract_column_array(int, Array, ColumnId) ->
    ExtractColumn = fun(Row) -> 
        {Result, _} = string:to_integer(lists:nth(ColumnId, Row)),
        Result
     end,
    lists:map(ExtractColumn, Array);

% Extract the specified column as floats
extract_column_array(float, Array, ColumnId) ->
    ExtractColumn = fun(Row) -> 
        {Result, _} = string:to_float(lists:nth(ColumnId, Row)),
        Result
     end,
    lists:map(ExtractColumn, Array).

% Extract a column (with the specified type of text, int, or float) 
% from the file and perfom the specified function on the data.
process_dataset(Filename, ColumnId, ColumnType, CalcFunction) ->
    Data = read_csv_file(Filename),
    DataColumn = extract_column_array(ColumnType, Data, ColumnId),
    Result = CalcFunction(DataColumn),
    Result.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test Code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test code for problem set 1
test_ps1() ->

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 1.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    % Chain the map, filter, and foldl functions in a single line as described in the problem


    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 1.2
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    L = [2, 4, 6, 8, 10, 12],
    %[12,16,20,24] = (greater_list(10))((multiply_list(2))(L)),
    %[24] = (multiply_list(2))((greater_list(10))(L)),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 1.3
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    %Multiples7 = multiples_of_list(7),
    %[7,14,21,28] = Multiples7(lists:seq(1,30)),
    %[42,49] = Multiples7(lists:seq(40,50)),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 1.4
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    % Write test code to chain multiples_of_list to problem 1.2 to find multiples of 8
    %[16,24] = write_code_here,
    %[24] = write_code_here,


    pass.

% Test code for problem set 2
test_ps2() ->

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 2.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    alert("Madison County","Winter Storm Warning","Expect 8-12 inches of Snow"),
    %((((curry3(fun alert/3))("Madison County")))("Winter Storm Warning"))("Expect 8-12 inches of Snow"),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 2.2
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    % Write test code to curry the first parameter and test the intermediate function twice


    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 2.3
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    % Write test code to curry the first and second parameter and test the intermediate function twice


    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 2.4
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    % Write test code to curry all parameters in the range_check function
    % Use the curried function with a filter function to filter
    % a list of numbers.  Test this in two different examples.


    pass.

% Test code for problem set 3
test_ps3() ->

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 3.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    % Useful lambdas for the map_filter_fold2 to use.
    Square = fun(X) -> X * X end,
    Triple = fun(X) -> 3 * X end,
    Odd = fun(X) -> X rem 2 == 1 end,
    Even = fun(X) -> X rem 2 == 0 end,
    Sum = fun(X,Y) -> X+Y end,
    Product = fun(X,Y) -> X*Y end,

    % Example not using any partial applications    
    %35 = (((map_filter_fold2(6))(Square))(Odd))(0, Sum),
    
    % Example creating and using partial application
    %First10Squares = (map_filter_fold2(10))(Square),
    %165 = (First10Squares(Odd))(0, Sum),
    %14745600 = (First10Squares(Even))(1,Product),

    % Write and test a partial application function to obtain the first 20 triples that are even


    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 3.2 & 3.3
    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    % Examples not using any partial applications  
    Avg_Temp = process_dataset("weather.csv", 6, int, fun list_average/1),
    io:format("Avg Temp = ~p~n",[Avg_Temp]), % Answer = 24.8472
    Count_Snow = process_dataset("weather.csv", 5, text, list_text_count("Snow")),
    io:format("Count Snow = ~p~n",[Count_Snow]), % Answer = 32

    % Write partial application to read entire dataset only once and then test


    % Write partial application to read the entire dataset and extract the 
    % Observation column only once and then test.

    
    
    pass.
% CSE 382 Prove 11

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site.

% Instructions: Use this template file for this prove assignment.
% The details of what to do for each problem are found in 
% the reading. IMPORTANT: Comment out code that is not 
% running properly.  The `test_ps#` functions should return `ok`.
% When writing tests use the `expected_result` = `actual result` format.

-module(prove11).
-export([test_ps1/0, test_ps2/0, test_ps3/0]).

% Problem 1.1


% Problem 2.1


% Problem 2.2
% Complete the count function per the instructions.

count(nil) -> 0;
count(Node = #{done := nil}) -> 
    put_your_code_here;
count(Node) ->
    put_your_code_here.

% Code for use in Problem 3.1

% Open the file, skip the header row, and begin reading
% each row one at a time to produce a list of lists.
read_file(Filename) -> 
    {ok, FileHandle} = file:open(Filename, read),
    read_file(FileHandle, []).

% Read each line of the file and remove the end of line character.  
% Each line read will be added to Lines list.
read_file(FileHandle, Lines) ->
    Result = file:read_line(FileHandle),
    case Result of
        {ok, Line} -> read_file(FileHandle, [string:trim(Line)|Lines]);
        eof -> Lines
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test Code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test code for problem set 1
test_ps1() ->

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 1.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    % ASCII Table Lookup: https://www.asciitable.com/

    % T1 = add("day",nil),
    % #{100 := #{97 := #{121 := #{done := nil}}}} = T1,

    % T2 = add("date",T1),
    % #{100 := #{97 := #{116 := #{101 := #{done := nil}},121 := #{done := nil}}}} = T2,

    % T3 = add("days",T2),
    % #{100 := #{97 := #{116 := #{101 := #{done := nil}}, 121 := #{115 := #{done := nil},done := nil}}}} = T3,

    % T4 = add("",T3),
    % #{100 := #{97 := #{116 := #{101 := #{done := nil}}, 121 := #{115 := #{done := nil},done := nil}}}, done := nil} = T4,

    % T5 = add("cow",T4),
    % #{99 := #{111 := #{119 := #{done := nil}}}, 100 := #{97 := #{116 := #{101 := #{done := nil}}, 121 := #{115 := #{done := nil},done := nil}}}, done := nil} = T5,

    % T6 = add("cold",T5),
    % #{99 := #{111 := #{108 := #{100 := #{done := nil}},119 := #{done := nil}}}, 100 := #{97 := #{116 := #{101 := #{done := nil}}, 121 := #{115 := #{done := nil},done := nil}}}, done := nil} = T6,

    % T7 = add("dog",T6),
    % #{99 := #{111 := #{108 := #{100 := #{done := nil}},119 := #{done := nil}}}, 100 := #{97 := #{116 := #{101 := #{done := nil}}, 121 := #{115 := #{done := nil},done := nil}}, 111 := #{103 := #{done := nil}}}, done := nil} = T7,

    ok.

% Test code for problem set 2
test_ps2() ->

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 2.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    % Trie = lists:foldl(fun add/2, nil, ["day","date","days","","cow","cold","dog"]),
    % true = search("day",Trie),
    % true = search("date",Trie),
    % true = search("days",Trie),
    % false = search("da",Trie),
    % false = search("dates",Trie),
    % true = search("",Trie),
    % true = search("cow",Trie),
    % true = search("cold",Trie),
    % true = search("dog",Trie),
    % false = search("colt",Trie),
    % false = search("pig",Trie),
    % false = search("bob",nil), % Test with an empty Trie

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 2.2
    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    % 7 = count(Trie),
    % 0 = count(nil),
    
    ok.

% Test code for problem set 3
test_ps3() ->

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 3.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    % Write Test Code to compare the memory size of the phone numbers stored in a list
    % versus stored in a Trie.  

    % PhoneList = read_file("phone.txt"),
    % PhoneTrie = put_your_code_here,
    % SizeList = put_your_code_here,
    % SizeTrie = put_your_code_here,
    % io:format("Size List = ~p  Size Trie = ~p~n",[SizeList, SizeTrie]),

    % Observations (see instructions): 

    ok.
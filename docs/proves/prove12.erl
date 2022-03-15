% CSE 382 Prove 12

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed in a public file sharing site.

% Instructions: Use this template file for this prove assignment.
% The details of what to do for each problem are found in 
% the reading. IMPORTANT: Comment out code that is not 
% running properly.  The `test_ps#` functions should return `pass`.

-module(prove12).
-export([test_ps1/0, test_ps2/0, test_ps3/0]).

% Problem 1.1 & 2.1
% The create and empty function are written for you.
% You need to write enqueue, dequeue, and head per the
% instructions.  Note that a placeholder has been created
% for the head function so that the check functions
% below will work.

% For problem 2.1, you will need to update the dequeue 
% and head functions to support the deque per the 
% instructions. 

create() -> {[], []}.

empty({[], []}) -> true;
empty(_) -> false.

head(Parameters) -> todo.


% Problem 2.1
% You need to write enqueue_front, dequeue_back, and tail
% per the instructions.  Note that a placeholder has 
% been created for the tail function so that the check
% functions below will work.  You will also need to update 
% the dequeue and head functions you previously 
% implemented to support the deque.

tail(Parameters) -> todo.


% Code for use in Problem 3.1

start_perf() ->
    eprof:start(),
    eprof:start_profiling([self()]).

stop_perf(Title) ->
    io:format("Perf (~p): ~n",[Title]),
    eprof:stop_profiling(),
    eprof:analyze(total),
    eprof:stop().

% The following code is used in the test cases to 
% display the queue/deque and check if the head, tail
% and empty are correct.

check_queue(Queue, Head, Empty) ->
    io:format("Queue: ~w~n",[Queue]),
    Head = head(Queue),
    Empty = empty(Queue).

check_deque(Deque, Head, Tail, Empty) ->
    io:format("Deque: ~w~n",[Deque]),
    Head = head(Deque),
    Tail = tail(Deque),
    Empty = empty(Deque).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test Code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test code for problem set 1
test_ps1() ->

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 1.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    % Q1 = enqueue(1, create()),
    % check_queue(Q1,1,false),
    % Q2 = enqueue(2, Q1),
    % check_queue(Q2,1,false),
    % Q3 = enqueue(3, Q2),
    % check_queue(Q3,1,false),
    % Q4 = enqueue(4, Q3),
    % check_queue(Q4,1,false),
    % Q5 = enqueue(5, Q4),
    % check_queue(Q5,1,false),
    % Q6 = enqueue(6, Q5),
    % check_queue(Q6,1,false),

    % Q7 = dequeue(Q6),
    % check_queue(Q7,2,false),
    % Q8 = dequeue(Q7),
    % check_queue(Q8,3,false),
    % Q9 = dequeue(Q8),
    % check_queue(Q9,4,false),

    % Q10 = enqueue(7,Q9),
    % check_queue(Q10,4,false),
    % Q11 = enqueue(8,Q10),
    % check_queue(Q11,4,false),
    % Q12 = enqueue(9,Q11),
    % check_queue(Q12,4,false),

    % Q13 = dequeue(Q12),
    % check_queue(Q13,5,false),
    % Q14 = dequeue(Q13),
    % check_queue(Q14,6,false),
    % Q15 = dequeue(Q14),
    % check_queue(Q15,7,false),
    % Q16 = dequeue(Q15),
    % check_queue(Q16,8,false),
    % Q17 = dequeue(Q16),
    % check_queue(Q17,9,false),
    % Q18 = dequeue(Q17),
    % check_queue(Q18,nil,true),

    pass.

% Test code for problem set 2
test_ps2() ->

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 2.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    % Q1 = enqueue(1, create()),
    % check_deque(Q1,1,1,false),
    % Q2 = enqueue(2, Q1),
    % check_deque(Q2,1,2,false),
    % Q3 = enqueue(3, Q2),
    % check_deque(Q3,1,3,false),
    % Q4 = enqueue(4, Q3),
    % check_deque(Q4,1,4,false),
    % Q5 = enqueue(5, Q4),
    % check_deque(Q5,1,5,false),
    % Q6 = enqueue(6, Q5),
    % check_deque(Q6,1,6,false),

    % Q7 = dequeue(Q6),
    % check_deque(Q7,2,6,false),
    % Q8 = dequeue(Q7),
    % check_deque(Q8,3,6,false),
    % Q9 = dequeue(Q8),
    % check_deque(Q9,4,6,false),

    % Q10 = enqueue(7,Q9),
    % check_deque(Q10,4,7,false),
    % Q11 = enqueue(8,Q10),
    % check_deque(Q11,4,8,false),
    % Q12 = enqueue(9,Q11),
    % check_deque(Q12,4,9,false),

    % Q13 = enqueue_front(10,Q12),
    % check_deque(Q13,10,9,false),
    % Q14 = enqueue_front(11,Q13),
    % check_deque(Q14,11,9,false),
    % Q15 = enqueue_front(12,Q14),
    % check_deque(Q15,12,9,false),

    % Q16 = dequeue_back(Q15),
    % check_deque(Q16,12,8,false),
    % Q17 = dequeue_back(Q16),
    % check_deque(Q17,12,7,false),
    % Q18 = dequeue_back(Q17),
    % check_deque(Q18,12,6,false),
    % Q19 = dequeue_back(Q18),
    % check_deque(Q19,12,5,false),
    % Q20 = dequeue_back(Q19),
    % check_deque(Q20,12,4,false),

    % Q21 = dequeue(Q20),
    % check_deque(Q21,11,4,false),
    % Q22 = dequeue(Q21),
    % check_deque(Q22,10,4,false),
    % Q23 = dequeue(Q22),
    % check_deque(Q23,4,4,false),
    % Q24 = dequeue(Q23),
    % check_deque(Q24,nil,nil,true),
  
    pass.

% Test code for problem set 3
test_ps3() ->

    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 3.1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    % Write Test Code to compare the performance of 
    % enqueue and dequeue on 1 million values
    % per the instructions.



    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Test Problem 3.2
    %%%%%%%%%%%%%%%%%%%%%%%%%%%

    % Write Test Code to compare the performance of 
    % enqueue_front and dequeue_back on 1 million values
    % per the instructions.



    pass.
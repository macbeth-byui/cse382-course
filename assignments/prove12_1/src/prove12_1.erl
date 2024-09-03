% CSE 382 Prove 12 - Problem Set 1 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit

-module(prove12_1).
-export([create/0, empty/1, enqueue/2, dequeue/1, head/1, check_queue/3]).

% Problem 1.1
% The create and empty function are written for you.
create() -> {[], []}.

empty({[], []}) -> true;
empty(_) -> false.



% The following code is used in the test cases to 
% check if the head, tail, and empty are correct.

check_queue(Queue, Head, Empty) ->
    Head = head(Queue),
    Empty = empty(Queue).

% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

queue_test() ->
    Q1 = enqueue(1, create()),
    check_queue(Q1,1,false),
    Q2 = enqueue(2, Q1),
    check_queue(Q2,1,false),
    Q3 = enqueue(3, Q2),
    check_queue(Q3,1,false),
    Q4 = enqueue(4, Q3),
    check_queue(Q4,1,false),
    Q5 = enqueue(5, Q4),
    check_queue(Q5,1,false),
    Q6 = enqueue(6, Q5),
    check_queue(Q6,1,false),

    Q7 = dequeue(Q6),
    check_queue(Q7,2,false),
    Q8 = dequeue(Q7),
    check_queue(Q8,3,false),
    Q9 = dequeue(Q8),
    check_queue(Q9,4,false),

    Q10 = enqueue(7,Q9),
    check_queue(Q10,4,false),
    Q11 = enqueue(8,Q10),
    check_queue(Q11,4,false),
    Q12 = enqueue(9,Q11),
    check_queue(Q12,4,false),

    Q13 = dequeue(Q12),
    check_queue(Q13,5,false),
    Q14 = dequeue(Q13),
    check_queue(Q14,6,false),
    Q15 = dequeue(Q14),
    check_queue(Q15,7,false),
    Q16 = dequeue(Q15),
    check_queue(Q16,8,false),
    Q17 = dequeue(Q16),
    check_queue(Q17,9,false),
    Q18 = dequeue(Q17),
    check_queue(Q18,nil,true).

-endif.

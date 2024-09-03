% CSE 382 Prove 12 - Problem Set 2 - Solution

% (c) BYU-Idaho - It is an honor code violation to post this
% file completed or uncompleted in a public file sharing site. W5

% Compile without running tests: rebar3 compile
% Compile and run tests: rebar3 eunit

-module(prove12_2).
-export([create/0, empty/1, enqueue/2, dequeue/1, head/1, enqueue_front/2, dequeue_back/1, tail/1, check_deque/4]).

% Problem 2.1
% The create and empty function are written for you.  Copy and paste the 
% enqueue, dequeue, and head functions from the previous problem set.  Modify
% those funcitons to support a deque.  Implement enqueue_front, dequeue_back,
% and the tail function to fully support a deque.

create() -> {[], []}.

empty({[], []}) -> true;
empty(_) -> false.



% The following code is used in the test cases to 
% the head, tail, and empty are correct.

check_deque(Deque, Head, Tail, Empty) ->
    io:format("Deque: ~w~n",[Deque]),
    Head = head(Deque),
    Tail = tail(Deque),
    Empty = empty(Deque).

% TEST FUNCTIONS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

deque_test() ->
    Q1 = enqueue(1, create()),
    check_deque(Q1,1,1,false),
    Q2 = enqueue(2, Q1),
    check_deque(Q2,1,2,false),
    Q3 = enqueue(3, Q2),
    check_deque(Q3,1,3,false),
    Q4 = enqueue(4, Q3),
    check_deque(Q4,1,4,false),
    Q5 = enqueue(5, Q4),
    check_deque(Q5,1,5,false),
    Q6 = enqueue(6, Q5),
    check_deque(Q6,1,6,false),

    Q7 = dequeue(Q6),
    check_deque(Q7,2,6,false),
    Q8 = dequeue(Q7),
    check_deque(Q8,3,6,false),
    Q9 = dequeue(Q8),
    check_deque(Q9,4,6,false),

    Q10 = enqueue(7,Q9),
    check_deque(Q10,4,7,false),
    Q11 = enqueue(8,Q10),
    check_deque(Q11,4,8,false),
    Q12 = enqueue(9,Q11),
    check_deque(Q12,4,9,false),

    Q13 = enqueue_front(10,Q12),
    check_deque(Q13,10,9,false),
    Q14 = enqueue_front(11,Q13),
    check_deque(Q14,11,9,false),
    Q15 = enqueue_front(12,Q14),
    check_deque(Q15,12,9,false),

    Q16 = dequeue_back(Q15),
    check_deque(Q16,12,8,false),
    Q17 = dequeue_back(Q16),
    check_deque(Q17,12,7,false),
    Q18 = dequeue_back(Q17),
    check_deque(Q18,12,6,false),
    Q19 = dequeue_back(Q18),
    check_deque(Q19,12,5,false),
    Q20 = dequeue_back(Q19),
    check_deque(Q20,12,4,false),

    Q21 = dequeue(Q20),
    check_deque(Q21,11,4,false),
    Q22 = dequeue(Q21),
    check_deque(Q22,10,4,false),
    Q23 = dequeue(Q22),
    check_deque(Q23,4,4,false),
    Q24 = dequeue(Q23),
    check_deque(Q24,nil,nil,true),

    Q25 = enqueue_front(100,Q24),
    check_deque(Q25,100,100,false),
    Q26 = enqueue(200,Q25),
    check_deque(Q26,100,200,false),
    Q27 = dequeue_back(Q26),
    check_deque(Q27,100,100,false),
    Q28 = dequeue(Q27),
    check_deque(Q28,nil,nil,true).

-endif.

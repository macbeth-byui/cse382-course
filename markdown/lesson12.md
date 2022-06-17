# CSE 382 Lesson 12 - Queues and Deques

In Erlang you have noticed that it is more efficient to access the front of a list as opposed to the back of the list.  This means that it is very efficient to implement a stack.  Queues on the other hand are more expensive since we have to recursively traverse to the end of the list to enqueue new values.  To resolve this problem we will use a technique that combines two stacks together to form a queue.

You can find the template for the problem sets in this lesson here: [prove12.erl](https://macbeth-byui.github.io/cse382-course/proves/prove12.erl)

## Part 1 - Queues

The basic functions of a queue include the ability to do the following:
* `enqueue` - insert a new value at the back (or end) of the queue
* `dequeue` - remove a value from the front (or start) of the queue
* `head` - get the value from the front of the queue

If we used a list to do this, we would have O(1) for the `dequeue` and the head but O(n) for the `enqueue`.  To resolve this problem, we will use two lists and treat both them as stacks as follows:
* `Front` - This stack will always have the next value to `dequeue` available at the front of the stack.  The `head` function will use this stack as well.
* `Back` - This stack will always have the next value to `enqueue` be available at the front of the stack.

![Queue](https://macbeth-byui.github.io/cse382-course/images/queue.drawio.png)

The dilemma with this arrangement is that we have no way to get values added to `Back` to migrate over to the `Front`.  This would be mean that `Front` would always be empty.  To resolve this problem we introduce a rule, or an invariant, which is that the `Front` can only be empty if the `Back` is also empty.  If both are empty, then we say the queue is empty as well.  This rule requires us to move items between the two stacks as needed.

We will store both stacks in a single structure:

>$struct ~ ~ queue \lbrace [a] : Front, [a] : Back \rbrace.$

Our three functions are specified as follows:

>$spec ~ ~ enqueue :: a ~ ~ queue \rightarrow queue.$
>$spec ~ ~ dequeue :: queue \rightarrow queue.$
>$spec ~ ~ head :: queue \rightarrow a.$

When calling the `enqueue` function, we must ensure that `Front` is only empty when `Back` is empty.  Normally, we put the new value into the `Back` stack.  However, if both stacks are empty, then we will put the first new value into the `Front`.

|Operation|Front|Back|
|-|-|-|
|Create Empty|`[]`|`[]`|
|Enqueue 1|`[1]`|`[]`|
|Enqueue 2|`[1]`|`[2]`|
|Enqueue 3|`[1]`|`[3,2]`|
|Enqueue 4|`[1]`|`[4,3,2]`|

>$de\mathit{f} ~ ~ enqueue :: Value ~ ~ \lbrace [], [] \rbrace \rightarrow \lbrace [Value], [] \rbrace;$
>$de\mathit{f} ~ ~ enqueue :: Value ~ ~ \lbrace Front, Back \rbrace \rightarrow \lbrace Front, [Value|Back] \rbrace.$

Ensuring our rule regarding the `Front` being empty only when `Back` is empty is a little more complicated for the `dequeue` function.  Normally we remove the value from the `Front` stack.  However, if the `Front` stack only has one value, then we must prevent it from going empty.  This is accomplished by taking the values in the `Back` and moving them to the `Front`.  However, the order of the values must be swapped because the first one of the `Back` should be the last one to be remove from the `Front`.

|Operation|Front|Back|
|-|-|-|
|Queue from Previous Table|`[1]`|`[4,3,2]`|
|Dequeue 1|`[2,3,4]`|`[]`|
|Dequeue 2|`[3,4]`|`[]`|
|Enqueue 5|`[3,4]`|`[5]`|
|Enqueue 6|`[3,4]`|`[6,5]`|
|Dequeue 3|`[4]`|`[6,5]`|
|Dequeue 4|`[5,6]`|`[]`|


Note in the definition below the first clause is for error checking (can't `dequeue` from an empty queue).  Also, we assume a `reverse` function is available:

>$de\mathit{f} ~ ~ dequeue :: \lbrace [], [] \rbrace \rightarrow \lbrace [], [] \rbrace;$
>$de\mathit{f} ~ ~ dequeue :: \lbrace [One], Back \rbrace \rightarrow \lbrace (reverse ~ ~ Back), [] \rbrace;$
>$de\mathit{f} ~ ~ dequeue :: \lbrace [First|Rest], Back \rbrace \rightarrow \lbrace Rest, Back \rbrace.$

Our `head` function is really simple since we ensured that the `Front` would never be empty when the queue was empty.

>$de\mathit{f} ~ ~ head :: \lbrace [], [] \rbrace \rightarrow nil;$
>$de\mathit{f} ~ ~ head :: \lbrace [First|Rest], Back \rbrace \rightarrow First.$

## Problem Set 1

1. Implement the `enqueue`, `dequeue`, and `head` functions as described in the definitions above.  Use the test code provided to verify your implementations.  A `create` and `empty` function are already provided for you.

## Part 2 - Deques

Common queue implementations also include double-ended support.  Called a deque (pronounced 'deck'), this should include support for the following additional functions:

* `enqueue_front` - insert a new value at the front of the queue
* `dequeue_back` - remove a value from the back of the queue
* `tail` - get the value from the back of the queue

![Deque](https://macbeth-byui.github.io/cse382-course/images/deque.drawio.png)

The rule that we had for the Queue (`Front` can't be empty unless the `Back` was empty also) resulted in us having a simple `head` function.  If we don't do something similar for our new deque functions, we will be forced to use recursion for our Tail.  Right now it is very common for our `Front` to contain all the values because we reversed the values from `Back` and moved them all over.  This would mean that we would need to recurse to the end of `Front` to find the tail.

We introduce an modified rule for the Deque which is that neither the `Front` nor the `Back` can be empty if there are 2 or more items in the Deque.  This means that our stacks will always take the following formats:
* `Front` is empty and `Back` is empty - Empty Deque
* `Front` has one item and `Back` is empty - Deque with only one item
* `Front` is empty and `Back` has one item - Another valid deque with only one item
* `Front` is not empty and `Back` is not empty - Deque with more than two or more items

For this to work, we will need to modify our approach for `dequeue` that said to reverse all the values from `Back` and move them to the `Front`.  This action would cause `Front` to have potentially more than one item and `Back` to be empty.

Instead of reversing and moving all of the `Back` on a dequeue, we will reverse and move only half of the values.  This will ensure that we will always still have some value in the `Back` (assuming we have 2 or more items still in the deque).

|Operation|Front|Back|
|-|-|-|
|Queue from Previous Table|`[1]`|`[4,3,2]`|
|Dequeue 1|`[2,3]`|`[4]`|
|Dequeue 2|`[3]`|`[4]`|
|Enqueue 5|`[3]`|`[5,4]`|
|Enqueue 6|`[3]`|`[6,5,4]`|
|Enqueue 7|`[3]`|`[7,6,5,4]`|
|Dequeue 3|`[4,5]`|`[7,6]`|
|Dequeue 4|`[5]`|`[7,6]`|
|Dequeue 5|`[6]`|`[7]`|
|Dequeue 6|`[7]`|`[]`|

With the definition below, we will assume we have a `split` and a `length` function that we will use to split a list into two halves.

>$de\mathit{f} ~ ~ dequeue :: \lbrace [One], Back \rbrace \rightarrow$
>$\kern{2em}\lbrace List1, List2 \rbrace = (split ~ ~ (length ~ ~ Back) / 2 ~ ~ Back),$
>$\kern{2em}\lbrace (reverse ~ ~ List2), List1 \rbrace;$

Since it is possible with our new rule that the `Back` may have the one item instead of the `Front`, we also need a new clause for this situation:

>$de\mathit{f} ~ ~ dequeue :: \lbrace [], [One] \rbrace \rightarrow \lbrace [], [] \rbrace;$

A similar consideration needs to be made with our `head` function by adding this new clause:

>$de\mathit{f} ~ ~ head :: \lbrace [], [One] \rbrace \rightarrow One;$

To implement our three new functions, we will make them symmetrical with the three we already have.  The `enqueue_front` function will put the first new value in the `Back` and subsequent values in the `Front`.   There is another special case we need to consider.  What if the first call was to `enqueue` and then `enqueue_front` was called.  The `enqueue` function would put the first value in the `Front`.   If `enqueue_front` was called second, it would put the "subsequent" values in the `Front` as well thus causing an imbalance which is against our rules.  Therefore, we have included the 2nd clause to handle this case. 

>$de\mathit{f} ~ ~ enqueue\_front :: Value ~ ~ \lbrace [], [] \rbrace \rightarrow \lbrace [], [Value] \rbrace;$
>$de\mathit{f} ~ ~ enqueue\_front :: Value ~ ~ \lbrace [One], [] \rbrace \rightarrow \lbrace [One], [Value] \rbrace;$
>$de\mathit{f} ~ ~ enqueue\_front :: Value ~ ~ \lbrace Front, Back \rbrace \rightarrow \lbrace [Value|Front], Back \rbrace.$

That special case we added to `enqueue_front` also needs to be provided for in the `enqueue` function.  The 2nd clause below is added new for our deque.  In this case, we have to swap the first value to be in the `Front` allowing the second value to be put in the `Back`.

>$de\mathit{f} ~ ~ enqueue :: Value ~ ~ \lbrace [], [] \rbrace \rightarrow \lbrace [Value], [] \rbrace;$
>$de\mathit{f} ~ ~ enqueue :: Value ~ ~ \lbrace [], [One] \rbrace \rightarrow \lbrace [One], [Value] \rbrace;$
$de\mathit{f} ~ ~ enqueue :: Value ~ ~ \lbrace Front, Back \rbrace \rightarrow \lbrace Front, [Value|Back] \rbrace.$

The `dequeue_back` function will check for a single item in the `Front`, check for a potential empty list in the `Back` requiring a transfer of half the values in the `Front` to move to the `Back`, or a normal removal of the first value in the `Back`.

>$de\mathit{f} ~ ~ dequeue\_back :: \lbrace [], [] \rbrace \rightarrow \lbrace [], [] \rbrace;$
>$de\mathit{f} ~ ~ dequeue\_back :: \lbrace [One], [] \rbrace \rightarrow \lbrace [], [] \rbrace;$
>$de\mathit{f} ~ ~ dequeue\_back :: \lbrace Front, [One] \rbrace \rightarrow$
>$\kern{2em}\lbrace List1, List2 \rbrace = (split ~ ~ (length ~ ~ Front) / 2 ~ ~ Front),$
>$\kern{2em}\lbrace List1, (reverse ~ ~ List2) \rbrace;$
>$de\mathit{f} ~ ~ dequeue\_back :: \lbrace Front, [First|Rest] \rbrace \rightarrow \lbrace Front, Rest \rbrace.$

The `tail` function will check for either a single value in the `Front` or the first value in the `Back`.

>$de\mathit{f} ~ ~ tail :: \lbrace [], [] \rbrace \rightarrow nil;$
>$de\mathit{f} ~ ~ tail :: \lbrace [One], [] \rbrace \rightarrow One;$
>$de\mathit{f} ~ ~ tail :: \lbrace Front, [First|Rest] \rbrace \rightarrow First.$

## Problem Set 2

1.  Implement the `enqueue_front`, `dequeue_back`, and `tail` functions as described in the definitions above.  You will also need to modify the `dequeue` and `head` to support the deque.  Use the test code provided to verify your implementations.

## Part 3 - Performance

When we look at the performance of a deque with Python or C++, we see O(1) performance for all 6 functions we described in the previous sections.  Using pointers, the deque is easily modified.  However, in an immutable environment, we have had to be creative using Stacks and functions like `split` and `reverse`.  These utility functions were used to ensure O(1) performance on `head` and `tail`.  What affect has the `split` and `reverse` had on the cost of `dequeue` and `dequeue_back`?

If we do profile analysis on running `dequeue`, we will find first of all that the `split` and `reverse` were not always needed.  This only occurred a relatively small number of times.  When these functions are used, there is an O($n/2$) cost where $n$ is the size of the list being split.  However, each time `split` and `reverse` are called, the value of $n$ is getting smaller (assuming we are done enqueuing values).  In other words, its not really an O($n$) every time `dequeue` is called.  In comparison, its like the dynamic array in Python and C++ which has to double in size and copy values sometimes.  We don't say the cost of adding another item to the end is O(n) but rather O(1) amortized.  The cost of doing the occasional doubling (with the copy) is amortized across multiple adds which didn't require the extra work.

Let's consider what happens if we add 20 numbers to our deque using `enqueue`:

`Front = [1]  Back = [20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2]`

If we start calling `dequeue`, how many times does the `split` and `reverse` happen?

|Front|Back|Number Split/Reverse|
|-|-|-|
`[2,3,4,5,6,7,8,9,10,11]`|`[20,19,18,17,16,15,14,13,12]`|10
`[3,4,5,6,7,8,9,10,11]`|`[20,19,18,17,16,15,14,13,12]`|0
`[4,5,6,7,8,9,10,11]`|`[20,19,18,17,16,15,14,13,12]`|0
`[5,6,7,8,9,10,11]`|`[20,19,18,17,16,15,14,13,12]`|0
`[6,7,8,9,10,11]`|`[20,19,18,17,16,15,14,13,12]`|0
`[7,8,9,10,11]`|`[20,19,18,17,16,15,14,13,12]`|0
`[8,9,10,11]`|`[20,19,18,17,16,15,14,13,12]`|0
`[9,10,11]`|`[20,19,18,17,16,15,14,13,12]`|0
`[10,11]`|`[20,19,18,17,16,15,14,13,12]`|0
`[11]`|`[20,19,18,17,16,15,14,13,12]`|0
`[12,13,14,15,16]`|`[20,19,18,17]`|5
`[13,14,15,16]`|`[20,19,18,17]`|0
`[14,15,16]`|`[20,19,18,17]`|0
`[15,16]`|`[20,19,18,17]`|0
`[16]`|`[20,19,18,17]`|0
`[17,18]`|`[20,19]`|2
`[18]`|`[20,19]`|0
`[19]`|`[20]`|1
`[20]`|`[]`|1
`[]`|`[]`|0
||**TOTAL**|19

The number of times something had to be split off and reversed was only 19.  If we divide these 19 actions across the 20 `dequeue` calls, we get the amortized O(1) for `dequeue`.  

Contrast this with what you would see if we tried to remove the last element of a single list.  We would need to recurse to the end every time we wanted to `dequeue` for a total of over 200 times.  Remove the last item of the list in this fashion would give us O($n$) without any amortization possible.

In the problem set below, you will observe the actual time and function counts with a deque of size 1 million.  Your observations should convince you of the amortized nature of our deque.

## Problem Set 3

1. Using the `start_perf` and `stop_perf` functions provided, compare the time it takes to `enqueue` the numbers 1 to 1,000,000 versus the time it takes to `dequeue` those same numbers.  Identify how we achieved O(1) for these two functions.  
2. Perform the same timing test to compare `enqueue_front` and `dequeue_back`.  Identify how we achieved O(1) for these two functions as well.   Record your observations in the comments of the test code.

 


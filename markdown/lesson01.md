# CSE 382 Lesson 1 - Erlang

Welcome to CSE 382 - Design Patterns & Data Structures with Functional Programming!

You can find the template for the problem sets in this lesson here: [prove01.erl](https://macbeth-byui.github.io/cse382-course/proves/prove01.erl)

## Part 1 - Course Syllabus

In previous classes, you may have looked at design patterns and data structures that are commonly used in programming.  When using functional programming, there are unique design patterns and different approaches to data structures that we need to consider.  Here is the schedule for the course:

* Week 1 : Erlang Basics
* Week 2 : Persistence
* Weeks 3-7 : Design Patterns
	* Functors - Map, Filter, Fold, Unfold
	* Chaining
	* Currying
	* Partial Applications
	* Monoids
	* Monads
	* Streams (Lazy)
* Weeks 8-12  : Data Structures
	* Binary Search Trees
	* Min Heaps
	* Random Access Lists
	* Tries
	* Queues & Deques

This is a 3 credit class and there is a reading for each day (e.g Part 1 before class on Monday, Part 2 before class on Wednesday, and Part 3 before class on Friday).  Each reading includes a problem set to complete.    You should work on each problem set after you do each reading.  During class we will cover the material in the reading with examples.  The material can be complicated and fast paced so you should do the reading and attempt some of the problems before class.

We will be using Erlang in this class.  If you already know Clojure, you will find that Erlang is easier to use and learn (with much fewer parentheses!).  During the first week we will learn the basics of Erlang.  You will learn more about Erlang as you implement the material during the course.

You will be given starting code for each problem set which you must use.  You should submit all the problem sets weekly on Saturday evening to stay on track.

You can resubmit any of your work for a higher grade as many times as you want during the semester.  Late work will not be penalized so you can take more time if needed to learn the material.  However, submitting late work too frequently may result in getting behind which will have a negative impact on your learning and subsequent grade.  During Week 13 and 14 you will have an opportunity to catch up on any missed work.  

At the beginning of Week 10, you will be given an open book, open note, take-home final exam which will be due on the last day of the semester.  The final exam will assess your understanding of principles taught during the course.

Your grade will be composed of 80% for the weekly problem sets and 20% for the final exam.

Attendance to class is highly encouraged.  If you miss no more than 3 days during the semester (excluding during Week 14), you will receive a 5% bonus on your final grade.

Office hours and contact information for this semester will be put in an I-Learn announcement.  

## Problem Set 1
1. Review the syllabus (found above), install Visual Studio Code (you may use any IDE or just a text editor if you want), install [Erlang](https://www.erlang.org/downloads) (follow the hyperlink), and bookmark the following useful Erlang reference sites:

* Erlang Book: https://learning.oreilly.com/library/view/programming-erlang-2nd/9781941222454/

* CSE 121e (Erlang): https://byui-cse.github.io/cse121e-course/

* Erlang Reference: https://erlangbyexample.org/

NOTE: There is no code for this first problem set.

## Part 2 - Erlang Primer

This tutorial is not meant to provide complete coverage of the Erlang language.  You are encouraged to use the Erlang references you bookmarked as needed throughout the semester.

When writing code in Erlang, you will put your code into a `.erl` file.  Each Erlang file will include a `-module` and an `-export` tag.  If I create a file called `learn_erlang.erl`, then I would create the following file:

```erlang
-module(learn_erlang).
-export([]).

% Put my functions here
```

The `-export` tag is used to list all functions that can be run externally.  Functions are written in the format of `name(Parameters)  -> expressions.`  Note that parameters and variables always start with an uppercase letter.  Functions and atoms (which are labels without values) are always lowercase.

```erlang
-module(learn_erlang).
-export([hello/0, average/2]).

hello() -> io:format("Hello World!~n").

average(Number1, Number2) -> (Number1+Number2) / 2.
```

The `-export` tag shows the number of parameters (or the arity) of the function.  Notice that each function ends with a period.  To run the functions, execute the `erl` shell command from a terminal.  

```shell
Eshell V12.0  (abort with ^G)
1> c(learn_erlang).
{ok,learn_erlang}
2> learn_erlang:average(10,20).
15
```
The `c` command will compile a module.  To run a function, type `module_name:function_name(parameters).`  Notice the need to put the period just like in your code file.

```erlang
slope(X1, Y1, X2, Y2) ->
   Delta_X = X2 - X1,
   Delta_Y = Y2 - Y1,
   Delta_Y / Delta_X.
```
In the `slope` function, multiple expressions are used for a slightly more complicated function.  Commas are used to separate expressions in the function with a final period at the end.  The last expression represents the result of the function.

```erlang
add(N1, N2) -> N1 + N2.
add(N1, N2, N3) -> N1 + N2 + N3.
add(N1, N2, N3, N4) -> N1 + N2 + N3 + N4.
```

The function `add` exists with different arities which is why we write each one as a separate function separated by periods.  

```erlang
divide(_N1, 0) -> 0;
divide(N1, N2) -> N1 / N2.
```
In the function `divide`, we have only arity 2 but we have two different scenarios or clauses.  Each clause is separated by semicolons and is evaluated in order until a match is found.  If we divide by 0, then the first clause will run.  If we divide by non-zero, then the second clause will run.  

In the `divide` function, also note that the first clause did not need to use `N1` in the expression.  Unused parameters in Erlang are prefixed with an underscore.

```erlang
letter_grade(Grade) when Grade >= 90 -> "A";
letter_grade(Grade) when Grade >= 80 -> "B";
letter_grade(Grade) when Grade >= 70 -> "C";
letter_grade(_Grade) -> "F".
```

The `when` keyword is called a guard and can be used with clauses.  Basic Boolean logic can be done with a guard.  For more complicated logic, a `case` statement can be used which will be seen later.

```erlang
factorial(N) when N =< 1 -> 1;
factorial(N) -> N * factorial(N-1).
```

With the understanding of clauses, we can implement recursive solutions.  In this example, the base case for `factorial` is represented by the first clause (notice the `=<` syntax) and recursion is used in the second clause.  The first clauses could have been rewritten without a guard if we didn't worry about negative number as: `factorial(0) -> 1;` We call this method of recursion "body recursion" because there are operations (multiply by `N`) outside of the recursive call.

```erlang
factorial(N) -> factorial(N, 1).
factorial(0, Result) -> Result;
factorial(N, Result) -> factorial(N-1, N*Result).
```

We can rewrite the same `factorial` function in what is called "tail recursion" format.  In this case, there is no operation outside of the recursive call.  This requires that we have the `Result` parameter to keep track of the ever growing value.  To support a `Result` parameter, we provide a 1-arity version of the factorial function for the user to call which initializes the product to 1.  When the base case is reached (in this case N is equal to 0), then we can just return the result we have been recursively growing.  In Erlang, both of these methods result in the same performance.  The readability for the specific problem you are solving should determine the method you use.  One might argue that the "body recursion" method was easier to read.

```erlang
encrypt(Value, Cipher_Function) -> Cipher_Function(Value).

test_encrypt() ->
   Cipher1 = fun (X) -> X + 1 end,
   Cipher2 = fun (X) -> (2 * X) - 3 end,
   11 = encrypt(10, Cipher1),
   17 = encrypt(10, Cipher2),
   3.0 = encrypt(1000, fun math:log10/1),
   ok.
```
In this code, the second parameter `Cipher_Function` is a function.  Common in functional programming, functions are passed and used liked they were any parameter like integers and strings.  The `fun (parameters) -> expression end` syntax is used to define an anonymous function (often called a lambda function).  The `encrypt` function expects to receive a function that takes only one parameter.  The `fun math:log10/1` is an example of providing an existing 1-arity function.

In the `test_encrypt` function, note that `Cipher1` and `Cipher2` variables were created.  We could not change the value of `Cipher1` because all variables in Erlang are immutable.

## Problem Set 2

You will find the link to the starting code template at the beginning of each reading (see the top of this document).  As you work on these problems, remember to look at the test code provided in the template.  There is a separate function (e.g. `test_ps2`) for each problem set.  This will help you to understand how these problems work.  You will need to uncomment out code in the test code functions as you work through each of these problems.

1. Implement the `hello` function to display "Hello World!".
2. Implement the `add` function to add two numbers.
3. Implement the `multiply` function three different ways each with different number of parameters (i.e. different arity):
	* Multiply one number (not very exciting) 
	* Multiply two numbers 
	* Multiply three numbers
4. Implement the `water` function that will take a temperature in Fahrenheit and return "Frozen", "Gas", or "Liquid".  Implement this using three clauses that use the `when` guard.
6. Implement the `fib` function to calculate the n^th^ Fibonacci number.  Assume the 1st and 2nd number is 1.  You will need to use recursion.  Recommend using body recursion.
7. Implement the `sum` function to add up the numbers from 0 to N.  Use recursion instead of using any built in functions.  Recommend using body recursion.
8. Create lambda functions to pass to the `plot` function.  The `plot` function will create `{X,Y}` coordinates where `X` goes from -5 to 5 and `Y` is calculated from your lambda function.  Create lambda functions for the following scenarios:
	* Function that squares the number
	* Function that subtracts one from the number
	* Pass the `abs/1` built-in function directly
	* Function that divides the number by three and then uses the `math:floor/1` built-in function to remove the decimal.
   
## Part 3 - Lists in Erlang

Lists are built-in to Erlang at the syntax level.  During the course, we will write our own functions to work with lists and we will even re-create our own list data structure.  To support those activities, we need to understand how to use the built-in list first.

```erlang
playing_with_lists() ->
   L1 = [1, 2, 3, 4],
   ok.
```

Lists can be created using square brackets.  We can use a vertical bar to access the first element of the list.

```erlang
playing_with_lists() ->
   L1 = [1, 2, 3, 4],
   L2 = [0|L1],
   [0, 1, 2, 3, 4] = L2,
   ok.
```

The `[0|L1]` prepends the `0` in front of the `L1` list.  The `[0, 1, 2, 3, 4] = L2` will attempt to pattern match L2 with the list on the left hand side.  If this doesn't match, our "test" will fail.

```erlang
playing_with_lists() ->
   L1 = [1, 2, 3, 4],
   [A | _] = L1,
   1 = A,

   L2 = [2, 4, 6, 8],
   [B, C | _] = L2,
   2 = B,
   4 = C,
   ok.
```

Pattern matching can reuse the `[First|Rest]` format.  In the example above, we can extract the first or the first several values.  We can match multiple values at the front of the list using commas.  Note the use of the `_` which indicates we are ignoring the "rest" of the list.

```erlang
display_first([First|_Rest]) ->
	io:format("~p~n",[First]).

playing_with_lists() ->
	L1 = [1, 2, 3, 4],
	display_first(L1),   
	ok.
```

When the list `L1` is passed to the `display_first` function, the `[First|_Rest]` syntax will split the list up into the first value and the remainder of the list (the latter of which is not used in the function.

```erlang
display_first([]) ->
	io:format("EMPTY~n");
display_first([First|_Rest]) ->
	io:format("~p~n",[First]).

playing_with_lists() ->
	L1 = [1, 2, 3, 4],
	display_first(L1),   
	display_first([]),
	ok.
```
In this example, a special clause is added for the empty list.  We use `[]` to represent an empty list.  If we wanted to represent a list of one item, we could use `[One]`.  If we wanted to represent a list of more than one item, we should use `[First|Rest]`.

```erlang
display_all([]) ->
	io:format("end of list~n");
display_all([First|Rest]) ->
	io:format("~p ",[First]),
	display_all(Rest).

playing_with_lists() ->
	L1 = [1, 2, 3, 4],
	display_all(L1),   
	display_all([]),
	ok.
```
In this code, we need to use the entire list to display every value.  Recursion is used on the `Rest` of the list.  The base case is the empty list.  

```erlang
playing_with_lists() ->
	L1 = lists:seq(1,10),
	[1,2,3,4,5,6,7,8,9,10] = L1,
	
	L2 = [N || N <- lists:seq(1,10)],
	[1,2,3,4,5,6,7,8,9,10] = L2,

	L3 = [N*3 || N <- lists:seq(1,10)],
	[3,6,9,12,15,18,21,24,27,30] = L3,

	L4 = [N*3 || N <- lists:seq(1,10), N rem 2 == 0],
	[6,12,18,24,30] = L4,
	
	ok.
```
There are multiple ways to create lists.  The `lists:seq` function will create numbers in a list from the range specified.  The other method is the list comprehension.  The syntax for the list comprehension is: `[expression || generator, filter]`  The `generator` is written with a `<-` and provides a source list.  The `expression` uses the generated number to add to the list.  The optional `filter` is used to determine which generated values should be used to generate the list.  In the last example, the `rem` operator (often called modulo) is used to include only even numbers before tripling them.

## Problem Set 3

1. Write a `stack_push` function that treats the list as a stack (LIFO - Last In First Out).  The `stack_push` function will take two parameters, the `Stack` (ie, a list) and a value `N`.  This function should push the value `N` to the front of the `Stack`.
2. Write a `stack_pop` function that returns a new stack with the first item removed.  If the stack is empty, then return the empty list `[]`.
3. Finish the `quicksort` function provided to you in the starting code.  The algorithm for quicksort is to first pick a position in the list (in our case we will pick the first number) and call it our pivot.  We will then recursively call `quicksort` on all of the numbers less than the pivot and then call quicksort on all of the numbers greater than or equal to the pivot.  The correct sorted list is then the concatenation (`++`) of the following 3 lists: `[sorted list of numbers less than the pivot] ++ [the one pivot] ++ [sorted list of numbers greater than or equal to the pivot]`.  The code is setup already but you need to write list comprehensions for creating:
	* A sublist containing all numbers in the list less than the pivot (first number in the list) which will be passed to the `quicksort` function.
	* A sublist containing all numbers in the list greater than or equal to the pivot (first number in the list) which will be passed to the `quicksort` function.



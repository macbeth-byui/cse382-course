# CSE 382 Lesson 3 - Map and Filter Functor Design Patterns

The `map` and `filter` are common functions that are available in programming languages to simplify the process of processing a loop.  Frequently also simplified with a list comprehension syntax, these functions provide an introduction to a very common subset of functions called functors.

You can find the template for the problem sets in this lesson here: [prove03.erl](https://macbeth-byui.github.io/cse382-course/proves/prove03.erl)

## Part 1 - Functors and Map

A functor is a mathematical term used in the field of category theory.  The concept is that a functor is something that can convert from one "category" to another "category".  In computer science, we consider these "categories" to be data types.  The most common conversion involves the list.  The `map` and `filter` that we use this week convert from a list to another list.  The `fold` and `unfold` that we learn about next week convert from a list to a single result (and vice versa).

Let's consider the `map` function first.  The `map` converts a list to another list using a lambda function.  The lambda function. defines how to convert each item in the original list to an item in the result list.

![](https://macbeth-byui.github.io/cse382-course/images/map.drawio.png)

If I wanted to double all the values in the list, I would use the following lambda function:

>$spec ~ ~ \lambda : : a \rightarrow a.$
>
>$de\mathit{f} ~ ~ \lambda :: Value \rightarrow Value * 2.$

If I wanted to square of all values in the list, I would use the following lambda function 

>$spec ~ ~ \lambda :: a \rightarrow a.$
>
>$de\mathit{f} ~ ~ \lambda :: Value \rightarrow Value * Value.$

If I wanted to convert a list of strings to a list of string lengths, the lambda function would be (assuming you have a function called `length`):

>$spec ~ ~ \lambda :: string \rightarrow integer.$
>
>$de\mathit{f} ~ ~ \lambda :: Text ~ ~ \rightarrow (length ~ ~ Text).$

Notice that the `map` function is expecting that the lambda always has 2 input parameters.  

Here is the formal definition of the `map`:

>$spec ~ ~ \lambda :: a_1 \rightarrow a_2.$
>
>$spec ~ ~ map :: \lambda ~ ~ [a_1]\rightarrow [a_2].$
>
>$de\mathit{f} ~ ~ map :: \lambda ~ ~ [] \rightarrow [];$
>
>$de\mathit{f} ~ ~ map :: \lambda ~ ~ [First | Rest] \rightarrow [(\lambda ~ ~ First)|(map ~ ~ \lambda ~ ~ Rest)]$

First thing to notice is that `map` function does reply on the definition for $\lambda$.  Also notice the recursive nature of the `map` as it applies the lambda function to the each element one at a time starting with the first element ($First$).  The result of calling the lambda function ($\lambda ~ ~ First$) becomes the new value to add to the front of the resulting list.

Consider the code implementation in Erlang below.

```erlang
map(_Lambda, []) -> [];
map(Lambda, [First|Rest]) -> [Lambda(First)|map(Lambda, Rest)].
```
Functors in programming will frequently use a lambda function to allow us to write more generalized and abstract functions like `map`.  Notice that the `map` function doesn't know what the lambda will do except that it is a lambda that converts one value to another value.  Note that Erlang also has a built-in function `lists:map`.

## Problem Set 1
1. Implement the `map` function described above and write test code to convert a list of measurements in inches to a list of measurements in centimeters.  Use the formula $1 in = 2.54 cm.$
2. Using the `map` function you wrote, use a simple cipher to encrypt a list of characters.  The simple cipher should shift all characters by 1 per the [ASCII table](https://www.asciitable.com/) .  For example, "PASSWORD" should be "QBTTXPSE".  In Erlang, a string is represented as a list of characters.  Therefore, you can list notation with strings.  Additionally, each character is treated as a number as shown in the ASCII table which means you can add numbers to letters.
3. Rewrite the `map` function using a list comprehension and test it with your cipher test code.  Call the new function `map_2`.
 

## Part 2 - Filter

The `filter` function is a functor that converts from a list to a list just like the `map` function.  However, the lambda function used by `filter` is intended to return a boolean result that will be used to determine if the value in the original list will be included in the new list.

![](https://macbeth-byui.github.io/cse382-course/images/filter.drawio.png)

If I wanted to include only even number values in my list, then the lambda function would be as follows:

>$spec ~ ~ \lambda :: integer \rightarrow boolean$
>
>$de\mathit{f} ~ ~ \lambda :: Value \rightarrow Value ~ ~ mod ~ ~ 2 == 0.$

If I wanted to include only three digit numbers, then the lambda function would be:

>$spec ~ ~ \lambda :: integer \rightarrow boolean$
>
>$de\mathit{f} ~ ~ \lambda :: Value \rightarrow Value \ge 100 ~ ~ and ~ ~ Value \le 999.$

In both of these examples above, the lambda function is expected to return a boolean condition.  If it returns true, then the item will be included in the resulting list.

The formal definition of the `filter` is given below.  The implementation is left for an exercise.  Note that Erlang does have a built-in function called `lists:filter`.

>$spec ~ ~ \lambda :: a \rightarrow boolean$
>
>$spec ~ ~ \mathit{filter} :: \lambda ~ ~ [a] \rightarrow [a].$
>
>$de\mathit{f} ~ ~ \mathit{filter} :: \lambda ~ ~ [] \rightarrow [];$
>
>$de\mathit{f} ~ ~ \mathit{filter} :: \lambda ~ ~ [First | Rest] \rightarrow [First|(\mathit{filter} ~ ~ \lambda ~ ~ Rest)]  ~ ~ \text{when} ~ ~ (\lambda ~ ~ First) == true;$
>
>$de\mathit{f} ~ ~ \mathit{filter} :: \lambda ~ ~ [First | Rest] \rightarrow (\mathit{filter} ~ ~ \lambda ~ ~ Rest).$

In Erlang, we are limited in what we can put in a `when` guard including boolean operations and a limited subset of built-in functions. When we need to compare a computed result (in the case of the `filter` we need to run the lambda function and consider the result), you can use a `case` statement.  With a `case` statement, you can use `_Else` to represent the default or otherwise case.

```erlang
did_it_work(Number) ->
   Result = process_it(Number),
   case Result of
      42 -> do_something(Number);
      _Else -> do_something_else(Number)
   end.

try_something_else(Number) ->
   Result = process_it(Number),
   case Number > 42 of
      true -> do_something(Number);
      _Else -> do_something_else(Number)
   end.
```
Note that Erlang provides this function as a built-in function called `lists:filter`.

## Problem Set 2

1. Implement the `filter` function in Erlang.  Use a `case` block to determine whether an item in the list should be included.  Test the `filter` to get a list of even numbers from a list using the lambda described in the reading.
2. Rewrite the `filter` function so that it uses a list comprehension instead of using the `case`.  Test the new function with the same lambda function in the previous problem.  Call the new function `filter_2`.
3. Use the `filter` functions you wrote to filter a list of temperatures (in Celsius) that will support liquid water (as opposed to frozen ice or boiling steam).
4. Use the `filter` function you wrote to filter a list of result strings that started with the prefix "ERROR:".  Consider using the `string:prefix` function to solve this problem.

## Part 3 - Functor Properties

Our `map` and `filter` functions are special in that satisfy the following properties which apply in mathematics to functors:

1. Identity - The functor must be able to convert back to itself.  In math, 1 is an identity for multiplication and 0 is an identify for addition.  
2. Distributive - The functor must be able to behave with the distributive property.  In math, if I had a functor $f$ and two other functions $g$ and $h$, then the following must be true: $f(g(h)) = f(g)(f(h))$.  This is frequently written using composition notation: $f(g \circ h) = f(g) \circ f(h)$.    This means that I can apply my function $f$ to either $g$ and $h$ combined or I can apply function $f$ separately to $g$ and $h$ and then combine the results.

In the examples below, we will observe how these mathematical properties are satisfied by `map` and `filter`.    The identity property requires us to find a $\lambda$ that will result in no changes to our original list.

>$spec ~ ~ \lambda :: a \rightarrow a.$
>
>$de\mathit{f} ~ ~ \lambda :: Value \rightarrow Value.$

Here is the code that demonstrates that identity principle using the `map` written earlier:

```erlang
[1,2,3,4] = map(fun(Value) -> Value end, [1,2,3,4]),
```

Now lets consider the second rule of functors.  Does the distributive property hold?  In the formula $f(g \circ h) = f(g) \circ f(h)$, we will use the `map` function as our functor $f$.  The $g$ and the $h$ will be separate lambda functions which are randomly selected for demonstration purposes.

>$spec ~ ~ \lambda_g :: a \rightarrow a.$
> 
>$de\mathit{f} ~ ~ \lambda_g ::  Value \rightarrow 2 * Value.$

>$spec ~ ~ \lambda_h:: a \rightarrow a.$
> 
>$de\mathit{f} ~ ~ \lambda_h ::  Value \rightarrow (Value*Value) - 1.$

In the following code, we will implement both sides of the distributive property to see if it holds true using a simple list of numbers.

```erlang
G = fun(Value) -> 2 * Value end,
H = fun(Value) -> (Value * Value) - 1 end,

% Left Side of Distributive Property
G_H = fun(Value) -> G(H(Value)) end,
[0,6,16,30] = map(G_H, [1,2,3,4]),

% Right Side of Distributive Property
[0,6,16,30] = map(G, map(H, [1,2,3,4])),
```
While understanding the properties of functors is educational, the way the code is written in the two examples above is more instructive.  Notice the creation of the `G_H` function which is the composition of two functions.  Also note that the this was needed because the `map` function requires a lambda with only one parameter.  As you study the code above, notice how the `map` functions were combined together in the "Right Side" case.  We call this "chaining" which we will learn about next week.

Demonstrations of these two properties of functors for the `filter` function is left as an exercise.

## Problem Set 3
1. Write code in Erlang to demonstrate the identity property for the `filter` functor.  Reuse the `filter` function you wrote earlier.
2. Write code in Erlang to demonstrate the distributive property for the `filter` function.  Reuse the `filter` function you wrote earlier.  You will have to come up with your own appropriate `G` and `H` lambda functions in your demonstration.  Remember that the lambda functions for `filter` expect to return boolean.  In other words, when you demonstrate the distributive property, you will be composing (or chaining) boolean conditions together using the `and` operator.

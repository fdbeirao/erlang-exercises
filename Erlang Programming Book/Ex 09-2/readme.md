# Exercise 9-2: List Comprehensions

Using list comprehensions, create a set of integers between 1 and 10 that are divisible by three (e.g., `[3,6,9]`).

```erlang
1> [X || X <- lists:seq(1,10), X rem 3 == 0].
[3,6,9]
```
---
Using list comprehensions, remove all non-integers from a polymorphic list. Return the list of integers squared: `[1, hello, 100, boo, "boo", 9]` should retuns `[1, 10000, 81]`.
```erlang
1> CleanList = fun(L) -> [X*X || X <- L, is_number(X)] end.
#Fun<erl_eval.6.52032458>
2> CleanList([1, hello, 100, boo, "boo", 9]).
[1,10000,81]
```
---
Using list comprehensions and two lists, return a new list that is the intersection of the two lists (e.g., `[1,2,3,4,5]` and `[4,5,6,7,8]` should return `[4,5]`).

Hint: assume that the lists contain no duplicates.

```erlang
1> Intersection = fun(L1, L2) -> [A || A <- L1, B <- L2, A == B] end.
#Fun<erl_eval.12.52032458>
2> Intersection([1,2,3,4,5],[4,5,6,7,8]).                              
[4,5]
```
---
Using list comprehensions and given two lists, return a new list that is the symmetric difference of the two lists. Using `[1,2,3,4,5]` and `[4,5,6,7,8]` should return `[1,2,3,6,7,8]`.

Hint: assume that the lists contain no duplicates.

```erlang
1> Diff = fun(L1, L2) -> [A || A <- L1++L2, not (lists:member(A, L1) and lists:member(A,L2))] end. 
#Fun<erl_eval.12.52032458>
2> Diff([1,2,3,4,5],[4,5,6,7,8]).                                            
[1,2,3,6,7,8]  
```
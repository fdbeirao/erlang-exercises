# Exercise 9-1: Higher-Order Functions

Using funs and higher-order functions, write a function that prints out the integers between 1 and N.

Hint: use `lists:seq(1, N)`.

```erlang
1> Seqs = fun(N) -> lists:seq(1, N) end.
#Fun<erl_eval.6.52032458>
2> Seqs(2).
[1,2]
3> Seqs(4).
[1,2,3,4]
```
---
Using funs and higher-order functions, write a function that, given a list of integers and an integer, will return all integers smaller than or equal to that integer.

```erlang
1> Smaller = fun (L, N) -> lists:filter(fun (E) -> E < N end, L) end.
#Fun<erl_eval.12.52032458>
2> Smaller([1,2,3,4,5], 3).
[1,2]
3> Smaller([1,2,3,4,5], 7).
[1,2,3,4,5]
4> Smaller([1,2,3,4,5], 0).
[]
```
---
Using funs and higher-order functions, write a function that prints out the even integers between 1 and N.

Hint: solve your problem in two steps, or use two clauses in your fun.

```erlang
1> EvenNumsToN = fun(N) -> lists:foreach(fun (Elm) -> io:format("~p~n", [Elm]) end, lists:filter(fun (Elm) -> Elm rem 2 == 0 end, lists:seq(1,N))) end.
#Fun<erl_eval.6.52032458>
2> EvenNumsToN(10).
2
4
6
8
10
ok
3> EvenNumsToN(9). 
2
4
6
8
ok
```
---
Using funs and higher-order functions, write a function that, given a list of lists, will concatenate them.

```erlang
1> Concat = fun(L) -> lists:foldl(fun(Elem, Acc) -> Acc ++ Elem end,[],L) end. 
#Fun<erl_eval.6.52032458>
2> Concat([[1,2],[3,4],[5]]).
[1,2,3,4,5]
```
---
Using funs and higher-order functions, write a function that, given a list of integers, returns the sum of the integers.

Hint: use `lists:foldl` and try figure out why we prefer to use `foldl` rather than `foldr`.

```erlang
1> Sum = fun(L) -> lists:foldl(fun (Elem, Acc) -> Elem + Acc end, 0, L) end.
#Fun<erl_eval.6.52032458>
2> Sum([1,2,3,4,5,6,7,8,9]).
45
3> Sum(lists:seq(1,9)).
45
```
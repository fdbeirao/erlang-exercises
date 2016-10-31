# Exercise 9-3: Zip Functions

Define the function `zip`, which turns a pair of lists into a list of pairs:

```
zip([1,2], [3,4,5]) = [{1,3}, {2,4}]
```

Using this example, define the function `zipWith` that applies a binary function to two lists of arguments, in lock step:

```
add(X,Y) -> X+Y.
zipWith(Add, [1,2], [3,4,5]) = [4,6]
```

Note that in both cases, the longer of the lists is effectively truncated.

---
```erlang
1> c(zip).
{ok,zip}
2> zip:zip([1,2], [3,4,5]).
[{1,3},{2,4}]
3> Add = fun(X, Y) -> X+Y end.
#Fun<erl_eval.12.52032458>
4> zip:zipWith(Add, [1,2], [3,4,5]).
[4,6]
```
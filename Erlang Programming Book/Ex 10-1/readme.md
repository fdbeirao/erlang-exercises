# Exercise 10-1: Pretty-Printing

This exercise asks you to complete the definition of `prettyEntry` from the index example earlier in this chapter.

In defining `prettyEntry`, you might find it useful to define these functions:

`accumulate/1`
> This function should take a list of line numbers, in descending order, and produce a list containing ranges as well as removing duplicates. For instance:
```erlang
accumulate([7,6,6,5,3,3,1,1]) = [{1},{3},{5,7}]
```

`prettyList/1`
> This function will print the output of `accumulate` so that on the list `[{1},{3},{5,7}]` the output is `1,3,5-7.`.

`pad/2`
> This function, called with number `N` and string `Word`, will return the string padded to the right with spaces to give it length `N` (assuming `Word` is not longer than `N`).
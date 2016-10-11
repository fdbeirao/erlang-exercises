# Exercise 7-6: Parameterized Macros

Define a parameterized macro `SHOW_EVAL` that will simply return the result of an expression when the `show` mode is switched off, but which will also print the expression and its value when the `show` flag is on. You should ensure that the expression is evaluated only once whichever case holds.
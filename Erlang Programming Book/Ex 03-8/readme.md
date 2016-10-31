# Exercise 3-8: Evaluating and Compiling Expressions

This exercise asks you to build a collection of functions that manipulate arithmetical expressions. Start with an expression such as the following:
```
((2+3)-4)
4
~((2*3)+(3*4))
```
which are fully bracketed and where you use a tilde (~) for unary minus.

First, write a *parser* for these, turning them into Erlang representations, such as the following:
```
{minus, {plus, {num, 2}, {num, 3}}, {num, 4}}
{num, 4}
{negative,{plus,{multiply,{num,2},{num,3}},{multiply,{num,3},{num,4}}}}
```
We call these *exps*. Now write a number of functions:
* An *evaluator*, which takes an *exp* and returns its value;
-module(ex76).
-export([expr/0, expr/1]).

-ifdef(show).
  -define(SHOW_EVAL(Expr), Val = Expr, io:format("~p = ~p~n",[??Expr, Val]), Val).
-else.
  -define(SHOW_EVAL(Expr), Expr).
-endif.

expr() -> ?SHOW_EVAL(5).
expr(Input) -> ?SHOW_EVAL(Input + 4).
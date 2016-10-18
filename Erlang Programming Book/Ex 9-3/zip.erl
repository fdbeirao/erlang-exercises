-module(zip).
-export([zip/2, zipWith/3]).

zip(L1, L2) ->
  zip(L1, L2, []).

zip([], _, Acc) -> lists:reverse(Acc);
zip(_, [], Acc) -> lists:reverse(Acc);
zip([H1|L1], [H2|L2], Acc) ->
  zip(L1, L2, [{H1, H2}|Acc]).

zipWith(F, L1, L2) ->
  Zipped = zip(L1, L2),
  [F(A,B) || {A,B} <- Zipped].
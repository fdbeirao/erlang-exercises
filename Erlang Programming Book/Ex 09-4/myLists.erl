-module(myLists).
-export([all/2, any/2, dropwhile/2, filter/2, filtermap/2, flatmap/2, foldl/3, foldr/3, foreach/2]).

all(_Pred, []) -> true; %% tested with Erlang 19
all(Pred, [Elm|List]) -> Pred(Elm) andalso all(Pred, List).

any(_Pred, []) -> false; %% tested with Erlang 19
any(Pred, [Elm|List]) -> Pred(Elm) orelse any(Pred, List).

dropwhile(_Pred, []) -> [];
dropwhile(Pred, [H|T] = L) -> case Pred(H) of true -> dropwhile(Pred, T); false -> L end. 

filter(Pred, List) -> [ Elem || Elem <- List, Pred(Elem) ].

filtermap(Fun, List) -> filtermap(Fun, List, []).
filtermap(_Fun, [], Acc) -> lists:reverse(Acc);
filtermap(Fun, [H|T], Acc) -> case Fun(H) of false -> filtermap(Fun, T, Acc); {true, Value} -> filtermap(Fun, T, [Value|Acc]) end.

flatmap(Fun, List) -> lists:append([Fun(Elem) || Elem <- List]).

foldl(_Fun, Acc0, []) -> Acc0;
foldl(Fun, Acc, [H|T]) -> foldl(Fun, Fun(H, Acc), T).

foldr(_Fun, Acc0, []) -> Acc0;
foldr(Fun, Acc, [H|T]) -> Fun(H, foldr(Fun, Acc, T)).

foreach(_Fun, []) -> ok;
foreach(Fun, [H|T]) -> Fun(H), foreach(Fun, T).
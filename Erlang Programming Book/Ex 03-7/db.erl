-module(db).
-export([new/0, destroy/1, write/3, read/2, delete/2, match/2]).

%% Public API

new() -> 
  [].

destroy(_Db) ->
  ok.

write(Key, Element, Db) ->
  lists:keystore(Key, 1, Db, {Key, Element}).

read(Key, Db) ->
  case lists:keyfind(Key, 1, Db) of
    {Key, Element} -> {ok, Element};
    false -> {error,instance}
  end.

delete(Key, Db) ->
  lists:keydelete(Key, 1, Db).

match(Element, Db) ->
  Filtered = lists:filter(fun(Item) -> element(2, Item) =:= Element end, Db),
  lists:map(fun(Item) -> element(1, Item) end, Filtered).
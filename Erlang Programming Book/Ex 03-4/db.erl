-module(db).
-export([new/0, destroy/1, write/3, read/2, delete/2, match/2]).

%% Public API

new() -> 
  [].

destroy(_Db) ->
  ok.

write(Key, Element, Db) ->
  add_or_replace_key(Key, Element, Db).

read(_Key, []) ->
  {error,instance};
read(Key, [{Key, Element}|_]) ->
  {ok,Element};
read(Key, [_|Tail]) ->
  read(Key, Tail).

delete(_Key, []) ->
  [];
delete(Key, [{Key, _}|Tail]) ->
  Tail;
delete(Key, [Head|Tail]) ->
  [Head|delete(Key, Tail)].

match(Element, Db) ->
  find_matches(Element, Db, []).

%% Private functions

add_or_replace_key(Key, Element, []) ->
  [{Key, Element}];
add_or_replace_key(Key, Element, [{Key, _} | Tail]) ->
  [{Key,Element} | Tail];
add_or_replace_key(Key, Element, [Head|Tail]) ->
  [Head|add_or_replace_key(Key,Element, Tail)].

find_matches(_Element, [], MatchesFound) ->
  MatchesFound;
find_matches(Element, [{Key, Element}|Tail], MatchesFound) ->
  find_matches(Element, Tail, MatchesFound ++ [Key]);
find_matches(Element, [_|Tail], MatchesFound) ->
  find_matches(Element, Tail, MatchesFound).
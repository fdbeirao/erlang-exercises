-module(index).
-export([index/1]).
-define(Punctuation, "[\\ |\\,|\\.|\\;|\\:|\\t|\\n|\\(|\\)|\\?]").

index(File) ->
  ets:new(indexTable, [ordered_set, named_table]),
  processFile(File),
  prettyIndex().

processFile(File) ->
  {ok, IoDevice} = file:open(File, [read]),
  processLines(IoDevice, 1).

processLines(IoDevice, N) ->
  case io:get_line(IoDevice, "") of
    eof ->
      ok;
    Line ->
      processLine(Line, N),
      processLines(IoDevice, N+1)
  end.

processLine(Line, N) ->
  case re:split(Line, ?Punctuation, [{return, list}]) of
    {ok, Words} -> processWords(Words, N);
    _ -> []
  end.

processWords([], _) -> ok;
processWords([Word|Rest], N) when length(Word) < 3 -> processWords(Rest, N);
processWords([Word|Rest], N) ->
  Normalize = string:to_lower(Word),
  ets:insert(indexTable, {{Normalize, N}}),
  processWords(Rest, N).
  
prettyIndex() ->
  case ets:first(indexTable) of
    '$end_of_table' -> ok;
    First ->
      case First of
        {Word, N} -> IndexEntry = {Word, [N]}
      end,
      prettyIndexNext(First, IndexEntry)
  end.

prettyIndexNext(Entry, {Word, Lines} = IndexEntry) ->
  Next = ets:next(indexTable, Entry),
  case Next of
    '$end_of_table' -> prettyEntry(IndexEntry);
    {Word, M} -> prettyIndexNext(Next, {Word, [M|Lines]});
    {NextWord, M} -> 
      prettyEntry(IndexEntry),
      prettyIndexNext(Next, {NextWord, [M]})
  end.
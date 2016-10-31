-module(index).
-export([pad/2, prettyList/1, accumulate/1]).

pad(N, Word) when N >= length(Word) -> 
  pad_internal(N - length(Word), Word).

pad_internal(0, Word) -> Word;
pad_internal(N, Word) -> pad_internal(N-1, [$ |Word]).

prettyList([{N}|T]) ->
  io:format("~p", [N]),
  print_continuation(T);
prettyList([{A,B}|T]) ->
  io:format("~p-~p", [A, B]),
  print_continuation(T).

print_continuation([]) ->
  io:format(".~n", []);
print_continuation(L) ->
  io:format(",", []),
  prettyList(L).

accumulate(L) ->
  Dedup = dedup_sorted(L),
  [H|T] = Dedup,
  accumulate(T, {H, H}, []).

accumulate([], {N,N}, Acc) -> [{N}|Acc];
accumulate([], {High, Low}, Acc) -> [{Low, High}|Acc];
accumulate([Head|Tail], {High, Low}, Acc) when Head =:= Low-1 -> accumulate(Tail, {High, Head}, Acc);
accumulate([Head|Tail], {N, N}, Acc)      -> accumulate(Tail, {Head, Head}, [{N}|Acc]);
accumulate([Head|Tail], {High, Low}, Acc) -> accumulate(Tail, {Head, Head}, [{Low, High}|Acc]).

dedup_sorted(L) -> dedup_sorted(L, []).
dedup_sorted([], Acc) -> lists:reverse(Acc);
dedup_sorted([H,H|T], Acc) -> dedup_sorted([H|T], Acc);
dedup_sorted([H|T], Acc) -> dedup_sorted(T, [H|Acc]).
-module(ex9).

%% Public API
%% Unfinished exercise %%

%% Private functions

%% split_into_lines() receives a string as input: "aa\nbb\ncc" 
%% and returns [{1, "aa"}, {2, "bb"}, {3, "cc"}]

split_into_lines(Input) ->
  split_into_lines(Input, [], []).

split_into_lines([], Line, Document) ->
  ReversedLine = lists:reverse(Line),
  NewDocument = append_line_to_document(ReversedLine, Document),
  lists:reverse(NewDocument);
split_into_lines([$\n|Rest], Line, Document) ->
  ReversedLine = lists:reverse(Line),
  split_into_lines(Rest, [], append_line_to_document(ReversedLine, Document));
split_into_lines([Chr|Rest], Line, Document) ->
  split_into_lines(Rest, [Chr|Line], Document).

append_line_to_document(Line, Document) -> [{get_line_number(Document), Line}|Document].
get_line_number(Document) -> length(Document) + 1.

%% Unfinished exercise %%
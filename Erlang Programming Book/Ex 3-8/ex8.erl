-module(ex8).
-export([parse/1]).

%% Public API

tokenize(Expr) ->
  tokenize(Expr, []).

parse(Expr) ->
  Tokenized = tokenize(Expr),
  AggregatedNumbers = aggregate_numbers(Tokenized),
  ParsToLists = pars_to_list(AggregatedNumbers),
  parse_int(ParsToLists).

%% Private functions

%% tokenize() takes a string (list of ints) as input such as `((2+12)-5)`
%% and returns a list of tokens: [{par, open}, {par, open}, {num, 2}, {oper, plus}, {num, 1}, {num, 2}, {par, close}, {oper, minus}, {num, 5}, {par, close}]

tokenize([], Tokens) -> lists:reverse(Tokens);
tokenize([Char|Rest], Tokens) ->
  tokenize(Rest, [get_token(Char)|Tokens]).

get_token($() -> {par, open};
get_token($)) -> {par, close};
get_token($+) -> {dual_oper, plus};
get_token($-) -> {dual_oper, minus};
get_token($*) -> {dual_oper, multiply};
get_token($~) -> {sing_oper, negative};
get_token(Num) when Num >= $0 andalso Num =< $9 -> {num, Num - $0}.

%% aggregate_numbers() takes a token list such as [{num, 2}, {num, 5}, {oper, X}, {num, 3}, {num, 7}] 
%% and returns a token list such as [{num, 25}, {oper, X}, {num, 37}]

aggregate_numbers(List) ->
  aggregate_numbers(List, []).

aggregate_numbers([], Acc) -> lists:reverse(Acc);
aggregate_numbers([{num, Num}|Rest], Acc) ->
  ContiguousNumbers = lists:takewhile(fun(Elm) -> element(1, Elm) =:= num end, [{num, Num}|Rest]),
  NumValue = calcNumericValue(ContiguousNumbers),
  NewRest = case Rest of
    [] ->  [];
    _  -> element(2, lists:split(length(ContiguousNumbers) - 1, Rest))
  end,
  aggregate_numbers(NewRest, [{num, NumValue}|Acc]);
aggregate_numbers([Head|Rest], Acc) ->
  aggregate_numbers(Rest, [Head|Acc]).

%% calcNumericValue() takes a list of num tokens such as [{num, 2}, {num, 5}]
%% and returns one single token {num, 25}

calcNumericValue(ContiguousNumbers) ->
  Fold = lists:foldr(fun(Elm, Acc) -> 
    Index = element(1, Acc),
    CurrentSum = element(2, Acc),
    InputVal = element(2, Elm), % { num, 5 }
    { Index + 1, InputVal * trunc(math:pow(10, Index)) + CurrentSum } 
    end, {0,0}, ContiguousNumbers),
  element(2, Fold). % { Index, Sum }

%% pars_to_list() takes a list of tokens such as [{par, open}, {par, open}, {num, 2}, {oper, plus}, {num, 12}, {par, close}, {oper, minus}, {num, 5}, {par, close}]
%% and returns a list of lists: [[{num,2}, {oper, plus}, {num, 12}], {oper, minus}, {num, 5}]

 pars_to_list(List) -> pars_to_list(List, []).
 pars_to_list([], Res) -> Res;
 pars_to_list([{par, open}|Rem], Res) -> 
   MatchingParIndex = find_matching_par_index(Rem),
   SubExpr = lists:sublist(Rem, MatchingParIndex -1),
   NewRem = lists:sublist(Rem, MatchingParIndex + 1, length(Rem) - MatchingParIndex),
   [pars_to_list(SubExpr)|Res] ++ pars_to_list(NewRem);
 pars_to_list([Elem|Rem], Acc) -> [Elem|pars_to_list(Rem, Acc)].

find_matching_par_index(List) -> find_matching_par_index(List, 0, 0).
find_matching_par_index([{par, close}|_], Index, 0) -> Index + 1;
find_matching_par_index([{par, close}|R], Index, Indent) -> find_matching_par_index(R, Index+1, Indent-1);
find_matching_par_index([{par, open}|R], Index, Indent) -> find_matching_par_index(R, Index+1, Indent+1);
find_matching_par_index([_|R], Index, Indent) -> find_matching_par_index(R, Index+1,Indent).

%% parse_int() takes a list of lists such as [[{num,2},{oper, plus}, {num, 3}], {oper, minus}, [{num,5}, {oper, minus}, {num, 2}]]
%% and retuns a parsed expr: {minus,{plus,2,3},{minus,5,2}}

parse_int({num, N}) -> {num, N};
parse_int([{num, N}]) -> {num, N};
%parse_int([{num, N1}, {dual_oper, Oper}, {num, N2}]) -> {Oper, {num, N1}, {num, N2}};
parse_int([ExprA, {dual_oper, Oper}, ExprB]) -> {Oper, parse_int(ExprA), parse_int(ExprB)};
parse_int([{sing_oper, Oper}, Expr]) -> {Oper, parse_int(Expr)};
parse_int([Expr]) -> parse_int(Expr).
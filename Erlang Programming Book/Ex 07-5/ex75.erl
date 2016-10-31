-module(ex75).
-include("records.hrl").
-export([sum/1, max/1]).
-export([is_balanced/1, insert_ordered/2]).

sum(undefined) -> 0;
sum(#node{left=Left, value=Value, right=Right}) ->
  sum(Left)+Value+sum(Right).

max(#node{left=Left, value=Value, right=Right}) -> max(max(max(Left), max(Right)), Value);
max(undefined) -> 0.

is_balanced(#node{left=undefined, value=NodeValue, right=#node{value=RightValue}}) ->
  RightValue >= NodeValue;
is_balanced(#node{left=#node{value=LeftValue}, value=NodeValue, right=undefined}) ->
  LeftValue =< NodeValue;
is_balanced(#node{left=#node{value=LeftValue} = LeftNode, value=NodeValue, right=#node{value=RightValue} = RightNode}) ->
  LeftValue =< NodeValue andalso 
  RightValue >= NodeValue andalso
  is_balanced(LeftNode) andalso
  is_balanced(RightNode);
is_balanced(_) -> true.

insert_ordered(#node{left=undefined, value=NodeValue} = Node, NewValue) when NewValue =< NodeValue -> 
  Node#node{left=#node{value=NewValue}};
insert_ordered(#node{value=NodeValue, right=undefined} = Node, NewValue) when NewValue >= NodeValue -> 
  Node#node{right=#node{value=NewValue}};

insert_ordered(#node{left=LeftNode, value=NodeValue, right=RightNode} = Node, NewValue) ->
  case NewValue =< NodeValue  of
    true -> Node#node{left=insert_ordered(LeftNode, NewValue)};
    _    -> Node#node{right=insert_ordered(RightNode, NewValue)}
  end.

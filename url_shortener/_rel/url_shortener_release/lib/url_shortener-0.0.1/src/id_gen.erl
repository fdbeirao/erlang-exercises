-module(id_gen).

-export([get_next_id/1]).

-spec(get_next_id(string()) -> string()).
get_next_id(Id) ->
    case get_next_id_inner(Id) of
        { ok, NewId } -> NewId;
        { overflow, NewId } -> [start_char() | NewId]
    end.

get_next_id_inner([LastChar | []]) -> get_next_char(LastChar);
get_next_id_inner([CurrentChar | RemainingChars]) ->
    case get_next_id_inner(RemainingChars) of
        { ok, StrToTheRight } -> { ok, [CurrentChar | StrToTheRight] };
        { overflow, StrToTheRight } -> 
            case get_next_char(CurrentChar) of
                { ok, [NewCharAtThisPosition] } -> { ok, [NewCharAtThisPosition | StrToTheRight]};
                { overflow, [NewCharAtThisPosition] } -> { overflow, [NewCharAtThisPosition | StrToTheRight] }
            end
    end.

get_next_char($Z) -> { overflow, [start_char()] };
get_next_char($z) -> { ok, [$A] };
get_next_char(Char) -> { ok, [Char+1] }.

start_char() -> $a.
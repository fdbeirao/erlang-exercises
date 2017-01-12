-module(board).
-include("board.hrl").
-export([create_board/3, play/4]).

%% Public API

create_board(Width, Height, TotalMines) when Width >= 3 andalso Height >= 3 andalso TotalMines rem 2 =/= 0 ->
  #board{ width=Width, height=Height, mines=mines(Width, Height, TotalMines) }.

play(Board, X, Y, Player) when is_record(Board, board) ->
  play(Board, playtype(Board, X, Y), X, Y, Player).

%% Private functions

mines(Width, Height, TotalMines) ->
  AllCells = [cell(X,Y) || X <- lists:seq(1, Width), Y <- lists:seq(1, Height)],
  Shuffled = lists:foldl(fun(_,Acc) -> shuffle(Acc) end, AllCells, lists:seq(1, 10)),
  lists:sublist(Shuffled, TotalMines).

shuffle(List) -> lists:sort(fun(_,_) -> rand:uniform() >= 0.5 end, List).

is_mine(Board, X, Y) when is_record(Board, board) -> is_mine(Board#board.mines, X, Y);
is_mine(Mines, X, Y) -> lists:member(cell(X,Y), Mines).

playtype(Board, X, Y) when 
  X < 1 orelse 
  X > Board#board.width orelse 
  Y < 1 orelse
  Y > Board#board.height -> out_of_bounds;
playtype(Board, X, Y) -> 
  case is_repeated_play(Board, X, Y) of 
    true -> repeated;
    false ->
     case is_mine(Board, X, Y) of 
        true -> mine;
        false -> { cell, mines_around(Board, X, Y) }
      end
  end.

mines_around(Board, X, Y) ->
  lists:foldl(fun(Elem, Acc) -> Acc + add_if_mine(Board, Elem#cell.x, Elem#cell.y) end, 0, surroundingCells(X, Y)).

surroundingCells(X, Y) ->
  [cell(CellX, CellY) || CellX<-lists:seq(X-1, X+1), CellY<-lists:seq(Y-1, Y+1), not (CellX == X andalso CellY == Y)].

add_if_mine(Board, X, Y) ->
  case is_mine(Board, X, Y) of
    true -> 1;
    false -> 0
  end.

play(Board, out_of_bounds,       _X, _Y, _Player)                    -> Board;
play(Board, repeated,            _X, _Y, _Player)                    -> Board;
play(Board, mine,                X, Y, Player)                       -> save_play(Board, X, Y, Player, mine);
play(Board, {cell, MinesAround}, X, Y, Player) when MinesAround > 0  -> save_play(Board, X, Y, Player, {mines_around, MinesAround});
play(Board, {cell, MinesAround}, X, Y, Player) when MinesAround == 0 -> 
  NewBoard = save_play(Board, X, Y, Player, empty), 
  play_around(NewBoard, X, Y, Player).

play_around(Board, X, Y, Player) ->
  lists:foldl(fun(Cell, NewBoard) -> play(NewBoard, Cell#cell.x, Cell#cell.y, Player) end, Board, surroundingCells(X, Y)).

save_play(Board, X, Y, Player, CellType) ->
  Board#board{plays = [play(Player, cell(X, Y), CellType)|Board#board.plays]}.

is_repeated_play(Board, X, Y) ->
  lists:keyfind(cell(X, Y), #play.cell, Board#board.plays) =/= false.

play(Player, Cell, CellType) when is_record(Cell, cell) ->
  #play{player = Player, cell = Cell, celltype = CellType}.

cell(X, Y) ->
  #cell{x=X, y=Y}.
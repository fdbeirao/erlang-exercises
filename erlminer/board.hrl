-record(cell, { x, % int() 
                y  % int()
              }).

-record(play, { player,   % atom()
                cell  ,   % atom(), #cell
                celltype  % atom(), mine | empty | { mines_around, int() }
              }).

-record(board, { width,      % int()
                 height,     % int()
                 mines = [], % [#cell()]
                 plays = []  % [#play()]
               }).
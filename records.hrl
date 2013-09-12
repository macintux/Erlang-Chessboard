-type square() :: tuple(pos_integer(), pos_integer()).
-type side() :: 'white'|'black'.
-type movetype() :: 'move'|'capture'|'castle'.
-type piecetype() :: 'pawn'|'knight'|'bishop'|'rook'|'queen'|'king'|'none'.
%% none is for blank squares
-type movefun() :: fun((square(), square(), side()|{side(), 'castle'}) -> tuple(square(), list(square()))).
-type movedef() :: tuple(atom(), movefun()).
-type piecemove() :: 'pawnmove'|'pawncap'|'kingmove'|
                     'knightmove'|'bishopmove'|'rookmove'|'queenmove'.

-record(piece, { type=pawn :: piecetype(),
                 team=white :: side(),
                 history=[] :: list(move()),
                 movefun :: movefun(),
                 capturefun :: movefun()}).
-type piece() :: #piece{}.

-record(move, { piece :: piece(),
                start :: square(),
                target :: square(),
                movetype :: movetype() }).
-type move() :: #move{}.

-record(historyitem, { endsquare :: square(),
                       traversed=[] :: list(square())}).
-type historyitem() :: #historyitem{}.


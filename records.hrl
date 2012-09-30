-record(move, { piece,
                start,
                target,
                movetype }).
-record(piece, { type=pawn,
                 team=white,
                 history=[],
                 movefun=pawnmove,
                 capturefun=pawncap }).
-record(historyitem, { endsquare,
                       traversed=[] }).
-record(boardstate, { pieces,
                      whiteking={5, 1},
                      blackking={5, 8} }).


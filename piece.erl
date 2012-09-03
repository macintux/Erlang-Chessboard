%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2012, John Daily
%%% @doc
%%%
%%% @end
%%% Created : 30 Aug 2012 by John Daily <jd@epep.us>

-module(piece).
-compile(export_all).

piece_loop(Piece, Move, Capture) ->
    receive
        { Pid, move, Start, End, Team } ->
            move_response(Pid, Piece, Start, End, Move(Start, End, Team)),
            piece_loop(Piece, Move, Capture);
        { Pid, capture, Start, End, Team } ->
            move_response(Pid, Piece, Start, End, Capture(Start, End, Team)),
            piece_loop(Piece, Move, Capture);
        done ->
            done;
        Message ->
            throw({unknown_message, Message})
    end.

move_response(Pid, Piece, Start, End, { Start, Traverse }) ->
    Pid ! { no, Piece, Start, End, Traverse };
move_response(Pid, Piece, Start, End, { End, Traverse }) ->
    Pid ! { yes, Piece, Start, End, Traverse }.



movefuns() ->
    [ 
      { pawnmove, fun({ X, OldY }, { X, NewY }, white) when NewY - OldY =:= 1 -> { { X, NewY }, [] };
                     ({ X, OldY }, { X, NewY }, black) when NewY - OldY =:= -1 -> { { X, NewY }, [] };
                     ({ X, 2 }, { X, 4 }, white) -> { { X, 4 }, [ { X, 3 } ] };
                     ({ X, 7 }, { X, 5 }, black) -> { { X, 5 }, [ { X, 6 } ] };
                     (Loc, _, _) -> { Loc, [] } end
      },

      { pawncap, fun({ OldX, OldY }, { NewX, NewY }, white)
                       when abs(NewX - OldX) =:= 1, NewY - OldY =:= 1 -> { { NewX, NewY }, [] };
                    ({ OldX, OldY }, { NewX, NewY }, black)
                       when abs(NewX - OldX) =:= 1, NewY - OldY =:= -1 -> { { NewX, NewY }, [] };
                    (Loc, _, _) -> { Loc, [] } end
      },

      { kingmove, fun({ OldX, OldY }, { NewX, NewY }, _)
                        when abs(NewY - OldY) < 2, abs(NewX - OldX) < 2 -> { { NewX, NewY }, [] };
                     (Loc, _, _) -> { Loc, [] } end
      },

      { knightmove, fun({ OldX, OldY }, { NewX, NewY }, _)
                          when abs(NewY - OldY) =:= 2, abs(NewX - OldX) =:= 1 -> { { NewX, NewY }, [] };
                       ({ OldX, OldY }, { NewX, NewY }, _)
                          when abs(NewY - OldY) =:= 1, abs(NewX - OldX) =:= 2 -> { { NewX, NewY }, [] };
                       (Loc, _, _) -> { Loc, [] } end
      }

    ].

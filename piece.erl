%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2012, John Daily
%%% @doc
%%%
%%% @end
%%% Created : 30 Aug 2012 by John Daily <jd@epep.us>

-module(piece).
-compile(export_all).



%% @doc Maneuver is a function that determines the validity of the
%% attempted move, Location is an {x, y} tuple that indicates where
%% the piece is at the moment, and Team is black or white.
%%
%% Eventually I'll need additional PIDs to be passed in, like an
%% object to decide whether I'm pinned, something to help me decide
%% whether there are pieces in my way, etc.
%%
%% First step: just make the move.
piece_loop(Move, Capture, Location, Team) ->
    receive
        { From, move, {X, Y} } when X > 0, X < 9, Y > 0, Y < 9 ->
            piece_loop(Move, Capture,
                       move_response(From, Location, Move(Location, {X, Y}, Team)),
                       Team);
        { From, capture, {X, Y} } when X > 0, X < 9, Y > 0, Y < 9 ->
            piece_loop(Move, Capture,
                       move_response(From, Location, Capture(Location, {X, Y}, Team)),
                       Team);
        { _, _, {X, Y} } ->
            throw({invalid_destination, {X, Y}});
        { From, location } ->
            From ! Location,
            piece_loop(Move, Capture, Location, Team);
        { promote, NewMove, NewCapture } ->
            piece_loop(NewMove, NewCapture, Location, Team);
        done ->
            done;
        Message ->
            throw({unknown_message, Message})
    end.

%% @doc Tell the process which asked us to move whether the piece moved.
move_response(PID, Loc, Loc) ->
    PID ! { no, Loc },
    Loc;
move_response(PID, _, Loc) ->
    PID ! { yes, Loc },
    Loc.


movefuns() ->
    [ 
      { pawnmove, fun({ X, OldY }, { X, NewY }, white) when NewY - OldY =:= 1 -> { X, NewY };
                     ({ X, OldY }, { X, NewY }, black) when NewY - OldY =:= -1 -> { X, NewY };
                     ({ X, 2 }, { X, 4 }, white) -> { X, 4 };
                     ({ X, 7 }, { X, 5 }, black) -> { X, 5 };
                     (Loc, _, _) -> Loc end
      },

      { pawncap, fun({ OldX, OldY }, { NewX, NewY }, white)
                       when abs(NewX - OldX) =:= 1, NewY - OldY =:= 1 -> { NewX, NewY };
                    ({ OldX, OldY }, { NewX, NewY }, black)
                       when abs(NewX - OldX) =:= 1, NewY - OldY =:= -1 -> { NewX, NewY };
                    (Loc, _, _) -> Loc end
      },

      { kingmove, fun({ OldX, OldY }, { NewX, NewY }, _)
                        when abs(NewY - OldY) < 2, abs(NewX - OldX) < 2 -> { NewX, NewY };
                     (Loc, _, _) -> Loc end
      },

      { knightmove, fun({ OldX, OldY }, { NewX, NewY }, _)
                          when abs(NewY - OldY) =:= 2, abs(NewX - OldX) =:= 1 -> { NewX, NewY };
                       ({ OldX, OldY }, { NewX, NewY }, _)
                          when abs(NewY - OldY) =:= 1, abs(NewX - OldX) =:= 2 -> { NewX, NewY };
                       (Loc, _, _) -> Loc end
      }

    ].

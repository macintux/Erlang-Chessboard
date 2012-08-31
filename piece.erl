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
piece_loop(Maneuver, Location, Team) ->
    receive
	{ From, X, Y } when X > 0, X < 9, Y > 0, Y < 9 ->
            piece_loop(Maneuver,
                       move_response(From, Location, Maneuver(Location, {X, Y})),
                       Team);
        { From, location } ->
	    From ! Location,
            piece_loop(Maneuver, Location, Team);
        done ->
            done
    end.

%% @doc Tell the process which asked us to move whether the piece moved.
move_response(PID, Loc, Loc) ->
    PID ! { no, Loc },
    Loc;
move_response(PID, _, Loc) ->
    PID ! { yes, Loc },
    Loc.

	    



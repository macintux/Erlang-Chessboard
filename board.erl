%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2012, John Daily
%%% @doc
%%%
%%% @end
%%% Created :  6 Sep 2012 by John Daily <jd@epep.us>

-module(board).
-compile(export_all).
-import(piece).

mkrow(0, L, _) -> list_to_tuple(lists:reverse(L));
mkrow(N, L, Init) -> mkrow(N-1, [ spawn(piece, piece_loop, Init) | L], Init).


mkmatrix(0, _, _, M) -> list_to_tuple(lists:reverse(M));
mkmatrix(NR, NC, Init, M) ->
    Row = mkrow(NC, [], Init),
    mkmatrix(NR-1, NC, Init, [Row|M]).

mkmatrix(NR, NC) -> mkmatrix(NR, NC, [ blank_square, fun(Source, _Dest, _team) -> { Source, [] } end, fun(Source, _Dest, _team) -> { Source, [] } end ], []).

board_loop(Matrix) ->
    receive
        { Pid, move, {X, Y}, End, Team } ->
            element(X, element(Y, Matrix)) ! { self(), move, {X, Y}, End, Team, Pid },
            board_loop(Matrix);
        { Response, Piece, _Start, _End, _Traverse, Pid } ->
            Pid ! { Response, Piece },
            board_loop(Matrix)
    end.

    

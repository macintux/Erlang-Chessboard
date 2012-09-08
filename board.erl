%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2012, John Daily
%%% @doc
%%%
%%% @end
%%% Created :  6 Sep 2012 by John Daily <jd@epep.us>

-module(board).
-compile(export_all).
-import(piece).

mkrow(0, L) -> list_to_tuple(lists:reverse(L));
mkrow(N, L) -> mkrow(N-1, [ spawn(piece, blank_square, []) | L]).


mkmatrix(0, _, M) -> list_to_tuple(lists:reverse(M));
mkmatrix(NR, NC, M) ->
    Row = mkrow(NC, []),
    mkmatrix(NR-1, NC, [Row|M]).

mkmatrix(NR, NC) -> mkmatrix(NR, NC, []).

board_loop(Matrix) ->
    receive
        { Pid, move, {X, Y}, End } ->
            element(X, element(Y, Matrix)) ! { self(), move, {X, Y}, End, Pid },
            board_loop(Matrix);
        { Response, Piece, _Start, _End, _Traverse, Pid } ->
            Pid ! { Response, Piece },
            board_loop(Matrix)
    end.

    

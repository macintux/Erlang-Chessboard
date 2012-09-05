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

calculate_slope(Loc, Loc) ->
    none;
calculate_slope({_X1, _Y1}, {_X1, _Yf}) ->
    infinity;
calculate_slope({_X1, _Y1}, {_Xf, _Y1}) ->
    0.0;
calculate_slope({X1, Y1}, {Xf, Yf}) ->
    (Yf - Y1) / (Xf - X1).

next_point(Final, Final) ->
    [];
next_point({X1, Y1}, {Xf, Yf}) when X1 /= Xf, Y1 /= Yf ->
    M = calculate_slope({X1, Y1}, {Xf, Yf}),
    IntM = validate_diag_slope(M),
    B = Y1 - (IntM * X1),
    diag_next_point(IntM, trunc(B), {X1, Y1}, {Xf, Yf}, []);
next_point({X1, Y1}, {Xf, Yf}) when X1 =:= Xf ->
    row_next_point(X1, Y1, Yf, []);
next_point({X1, Y1}, {Xf, Yf}) when Y1 =:= Yf ->
    column_next_point(Y1, X1, Xf, []).
    

row_next_point(_, _Y, _Y, [_H|T]) ->
    lists:reverse(T);
row_next_point(X, Y1, Yf, Accum) when Y1 > Yf ->
    row_next_point(X, Y1 - 1, Yf, [{X, Y1 - 1}|Accum]);
row_next_point(X, Y1, Yf, Accum) when Y1 < Yf ->
    row_next_point(X, Y1 + 1, Yf, [{X, Y1 + 1}|Accum]).

column_next_point(_, _X, _X, [_H|T]) ->
    lists:reverse(T);
column_next_point(Y, X1, Xf, Accum) when X1 > Xf ->
    column_next_point(Y, X1 - 1, Xf, [{X1 - 1, Y}|Accum]);
column_next_point(Y, X1, Xf, Accum) when X1 < Xf ->
    column_next_point(Y, X1 + 1, Xf, [{X1 + 1, Y}|Accum]).


validate_diag_slope(1.0) ->
    1;
validate_diag_slope(-1.0) ->
    -1;
validate_diag_slope(M) ->
    throw({invalid_slope, M}).

diag_next_point(_, _, Final, Final, [_H|T]) ->
    lists:reverse(T);
diag_next_point(M, B, {X1, _Y}, {Xf, Yf}, Accum) when Xf < X1 ->
    NewX = X1 - 1,
    Loc = { NewX, M * NewX + B },
    diag_next_point(M, B, Loc, {Xf, Yf}, [Loc|Accum]);
diag_next_point(M, B, {X1, _Y}, {Xf, Yf}, Accum) when Xf > X1 ->
    NewX = X1 + 1,
    Loc = { NewX, M * NewX + B },
    diag_next_point(M, B, Loc, {Xf, Yf}, [Loc|Accum]).

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
      },

      { bishopmove, fun(Start, End, _) ->
                            case piece:calculate_slope(Start, End) of
                                1.0 ->
                                    { End, next_point(Start, End) };
                                -1.0 ->
                                    { End, next_point(Start, End) };
                                _ ->
                                    { Start, [] }
                            end
                    end
      },

      { rookmove, fun(Start, End, _) ->
                          case piece:calculate_slope(Start, End) of
                              0.0 ->
                                  { End, next_point(Start, End) };
                              infinity ->
                                  { End, next_point(Start, End) };
                              _ ->
                                  { Start, [] }
                            end
                    end
      },

      { queenmove, fun(Start, End, _) ->
                            case piece:calculate_slope(Start, End) of
                                1.0 ->
                                    { End, next_point(Start, End) };
                                -1.0 ->
                                    { End, next_point(Start, End) };
                                0.0 ->
                                    { End, next_point(Start, End) };
                                infinity ->
                                    { End, next_point(Start, End) };
                                _ ->
                                    { Start, [] }
                            end
                    end
      }

    ].

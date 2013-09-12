%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2012, John Daily
%%% @doc
%%%
%%% @end
%%% Created : 30 Aug 2012 by John Daily <jd@epep.us>

-module(newpiece).
-include_lib("eunit/include/eunit.hrl").
-include_lib("records.hrl").
-compile(export_all).

-spec testmove(move()) -> { boolean(), list(square()) }.
testmove(#move{
            piece=#piece{team=Team,movefun=Move},
            start=Start,
            target=End,
            movetype=move}) ->
    move_response(Start, End, Move(Start, End, Team));
testmove(#move{
            piece=#piece{team=Team,capturefun=Cap},
            start=Start,
            target=End,
            movetype=capture}) ->
    move_response(Start, End, Cap(Start, End, Team));
testmove(#move{
            piece=#piece{team=Team,movefun=Move},
            start=Start,
            target=End,
            movetype=castle}) ->
    move_response(Start, End, Move(Start, End, {Team, castle})).

%% Respond positively if I can attack the end square while traversing
%% the intermediate square - in other words, if I can successfully pin
%% a piece against its king
testlineattack(#move{
                  piece=#piece{team=Team,capturefun=Cap},
                  start=Start,
                  target=End,
                  movetype=capture},
               IntmdtSquare) ->
    case Cap(Start, End, Team) of
        { End, Traverse } ->
            case lists:member(IntmdtSquare, Traverse) of
                true ->
                    move_response(Start, End, { End, Traverse });
                false ->
                    move_response(Start, End, { Start, [] })
            end;
        _ ->
            move_response(Start, End, { Start, [] })
    end.

move_response(Start, _End, { Start, _Traverse }) ->
    { false, [] };
move_response(_Start, End, { End, Traverse }) ->
    { true, Traverse }.

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
    column_next_point(X1, Y1, Yf, []);
next_point({X1, Y1}, {Xf, Yf}) when Y1 =:= Yf ->
    row_next_point(Y1, X1, Xf, []).
    

column_next_point(_, _Y, _Y, [_H|T]) ->
    lists:reverse(T);
column_next_point(X, Y1, Yf, Accum) when Y1 > Yf ->
    column_next_point(X, Y1 - 1, Yf, [{X, Y1 - 1}|Accum]);
column_next_point(X, Y1, Yf, Accum) when Y1 < Yf ->
    column_next_point(X, Y1 + 1, Yf, [{X, Y1 + 1}|Accum]).

row_next_point(_, _X, _X, [_H|T]) ->
    lists:reverse(T);
row_next_point(Y, X1, Xf, Accum) when X1 > Xf ->
    row_next_point(Y, X1 - 1, Xf, [{X1 - 1, Y}|Accum]);
row_next_point(Y, X1, Xf, Accum) when X1 < Xf ->
    row_next_point(Y, X1 + 1, Xf, [{X1 + 1, Y}|Accum]).


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

np_diag1_test() ->
    [ { 3, 4 } ] = next_point( { 4, 5 }, { 2, 3 } ).
np_diag2_test() ->
    [ { 3, 4 }, { 2, 5 } ] = next_point( { 4, 3 }, { 1, 6 } ).
np_col1_test() ->
    [] = next_point( { 0, 0 }, { 0, -1 } ).
np_col2_test() ->
    [ {-3, -6 } ] = next_point( { -3, -7 }, { -3, -5 } ).
np_row1_test() ->
    [ { 1, 5 }, { 2, 5 }, { 3, 5 } ] = next_point( { 0, 5 }, { 4, 5 } ).

row_1_test() ->
    [ { -42, 15 }, { -41, 15 } ] = row_next_point(15, -43, -40, []).
row_2_test() ->
    [] = row_next_point(3, 4, 3, []).

col_1_test() ->
    [ { 8, 1 }, { 8, 2 }, { 8, 3 } ] = column_next_point(8, 0, 4, []).

slope_1_test() ->
    infinity = calculate_slope( { 3, 5 }, { 3, -14 } ).
slope_2_test() ->
    none = calculate_slope( { 3, 5 }, { 3, 5 } ).
slope_3_test() ->
    0.0 = calculate_slope( { 3, 5 }, { 99, 5 } ).
slope_4_test() ->
    -1.0 = calculate_slope( { 3, 5 }, { 8, 0 } ).
slope_5_test() ->
    1.0 = calculate_slope( { 3, 5 }, { -2, 0 } ).


-spec movefuns() -> list(movedef()).
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

      { kingmove, fun({ 5, 1 }, { NewX, 1 }, {white, castle})
                        when abs(NewX - 5) =:= 2 -> { { NewX, 1 }, [] };
                     ({ 5, 8 }, { NewX, 8 }, {black, castle})
                        when abs(NewX - 5) =:= 2 -> { { NewX, 8 }, [] };
                     ({ OldX, OldY }, { NewX, NewY }, _)
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
                            case calculate_slope(Start, End) of
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
                          case calculate_slope(Start, End) of
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
                            case calculate_slope(Start, End) of
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

queen_nomove_test() ->
    [ Q ] = [ X || {queenmove, X} <- movefuns() ],
    { {3, 3}, [] } = Q( {3, 3}, {5, 6}, white ).

queen_diag_test() ->
    [ Q ] = [ X || {queenmove, X} <- movefuns() ],
    { {6, 42}, [ { 8, 40 }, { 7, 41 } ] } = Q( { 9, 39 }, { 6, 42 }, black ).

queen_col_test() ->
    [ Q ] = [ X || {queenmove, X} <- movefuns() ],
    { {6, 42}, [ { 6, 40 }, { 6, 41 } ] } = Q( { 6, 39 }, { 6, 42 }, black ).

rook_col_test() ->
    [ R ] = [ X || {rookmove, X} <- movefuns() ],
    { {2, 35}, [ { 2, 36 } ] } = R( { 2, 37 }, { 2, 35 }, black ).

rook_row_test() ->
    [ R ] = [ X || {rookmove, X} <- movefuns() ],
    { {2, 35}, [ { 3, 35 } ] } = R( { 4, 35 }, { 2, 35 }, black ).

pawn_nocap_test() ->
    [ Pc ] = [ X || {pawncap, X} <- movefuns() ],
    { {3, 4}, [] } = Pc({3, 4}, {3, 5}, white).

pawn_cap1_test() ->
    [ Pc ] = [ X || {pawncap, X} <- movefuns() ],
    { {4, 5}, [] } = Pc({3, 4}, {4, 5}, white).

pawn_cap2_test() ->
    [ Pc ] = [ X || {pawncap, X} <- movefuns() ],
    { {4, 5}, [] } = Pc({5, 6}, {4, 5}, black).

pawn_move1_test() ->
    [ Pm ] = [ X || {pawnmove, X} <- movefuns() ],
    { {4, 4}, [{4, 3}] } = Pm({4, 2}, {4, 4}, white).

pawn_move2_test() ->
    [ Pm ] = [ X || {pawnmove, X} <- movefuns() ],
    { {4, 2}, [] } = Pm({4, 2}, {4, 4}, black).

pawn_move3_test() ->
    [ Pm ] = [ X || {pawnmove, X} <- movefuns() ],
    { {4, 4}, [] } = Pm({4, 3}, {4, 4}, white).

pawn_move4_test() ->
    [ Pm ] = [ X || {pawnmove, X} <- movefuns() ],
    { {4, 4}, [] } = Pm({4, 5}, {4, 4}, black).

%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2012, John Daily
%%% @doc
%%%
%%% @end
%%% Created :  6 Sep 2012 by John Daily <jd@epep.us>

-module(newboard).
-compile(export_all).
-include_lib("records.hrl").
-import(orddict, [ new/0, store/3, find/2 ]).
-import(newpiece).

init() ->
    RowSequence =
        [ { pieces, [ { rook, rookmove, rookmove },
                      { knight, knightmove, knightmove },
                      { bishop, bishopmove, bishopmove },
                      { queen, queenmove, queenmove },
                      { king, kingmove, kingmove },
                      { bishop, bishopmove, bishopmove },
                      { knight, knightmove, knightmove },
                      { rook, rookmove, rookmove }
                    ]
          },
          { pawns, [ { pawn, pawnmove, pawncap },
                     { pawn, pawnmove, pawncap },
                     { pawn, pawnmove, pawncap },
                     { pawn, pawnmove, pawncap },
                     { pawn, pawnmove, pawncap },
                     { pawn, pawnmove, pawncap },
                     { pawn, pawnmove, pawncap },
                     { pawn, pawnmove, pawncap }
                   ]
          }
        ],
    Board = initialize(new(), RowSequence),
    MyState = #boardstate{pieces=Board},
    spawn(?MODULE, board_loop, [ MyState ]).

board_loop(MyState) ->
    receive
        { Pid, showboard } ->
            io:format("~s~n", [matrix_to_text(MyState#boardstate.pieces)]),
            Pid ! io_lib:format("~s~n", [matrix_to_text(MyState#boardstate.pieces)]),
            board_loop(MyState);
        { Pid, move, Start, End } ->
            case evaluate_legal_move(MyState, move, Start, End) of
                { true, Piece, undefined } ->
                    %% Don't forget to update history on the piece
                    NewDict =
                        store(Start, #piece{type=none},
                              store(End, Piece, MyState#boardstate.pieces)
                             ),
                    %% Must also update whiteking/blackking if one of them moves
                    NewState = #boardstate{ whiteking=MyState#boardstate.whiteking,
                                            blackking=MyState#boardstate.blackking,
                                            pieces=NewDict },
                    IsCheckOrMate = check_or_mate(NewState),
                    Pid ! { move, true, IsCheckOrMate },
                    case IsCheckOrMate of
                        { checkmate, WhoWins } ->
                            io:format("~s~n", [matrix_to_text(NewState#boardstate.pieces)]),
                            exit(WhoWins);
                        _ ->
                            nothing_to_do
                    end,
                    board_loop(NewState);
                { false, Reason } ->
                    Pid ! { false, Reason },
                    board_loop(MyState)
            end;
        { Pid, examine, Square} ->
            { ok, Piece } = find(Square, MyState#boardstate.pieces),
            Pid ! Piece,
            board_loop(MyState)
    end.

%% XXX populate later
check_or_mate(_State) ->
    none.

%% Castling will be a whole different matter.  False for now
evaluate_legal_move(_State, castle, _Start, _End) ->
    { false, "Castling not yet supported" };
evaluate_legal_move(State, MoveType, Start, End) ->
    { ok, Piece } = find(Start, State#boardstate.pieces),
    { CanMove, Traverse } =
        newpiece:testmove(#move{
                             piece=Piece,
                             start=Start,
                             target=End,
                             movetype=MoveType
                            }),
    case MoveType of
        move ->
            %% Must make sure no piece at end as well as nothing in the path
            evaluate_legal_aux(CanMove, move, Traverse ++ End, Start, End, Piece, State);
        capture ->
            %% Make sure there is a piece at the target before wasting our time
            case find(End, State#boardstate.pieces) of
                { ok, #piece{type=none} } ->
                    { false, io_lib:format("No piece at ~p to capture", [End]) };
                _ ->
                    evaluate_legal_aux(CanMove, capture, Traverse, Start, End, Piece, State)
            end
    end.

%% First, illegal moves
evaluate_legal_aux(false, _, _, _, _, Piece, _) ->
    { false, io_lib:format("Illegal move for ~s", [Piece#piece.type]) };
%% No piece in the way, check for pins
evaluate_legal_aux(true, move, [], Start, _End, Piece, State) ->
    case look_for_pins(Start, Piece#piece.team,
                       State#boardstate.whiteking, State#boardstate.blackking) of
        { true, PinningPiece, Where } ->
            { false, io_lib:format("Piece pinned by ~s at ~p",
                                   [PinningPiece#piece.type,
                                    Where]) };
        false ->
            { true, Piece, undefined }
    end;
%% Attempted capture, no piece in the way
evaluate_legal_aux(true, capture, [], Start, End, Piece, State) ->
    case look_for_pins(Start, Piece#piece.team,
                       State#boardstate.whiteking, State#boardstate.blackking) of
        { true, PinningPiece, Where } ->
            { false, io_lib:format("Piece pinned by ~s at ~p",
                                   [PinningPiece#piece.type,
                                    Where]) };
        false ->
            { ok, Captured } = find(End, State#boardstate.pieces),
            { true, Piece, Captured }
    end;
%% Attempted move or capture, evaluate traversal squares for obstacles
evaluate_legal_aux(true, MoveType, [H | T], Start, End, Piece, State) ->
    case find(H, State#boardstate.pieces) of
        { ok, #piece{type=none} } ->
            evaluate_legal_aux(true, MoveType, T, Start, End, Piece, State);
        { ok, #piece{type=Obstacle} } ->
            { false, io_lib:format("Blocking piece (~s) at ~p", [Obstacle, H]) }
    end.
    

%% XXX populate later
look_for_pins(_Square, white, _WhiteKing, _BlackKing) ->
    false;
look_for_pins(_Square, black, _WhiteKing, _BlackKing) ->
    false.



matrix_to_text(Matrix) ->
    row_to_text(8, 1, Matrix, []).

%% X and Y are swapped here because we're focusing on rows.  Made more
%% sense when I had a tuple of rows to navigate as my data structure.
row_to_text(0, _, _, Accum) ->
    Accum ++ "\n";
row_to_text(Y, 9, Matrix, Accum) ->
    row_to_text(Y - 1, 1, Matrix, ["\n\n|" | Accum]);
row_to_text(Y, X, Matrix, Accum) ->
    row_to_text(Y, X + 1, Matrix, [ cell_to_text({X, Y}, Matrix) ++ "|" | Accum ]).

cell_to_text(Square, Matrix) ->
    {ok, #piece{type=Type}} = find(Square, Matrix),
    piece_to_shorthand(Type).

piece_to_shorthand(Piece) ->
    case Piece of 
        none ->
            "   ";
        pawn ->
            " p ";
        rook ->
            " R ";
        knight ->
            " N ";
        bishop ->
            " B ";
        queen ->
            " Q ";
        king ->
            " K "
    end.

initialize(Board, Proplist) ->
    Pieces = proplists:get_value(pieces, Proplist),
    Pawns = proplists:get_value(pawns, Proplist),
    %% The Board dictionary gets revised with each call to initialize_row
    Board1 = initialize_row(Board, Pieces, {1, 1}, white),
    Board2 = initialize_row(Board1, Pawns, {1, 2}, white),
    Board3 = initialize_blanks(Board2, 1, 3),
    Board4 = initialize_row(Board3, Pawns, {1, 7}, black),
    initialize_row(Board4, Pieces, {1, 8}, black).

initialize_blanks(Board, _, 7) ->
    Board;
initialize_blanks(Board, 9, Y) ->
    initialize_blanks(Board, 1, Y + 1);
initialize_blanks(Board, X, Y) ->
    initialize_blanks(store({X, Y}, #piece{type=none}, Board), X + 1, Y).
    
initialize_row(Board, [], _, _) ->
    Board;
initialize_row(Board, [{Piece, M, C}|T], {X, Y}, Team) ->
    Move = proplists:get_value(M, newpiece:movefuns()),
    Capture = proplists:get_value(C, newpiece:movefuns()),
    initialize_row(store({X, Y}, #piece{type=Piece,
                                        team=Team,
                                        movefun=Move,
                                        capturefun=Capture},
                        Board),
                   T, {X + 1, Y}, Team).
    
    
    


    %% Pieces = [ { rook, [ { 1, 1 }, { 8, 1 }, { 1, 8 }, { 8, 8 } ] },
    %%            { bishop, [ { 2, 1 }, { 7, 1 }, { 2, 8 }, { 7, 8 } ] },
    %%            { knight, [ { 3, 1 }, { 6, 

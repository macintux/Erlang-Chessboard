%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2012, John Daily
%%% @doc
%%%
%%% @end
%%% Created :  6 Sep 2012 by John Daily <jd@epep.us>

-module(board).
-compile(export_all).
-import(orddict, [ dict, store, find ]).
-import(piece).

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
    Board = initialize(dict(), MoveFuns, RowSequence),
    MyState = #boardstate{pieces=Board},
    spawn(?MODULE, board_loop, [ MyState ]).

matrix_to_text(Matrix) ->
    row_to_text(8, 1, Matrix, []).

row_to_text(0, _, Accum) ->
    Accum;
row_to_text(Y, 9, Matrix, Accum) ->
    row_to_text(Y - 1, 1, Matrix, ["\n\n" | Accum]);
row_to_text(Y, X, Matrix, Accum) ->
    row_to_text(Y, X - 1, Matrix, [ cell_to_text({X, Y}, Matrix) | Accum ]).

cell_to_text(Square, Matrix) ->
    piece_to_shorthand(find(Square, Matrix)).

piece_to_shorthand(DictResponse) ->
    case DictResponse of 
        error ->
            "   ";
        {ok, #piece{type = pawn}} ->
            " p ";
        {ok, #piece{type = rook}} ->
            " R ";
        {ok, #piece{type = knight}} ->
            " N ";
        {ok, #piece{type = bishop}} ->
            " B ";
        {ok, #piece{type = queen}} ->
            " Q ";
        {ok, #piece{type = king}} ->
            " K "
    end.

%% If no elements in last list ("Traverse") then this must be a knight
%% or one-square move.  In either case, we don't need to check for
%% obstacles
context_allows_move(Matrix, Piece, Team, Start, End, Traverse, WhiteKingLoc, BlackKingLoc) ->
    case check_for_obstacles(Matrix, Traverse) of
        { yes, Piece } ->
            { false, "Piece in way: " ++ [ atom_to_list(Piece) ] };
        { no } ->
            check_for_pin(Matrix, Piece, Team, Start, End, WhiteKingLoc, BlackKingLoc)
    end.


check_for_obstacles(_, []) ->
    { no };
check_for_obstacles(Matrix, [H | T]) ->
    case find(H, Matrix) of
        error ->
            check_for_obstacles(Matrix, T);
        { ok, #piece{type = PieceType } ->
            { yes, PieceType }
    end.

%% scan_matrix(Matrix, Callback) ->
%%     scan_matrix(Matrix, Callback, 8).

%% scan_matrix(Matrix, Callback, 0) ->
%%     done;
%% scan_matrix(Matrix, Callback, X) ->

    
    
%% find_king(Matrix, Team) ->
    

%% XXX Comments here make no sense to me now
%% For each piece on the opposite team
%%   See if the piece can attack the king's target square
%%   See if Loc is in the list of traversed squares
%%   See if any other pieces are in the list of traversed squares
%% check_for_check(Matrix, Team, KingLoc, Target) ->
%%     check_for_attack(Matrix, Team, Target).


%% check_dual_capture(Matrix, Team, Me, MyKing) ->
    

%% check_for_pin(Matrix, king, Team, Loc, Target, WhiteKingLoc, BlackKingLoc) ->
%%     %% King cannot be pinned but must not move into check
%%     check_for_check(Matrix, Team, Loc, Target);
%% check_for_pin(Matrix, Piece, white, Loc, Target, WhiteKingLoc, BlackKingLoc) ->
%%     %% Check to see if any non-king/non-knight/non-pawn piece can
%%     %% attack both this piece and its king
%%     check_dual_capture(Matrix, black, Loc, WhiteKingLoc).
%% check_for_pin(Matrix, Piece, black, Loc, Target, WhiteKingLoc, BlackKingLoc) ->
%%     %% Check to see if any non-king/non-knight/non-pawn piece can
%%     %% attack both this piece and its king
%%     check_dual_capture(Matrix, white, Loc, BlackKingLoc).

%% Each king will be a critical part of each move, so it's much more
%% efficient to track each king's location than to re-scan.  Each
%% "*King" argument is a location tuple.
board_loop(Matrix, WhiteKing, BlackKing) ->
    receive
        { showboard } ->
            io:format("~s~n", [matrix_to_text(Matrix)]);
        { Pid, move, Start, End } ->
            Square = element_of(Matrix, Start),
            Square ! { self(), testmove, Start, End, Pid },
            receive
                { yes, Piece, Team, _, _, Traverse, _ } ->
                    %% For a move (not a capture) the end square must be checked for obstacles
                    TraversedSquares = [ End | Traverse ],
                    case context_allows_move(Matrix, Piece, Team, Start, TraversedSquares, WhiteKing, BlackKing) of
                        { false, Reason } ->
                            Pid ! { no, Reason };
                        { true, NewSquare } ->
                            Square ! { moveto, Start, End, NewSquare },
                            Pid ! { yes },
                            case { Piece, Team } of
                                { king, white } ->
                                    board_loop(Matrix, End, BlackKing);
                                { king, black } ->
                                    board_loop(Matrix, WhiteKing, End)
                            end
                    end,
                { no, Piece, Team, _, _, _, _ } ->
                    Pid ! { no, Piece, Team, "Illegal move" }
            end,
        { Response, Piece, _Start, _End, _Traverse, Pid } ->
            Pid ! { Response, Piece },
            board_loop(Matrix)
    end.

replace(Matrix, {X, Y}, Piece, Team, Move, Capture) ->
    element_of(Matrix, { X, Y }) ! { replace, Piece, Team, [], Move, Capture }.

initialize(Board, MoveFuns, Proplist) ->
    Pieces = proplists:get_value(pieces, Proplist),
    Pawns = proplists:get_value(pawns, Proplist),
    %% The Board dictionary gets revised with each call to initialize_row
    Board1 = initialize_row(Board, Pieces, {1, 1}, white),
    Board2 = initialize_row(Board1, Pawns, {1, 2}, white),
    Board3 = initialize_row(Board2, Pawns, {1, 7}, black),
    initialize_row(Board3, Pieces, {1, 8}, black).

initialize_row(Board, [], _, _) ->
    Board;
initialize_row(Board, [{Piece, M, C}|T], {X, Y}, Team) ->
    Move = proplists:get_value(M, piece:movefuns()),
    Capture = proplists:get_value(C, piece:movefuns()),
    initialize(store({X, Y}, #piece{type=Piece,
                                    team=Team,
                                    movefun=Move,
                                    capturefun=Capture}),
               T, {X + 1, Y}, Team).
    
    
    


    %% Pieces = [ { rook, [ { 1, 1 }, { 8, 1 }, { 1, 8 }, { 8, 8 } ] },
    %%            { bishop, [ { 2, 1 }, { 7, 1 }, { 2, 8 }, { 7, 8 } ] },
    %%            { knight, [ { 3, 1 }, { 6, 

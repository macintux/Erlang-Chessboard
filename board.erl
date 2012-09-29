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

element_of(Matrix, { X, Y }) ->
    element(X, element(Y, Matrix)).

init() ->
    Matrix = mkmatrix(8, 8),
    Pieces = [ { pieces, [ { rook, rookmove, rookmove },
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
    initialize(Matrix, Pieces),
    spawn(?MODULE, board_loop, [ Matrix, { 5, 1 }, { 5, 8 } ]).

matrix_to_text(Matrix) ->
    row_to_text(8, Matrix, []).

row_to_text(0, _, Accum) ->
    Accum;
row_to_text(N, Matrix, Accum) ->
    Row = element(N, Matrix),
    row_to_text(N - 1, Matrix, [ cell_to_text(8, Row, ["|\n\n"]) | Accum ]).


cell_to_text(0, _, Accum) ->
    ["|" | Accum];
cell_to_text(N, Row, Accum) when N > 0 ->
    cell_to_text(N - 1, Row, [ proc_to_text(element(N, Row)) | Accum ]).

piece_to_shorthand(Piece) ->
    case Piece of
        blank_square ->
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

proc_to_text(Pid) ->
    Pid ! { self(), what_am_i, none },
    receive
        { Piece, _Team, _, _ } ->
            piece_to_shorthand(Piece)
    end.

proc_to_piece(Pid) ->
    Pid ! { self(), what_am_i, none },
    receive
        { Piece, Team, _, _ } ->
            { Piece, Team }
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
    case proc_to_piece(element_of(Matrix, H)) of
        { blank_square, _ } ->
            check_for_obstacles(Matrix, T);
        { Piece, _ } ->
            { no, Piece }
    end.

scan_matrix(Matrix, Callback) ->
    scan_matrix(Matrix, Callback, 8).

scan_matrix(Matrix, Callback, 0) ->
    done;
scan_matrix(Matrix, Callback, X) ->
    
    
    
%% find_king(Matrix, Team) ->
    

%% XXX Comments here make no sense to me now
%% For each piece on the opposite team
%%   See if the piece can attack the king's target square
%%   See if Loc is in the list of traversed squares
%%   See if any other pieces are in the list of traversed squares
check_for_check(Matrix, Team, KingLoc, Target) ->
    check_for_attack(Matrix, Team, Target).


check_dual_capture(Matrix, Team, Me, MyKing) ->
    

check_for_pin(Matrix, king, Team, Loc, Target, WhiteKingLoc, BlackKingLoc) ->
    %% King cannot be pinned but must not move into check
    check_for_check(Matrix, Team, Loc, Target);
check_for_pin(Matrix, Piece, white, Loc, Target, WhiteKingLoc, BlackKingLoc) ->
    %% Check to see if any non-king/non-knight/non-pawn piece can
    %% attack both this piece and its king
    check_dual_capture(Matrix, black, Loc, WhiteKingLoc).
check_for_pin(Matrix, Piece, black, Loc, Target, WhiteKingLoc, BlackKingLoc) ->
    %% Check to see if any non-king/non-knight/non-pawn piece can
    %% attack both this piece and its king
    check_dual_capture(Matrix, white, Loc, BlackKingLoc).

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

initialize(Matrix, Proplist) ->
    Pieces = proplists:get_value(pieces, Proplist),
    Pawns = proplists:get_value(pawns, Proplist),
    initialize(Matrix, Pieces, 1, 1, white),
    initialize(Matrix, Pawns, 1, 2, white),
    initialize(Matrix, Pawns, 1, 7, black),
    initialize(Matrix, Pieces, 1, 8, black).

initialize(_, [], _, _, _) ->
    done;
initialize(Matrix, [{Piece, M, C}|T], X, Y, Team) ->
    Move = proplists:get_value(M, piece:movefuns()),
    Capture = proplists:get_value(C, piece:movefuns()),
    replace(Matrix, {X, Y}, Piece, Team, Move, Capture),
    initialize(Matrix, T, X + 1, Y, Team).
    
    
    


    %% Pieces = [ { rook, [ { 1, 1 }, { 8, 1 }, { 1, 8 }, { 8, 8 } ] },
    %%            { bishop, [ { 2, 1 }, { 7, 1 }, { 2, 8 }, { 7, 8 } ] },
    %%            { knight, [ { 3, 1 }, { 6, 

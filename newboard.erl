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
    Board = initialize(new(), RowSequence).
%    MyState = #boardstate{pieces=Board},
%    spawn(?MODULE, board_loop, [ MyState ]).

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

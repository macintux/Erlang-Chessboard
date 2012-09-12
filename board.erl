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
    spawn(?MODULE, board_loop, [ Matrix ]).
        
board_loop(Matrix) ->
    receive
        { Pid, testmove, {X, Y}, End } ->
            element_of(Matrix, {X, Y}) ! { self(), testmove, {X, Y}, End, Pid },
            board_loop(Matrix);
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

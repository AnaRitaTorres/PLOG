%%%%%%%%%%%%%%%%%%%%%%%%
%%% BOARD MANAGEMENT %%%
%%%%%%%%%%%%%%%%%%%%%%%%

dentroTabuleiro(X, Y) :- 	X >= 0, X =< 11,
							Y >= 0, Y =< 11.

naoSiPropria(X1,Y1,X2,Y2):- X1 \= X2 ; Y1 \= Y2.
							
vertical(X1, X2) :- 	X1 =:= X2.
vertical(X1, _, X2, _) :- vertical(X1, X2).

horizontal(Y1, Y2) :- 	Y1 =:= Y2.
horizontal(_, Y1, _, Y2) :- horizontal(Y1, Y2).

diagonal(X1, Y1, X2, Y2) :- abs(X1 - X2) =:= abs(Y1 - Y2).

distance(X1, Y1, X2, Y2, Distance) :- Distance = sqrt( (X1 - X2)*(X1 - X2) + (Y1 - Y2)*(Y1 - Y2) ).

classifyMovement(CurrentX, CurrentY, TargetX, TargetY, Movement) :- CurrentX =:= TargetX ->
																		(CurrentY =:= TargetY -> Movement = sameSpot, !;
																		CurrentY < TargetY -> Movement = verticalDown, !;
																		CurrentY > TargetY -> Movement = verticalUp, !);
																	CurrentX < TargetX -> 
																		(CurrentY =:= TargetY -> Movement = horizontalRight, !;
																		CurrentY < TargetY -> Movement = diagonalRightDown, !;
																		CurrentY > TargetY -> Movement = diagonalRightUp, !);
																	CurrentX > TargetX ->
																		(CurrentY =:= TargetY -> Movement = horizontalLeft, !;
																		CurrentY < TargetY -> Movement = diagonalLeftDown, !;
																		CurrentY > TargetY -> Movement = diagonalLeftUp, !).

piecesBetween(X, Y, TargetX, TargetY, BoardIn, Pieces) :- 	classifyMovement(X,Y,TargetX,TargetY,Movement),
															piecesBetweenAux(Movement, X, Y, TargetX, TargetY, BoardIn, -1, Pieces).
piecesBetweenAux(Movement, X, Y, TargetX, TargetY, BoardIn, Pieces, ReturnPieces) :- Movement == verticalUp -> 
																			((Y =:=  TargetY -> ReturnPieces = Pieces, !);
																			getPiece(X, Y, BoardIn, Symbol),
																			(Symbol \= empty -> PiecesNew is (Pieces + 1);
																								PiecesNew is Pieces),
																								piecesBetweenAux(Movement, X, Y - 1, TargetX, TargetY, BoardIn, PiecesNew, ReturnPieces));
																		Movement == verticalDown -> 
																			((Y =:=  TargetY -> ReturnPieces = Pieces, !);
																			getPiece(X, Y, BoardIn, Symbol),
																			(Symbol \= empty -> PiecesNew is (Pieces + 1);
																								PiecesNew is Pieces),
																								piecesBetweenAux(Movement, X, Y + 1, TargetX, TargetY, BoardIn, PiecesNew, ReturnPieces));
																		Movement == horizontalLeft -> 
																			((X =:=  TargetX -> ReturnPieces = Pieces, !);
																			getPiece(X, Y, BoardIn, Symbol),
																			(Symbol \= empty -> PiecesNew is (Pieces + 1);
																								PiecesNew is Pieces),
																								piecesBetweenAux(Movement, X - 1, Y, TargetX, TargetY, BoardIn, PiecesNew, ReturnPieces));
																		Movement == horizontalRight -> 
																			((X =:=  TargetX -> ReturnPieces = Pieces, !);
																			getPiece(X, Y, BoardIn, Symbol),
																			(Symbol \= empty -> PiecesNew is (Pieces + 1);
																								PiecesNew is Pieces),
																								piecesBetweenAux(Movement, X + 1, Y, TargetX, TargetY, BoardIn, PiecesNew, ReturnPieces));
																		Movement == diagonalLeftUp -> 
																			(((X =:=  TargetX, Y =:= TargetY) -> ReturnPieces = Pieces, !);
																			getPiece(X, Y, BoardIn, Symbol),
																			(Symbol \= empty -> PiecesNew is (Pieces + 1);
																								PiecesNew is Pieces),
																								piecesBetweenAux(Movement, X - 1, Y - 1, TargetX, TargetY, BoardIn, PiecesNew, ReturnPieces));
																		Movement == diagonalLeftDown -> 
																			(((X =:=  TargetX, Y =:= TargetY) -> ReturnPieces = Pieces, !);
																			getPiece(X, Y, BoardIn, Symbol),
																			(Symbol \= empty -> PiecesNew is (Pieces + 1);
																								PiecesNew is Pieces),
																								piecesBetweenAux(Movement, X - 1, Y + 1, TargetX, TargetY, BoardIn, PiecesNew, ReturnPieces));
																		Movement == diagonalRightUp -> 
																			(((X =:=  TargetX, Y =:= TargetY) -> ReturnPieces = Pieces, !);
																			getPiece(X, Y, BoardIn, Symbol),
																			(Symbol \= empty -> PiecesNew is (Pieces + 1);
																								PiecesNew is Pieces),
																								piecesBetweenAux(Movement, X + 1, Y - 1, TargetX, TargetY, BoardIn, PiecesNew, ReturnPieces));
																		Movement == diagonalRightDown -> 
																			(((X =:=  TargetX, Y =:= TargetY) -> ReturnPieces = Pieces, !);
																			getPiece(X, Y, BoardIn, Symbol),
																			(Symbol \= empty -> PiecesNew is (Pieces + 1);
																								PiecesNew is Pieces),
																								piecesBetweenAux(Movement, X + 1, Y + 1, TargetX, TargetY, BoardIn, PiecesNew, ReturnPieces)).
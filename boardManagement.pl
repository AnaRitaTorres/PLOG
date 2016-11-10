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


piecesBetweenAux(verticalUp, X, Y, TargetX, TargetY, BoardIn, Pieces, ReturnPieces) :-	(Y =:=  TargetY -> ReturnPieces = Pieces, !);
																						getPiece(X, Y, BoardIn, Symbol),
																						(Symbol \= empty -> PiecesNew is (Pieces + 1);
																								PiecesNew is Pieces),
																								piecesBetweenAux(verticalUp, X, Y - 1, TargetX, TargetY, BoardIn, PiecesNew, ReturnPieces).
piecesBetweenAux(verticalDown, X, Y, TargetX, TargetY, BoardIn, Pieces, ReturnPieces) :-	(Y =:=  TargetY -> ReturnPieces = Pieces, !);
																							getPiece(X, Y, BoardIn, Symbol),
																							(Symbol \= empty -> PiecesNew is (Pieces + 1);
																								PiecesNew is Pieces),
																								piecesBetweenAux(verticalDown, X, Y + 1, TargetX, TargetY, BoardIn, PiecesNew, ReturnPieces).
piecesBetweenAux(horizontalLeft, X, Y, TargetX, TargetY, BoardIn, Pieces, ReturnPieces) :-	(X =:=  TargetX -> ReturnPieces = Pieces, !);
																							getPiece(X, Y, BoardIn, Symbol),
																							(Symbol \= empty -> PiecesNew is (Pieces + 1);
																								PiecesNew is Pieces),
																								piecesBetweenAux(horizontalLeft, X - 1, Y, TargetX, TargetY, BoardIn, PiecesNew, ReturnPieces).
piecesBetweenAux(horizontalRight, X, Y, TargetX, TargetY, BoardIn, Pieces, ReturnPieces) :-	(X =:=  TargetX -> ReturnPieces = Pieces, !);
																							getPiece(X, Y, BoardIn, Symbol),
																							(Symbol \= empty -> PiecesNew is (Pieces + 1);
																								PiecesNew is Pieces),
																								piecesBetweenAux(horizontalRight, X + 1, Y, TargetX, TargetY, BoardIn, PiecesNew, ReturnPieces).	
piecesBetweenAux(diagonalLeftUp, X, Y, TargetX, TargetY, BoardIn, Pieces, ReturnPieces) :-	((X =:=  TargetX, Y =:= TargetY) -> ReturnPieces = Pieces, !);
																							getPiece(X, Y, BoardIn, Symbol),
																							(Symbol \= empty -> PiecesNew is (Pieces + 1);
																								PiecesNew is Pieces),
																								piecesBetweenAux(diagonalLeftUp, X - 1, Y - 1, TargetX, TargetY, BoardIn, PiecesNew, ReturnPieces).																									
piecesBetweenAux(diagonalLeftDown, X, Y, TargetX, TargetY, BoardIn, Pieces, ReturnPieces) :-((X =:=  TargetX, Y =:= TargetY) -> ReturnPieces = Pieces, !);
																							getPiece(X, Y, BoardIn, Symbol),
																							(Symbol \= empty -> PiecesNew is (Pieces + 1);
																								PiecesNew is Pieces),
																								piecesBetweenAux(diagonalLeftDown, X - 1, Y + 1, TargetX, TargetY, BoardIn, PiecesNew, ReturnPieces).
piecesBetweenAux(diagonalRightUp, X, Y, TargetX, TargetY, BoardIn, Pieces, ReturnPieces) :- ((X =:=  TargetX, Y =:= TargetY) -> ReturnPieces = Pieces, !);
																							getPiece(X, Y, BoardIn, Symbol),
																							(Symbol \= empty -> PiecesNew is (Pieces + 1);
																								PiecesNew is Pieces),
																								piecesBetweenAux(diagonalRightUp, X + 1, Y - 1, TargetX, TargetY, BoardIn, PiecesNew, ReturnPieces).
piecesBetweenAux(diagonalRightDown, X, Y, TargetX, TargetY, BoardIn, Pieces, ReturnPieces) :- ((X =:=  TargetX, Y =:= TargetY) -> ReturnPieces = Pieces, !);
																								getPiece(X, Y, BoardIn, Symbol),
																								(Symbol \= empty -> PiecesNew is (Pieces + 1);
																								PiecesNew is Pieces),
																								piecesBetweenAux(diagonalRightDown, X + 1, Y + 1, TargetX, TargetY, BoardIn, PiecesNew, ReturnPieces).
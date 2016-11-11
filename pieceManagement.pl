%%%%%%%%%%%%%%%%%%%%%%%%
%%% PIECE MANAGEMENT %%% 
%%%%%%%%%%%%%%%%%%%%%%%%

% ------------------------------------------------------------
% --------------------- GET PIECE ----------------------------
% ------------------------------------------------------------

reachedY(Y, [Head|_], Row) :- Y == 0, Row = Head.
reachedY(Y, [_|OtherRows], Row) :-  Y \= 0 -> (Y1 is Y-1, reachedY(Y1, OtherRows, Row)).

reachedX(X, [Head|_], Elem) :- X == 0, Elem = Head.
reachedX(X, [_|OtherElems], Elem) :- X \= 0 -> (X1 is X-1, reachedX(X1, OtherElems, Elem)).

getPiece(X, Y, Board, Symbol) :- dentroTabuleiro(X,Y), (X,Y,Board)^(reachedY(Y, Board, Row), reachedX(X, Row, Elem), Symbol = Elem).

% ------------------------------------------------------------
% --------------------- SET PIECE ----------------------------
% ------------------------------------------------------------

setPiece(X, Y, Symbol, BoardIn, BoardOut) :- 	setPieceLineAux(0,X,Y,Symbol,BoardIn,BoardOut), !.

setPieceLineAux(_,_,_,_,[],[]).
setPieceLineAux(Y, X, Y, Symbol, [Line | Tail], [Line2 | Tail2]) :- 	setPieceColAux(0, X, Symbol, Line, Line2),
																		Ynow is Y + 1,
																		setPieceLineAux(Ynow, X, Y, Symbol, Tail, Tail2).
setPieceLineAux(Ynow, X, Y, Symbol, [Line | Tail], [Line | Tail2]) :- 	Ynow \= Y,
																		Ynow2 is Ynow + 1,
																		setPieceLineAux(Ynow2, X, Y, Symbol, Tail, Tail2).

setPieceColAux(_,_,_,[],[]).
setPieceColAux(X, X, Symbol, [ _ | Tail], [Symbol | Tail2]) :- 			Xnow is X + 1,
																		setPieceColAux(Xnow, X, Symbol, Tail, Tail2).																	
setPieceColAux(Xnow, X, Symbol, [Element | Tail], [Element | Tail2]) :- Xnow \= X,
																		Xnow2 is Xnow + 1,
																		setPieceColAux(Xnow2, X, Symbol, Tail, Tail2).

% ----------------------------------------------------------
% --------------------- FIND QUEEN -------------------------
% ----------------------------------------------------------																	

findQueen(Player, BoardIn, QueenX, QueenY) :- Player == ivory, findQueenAux(ivoryQueen, BoardIn, 0, 0, QueenX, QueenY).

findQueen(Player, BoardIn, QueenX, QueenY) :- Player == cigar, findQueenAux(cigarQueen, BoardIn, 0, 0, QueenX, QueenY).
												
findQueenAux(_,[],_,_,_,_).
findQueenAux(Queen, [ThisRow | OtherRows], X, Y, QueenX, QueenY) :- findQueenAuxTrue(Queen, ThisRow, X, Y, QueenX, QueenY),
																	Ynow is Y + 1,
																	findQueenAux(Queen, OtherRows, X, Ynow, QueenX, QueenY).

findQueenAuxTrue(_,[],_,_,_,_).																
findQueenAuxTrue(Queen, [Elem | OtherElems], X, Y, QueenX, QueenY) :- (Elem == Queen -> QueenX is X, QueenY is Y, !;
																		Xnow is X + 1, findQueenAuxTrue(Queen, OtherElems, Xnow, Y, QueenX, QueenY)).
								
% ----------------------------------------------------------
% --------------------- TRUTH PREDICATES -------------------
% ----------------------------------------------------------

getColor(X, Y, Board, Color) :- isIvory(X,Y,Board), Color = ivory.
getColor(X, Y, Board, Color) :- isCigar(X,Y,Board), Color = cigar.
								
isIvory(X,Y,Board) :- 	getPiece(X,Y,Board,Symbol),
						(Symbol == ivoryQueen; Symbol == ivoryBaby).
						
isCigar(X,Y,Board) :- 	getPiece(X,Y,Board,Symbol),
						(Symbol == cigarQueen; Symbol == cigarBaby).
						
isFree(X,Y,Board) :-	getPiece(X,Y,Board,Symbol),
						Symbol = empty.
						
isQueen(X, Y, Board) :- getPiece(X,Y,Board,Symbol),
						(Symbol == ivoryQueen; Symbol == cigarQueen).
						
isBaby(X, Y, Board) :- 	getPiece(X,Y,Board,Symbol),
						(Symbol == ivoryBaby; Symbol == cigarBaby).

areOpponents(X1, Y1, X2, Y2, Board) :- 	isIvory(X1, Y1, Board), isCigar(X2, Y2, Board).									
areOpponents(X1, Y1, X2, Y2, Board) :- 	isCigar(X1, Y1, Board), isIvory(X2, Y2, Board).	

validatePlayer(ivory).
validatePlayer(cigar).
										
stackCritical(Stack) :- Stack < 2 .
checkStack(Stack, GameOver) :- stackCritical(Stack), GameOver = true.
checkStack(Stack, GameOver) :- \+ stackCritical(Stack), GameOver = false.

validateCurrentCoords(ivory, X, Y, Board):- 	dentroTabuleiro(X,Y),
												\+ isFree(X, Y, Board),
												isIvory(X,Y,Board).

validateCurrentCoords(cigar, X, Y, Board):- 	dentroTabuleiro(X,Y),
												\+ isFree(X, Y, Board),
												isCigar(X,Y,Board).
												 
validateTargetCoords(_, X, Y, TargetX, TargetY, Board) :- dentroTabuleiro(TargetX,TargetY),
																naoSiPropria(X,Y,TargetX,TargetY),
																isFree(TargetX,TargetY,Board),
																vertical(X,Y,TargetX,TargetY).
validateTargetCoords(_, X, Y, TargetX, TargetY, Board) :- dentroTabuleiro(TargetX,TargetY),
																naoSiPropria(X,Y,TargetX,TargetY),
																isFree(TargetX,TargetY,Board),
																horizontal(X,Y,TargetX,TargetY).
validateTargetCoords(_, X, Y, TargetX, TargetY, Board) :- dentroTabuleiro(TargetX,TargetY),
																naoSiPropria(X,Y,TargetX,TargetY),
																isFree(TargetX,TargetY,Board),
																diagonal(X,Y,TargetX,TargetY).
validateTargetCoords(ivory, X, Y, TargetX, TargetY, Board) :- dentroTabuleiro(TargetX,TargetY),
																naoSiPropria(X,Y,TargetX,TargetY),
																isCigar(TargetX,TargetY,Board),
																vertical(X,Y,TargetX,TargetY).
validateTargetCoords(ivory, X, Y, TargetX, TargetY, Board) :- dentroTabuleiro(TargetX,TargetY),
																naoSiPropria(X,Y,TargetX,TargetY),
																isCigar(TargetX,TargetY,Board),
																horizontal(X,Y,TargetX,TargetY).														
validateTargetCoords(ivory, X, Y, TargetX, TargetY, Board) :- dentroTabuleiro(TargetX,TargetY),
																naoSiPropria(X,Y,TargetX,TargetY),
																isCigar(TargetX,TargetY,Board),
																diagonal(X,Y,TargetX,TargetY).
validateTargetCoords(cigar, X, Y, TargetX, TargetY, Board) :- dentroTabuleiro(TargetX,TargetY),
																naoSiPropria(X,Y,TargetX,TargetY),
																isIvory(TargetX,TargetY,Board),
																vertical(X,Y,TargetX,TargetY).
validateTargetCoords(cigar, X, Y, TargetX, TargetY, Board) :- dentroTabuleiro(TargetX,TargetY),
																naoSiPropria(X,Y,TargetX,TargetY),
																isIvory(TargetX,TargetY,Board),
																horizontal(X,Y,TargetX,TargetY).														
validateTargetCoords(cigar, X, Y, TargetX, TargetY, Board) :- 	dentroTabuleiro(TargetX,TargetY),
																naoSiPropria(X,Y,TargetX,TargetY),
																isIvory(TargetX,TargetY,Board),
																diagonal(X,Y,TargetX,TargetY).																

getPieceOfColor(_, empty, Output) :- 		Output = empty.
getPieceOfColor(ivory, baby, Output) :- 	Output = ivoryBaby.
getPieceOfColor(ivory, queen, Output) :- 	Output = ivoryQueen.
getPieceOfColor(cigar, baby, Output) :- 	Output = cigarBaby.
getPieceOfColor(cigar, queen, Output) :- 	Output = cigarQueen.
															
checkReducedDistance(ivory, CurrentX, CurrentY, TargetX, TargetY, BoardIn) :-  findQueen(cigar, BoardIn, QueenX, QueenY),
																				distance(CurrentX,CurrentY,QueenX,QueenY, DistanceCurrent),
																				distance(TargetX, TargetY, QueenX, QueenY, DistanceTarget),
																				DistanceCurrent > DistanceTarget .
checkReducedDistance(cigar, CurrentX, CurrentY, TargetX, TargetY, BoardIn) :-  findQueen(ivory, BoardIn, QueenX, QueenY),
																				distance(CurrentX,CurrentY,QueenX,QueenY, DistanceCurrent),
																				distance(TargetX, TargetY, QueenX, QueenY, DistanceTarget),
																				DistanceCurrent > DistanceTarget .
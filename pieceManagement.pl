%%%%%%%%%%%%%%%%%%%%%%%%
%%% PIECE MANAGEMENT %%%
%%%%%%%%%%%%%%%%%%%%%%%%

% Procedimentos para analisar uma casa qualquer do tabuleiro
reachedY(Y, [Head|_], Row) :- Y == 0, Row = Head.
reachedY(Y, [_|OtherRows], Row) :-  Y \= 0 -> (Y1 is Y-1, reachedY(Y1, OtherRows, Row)).

reachedX(X, [Head|_], Elem) :- X == 0, Elem = Head.
reachedX(X, [_|OtherElems], Elem) :- X \= 0 -> (X1 is X-1, reachedX(X1, OtherElems, Elem)).

getPiece(X, Y, Board, Symbol) :- dentroTabuleiro(X,Y), (X,Y,Board)^(reachedY(Y, Board, Row), reachedX(X, Row, Elem), Symbol = Elem).

getColor(X, Y, Board, Color) :- (isIvory(X,Y,Board) -> Color = ivory );
										(isCigar(X,Y,Board) -> Color = cigar ).

setPiece(X, Y, Symbol, Board1, Board2).

% verifica a peca nas coordenadas
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

areOpponents( X1, Y1, X2, Y2, Board):- 	(isIvory(X1, Y1, Board), isCigar(X2, Y2, Board));
										(isCigar(X1, Y1, Board), isIvory(X2, Y2, Board)).

% checks for a very basic move.
possibleBasicMovement(X,Y, XTarget,YTarget) :-	dentroTabuleiro(XTarget, YTarget), 
												(horizontal(X,Y, XTarget,YTarget) ; vertical(X,Y, XTarget,YTarget); diagonal(X,Y, XTarget,YTarget)).												
												
%%%%%%%%%%%%%%%% CAPTURING MOVE %%%%%%%%%%%%%%%%%%
%possibleCapturingQueenMove(X, Y, XTarget, YTarget, Board) :- 	(dentroTabuleiro(XTarget, YTarget),
%																possibleBasicMovement(X, Y, XTarget, YTarget),
%																areOpponents(X, Y, XTarget, YTarget, Board))
																% peca alvo e' baby -> come
																% peca alvo e' queen -> gameOver
																
														
												
%capturingBabyMove().
													
												
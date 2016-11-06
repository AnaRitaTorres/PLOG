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

areOpponents(X1, Y1, X2, Y2, Board):- 	(isIvory(X1, Y1, Board), isCigar(X2, Y2, Board));
										(isCigar(X1, Y1, Board), isIvory(X2, Y2, Board)).

validatePlayer(Player) :- 	(Player == ivory -> !;
							 Player == cigar -> !;
							(write('invalid player submited.'), fail)).
										
stackCritical(Stack) :- Stack =< 2.

validateCurrentCoords(Player, X, Y, Board):- 	dentroTabuleiro(X,Y),
												\+ isFree(X, Y, Board),
												(Player == ivory -> isIvory(X,Y,Board);
												 Player == cigar -> isCigar(X,Y,Board)).
												 
validateTargetCoords(Player, X, Y, TargetX, TargetY, Board) :- 	dentroTabuleiro(TargetX,TargetY),
																naoSiPropria(X,Y,TargetX,TargetY),
																(isFree(TargetX,TargetY,Board);
																(Player == ivory -> isCigar(TargetX,TargetY,Board);
																 Player == cigar -> isIvory(TargetX,TargetY,Board))),
																(vertical(X,Y,TargetX,TargetY);
																 horizontal(X,Y,TargetX,TargetY);
																 diagonal(X,Y,TargetX,TargetY)).

getPlay(Player, Play, (IvoryStack, CigarStack, Board),(IvoryStackOut, CigarStackOut, BoardOut)) :-
				%repeat,
				write('x= '), read(X),
				write('y= '), read(Y), nl,
				write('target x= '), read(TargetX),
				write('target y= '), read(TargetY), nl,
				Play = (Player, X, Y, TargetX, TargetY),
				makePlay(Play,(IvoryStack,CigarStack,Board), (IvoryStackOut, CigarStackOut, BoardOut)).
	
% Play: 
% 1. check stack. if <= 2, nope.
% 2. get current coordinates (check if valid: 	- inside board
%												- space not free
% 												- piece of the right color)
% 3. get target coordinates (check if valid:	- inside board
%												- space free, or of the opposite color
%												- diagonal, horizontal or vertical to current coords)
% 4. compute move... :
% 		if it's a queen move
%			if target is an empty space
%				replace target with queen
%				replace current coords with a baby
%				reduce stack by 1
%			if target is baby
%				replace target with queen
%				replace current coords with empty space
%			if target is queen
%				replace target with dominant queen
%				replace current with empty
%				check-mate
%		if it's a baby move
%			if target is empty space
%				if distance to queen is reduced
%					replace target with baby
%					replace current with empty
%			if target is baby
%				replace target with our baby
%				replace current with empty
%			if target is queen
%				replace target with baby
%				replace current to empty
%				check-mate

%makePlay(+(Player,X,Y,TargetX,TargetY), +(IvoryStack, CigarStack, Board), -(IvoryStackOut, CigarStackOut, BoardOut))
makePlay((Player,X,Y,TargetX,TargetY),(IvoryStack,CigarStack,Board),(IvoryStackOut, CigarStackOut, BoardOut)):-

		validatePlayer(Player),
		\+ stackCritical(IvoryStack), \+ stackCritical(CigarStack),
		validateCurrentCoords(Player, X, Y, Board),
		validateTargetCoords(Player,X,Y,TargetX,TargetY,Board),
		
		%%%%%%%%%%%%%%%%%%%%%% TODO %%%%%%%%%%%%%%%%%%%%%%%%
		
		(isQueen(X,Y,Board) -> 
			(isFree(TargetX,TargetY,Board) -> !;
			 isBaby(TargetX,TargetY,Board) -> !;
			 isQueen(TargetX,TargetY,Board) -> !), !;
		 isBaby(X,Y,Board) ->
			(isFree(TargetX,TargetY,Board) -> !;
			 isBaby(TargetX,TargetY,Board) -> !;
			 isQueen(TargetX,TargetY,Board) -> !), !
		).
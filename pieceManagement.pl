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
										
stackCritical(Stack) :- Stack <= 2.
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



getPieceofColor(Player, QueenOrBabyOrEmpty, Output) :- (QueenOrBabyOrEmpty == empty -> Output = empty;
														Player == ivory ->
															(QueenOrBabyOrEmpty == queen -> Output = ivoryQueen;
															QueenOrBabyOrEmpty == baby -> Output = ivoryBaby);
														Player == cigar ->
															(QueenOrBabyOrEmpty == queen -> Output = cigarQueen;
															QueenOrBabyOrEmpty == baby -> Output = cigarBaby)).
															
checkReducedDistance(ivory, CurrentX, CurrentY, TargetX, TargetY, BoardIn) :-  findQueen(cigar, BoardIn, QueenX, QueenY),
																				distance(CurrentX,CurrentY,QueenX,QueenY, DistanceCurrent),
																				distance(TargetX, TargetY, QueenX, QueenY, DistanceTarget),
																				DistanceCurrent > DistanceTarget .
checkReducedDistance(cigar, CurrentX, CurrentY, TargetX, TargetY, BoardIn) :-  findQueen(ivory, BoardIn, QueenX, QueenY),
																				distance(CurrentX,CurrentY,QueenX,QueenY, DistanceCurrent),
																				distance(TargetX, TargetY, QueenX, QueenY, DistanceTarget),
																				DistanceCurrent > DistanceTarget .
% -------------------------------------------------------------------------
% --------------------------- PLAY MAKING ---------------------------------
% -------------------------------------------------------------------------
														 
getPlay(Player, Play, (IvoryStack, CigarStack, Board),(IvoryStackOut, CigarStackOut, BoardOut), GameOver) :-
				write('x= '), read(X),
				write('y= '), read(Y), nl,
				write('target x= '), read(TargetX),
				write('target y= '), read(TargetY), nl,
				Play = (Player, X, Y, TargetX, TargetY),
				BoardOut ^ (makePlay(Play,(IvoryStack,CigarStack,Board), (IvoryStackOut, CigarStackOut, BoardOut), GameOver)).
	
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

%makePlay(+(Player,X,Y,TargetX,TargetY), +(IvoryStack, CigarStack, Board), -(IvoryStackOut, CigarStackOut, BoardOut),-GameOver)
makePlay((Player,X,Y,TargetX,TargetY),(IvoryStack,CigarStack,BoardIn),(IvoryStackOut, CigarStackOut, BoardOut), GameOver) :-

		validatePlayer(Player),
		
		validateCurrentCoords(Player,X, Y, BoardIn),
		validateTargetCoords(Player,X,Y,TargetX,TargetY,BoardIn),
		piecesBetween(X, Y, TargetX, TargetY, BoardIn, Pieces),
		Pieces =:= 0,
		
		(isQueen(X,Y,BoardIn) ->
            (isFree(TargetX,TargetY,BoardIn) ->
                (getPieceofColor(Player,queen,Output),
                setPiece(TargetX, TargetY,Output, BoardIn, BoardIn1),
                getPieceofColor(Player,baby,Output1),
                setPiece(X, Y,Output1, BoardIn1, BoardOut),
                (Player == ivory -> (IvoryStackOut is IvoryStack - 1, CigarStackOut is CigarStack);
				 Player == cigar -> (IvoryStackOut is IvoryStack, CigarStackOut is CigarStack - 1))),
                GameOver = false, !;
             isBaby(TargetX,TargetY,BoardIn) ->
                (getPieceofColor(Player,queen,Output),
                setPiece(TargetX, TargetY,Output, BoardIn, BoardIn1),
                getPieceofColor(Player,empty,Output1),
                setPiece(X, Y,Output1, BoardIn1, BoardOut),
                IvoryStackOut is IvoryStack,
                CigarStackOut is CigarStack), 
                GameOver = false, !;
             isQueen(TargetX,TargetY,BoardIn) ->
                (getPieceofColor(Player,queen,Output),
                setPiece(TargetX, TargetY,Output, BoardIn, BoardIn1),
                getPieceofColor(Player,empty,Output1),
                setPiece(X, Y,Output1, BoardIn1, BoardOut),
                IvoryStackOut is IvoryStack,
                CigarStackOut is CigarStack,
				GameOver = true), !), !;
				
		 isBaby(X,Y,BoardIn) ->
			(isFree(TargetX,TargetY,BoardIn) ->
				checkReducedDistance(Player, X, Y, TargetX, TargetY, BoardIn) -> 
					(getPieceofColor(Player, baby, Output),
					setPiece(TargetX, TargetY, Output, BoardIn, BoardIn1),
					getPieceofColor(Player, empty, Output1),
					setPiece(X, Y, Output1, BoardIn1, BoardOut), 
					IvoryStackOut is IvoryStack, 
					CigarStackOut is CigarStack), 
					GameOver = false, !;
			 isBaby(TargetX,TargetY,BoardIn) ->
				(getPieceofColor(Player, baby, Output),
				setPiece(TargetX, TargetY, Output, BoardIn, BoardIn1),
				getPieceofColor(Player, empty, Output1),
				setPiece(X, Y, Output1, BoardIn1, BoardOut), 
				IvoryStackOut is IvoryStack, 
				CigarStackOut is CigarStack), 
				GameOver = false, !;
			 isQueen(TargetX,TargetY,BoardIn) ->
				(getPieceofColor(Player, baby, Output),
				setPiece(TargetX, TargetY, Output, BoardIn, BoardIn1),
				getPieceofColor(Player, empty, Output1),
				setPiece(X, Y, Output1, BoardIn1, BoardOut), 
				IvoryStackOut is IvoryStack, 
				CigarStackOut is CigarStack,
				GameOver = true), !), !
		).
		
insistOnCorrectPlay(Player, Play, (IvoryStackIn, CigarStackIn, BoardIn),(IvoryStackOut, CigarStackOut, BoardOut), GameOver):- repeat,
																															  getPlay(Player, Play, (IvoryStackIn, CigarStackIn, BoardIn),(IvoryStackOut, CigarStackOut, BoardOut), GameOver).

%%%%%%%%%%%%%%%%%%%%% PLAY CALLING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

play(ivory, (IvoryStackIn,CigarStackIn,BoardIn), false) :- 	clr,
														checkStack(IvoryStackIn, GameOver),	GameOver == false,
														checkStack(CigarStackIn, GameOver),	GameOver == false,
														\+ printFancyBoard(IvoryStackIn,CigarStackIn,BoardIn),
														write(ivory), write(' turn!'), nl,
														insistOnCorrectPlay(ivory, Play, (IvoryStackIn, CigarStackIn, BoardIn),(IvoryStackOut, CigarStackOut, BoardOut), GameOverNew),
														play(cigar, (IvoryStackOut,CigarStackOut,BoardOut), GameOverNew).												
play(cigar, (IvoryStackIn,CigarStackIn,BoardIn), false) :- 	clr,
														checkStack(IvoryStackIn, GameOver), GameOver == false,
														checkStack(CigarStackIn, GameOver),	GameOver == false,
														\+ printFancyBoard(IvoryStackIn,CigarStackIn,BoardIn),
														write(cigar), write(' turn!'), nl,
														insistOnCorrectPlay(cigar, Play, (IvoryStackIn, CigarStackIn, BoardIn),(IvoryStackOut, CigarStackOut, BoardOut), GameOverNew),
														play(ivory, (IvoryStackOut,CigarStackOut,BoardOut), GameOverNew).

%%%%%%%%%%%%%%%%%%%% DEFEAT CLAUSES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
play(ivory, (IvoryStackIn,CigarStackIn,BoardIn), false) :- 	clr,
														checkStack(IvoryStackIn, GameOver),	GameOver == true,
														play(cigar, (IvoryStackIn,CigarStackIn,BoardIn), true).
play(ivory, (IvoryStackIn,CigarStackIn,BoardIn), false) :- 	clr,
														checkStack(IvoryStackIn, GameOver),	GameOver == false,
														checkStack(CigarStackIn, GameOver),	GameOver == true,
														play(cigar, (IvoryStackIn,CigarStackIn,BoardIn), true).
play(cigar, (IvoryStackIn,CigarStackIn,BoardIn), false) :- 	clr,
														checkStack(IvoryStackIn, GameOver),	GameOver == true,
														play(ivory, (IvoryStackIn,CigarStackIn,BoardIn), true).
play(cigar, (IvoryStackIn,CigarStackIn,BoardIn), false) :- 	clr,
														checkStack(IvoryStackIn, GameOver),	GameOver == false,
														checkStack(CigarStackIn, GameOver),	GameOver == true,
														play(ivory, (IvoryStackIn,CigarStackIn,BoardIn), true).


play(ivory, (IvoryStackIn, CigarStackIn, _), true) :- 	write('Cigar Won! '), nl,
														write('Ivory had '), write(IvoryStackIn), write(' pieces left on the queen.'), nl,
														write('Cigar had '), write(CigarStackIn), write(' pieces left on the queen.').
play(cigar, (IvoryStackIn, CigarStackIn, _), true) :- 	write('Ivory Won! '), nl,
														write('Ivory had '), write(IvoryStackIn), write(' pieces left on the queen.'), nl,
														write('Cigar had '), write(CigarStackIn), write(' pieces left on the queen.').

%%%%%%%%%%%%%%%%%%%% PLAY PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bootPlayDefault :- BoardIn ^ (emptyBoard(BoardIn), play(ivory, (20,20,BoardIn), false)).

bootPlay(IvoryStackIn, CigarStackIn) :- \+ stackCritical(IvoryStackIn), \+ stackCritical(CigarStackIn),
										BoardIn ^ (emptyBoard(BoardIn), play(ivory, (IvoryStackIn,CigarStackIn,BoardIn), false)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

findQueen(Player, BoardIn, QueenX, QueenY) :- 	(Player == ivory -> findQueenAux(ivoryQueen, BoardIn, 0, 0, QueenX, QueenY);
										Player == cigar -> findQueenAux(cigarQueen, BoardIn, 0, 0, QueenX, QueenY)).
												
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

getColor(X, Y, Board, Color) :- (isIvory(X,Y,Board) -> Color = ivory );
										(isCigar(X,Y,Board) -> Color = cigar ).
								
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
							write('invalid player submited.'), fail).
										
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

getPieceofColor(Player, QueenOrBabyOrEmpty, Output) :- (QueenOrBabyOrEmpty == empty -> Output = empty;
														Player == ivory ->
															(QueenOrBabyOrEmpty == queen -> Output = ivoryQueen;
															QueenOrBabyOrEmpty == baby -> Output = ivoryBaby);
														Player == cigar ->
															(QueenOrBabyOrEmpty == queen -> Output = cigarQueen;
															QueenOrBabyOrEmpty == baby -> Output = cigarBaby)).
															
checkReducedDistance(Player, CurrentX, CurrentY, TargetX, TargetY, BoardIn) :- (Player == ivory ->  findQueen(cigar, BoardIn, QueenX, QueenY);
																				Player == cigar ->  findQueen(ivory, BoardIn, QueenX, QueenY)),
																				distance(CurrentX,CurrentY,QueenX,QueenY, DistanceCurrent),
																				distance(TargetX, TargetY, QueenX, QueenY, DistanceTarget),
																				DistanceCurrent > DistanceTarget .
																				
% -------------------------------------------------------------------------
% --------------------------- PLAY MAKING ---------------------------------
% -------------------------------------------------------------------------
														 
getPlay(Player, Play, (IvoryStack, CigarStack, Board),(IvoryStackOut, CigarStackOut, BoardOut)) :-
				write('x= '), read(X),
				write('y= '), read(Y), nl,
				write('target x= '), read(TargetX),
				write('target y= '), read(TargetY), nl,
				Play = (Player, X, Y, TargetX, TargetY),
				BoardOut ^ (makePlay(Play,(IvoryStack,CigarStack,Board), (IvoryStackOut, CigarStackOut, BoardOut))).
	
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
makePlay((Player,X,Y,TargetX,TargetY),(IvoryStack,CigarStack,BoardIn),(IvoryStackOut, CigarStackOut, BoardOut)) :-

		validatePlayer(Player),
		
		\+ stackCritical(IvoryStack), \+ stackCritical(CigarStack),
		
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
				 Player == cigar -> (IvoryStackOut is IvoryStack, CigarStackOut is CigarStack - 1))) ,!;
             isBaby(TargetX,TargetY,BoardIn) ->
                (getPieceofColor(Player,queen,Output),
                setPiece(TargetX, TargetY,Output, BoardIn, BoardIn1),
                getPieceofColor(Player,empty,Output1),
                setPiece(X, Y,Output1, BoardIn1, BoardOut),
                IvoryStackOut is IvoryStack,
                CigarStackOut is CigarStack), !;
             isQueen(TargetX,TargetY,BoardIn) ->
                (getPieceofColor(Player,queen,Output),
                setPiece(TargetX, TargetY,Output, BoardIn, BoardIn1),
                getPieceofColor(Player,empty,Output1),
                setPiece(X, Y,Output1, BoardIn1, BoardOut),
                IvoryStackOut is IvoryStack,
                CigarStackOut is CigarStack,
				write('CHECK MATE')), !), !;
				
		 isBaby(X,Y,BoardIn) ->
			(isFree(TargetX,TargetY,BoardIn) ->
				checkReducedDistance(Player, X, Y, TargetX, TargetY, BoardIn) -> 
					(getPieceofColor(Player, baby, Output),
					setPiece(TargetX, TargetY, Output, BoardIn, BoardIn1),
					getPieceofColor(Player, empty, Output1),
					setPiece(X, Y, Output1, BoardIn1, BoardOut), 
					IvoryStackOut is IvoryStack, 
					CigarStackOut is CigarStack), !;
			 isBaby(TargetX,TargetY,BoardIn) ->
				(getPieceofColor(Player, baby, Output),
				setPiece(TargetX, TargetY, Output, BoardIn, BoardIn1),
				getPieceofColor(Player, empty, Output1),
				setPiece(X, Y, Output1, BoardIn1, BoardOut), 
				IvoryStackOut is IvoryStack, 
				CigarStackOut is CigarStack), !;
			 isQueen(TargetX,TargetY,BoardIn) ->
				(getPieceofColor(Player, baby, Output),
				setPiece(TargetX, TargetY, Output, BoardIn, BoardIn1),
				getPieceofColor(Player, empty, Output1),
				setPiece(X, Y, Output1, BoardIn1, BoardOut), 
				IvoryStackOut is IvoryStack, 
				CigarStackOut is CigarStack,
				write('CHECK MATE!')), !), !
		).
		
play(Player, (IvoryStackIn,CigarStackIn,BoardIn)) :- 	\+ printFancyBoard(IvoryStackIn,CigarStackIn,BoardIn),
														write(Player), write(' turn!'), nl,
														getPlay(Player, Play, (IvoryStackIn, CigarStackIn, BoardIn),(IvoryStackOut, CigarStackOut, BoardOut)),
														(Player == ivory -> play(cigar, (IvoryStackOut,CigarStackOut,BoardOut));
														Player == cigar -> play(ivory, (IvoryStackOut,CigarStackOut,BoardOut))).
														
bootPlay :- BoardIn ^ (emptyBoard(BoardIn), play(ivory, (20,20,BoardIn))).

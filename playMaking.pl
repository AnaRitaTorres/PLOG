% -------------------------------------------------------------------------
% --------------------------- PLAY MAKING ---------------------------------
% -------------------------------------------------------------------------

matchInput(a, 0).
matchInput(b, 1).
matchInput(c, 2).
matchInput(d, 3).
matchInput(e, 4).
matchInput(f, 5).
matchInput(g, 6).
matchInput(h, 7).
matchInput(i, 8).
matchInput(j, 9).
matchInput(k, 10).
matchInput(l, 11).

getPlay(Player, Play, (IvoryStack, CigarStack, Board),(IvoryStackOut, CigarStackOut, BoardOut), GameOver) :-
				write('x= '), read(X),
				write('y= '), read(Y), nl,
				matchInput(X,Z),
				matchInput(Y,W),
				write('target x= '), read(TargetX), 
				write('target y= '), read(TargetY), nl,
				matchInput(TargetX,TargetZ),
				matchInput(TargetY,TargetW),
				Play = (Player, Z, W, TargetZ, TargetW),
				BoardOut ^ (makePlay(Play,(IvoryStack,CigarStack,Board), (IvoryStackOut, CigarStackOut, BoardOut), GameOver)).
	
%makePlay(+(Player,X,Y,TargetX,TargetY), +(IvoryStack, CigarStack, Board), -(IvoryStackOut, CigarStackOut, BoardOut),-GameOver)
makePlay((Player,X,Y,TargetX,TargetY),(IvoryStack,CigarStack,BoardIn),(IvoryStackOut, CigarStackOut, BoardOut), GameOver) :-
	
		validatePlayer(Player),
		
		validateCurrentCoords(Player,X, Y, BoardIn),
		validateTargetCoords(Player,X,Y,TargetX,TargetY,BoardIn),
		piecesBetween(X, Y, TargetX, TargetY, BoardIn, Pieces),
		Pieces =:= 0,
		
		(isQueen(X,Y,BoardIn) ->
            (isFree(TargetX,TargetY,BoardIn) ->
                executeMoveQueenFree(Player, X, Y, TargetX, TargetY, (IvoryStack, CigarStack, BoardIn), (IvoryStackOut, CigarStackOut, BoardOut), GameOver), !;
             isBaby(TargetX,TargetY,BoardIn) ->
               	executeMoveQueenBaby(Player, X, Y, TargetX, TargetY, (IvoryStack, CigarStack, BoardIn), (IvoryStackOut, CigarStackOut, BoardOut), GameOver), !;
             isQueen(TargetX,TargetY,BoardIn) ->
                executeMoveQueenQueen(Player, X, Y, TargetX, TargetY, (IvoryStack, CigarStack, BoardIn), (IvoryStackOut, CigarStackOut, BoardOut), GameOver), !), !;
				
		 isBaby(X,Y,BoardIn) ->
			(isFree(TargetX,TargetY,BoardIn) ->
				checkReducedDistance(Player, X, Y, TargetX, TargetY, BoardIn) -> 
					executeMoveBabyFree(Player, X, Y, TargetX, TargetY, (IvoryStack, CigarStack, BoardIn), (IvoryStackOut, CigarStackOut, BoardOut), GameOver), !;
			 isBaby(TargetX,TargetY,BoardIn) ->
					executeMoveBabyBaby(Player, X, Y, TargetX, TargetY, (IvoryStack, CigarStack, BoardIn), (IvoryStackOut, CigarStackOut, BoardOut), GameOver), !;
			 isQueen(TargetX,TargetY,BoardIn) ->
					executeMoveBabyQueen(Player, X, Y, TargetX, TargetY, (IvoryStack, CigarStack, BoardIn), (IvoryStackOut, CigarStackOut, BoardOut), GameOver), !), !
).
		
executeMoveQueenFree(ivory, X, Y, TargetX, TargetY, (IvoryStackIn, CigarStackIn, BoardIn), (IvoryStackOut, CigarStackOut, BoardOut), GameOver) :-
        setPiece(TargetX, TargetY,ivoryQueen, BoardIn, BoardIn1),
        setPiece(X, Y,ivoryBaby, BoardIn1, BoardOut),
        IvoryStackOut is IvoryStackIn - 1, 
        CigarStackOut is CigarStackIn, 
        GameOver = false.

executeMoveQueenFree(cigar, X, Y, TargetX, TargetY, (IvoryStackIn, CigarStackIn, BoardIn), (IvoryStackOut, CigarStackOut, BoardOut), GameOver) :-
        setPiece(TargetX, TargetY,cigarQueen, BoardIn, BoardIn1),
        setPiece(X, Y,cigarBaby, BoardIn1, BoardOut),
        IvoryStackOut is IvoryStackIn, 
        CigarStackOut is CigarStackIn - 1,
        GameOver = false.

executeMoveQueenBaby(ivory, X, Y, TargetX, TargetY, (IvoryStackIn, CigarStackIn, BoardIn), (IvoryStackOut, CigarStackOut, BoardOut), GameOver) :-
		setPiece(TargetX, TargetY,ivoryQueen, BoardIn, BoardIn1),
        setPiece(X, Y,empty, BoardIn1, BoardOut),
        IvoryStackOut is IvoryStackIn,
        CigarStackOut is CigarStackIn, 
        GameOver = false.

executeMoveQueenBaby(cigar, X, Y, TargetX, TargetY, (IvoryStackIn, CigarStackIn, BoardIn), (IvoryStackOut, CigarStackOut, BoardOut), GameOver) :-
		setPiece(TargetX, TargetY,cigarQueen, BoardIn, BoardIn1),
        setPiece(X, Y,empty, BoardIn1, BoardOut),
        IvoryStackOut is IvoryStackIn,
        CigarStackOut is CigarStackIn, 
        GameOver = false.

executeMoveQueenQueen(ivory, X, Y, TargetX, TargetY, (IvoryStackIn, CigarStackIn, BoardIn), (IvoryStackOut, CigarStackOut, BoardOut), GameOver) :-
		setPiece(TargetX, TargetY,ivoryQueen, BoardIn, BoardIn1),
        setPiece(X, Y,empty, BoardIn1, BoardOut),
        IvoryStackOut is IvoryStackIn,
        CigarStackOut is CigarStackIn,
		GameOver = true.

executeMoveQueenQueen(cigar, X, Y, TargetX, TargetY, (IvoryStackIn, CigarStackIn, BoardIn), (IvoryStackOut, CigarStackOut, BoardOut), GameOver) :-
		setPiece(TargetX, TargetY,cigarQueen, BoardIn, BoardIn1),
        setPiece(X, Y,empty, BoardIn1, BoardOut),
        IvoryStackOut is IvoryStackIn,
        CigarStackOut is CigarStackIn,
		GameOver = true.

executeMoveBabyFree(ivory, X, Y, TargetX, TargetY, (IvoryStackIn, CigarStackIn, BoardIn), (IvoryStackOut, CigarStackOut, BoardOut), GameOver) :-
		setPiece(TargetX, TargetY, ivoryBaby, BoardIn, BoardIn1),
		setPiece(X, Y, empty, BoardIn1, BoardOut), 
		IvoryStackOut is IvoryStackIn, 
		CigarStackOut is CigarStackIn, 
		GameOver = false.

executeMoveBabyFree(cigar, X, Y, TargetX, TargetY, (IvoryStackIn, CigarStackIn, BoardIn), (IvoryStackOut, CigarStackOut, BoardOut), GameOver) :-
		setPiece(TargetX, TargetY, cigarBaby, BoardIn, BoardIn1),
		setPiece(X, Y, empty, BoardIn1, BoardOut), 
		IvoryStackOut is IvoryStackIn, 
		CigarStackOut is CigarStackIn, 
		GameOver = false.

executeMoveBabyBaby(ivory, X, Y, TargetX, TargetY, (IvoryStackIn, CigarStackIn, BoardIn), (IvoryStackOut, CigarStackOut, BoardOut), GameOver) :-
		setPiece(TargetX, TargetY, ivoryBaby, BoardIn, BoardIn1),
		setPiece(X, Y, empty, BoardIn1, BoardOut), 
		IvoryStackOut is IvoryStackIn, 
		CigarStackOut is CigarStackIn, 
		GameOver = false.

executeMoveBabyBaby(cigar, X, Y, TargetX, TargetY, (IvoryStackIn, CigarStackIn, BoardIn), (IvoryStackOut, CigarStackOut, BoardOut), GameOver) :-
		setPiece(TargetX, TargetY, cigarBaby, BoardIn, BoardIn1),
		setPiece(X, Y, empty, BoardIn1, BoardOut), 
		IvoryStackOut is IvoryStackIn, 
		CigarStackOut is CigarStackIn, 
		GameOver = false.

executeMoveBabyQueen(ivory, X, Y, TargetX, TargetY, (IvoryStackIn, CigarStackIn, BoardIn), (IvoryStackOut, CigarStackOut, BoardOut), GameOver):-
		setPiece(TargetX, TargetY, ivoryBaby, BoardIn, BoardIn1),
		setPiece(X, Y, empty, BoardIn1, BoardOut), 
		IvoryStackOut is IvoryStackIn, 
		CigarStackOut is CigarStackIn,
		GameOver = true.

executeMoveBabyQueen(cigar, X, Y, TargetX, TargetY, (IvoryStackIn, CigarStackIn, BoardIn), (IvoryStackOut, CigarStackOut, BoardOut), GameOver):-
		setPiece(TargetX, TargetY, cigarBaby, BoardIn, BoardIn1),
		setPiece(X, Y, empty, BoardIn1, BoardOut), 
		IvoryStackOut is IvoryStackIn, 
		CigarStackOut is CigarStackIn,
		GameOver = true.

insistOnCorrectPlay(Player, Play, (IvoryStackIn, CigarStackIn, BoardIn),(IvoryStackOut, CigarStackOut, BoardOut), GameOver):- repeat,
					  getPlay(Player, Play, (IvoryStackIn, CigarStackIn, BoardIn),(IvoryStackOut, CigarStackOut, BoardOut), GameOver).

%%%%%%%%%%%%%%%%%%%%% PLAY CALLING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

play(ivory, (IvoryStackIn,CigarStackIn,BoardIn), false) :- 	%clr,
														checkStack(IvoryStackIn, GameOver),	GameOver == false,
														checkStack(CigarStackIn, GameOver),	GameOver == false,
														\+ printFancyBoard(IvoryStackIn,CigarStackIn,BoardIn),
														write(ivory), write(' turn!'), nl,
														insistOnCorrectPlay(ivory, Play, (IvoryStackIn, CigarStackIn, BoardIn),(IvoryStackOut, CigarStackOut, BoardOut), GameOverNew),
														play(cigar, (IvoryStackOut,CigarStackOut,BoardOut), GameOverNew).												
play(cigar, (IvoryStackIn,CigarStackIn,BoardIn), false) :- 	%clr,
														checkStack(IvoryStackIn, GameOver), GameOver == false,
														checkStack(CigarStackIn, GameOver),	GameOver == false,
														\+ printFancyBoard(IvoryStackIn,CigarStackIn,BoardIn),
														write(cigar), write(' turn!'), nl,
														insistOnCorrectPlay(cigar, Play, (IvoryStackIn, CigarStackIn, BoardIn),(IvoryStackOut, CigarStackOut, BoardOut), GameOverNew),
														play(ivory, (IvoryStackOut,CigarStackOut,BoardOut), GameOverNew).

%%%%%%%%%%%%%%%%%%%% DEFEAT CLAUSES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

play(ivory, (IvoryStackIn,CigarStackIn,BoardIn), false) :- 	%clr,
														checkStack(IvoryStackIn, GameOver),	GameOver == true,
														play(cigar, (IvoryStackIn,CigarStackIn,BoardIn), true).
play(ivory, (IvoryStackIn,CigarStackIn,BoardIn), false) :- 	%clr,
														checkStack(CigarStackIn, GameOver),	GameOver == true,
														play(cigar, (IvoryStackIn,CigarStackIn,BoardIn), true).
play(cigar, (IvoryStackIn,CigarStackIn,BoardIn), false) :- 	%clr,
														checkStack(IvoryStackIn, GameOver),	GameOver == true,
														play(ivory, (IvoryStackIn,CigarStackIn,BoardIn), true).
play(cigar, (IvoryStackIn,CigarStackIn,BoardIn), false) :- 	%clr,
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

bootPlay(StackSize) :- \+ stackCritical(StackSize),	BoardIn ^ (emptyBoard(BoardIn), play(ivory, (StackSize, StackSize, BoardIn), false)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
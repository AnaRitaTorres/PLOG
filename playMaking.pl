% -------------------------------------------------------------------------
% --------------------------- PLAY MAKING ---------------------------------
% -------------------------------------------------------------------------

matchInput(a, 0).	matchInput(0, a).
matchInput(b, 1).	matchInput(1, b).
matchInput(c, 2).	matchInput(2, c).
matchInput(d, 3).	matchInput(3, d).
matchInput(e, 4).	matchInput(4, e).
matchInput(f, 5).	matchInput(5, f).
matchInput(g, 6).	matchInput(6, g).
matchInput(h, 7).	matchInput(7, h).
matchInput(i, 8).	matchInput(8, i).
matchInput(j, 9).	matchInput(9, j).
matchInput(k, 10).	matchInput(10,k).
matchInput(l, 11).	matchInput(11,l).

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

play(ivory, (IvoryStackIn,CigarStackIn,BoardIn), false, human, Type) :- 	clr,
														checkStack(IvoryStackIn, GameOver),	GameOver == false,
														checkStack(CigarStackIn, GameOver),	GameOver == false,
														\+ printFancyBoard(IvoryStackIn,CigarStackIn,BoardIn),
														write(ivory), write(' turn!'), nl,
														insistOnCorrectPlay(ivory, _, (IvoryStackIn, CigarStackIn, BoardIn),(IvoryStackOut, CigarStackOut, BoardOut), GameOverNew),
														play(cigar, (IvoryStackOut,CigarStackOut,BoardOut), GameOverNew, Type, human).
play(ivory, (IvoryStackIn,CigarStackIn,BoardIn), false, botDif1, Type) :- 	clr,
														checkStack(IvoryStackIn, GameOver),	GameOver == false,
														checkStack(CigarStackIn, GameOver),	GameOver == false,
														\+ printFancyBoard(IvoryStackIn,CigarStackIn,BoardIn),
														write(ivory), write(' turn!'), nl,
														insistOnCorrectBotRandomPlay(ivory, Play, (IvoryStackIn, CigarStackIn, BoardIn),(IvoryStackOut, CigarStackOut, BoardOut), GameOverNew),
														writeBotPlay(Play),
														get_code(_), get_code(_),
														play(cigar, (IvoryStackOut,CigarStackOut,BoardOut), GameOverNew, Type, botDif1).		
play(ivory, (IvoryStackIn,CigarStackIn,BoardIn), false, botDif2, Type) :- 	clr,
														checkStack(IvoryStackIn, GameOver),	GameOver == false,
														checkStack(CigarStackIn, GameOver),	GameOver == false,
														\+ printFancyBoard(IvoryStackIn,CigarStackIn,BoardIn),
														write(ivory), write(' turn!'), nl,
														playBestBot(ivory, Play, (IvoryStackIn, IvoryStackIn, BoardIn),(IvoryStackOut, CigarStackOut, BoardOut), GameOverNew),
														writeBotPlay(Play),
														get_code(_), get_code(_),
														play(cigar, (IvoryStackOut,CigarStackOut,BoardOut), GameOverNew, Type, botDif2).												
play(cigar, (IvoryStackIn,CigarStackIn,BoardIn), false, human, Type) :- 	clr,
														checkStack(IvoryStackIn, GameOver), GameOver == false,
														checkStack(CigarStackIn, GameOver),	GameOver == false,
														\+ printFancyBoard(IvoryStackIn,CigarStackIn,BoardIn),
														write(cigar), write(' turn!'), nl,
														insistOnCorrectPlay(cigar, _, (IvoryStackIn, CigarStackIn, BoardIn),(IvoryStackOut, CigarStackOut, BoardOut), GameOverNew),
														play(ivory, (IvoryStackOut,CigarStackOut,BoardOut), GameOverNew, Type, human).
play(cigar, (IvoryStackIn,CigarStackIn,BoardIn), false, botDif1, Type) :- 	clr,
														checkStack(IvoryStackIn, GameOver), GameOver == false,
														checkStack(CigarStackIn, GameOver),	GameOver == false,
														\+ printFancyBoard(IvoryStackIn,CigarStackIn,BoardIn),
														write(cigar), write(' turn!'), nl,
														insistOnCorrectBotRandomPlay(cigar, Play, (IvoryStackIn, CigarStackIn, BoardIn),(IvoryStackOut, CigarStackOut, BoardOut), GameOverNew),
														writeBotPlay(Play),
														get_code(_), get_code(_),
														play(ivory, (IvoryStackOut,CigarStackOut,BoardOut), GameOverNew, Type, botDif1).
play(cigar, (IvoryStackIn,CigarStackIn,BoardIn), false, botDif2, Type) :- 	clr,
														checkStack(IvoryStackIn, GameOver),	GameOver == false,
														checkStack(CigarStackIn, GameOver),	GameOver == false,
														\+ printFancyBoard(IvoryStackIn,CigarStackIn,BoardIn),
														write(cigar), write(' turn!'), nl,
														playBestBot(cigar, Play, (IvoryStackIn, IvoryStackIn, BoardIn),(IvoryStackOut, CigarStackOut, BoardOut), GameOverNew),
														writeBotPlay(Play),
														get_code(_), get_code(_),
														play(ivory, (IvoryStackOut,CigarStackOut,BoardOut), GameOverNew, Type, botDif2).	

%%%%%%%%%%%%%%%%%%%% DEFEAT CLAUSES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

play(ivory, (IvoryStackIn,CigarStackIn,BoardIn), false, Type1, Type2) :- 	clr,
														checkStack(IvoryStackIn, GameOver),	GameOver == true,
														play(cigar, (IvoryStackIn,CigarStackIn,BoardIn), true, Type2, Type1).
play(ivory, (IvoryStackIn,CigarStackIn,BoardIn), false, Type1, Type2) :- 	clr,
														checkStack(CigarStackIn, GameOver),	GameOver == true,
														play(cigar, (IvoryStackIn,CigarStackIn,BoardIn), true, Type2, Type1).
play(cigar, (IvoryStackIn,CigarStackIn,BoardIn), false, Type1, Type2) :- 	clr,
														checkStack(IvoryStackIn, GameOver),	GameOver == true,
														play(ivory, (IvoryStackIn,CigarStackIn,BoardIn), true, Type2, Type1).
play(cigar, (IvoryStackIn,CigarStackIn,BoardIn), false, Type1, Type2) :- 	clr,
														checkStack(CigarStackIn, GameOver),	GameOver == true,
														play(ivory, (IvoryStackIn,CigarStackIn,BoardIn), true, Type2, Type1).


play(ivory, (IvoryStackIn, CigarStackIn, _), true, _, _) :- 	gameOverMenu(cigar, IvoryStackIn, CigarStackIn), get_code(_), mainMenu.
play(cigar, (IvoryStackIn, CigarStackIn, _), true,  _, _) :- 	gameOverMenu(ivory, IvoryStackIn, CigarStackIn), get_code(_), mainMenu.

%%%%%%%%%%%%%%%%%%%% PLAY PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bootPlayDefault :- BoardIn ^ (emptyBoard(BoardIn), play(ivory, (20,20,BoardIn), false, human, human)).
bootPlay(StackSize, Player1, Player2) :- BoardIn ^ (emptyBoard(BoardIn), play(ivory, (StackSize,StackSize,BoardIn), false, Player1, Player2)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
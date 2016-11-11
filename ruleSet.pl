%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% GAME RULE SET %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- use_module(library(random)).

getRandomCoords(X) :- random(0, 11, X).
	
getPlayBotRandom(Player, Play, (IvoryStack, CigarStack, Board),(IvoryStackOut, CigarStackOut, BoardOut), GameOver) :-
				getRandomCoords(X), getRandomCoords(Y),
				getRandomCoords(TargetX), getRandomCoords(TargetY),
				write(X),write(Y),write(TargetX),write(TargetY),
				Play = (Player, X, Y, TargetX, TargetY),
				BoardOut ^ (makePlay(Play,(IvoryStack,CigarStack,Board), (IvoryStackOut, CigarStackOut, BoardOut), GameOver)).

insistOnCorrectBotRandomPlay(Player, Play, (IvoryStackIn, CigarStackIn, BoardIn),(IvoryStackOut, CigarStackOut, BoardOut), GameOver):- repeat,
					  getPlayBotRandom(Player, Play, (IvoryStackIn, CigarStackIn, BoardIn),(IvoryStackOut, CigarStackOut, BoardOut), GameOver).
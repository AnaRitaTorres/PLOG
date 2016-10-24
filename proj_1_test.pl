%%%%%%%%%%%%%%%%%%
%%%%% TESTS %%%%%%
%%%%%%%%%%%%%%%%%%

% Check good coords [expected - yes]
checkDentro1 :- dentroTabuleiro(0,0).
% Check good coords [expected - no]
checkDentro2 :- dentroTabuleiro(-1, 0).
% Check good coords [expected - no]
checkDentro3 :- dentroTabuleiro(0,-1).
% Check good coords [expected - no]
checkDentro4 :- dentroTabuleiro(-1,12).

% Check Positions
checkVertical1 :- vertical(1,0,1,1). 			%[expected - yes]
checkVertical2 :- vertical(1,1,2,1). 			%[expected - no]
checkVertical3 :- vertical(0,1,1,0).		 	%[expected - no]
checkHorizontal1 :- horizontal(1,0,1,1).		%[expected - no]
checkHorizontal2 :- horizontal(1,3,2,3).		%[expected - yes]
checkHorizontal3 :- horizontal(0,1,1,0).		%[expected - no]
checkDiagonal1 :- diagonal(0,0,1,1).			%[expected - yes]
checkDiagonal2 :- diagonal(0,0,1,2).			%[expected - no]
checkDiagonal3 :- diagonal(1,4,2,3).			%[expected - yes]

% Check Positions
checkIsIvory1 :- Board^(emptyBoard(Board), isIvory(5,0,Board)).		%[expected - yes]
checkIsIvory2 :- Board^(emptyBoard(Board), isIvory(0,0,Board)).		%[expected - no]
checkIsCigar1 :- Board^(emptyBoard(Board), isCigar(6,11,Board)).	%[expected - yes]
checkIsCigar2 :- Board^(emptyBoard(Board), isCigar(5,0,Board)).		%[expected - no]
checkIsFree1 :- Board^(emptyBoard(Board), isFree(0,0,Board)).		%[expected - yes]

% Check if wrong player [expected - no]
checkMoveErr1 :- Board^(emptyBoard(Board), checkMove(player,0,0,0,0,Board)).
% Check if is ivory at coords [expected - no]
checkMoveErr2 :- Board^(emptyBoard(Board), checkMove(ivory,0,0,0,0,Board)).
% Check if it's free at target coords [expected - no]
checkMoveErr3 :- Board^(emptyBoard(Board), checkMove(ivory,5,0,6,11,Board)).
% This one should work tho [expected - yas]
checkMoveErr4 :- Board^(emptyBoard(Board), checkMove(ivory,5,0,0,0,Board)).
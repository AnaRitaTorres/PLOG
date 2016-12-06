%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% MAIN SOLVER AND RESTRICTION ANALYSER %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(clpfd)).
:- use_module(library(lists)).


% This is the predicate that runs the algorithm (restrictions) to solve the puzzle.

% Our solution involves separating the puzzle board into smaller regions.
% Each region corresponds to a number in the main puzzle, ie, the number 1 
% would have an arrow up, down, left or right of it. We don't need the direction
% for the solution. Simply see that the length specified by the number + 1 
% (this 1 is the square occupying the number) equals the total area of the region!
% Then, we just ID the region and see all the possible regions...

solver(Board, Result) :-
	
	% board is always square; calculate its side.	
	length(Board, N),

	getIndexedNumbers(Board, Indexed), nl, nl, write(Indexed), nl, nl,
	createEmptySolution(N, EmptyResult),
	initRegions(EmptyResult, Indexed, Result).



%%%%%%%%%%%%%%%%%%%%%%%%% AUXILARY PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%

% This predicate gets a matrix's element.

getNumberAtCoord(X, Y, Number, Board) :-
	nth0(Y, Board, Row),
	nth0(X, Row, Number).

% This predicate sets a matrix's element.

setNumberAtCoord(X, Y, Number, BoardIn, BoardOut) :- 	setLineAux(0,X,Y,Number,BoardIn,BoardOut), !.

setLineAux(_,_,_,_,[],[]).
setLineAux(Y, X, Y, Number, [Line | Tail], [Line2 | Tail2]) :- 	setColAux(0, X, Number, Line, Line2),
																Ynow is Y + 1,
																setLineAux(Ynow, X, Y, Number, Tail, Tail2).
setLineAux(Ynow, X, Y, Number, [Line | Tail], [Line | Tail2]) :- 	Ynow \= Y,
																	Ynow2 is Ynow + 1,
																	setLineAux(Ynow2, X, Y, Number, Tail, Tail2).

setColAux(_,_,_,[],[]).
setColAux(X, X, Number, [ _ | Tail], [Number | Tail2]) :- 	Xnow is X + 1,
															setColAux(Xnow, X, Number, Tail, Tail2).																	
setColAux(Xnow, X, Number, [Element | Tail], [Element | Tail2]) :-  Xnow \= X,
																	Xnow2 is Xnow + 1,
																	setColAux(Xnow2, X, Number, Tail, Tail2).


% This predicate gets all the puzzle's numbered squares, coupled with their (X, Y) coordinates

getNumbers(Board, Numbers) :-
	length(Board, N),
	getNumbersInRows(Board, 0, N, List),
	flatten_levels(List, Numbers).

getNumbersInRows(_, N, N, []).
getNumbersInRows(Board, Y, N, [Head | Tail]) :-
	nth0(Y, Board, Row),
	getNumbersInRow(Row, 0, Y, N, Head),
	Y1 is Y + 1,
	getNumbersInRows(Board, Y1, N, Tail).

getNumbersInRow(_, N, _, N, []).
getNumbersInRow(Row, X, Y, N, [Head | Tail]) :-
	nth0(X, Row, Number),
	Number \= -1,
	Head = [Number, X, Y],
	X1 is X + 1,
	getNumbersInRow(Row, X1, Y, N, Tail).
getNumbersInRow(Row, X, Y, N, Tail) :-
	nth0(X, Row, Number),
	Number == -1,
	X1 is X + 1,
	getNumbersInRow(Row, X1, Y, N, Tail).

% Auxiliary Predicate to flatten the list

flatten_levels([], []).
flatten_levels([HeadList| TailList],Result) :- 
	flatten_levels(TailList,NewTailList), 
	!, 
	append(HeadList,NewTailList,Result).
flatten_levels([HeadList|Tail],[HeadList|OtherTail]) :- 
	flatten_levels(Tail,OtherTail).

% A predicate to add an index to each member of the squares list

indexNumbers(Numbers, Indexed) :-
	indexNumbersAux(Numbers, 0,Indexed).
indexNumbersAux([], _, [] ).
indexNumbersAux([ NumbersHead | OtherNumbers ], Index, [IndexedHead | IndexedTail] ) :-
	append([Index], NumbersHead, IndexedHead),
	Index1 is Index + 1,
	indexNumbersAux(OtherNumbers, Index1, IndexedTail).

% Do this from the board

getIndexedNumbers(Board, Indexed) :-
	getNumbers(Board, Numbers),
	indexNumbers(Numbers, Indexed).

% creates a NxN list

createEmptySolution(N, Result) :-
	createEmptySolutionAux(N, N, Result).

createEmptySolutionAux(_, 0, []).
createEmptySolutionAux(N, NN, [Head | Tail]) :-
	length(Head, N),
	NN > 0,
	NN1 is NN - 1,
	createEmptySolutionAux(N, NN1, Tail).


% This is the basic frame for the regions to be constructed upon.

initRegions(Board, [], Board).
initRegions(Board, [[CurrIndex, _, X, Y] | Tail ], Result) :-
	setNumberAtCoord(X, Y, CurrIndex, Board, NewBoard),
	initRegions(NewBoard, Tail, Result).

% get a list that, for a cell, gets the first obstacle in each vertical or horizontal direction
% [VU, VD, HL, HR]. ex: [1, -1, 2, 1] means that region 1 is obstructing its right and up sides
% and no region is obstructing its down side.

getObstacles(X, Y, Board, List) :-
	getNumberAtCoord(X, Y, Number, Board),

	% Up
	getObstacle(X, Y, 0, -1, Board, ObstacleUp),

	% Down
	getObstacle(X, Y, 0, 1, Board, ObstacleDown),

	% Left
	getObstacle(X, Y, -1, 0, Board, ObstacleLeft),

	% Right
	getObstacle(X, Y, 1, 0, Board, ObstacleRight),

	List = [ObstacleUp, ObstacleDown, ObstacleLeft, ObstacleRight].

% Up
getObstacle(X, 0, 0, -1, Board, Obstacle) :-
	getNumberAtCoord(X, 0, Obstacle, Board).

% Down
getObstacle(X, Y, 0, 1, Board, Obstacle) :-
	length(Board, A),
	Y #= ( A - 1 ),
	getNumberAtCoord(X, Y, Obstacle, Board).

% Left
getObstacle(0, Y, -1, 0, Board, Obstacle) :-
	getNumberAtCoord(0, Y, Obstacle, Board).

% Right
getObstacle(X, Y, 1, 0, Board, Obstacle) :-
	length(Board, A),
	X #= (A - 1),
	getNumberAtCoord(X, Y, Obstacle, Board).

% This predicate gets the obstacle. IF current == -1, stop! Else, continue.

getObstacle(X, Y, IncX, IncY, Board, Obstacle) :-
	
	Xnow is X + IncX,
	Ynow is Y + IncY,

	getNumberAtCoord(Xnow, Ynow, NumberNow, Board),

	NumberNow #\= -1,
	!,
	Obstacle #= NumberNow.

getObstacle(X, Y, IncX, IncY, Board, Obstacle) :-
	
	Xnow is X + IncX,
	Ynow is Y + IncY,

	getNumberAtCoord(Xnow, Ynow, NumberNow, Board),

	getObstacle(Xnow, Ynow, IncX, IncY, Board, Obstacle).
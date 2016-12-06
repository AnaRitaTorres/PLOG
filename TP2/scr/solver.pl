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

	getIndexedNumbers(Board, Indexed),
	length(Indexed, MaxRegions1),
	MaxRegions #= MaxRegions1 - 1,

	createEmptySolution(N, MaxRegions, EmptyResult),
	initRegions(EmptyResult, Indexed, InitedRegions),

	constrainCells(Board, InitedRegions),
	%constrainArea

	flatten_levels(InitedRegions, Result),
	labeling([], Result).


%%%%%%%%%%%%%%%%%%%%%%%%% AUXILARY PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%

% This predicate gets a matrix's element.
getNumberAtCoord(-1,-1,-1,_).
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
setColAux(Xnow, X, Number, [Element | Tail], [Head | Tail2]) :-  Xnow \= X,
																	Xnow2 is Xnow + 1,
																	Head #= Element,
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

createEmptySolution(N, MaxRegions, Result) :-
	createEmptySolutionAux(N, N, Result),
	constrainDomain(MaxRegions, Result).

createEmptySolutionAux(_, 0, []).
createEmptySolutionAux(N, NN, [Head | Tail]) :-
	length(Head, N),
	NN > 0,
	NN1 is NN - 1,
	createEmptySolutionAux(N, NN1, Tail).

constrainDomain(_,[]).
constrainDomain(MaxRegions, [Head | Tail]) :-
	domain(Head, 0, MaxRegions),
	constrainDomain(MaxRegions, Tail).

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
getObstacle(X, -1, 0, -1, Board, [-1,-1,-1]).

% Down
getObstacle(X, Y, 0, 1, Board, [-1,-1,-1]) :-
	length(Board, Y).

% Left
getObstacle(-1, Y, -1, 0, Board, [-1,-1,-1]).

% Right
getObstacle(X, Y, 1, 0, Board, [-1,-1,-1]) :-
	length(Board, X).

% This predicate gets the obstacle. IF current == -1, stop! Else, continue.

getObstacle(X, Y, IncX, IncY, Board, Obstacle) :-
	
	getNumberAtCoord(X, Y, NumberNow, Board),

	Xnow #= X + IncX,
	Ynow #= Y + IncY,

	NumberNow #\= -1,
	!,
	Obstacle = [NumberNow, X, Y].

getObstacle(X, Y, IncX, IncY, Board, Obstacle) :-
	
	getNumberAtCoord(X, Y, NumberNow, Board),

	Xnow #= X + IncX,
	Ynow #= Y + IncY,
	
	Number #= -1,

	getObstacle(Xnow, Ynow, IncX, IncY, Board, Obstacle).


% This is predicate that thins down the regions for any cells.

constrainCells(Board, Regions) :-
	length(Board, N),
	constrainRows(Board, Regions, 0, N).
constrainRows(Board, Regions, N,N).
constrainRows(Board, Regions, Y, N) :-
	constrainCols(Board, Regions, 0, Y, N),
	Y1 #= Y + 1,
	constrainRows(Board, Regions, Y1, N).
constrainCols(Board, Regions, N, _, N).
constrainCols(Board, Regions, X, Y, N) :-
	constrainCell(X, Y, Board, Regions),
	X1 #= X + 1,
	constrainCols(Board, Regions, X1, Y, N).

% A predicate to thin down the cell. Checks for the regions up, down, left and right of it, and sets its domain to those values.

constrainCell(X, Y, Board, Regions) :-
	getObstacles(X, Y, Board, [[OU_V, OU_X, OU_Y], [OD_V, OD_X, OD_Y], [OL_V, OL_X, OL_Y], [OR_V, OR_X, OR_Y]]),

	getNumberAtCoord(OU_X, OU_Y, RegionUP, Regions),
	getNumberAtCoord(OD_X, OD_Y, RegionDOWN, Regions),
	getNumberAtCoord(OL_X, OL_Y, RegionLEFT, Regions),
	getNumberAtCoord(OR_X, OR_Y, RegionRIGHT, Regions),

	getNumberAtCoord(X,Y,Var,Regions),

	Var in {RegionUP,RegionDOWN,RegionLEFT,RegionRIGHT},
	Var #>= 0.
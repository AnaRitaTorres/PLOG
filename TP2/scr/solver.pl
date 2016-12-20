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
% Then, we just ID the region and see all the possible regions with restrictions...

% An example of an original board and the generated solution:
% -1  4 -1 -1				0  0  0  0
% -1 -1  0 -1				3  0  1  2		This means the 4 on coordinates (x=1, y=0) became the capital of region 0, which spreads
% -1 -1 -1  4		-->		3  2  2  2		as shown. The region's area is 5, which is the original 4 + 1.
%  4 -1 -1 -1				3  3  3  2

solver(Board, Solution) :-
	
	% board is always square; calculate its side.	
	length(Board, N),

	% see where the numbers are. Assign them to a region (give them an index).
	getIndexedNumbers(Board, Indexed),
	length(Indexed, MaxRegions1),
	MaxRegions #= MaxRegions1 - 1,

	% create an empty matrix, to be restricted
	createEmptySolution(N, MaxRegions, EmptyResult),

	% initiate statistics
	statistics(walltime, _),

	% start off the regions where the numbers are on the original board (these are called capitals). Place the capital index on the matrix, not the length.
	initRegions(EmptyResult, Indexed, Result),

	% See that cells above, below, left and right of these starting regions may only be occupied by these regions.
	% Ie, a cell that's diagonal to a capital can never be on that capital's region.
	constrainCells(Board, Result),

	% The true problem of "Four Winds". Constrain the board so that it creates regions with areas said by the numbers.
	constrainAreas(Indexed, Result, 0, MaxRegions1),

	% The last two constraints would have made disjoint regions of the same index possible... This one takes those out of the solution, though.
	constrainCohesions(Indexed, Result, 0, MaxRegions1),

	% We were opperating on a matrix because it was a lot easier to visualize and manipulate. To generate the solution, we need to flatten out the matrix on a single list.
	flatten_list(Result, Flat_Solution),

	% Generate solution
	labeling([bisect], Flat_Solution),

	% Obtain and print statistics
	statistics(walltime, [_, Elapsed | _]),
	format('Time taken to find solution: ~3d seconds', Elapsed), nl,
	fd_statistics,

	% Convert back, so it's easier to print.
	list_to_matrix(Flat_Solution, N, Solution).


%%%%%%%%%%%%%%%%%%%%%%%%% AUXILARY PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%

% This predicate gets a matrix's element at a given pair of coordinates.
getNumberAtCoord(-1,-1,-1,_).
getNumberAtCoord(X, Y, Number, Board) :-
	nth0(Y, Board, Row),
	nth0(X, Row, Number).

% This predicate sets a matrix's element at a given pair of coordinates.

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


% This predicate gets all the puzzle's numbered squares (capitals), coupled with their (X, Y) coordinates.

getNumbers(Board, Numbers) :-
	length(Board, N),
	getNumbersInRows(Board, 0, N, List),
	flatten_list(List, Numbers).

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

flatten_list([], []).
flatten_list( [HeadList| TailList], Result) :- 
	flatten_list( TailList, NewTailList), 
	!, 
	append(HeadList, NewTailList, Result).
flatten_list( [ HeadList | Tail ], [ HeadList | OtherTail ]) :- 
	flatten_list( Tail, OtherTail).

% A predicate to add an index to each member of the squares list
% Ie, this gets all the information we need about each capital: [Region_Index, Region_TotalArea, Region_CapitalX, Region_CapitalY].

indexNumbers(Numbers, Indexed) :-
	indexNumbersAux(Numbers, 0,Indexed).
indexNumbersAux([], _, [] ).
indexNumbersAux([ NumbersHead | OtherNumbers ], Index, [IndexedHead | IndexedTail] ) :-
	append([Index], NumbersHead, IndexedHead),
	Index1 is Index + 1,
	indexNumbersAux(OtherNumbers, Index1, IndexedTail).

getIndexedNumbers(Board, Indexed) :-
	getNumbers(Board, Numbers),
	indexNumbers(Numbers, Indexed).

% creates a NxN list (the empty solution). Also, constrain the domain from minRegion (always 0) to maxRegion (number of capitals).

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
% [[VU, VU_X, VU_Y], [VD, VD_X, VD_Y], [HL, HL_X, HL_Y], [HR, HR_X, HR_Y]]. Ie, the region VU, obstructs our cell at coordinates (VU_X, VU_Y). Same for all the other ones...
% If our cell already belongs to a region, it gets all obstacles to that region.

getObstacles(X, Y, Board, List) :-
	getNumberAtCoord(X, Y, _, Board),

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
getObstacle(_, -1, 0, -1, _, [-1,-1,-1]).

% Down
getObstacle(_, Y, 0, 1, Board, [-1,-1,-1]) :-
	length(Board, Y).

% Left
getObstacle(-1, _, -1, 0, _, [-1,-1,-1]).

% Right
getObstacle(X, _, 1, 0, Board, [-1,-1,-1]) :-
	length(Board, X).

% This predicate gets the obstacle. IF current == -1, stop! Else, continue.

getObstacle(X, Y, _, _, Board, Obstacle) :-
	
	getNumberAtCoord(X, Y, NumberNow, Board),

	NumberNow #\= -1,
	!,
	Obstacle = [NumberNow, X, Y].

getObstacle(X, Y, IncX, IncY, Board, Obstacle) :-
	
	getNumberAtCoord(X, Y, _, Board),

	Xnow #= X + IncX,
	Ynow #= Y + IncY,
	
	getObstacle(Xnow, Ynow, IncX, IncY, Board, Obstacle).


% This is predicate that thins down the regions for any cells.
% A cell can belong to any region above, below, left, and right of it (if they exist).
% This thins down cells in the sense that it restrits the cell's domain to only accept those 4 (max) possible regions.
% If nothing is obstructing the cell on any given side, that side does not interfere with the domain.

constrainCells(Board, Regions) :-
	length(Board, N),
	constrainRows(Board, Regions, 0, N).
constrainRows(_, _, N,N).
constrainRows(Board, Regions, Y, N) :-
	constrainCols(Board, Regions, 0, Y, N),
	Y1 #= Y + 1,
	constrainRows(Board, Regions, Y1, N).
constrainCols(_,_, N, _, N).
constrainCols(Board, Regions, X, Y, N) :-
	constrainCell(X, Y, Board, Regions),
	X1 #= X + 1,
	constrainCols(Board, Regions, X1, Y, N).

constrainCell(X, Y, Board, Regions) :-
	getObstacles(X, Y, Board, [[_, OU_X, OU_Y], [_, OD_X, OD_Y], [_ ,OL_X, OL_Y], [_, OR_X, OR_Y]]),

	getNumberAtCoord(OU_X, OU_Y, RegionUP, Regions),
	getNumberAtCoord(OD_X, OD_Y, RegionDOWN, Regions),
	getNumberAtCoord(OL_X, OL_Y, RegionLEFT, Regions),
	getNumberAtCoord(OR_X, OR_Y, RegionRIGHT, Regions),

	getNumberAtCoord(X,Y,Var,Regions),

	Var in {RegionUP,RegionDOWN,RegionLEFT,RegionRIGHT},
	Var #>= 0.

% This is the predicate that checks that all the regions on the board are acceptable (in area).
% The area is the number given on the original board plus one (counting the capital as part of the region!).
% It checks the cells and, if they are set to the region we want, they contribute to the region's total area.
% This creates some impossible scenarios, though.

constrainAreas(_,_, Length, Length).
constrainAreas(Indexed, Regions, Index, Length) :-	
	constrainArea(Index, Indexed, Regions,_),
	IndexNew #= Index + 1,
	constrainAreas(Indexed, Regions, IndexNew, Length).

constrainArea(Index, Indexed, Regions, GottenArea) :-
	member([Index, Area_1, _, _], Indexed),
	Area #= Area_1 + 1,
	getArea(Index, _, Regions, GottenArea), 
	GottenArea #= Area.

getArea(Index, _, Regions, GottenArea) :-
	length(Regions, N),
	getAreaRows(0, N, Index, Regions, GottenArea).

getAreaRows(N,N,_,_,0).
getAreaRows(Y,N,Index,Regions,GottenArea) :-
	getAreaCols(0,Y,N,Index,Regions,ThisGottenArea),
	Y1 #= Y + 1,
	getAreaRows(Y1, N, Index, Regions, OtherGottenArea),
	GottenArea #= OtherGottenArea + ThisGottenArea.

getAreaCols(N, _, N, _, _, 0).
getAreaCols(X, Y, N, Index, Regions, GottenArea) :-

	getNumberAtCoord(X, Y, Var, Regions),

	Var #= Index #<=> B,

	X1 #= X + 1,
	getAreaCols(X1, Y, N, Index, Regions, ThisGottenArea),
	GottenArea #= ThisGottenArea + B.

% This predicate restricts the board so that no region can be disjoint with itself, ie, it removes scenarios like this one (numbers indicate region index):

%	0 1 0 0 0
%	0 1 1 1 1		Look at region 0. It's disjoint with itself.
%	2 1 3 3 3
%	2 4 4 4 4
%	2 5 5 5 5

%	0 0 0 0 0
%	1 1 1 1 1		Should look like this.
%	2 1 3 3 3
%	2 4 4 4 4
%	2 5 5 5 5

constrainCohesions(_,_, Length, Length).
constrainCohesions(Indexed, Regions, Index, Length) :-	
	constrainCohesion(Indexed, Regions, Index),
	IndexNew #= Index + 1,
	constrainCohesions(Indexed, Regions, IndexNew, Length).

constrainCohesion(Indexed, Regions, Index) :-
	member([Index, Area_1, X, Y], Indexed),

	constrainCohesionUp(Index, Regions, X, Y, AreaUp),
	constrainCohesionDown(Index, Regions, X, Y, AreaDown),
	constrainCohesionLeft(Index, Regions, X, Y, AreaLeft),
	constrainCohesionRight(Index, Regions, X, Y, AreaRight),

	OffsettedUp #= AreaUp - 1, OffsettedDown #= AreaDown - 1,
	OffsettedLeft #= AreaLeft - 1, OffsettedRight #= AreaRight - 1,

	Area_1 #= OffsettedUp + OffsettedDown + OffsettedLeft + OffsettedRight.

constrainCohesionUp(_, _, _, -1, 0).
constrainCohesionUp(Index, Regions, X, Y, Area) :-
	getNumberAtCoord(X, Y, Var, Regions),

	Var #= Index,

	NewY #= Y - 1,
	constrainCohesionUp(Index, Regions, X, NewY, NewArea),
	Area #= NewArea + 1.
constrainCohesionUp(Index, Regions, X, Y, Area) :-
	getNumberAtCoord(X, Y, Var, Regions),

	Var #\= Index,
	Area #= 0.

constrainCohesionDown(_, Regions, _, Y, 0) :- length(Regions, Y). 
constrainCohesionDown(Index, Regions, X, Y, Area) :-
	getNumberAtCoord(X, Y, Var, Regions),

	Var #= Index,

	NewY #= Y + 1,
	constrainCohesionDown(Index, Regions, X, NewY, NewArea),
	Area #= NewArea + 1.
constrainCohesionDown(Index, Regions, X, Y, Area) :-
	getNumberAtCoord(X, Y, Var, Regions),

	Var #\= Index,
	Area #= 0.

constrainCohesionLeft(_, _, -1, _, 0).
constrainCohesionLeft(Index, Regions, X, Y, Area) :-
	getNumberAtCoord(X, Y, Var, Regions),

	Var #= Index,

	NewX #= X - 1,
	constrainCohesionLeft(Index, Regions, NewX, Y, NewArea),
	Area #= NewArea + 1.
constrainCohesionLeft(Index, Regions, X, Y, Area) :-
	getNumberAtCoord(X, Y, Var, Regions),

	Var #\= Index,
	Area #= 0.

constrainCohesionRight(_, Regions, X, _, 0) :- length(Regions, X). 
constrainCohesionRight(Index, Regions, X, Y, Area) :-
	getNumberAtCoord(X, Y, Var, Regions),

	Var #= Index,

	NewX #= X + 1,
	constrainCohesionRight(Index, Regions, NewX, Y, NewArea),
	Area #= NewArea + 1.
constrainCohesionRight(Index, Regions, X, Y, Area) :-
	getNumberAtCoord(X, Y, Var, Regions),

	Var #\= Index,
	Area #= 0.
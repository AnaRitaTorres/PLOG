%%%%%%%%%%%
%%PRINTER%%
%%%%%%%%%%%


% A predicate to print the element, whether it is a capital, a dash or an arrow.
printElement(Elem-Indexed-_, X, Y,_) :-
	member([Elem, Area, CapitalX, CapitalY], Indexed),
	CapitalX #= X, CapitalY #= Y,
	Area #>= 10,
	write(' '),
	write(Area),
	printColumnSeparator.

printElement(Elem-Indexed-_, X, Y,_) :-
	member([Elem, Area, CapitalX, CapitalY], Indexed),
	CapitalX #= X, CapitalY #= Y,
	Area #< 10,
	write(' '),
	write(Area),
	write(' '),
	printColumnSeparator.
	
% ^
printElement(Elem-Indexed-Solution,X,Y,_) :-
	member([Elem, _,CapitalX, CapitalY], Indexed),
	CapitalX #= X,
	Y  #< CapitalY,

	isLastOnBranch(Elem-Indexed-Solution,X,Y,0,-1),

	write(' ^'),
	write(' '),
	printColumnSeparator.

printElement(Elem-Indexed-Solution,X,Y,_) :-
	member([Elem, _,CapitalX, CapitalY], Indexed),
	CapitalX #= X,
	Y  #< CapitalY,

	\+ isLastOnBranch(Elem-Indexed-Solution,X,Y,0,-1),

	write(' |'),
	write(' '),
	printColumnSeparator.

% v
printElement(Elem-Indexed-Solution,X,Y,_) :-
	member([Elem, _,CapitalX, CapitalY], Indexed),
	CapitalX #= X,
	Y #> CapitalY,

	isLastOnBranch(Elem-Indexed-Solution,X,Y,0,1),

	write(' !'),
	write(' '),
	printColumnSeparator.

printElement(Elem-Indexed-Solution,X,Y,_) :-
	member([Elem, _,CapitalX, CapitalY], Indexed),
	CapitalX #= X,
	Y #> CapitalY,

	\+ isLastOnBranch(Elem-Indexed-Solution,X,Y,0,1),

	write(' |'),
	write(' '),
	printColumnSeparator.
	
% <
printElement(Elem-Indexed-Solution,X,Y,_) :-
	member([Elem, _,CapitalX, CapitalY], Indexed),
	CapitalY #= Y,
	X  #< CapitalX,

	isLastOnBranch(Elem-Indexed-Solution,X,Y,-1,0),

	write(' '),
	write('<-'),
	printColumnSeparator.
	
printElement(Elem-Indexed-Solution,X,Y,_) :-
	member([Elem, _,CapitalX, CapitalY], Indexed),
	CapitalY #= Y,
	X  #< CapitalX,

	\+ isLastOnBranch(Elem-Indexed-Solution,X,Y,-1,0),

	write('---'),
	printColumnSeparator.

% >
printElement(Elem-Indexed-Solution,X,Y,_) :-
	member([Elem, _,CapitalX, CapitalY], Indexed),
	CapitalY #= Y,
	X  #> CapitalX,

	isLastOnBranch(Elem-Indexed-Solution,X,Y,1,0),

	write('->'),
	write(' '),
	printColumnSeparator.

printElement(Elem-Indexed-Solution,X,Y,_) :-
	member([Elem, _,CapitalX, CapitalY], Indexed),
	CapitalY #= Y,
	X  #> CapitalX,

	\+ isLastOnBranch(Elem-Indexed-Solution,X,Y,1,0),

	write('---'),
	printColumnSeparator.

% The predicate to print the solution itself

printSolution(Solution-Indexed) :-
	length(Solution, N),
	nth0(0, Solution, Row),
	write(' '), 
	printRowHeader(Row), nl,
	printSolutionAux(Solution-Indexed, 0, N).

printSolutionAux(_-_, N, N).
printSolutionAux(Solution-Indexed, I, N) :-
	nth0(I, Solution, Row),
	printColumnSeparator,
	printRow(Row-Indexed-Solution, 0, I, N), nl,
	printColumnSeparator, printRowAppearance(Row), nl,
	NewI #= I + 1,
	printSolutionAux(Solution-Indexed, NewI, N).

printRow(_, N, _, N).
printRow(Row-Indexed-Solution, X, Y, N):-
	nth0(X, Row, Elem),
	printElement(Elem-Indexed-Solution, X, Y, N),
	NewX #= X + 1,
	printRow(Row-Indexed-Solution, NewX, Y, N).

printColumnSeparator :- write('|').

printRowHeader([]).
printRowHeader([_| Tail]) :-
 	printRowUnderline,
 	write(' '),
 	printRowHeader(Tail).

printRowAppearance([]).
printRowAppearance([_ | Tail]) :-
	printRowUnderline,
	printColumnSeparator,
	printRowAppearance(Tail).
printRowUnderline :- write('___').


% A predicate to print the unsolved puzzle.

printUnsolvedPuzzle(Puzzle):-
	length(Puzzle, N),
	nth0(0, Puzzle, Row),
	write(' '), printRowHeader(Row),
	nl,
	printUnsolvedPuzzle_Aux(Puzzle, 0, N).

printUnsolvedPuzzle_Aux(_,N,N).
printUnsolvedPuzzle_Aux(Puzzle, Y, N) :-
	nth0(Y,Puzzle, Row),
	printColumnSeparator,
	printUnsolvedPuzzleRow(Row, 0, N),
	NewY #= Y + 1,
	nl,
	printColumnSeparator,
	printRowAppearance(Row),
	nl,
	printUnsolvedPuzzle_Aux(Puzzle, NewY, N).

printUnsolvedPuzzleRow(_, N, N).
printUnsolvedPuzzleRow(Row, X, N) :-
	nth0(X, Row, Elem),
	printUnsolvedElem(Elem),
	printColumnSeparator,
	NewX #= X + 1,
	printUnsolvedPuzzleRow(Row, NewX, N).

printUnsolvedElem(Elem) :-
	Elem >= 10,
	write(Elem), write(' ').
printUnsolvedElem(Elem) :-
	Elem < 0,
	write('   ').
printUnsolvedElem(Elem) :-
	Elem < 10,
	write(' '), write(Elem), write(' ').

% A predicate to convert a list to a matrix of a given side.
% Used to convert the flattened out solution back into an easier-to-print matrix.

list_to_matrix([], _, []).
list_to_matrix(List, Size, [Row|Matrix]):-
  list_to_matrix_row(List, Size, Row, Tail),
  list_to_matrix(Tail, Size, Matrix).

list_to_matrix_row(Tail, 0, [], Tail).
list_to_matrix_row([Item|List], Size, [Item|Row], Tail):-
  NSize is Size-1,
  list_to_matrix_row(List, NSize, Row, Tail).


% A predicate used to determine whether this cell is the last on this region's branch. Used to check if it should be a dash or an arrow.

% ^
isLastOnBranch(_-_-_,_,Y,0,-1) :-
	Y #= 0.
isLastOnBranch(Elem-_-Solution,X,Y,0,-1) :-
	Y #\= 0,
	Above #= Y - 1,
	nth0(Above, Solution, Row), nth0(X, Row, Neighboor),
	Neighboor #\= Elem.

% v
isLastOnBranch(_-_-Solution,_,Y,0,1) :-
	length(Solution, N),
	Y #= N - 1.
isLastOnBranch(Elem-_-Solution,X,Y,0,1) :-
	length(Solution, N),
	Y #\= N - 1,
	Below #= Y + 1,
	nth0(Below, Solution, Row), nth0(X, Row, Neighboor),
	Neighboor #\= Elem.

% <
isLastOnBranch(_-_-_,X,_,-1,0) :-
	X #= 0.
isLastOnBranch(Elem-_-Solution,X,Y,-1,0) :-
	X #\= 0,
	Left #= X - 1,
	nth0(Y, Solution, Row), nth0(Left, Row, Neighboor),
	Neighboor #\= Elem.

%>
isLastOnBranch(_-_-Solution,X,_,1,0) :-
	length(Solution, N),
	X #= N - 1.
isLastOnBranch(Elem-_-Solution,X,Y,1,0) :-
	length(Solution,N),
	X #\= N - 1,
	Right #= X + 1,
	nth0(Y, Solution, Row), nth0(Right, Row, Neighboor),
	Neighboor #\= Elem.
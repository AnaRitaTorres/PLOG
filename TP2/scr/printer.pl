%%%%%%%%%%%
%%PRINTER%%
%%%%%%%%%%%
%→↑←↓

% A Predicate to print the board

% [IndiceRegiao, AreaTotal-1, X, Y]
printElement(Elem-Indexed, X, Y,_) :-
	member([Elem, Area, CapitalX, CapitalY], Indexed),
	CapitalX #= X, CapitalY #= Y,
	Area #>= 10,
	write(' '),
	write(Area),
	printColumnSeparator.

printElement(Elem-Indexed, X, Y,_) :-
	member([Elem, Area, CapitalX, CapitalY], Indexed),
	CapitalX #= X, CapitalY #= Y,
	Area #< 10,
	write(' '),
	write(Area),
	write(' '),
	printColumnSeparator.

printElement(Elem-Indexed, X, Y,N) :-
	member([Elem, _, CapitalX, _], Indexed),
	CapitalX #= X,
	Y #> 0,
	Size is N -1,
	Y #< Size,
	write(' |'),
	write(' '),
	printColumnSeparator.
	
printElement(Elem-Indexed, X, Y,N) :-
	member([Elem, _, CapitalX, _], Indexed),
	CapitalX #= X,
	Y #= 0,
	write(' ?'),
	write(' '),
	printColumnSeparator.
	
printElement(Elem-Indexed, X, Y,N) :-
	member([Elem, _, CapitalX, _], Indexed),
	CapitalX #= X,
	Size is N -1,
	Y #= Size,
	write(' !'),
	write(' '),
	printColumnSeparator.

printElement(Elem-Indexed, X, Y,N) :-
	member([Elem, _, _, CapitalY], Indexed),
	CapitalY #= Y,
	X #> 0,
	Size is N -1,
	X #< Size,
	write('--'),
	write(' '),
	printColumnSeparator.
	
printElement(Elem-Indexed,X,Y,N) :-
	member([Elem, _, _, CapitalY], Indexed),
	CapitalY #= Y,
	Size is N-1,
	X #= Size,
	write('->'),
	write(' '),
	printColumnSeparator.
	
printElement(Elem-Indexed,X,Y,_) :-
	member([Elem, _, _, CapitalY], Indexed),
	CapitalY #= Y,
	X #= 0,
	write('<-'),
	write(' '),
	printColumnSeparator.

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
	printRow(Row-Indexed, 0, I, N), nl,
	printColumnSeparator, printRowAppearance(Row), nl,
	NewI #= I + 1,
	printSolutionAux(Solution-Indexed, NewI, N).

printRow(_, N, _, N).
printRow(Row-Indexed, X, Y, N):-
	nth0(X, Row, Elem),
	printElement(Elem-Indexed, X, Y, N),
	NewX #= X + 1,
	printRow(Row-Indexed, NewX, Y, N).

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
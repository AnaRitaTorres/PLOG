%%%%%%%%%%%
%%PRINTER%%
%%%%%%%%%%%

solvePuzzle(Board) :-
	solver(Board, Result),
	getIndexedNumbers(Board, Indexed),
	printSolution(Result-Indexed), !.

% A Predicate to print the board

% [IndiceRegiao, AreaTotal-1, X, Y]
printElement(Elem-Indexed, X, Y) :-
	member([Elem, Area, CapitalX, CapitalY], Indexed),
	CapitalX #= X, CapitalY #= Y,
	Area #>= 10,
	write(Area),
	printColumnSeparator.

printElement(Elem-Indexed, X, Y) :-
	member([Elem, Area, CapitalX, CapitalY], Indexed),
	CapitalX #= X, CapitalY #= Y,
	Area #< 10,
	write(Area), write(' '),
	printColumnSeparator.

printElement(Elem-Indexed, X, Y) :-
	member([Elem, _, CapitalX, CapitalY], Indexed),
	CapitalX #= X,
	write(' |'),
	printColumnSeparator.

printElement(Elem-Indexed, X, Y) :-
	member([Elem, _, CapitalX, CapitalY], Indexed),
	CapitalY #= Y,
	write('--'),
	printColumnSeparator.

printSolution(Solution-Indexed) :-
	length(Solution, N),
	nth0(0, Solution, Row), write(' '), printRowHeader(Row), nl,
	printSolutionAux(Solution-Indexed, 0, N).

printSolutionAux(Solution-Indexed, N, N).
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
	printElement(Elem-Indexed, X, Y),
	NewX #= X + 1,
	printRow(Row-Indexed, NewX, Y, N).

printColumnSeparator :- write('|').

printRowHeader([]).
printRowHeader([Elem | Tail]) :-
 	printRowUnderline,
 	write(' '),
 	printRowHeader(Tail).

printRowAppearance([]).
printRowAppearance([Elem | Tail]) :-
	printRowUnderline,
	printColumnSeparator,
	printRowAppearance(Tail).
printRowUnderline :- write('__').

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
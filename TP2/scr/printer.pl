%%%%%%%%%%%
%%PRINTER%%
%%%%%%%%%%%

% TODO
% A Predicate to print the board
printElement(Elem) :- write(Elem).
printBoard([]).
printBoard([Row | Tail]) :-
	printRow(Row), nl,
	printBoard(Tail).

printRow([]).
printRow([Elem | Tail]):-
	printElement(Elem),
	printRow(Tail).

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
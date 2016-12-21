
:- use_module(library(random)).

clr:- write('\33\[2J').

fourWinds :-
	repeat,
	clr,
	nl,nl,
	write('------------------------- FOUR WINDS : A BOARD PUZZLE ------------------------'), nl,
	write('------------------------------------------------------------------------------'), nl, nl,
	write('Press the following to start: '), nl,
	write('1 - Run puzzle solver'), nl,
	write('2 - Read about Four Winds'), nl,
	write('3 - Quit'), nl, nl,
	read(Input),
	menu(Input).

menu(1) :- 
	repeat,
	clr,
	nl,nl,
	write('------------------------- FOUR WINDS : A BOARD PUZZLE ------------------------'), nl,
	write('------------------------------------------------------------------------------'), nl, nl,
	write('Choose a board side size (?x?, from 2x2 to 12x12)'), nl,
	read(Input),
	getAPuzzleOfSize(Input, Puzzle),
	write('Chose the following '), write(Input), write('x'), write(Input), write(' board for you: '), nl, nl,
	printUnsolvedPuzzle(Puzzle), nl, nl,
	write('Press a key to initiate solving...'),
	get_char(_), get_char(_),
	nl, write('Please wait'), nl,
	solvePuzzle(Puzzle),
	!,
	write('Press 4 to go back to the menu.'), nl,
	write('------------------------------------------------------------------------------'), nl, nl,
	read(Input),
	menu(Input).

menu(2) :- 
	repeat,
	clr,
	nl, nl,
	write('------------------------- FOUR WINDS : A BOARD PUZZLE ------------------------'), nl,
	write('------------------------------------------------------------------------------'), nl, nl,
	write('Draw one or more lines from each numbered cell so that each number'), nl, write('indicates the total length of lines that are drawn from that cell,'), nl, 
	write('excluding the cell itself.'), nl, nl,
	write('Lines are either horizontal or vertical and connect the centers of '), nl, write('adjacent cells without crossing or overlapping each other'), nl, write('and the given numbers.'), nl, nl,
	write('Press 4 to go back to the menu.'), nl,
	write('------------------------------------------------------------------------------'), nl, nl,
	read(Input),
	menu(Input).
menu(3) :- clr.
menu(4) :- fourWinds.

getPuzzlesOfSize(Size, Puzzles) :-
	findall(Board, puzzle(Size, Board), Puzzles).

getAPuzzleOfSize(Size, Puzzle) :-
	getPuzzlesOfSize(Size, Puzzles),
	length(Puzzles, N),
	random(0, N, X),
	nth0(X, Puzzles, Puzzle).

solvePuzzle(Board) :-
	solver(Board, Result),
	getIndexedNumbers(Board, Indexed),
	printSolution(Result-Indexed), !.
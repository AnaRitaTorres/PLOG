%%%%%%%%%%%%%%%%%%%
%%% PRINT BOARD %%%
%%%%%%%%%%%%%%%%%%%

rows( [' A','B','C','D','E','F','G','H','I','J','K','L'] ).
cols( ['A','B','C','D','E','F','G','H','I','J','K','L'] ).

printAlphaRows( [] ).
printAlphaRows( [Head|Rest] ) :- write('     '), write(Head), printAlphaRows(Rest).
printAlphaCol(AlphaCol) :- write(AlphaCol), write(' ').

% write the cell onto the console, according to the symbol on the board
escreveCell(Symbol, IvoryQueenStack, CigarQueenStack):-
			( 	Symbol == ivoryQueen -> (IvoryQueenStack > 9 -> write(IvoryQueenStack), write('I'); write(IvoryQueenStack), write(' '), write('I'));
				Symbol == cigarQueen -> (CigarQueenStack > 9 -> write(CigarQueenStack), write('C'); write(CigarQueenStack), write(' '), write('C'));
				Symbol == ivoryBaby -> write(' i ');
				Symbol == cigarBaby -> write(' c ');
				Symbol == empty -> write('   ');
				write('?')
			).

% print a row of the board
printBoardRow(_, _, [] ) :- write(' | ').
printBoardRow(IvoryQueenStack, CigarQueenStack, [Head|OtherHeads] ) :-  write(' | '), escreveCell(Head, IvoryQueenStack, CigarQueenStack), printBoardRow(IvoryQueenStack,CigarQueenStack,OtherHeads).

printBoardRowSeparatorCol(0).
printBoardRowSeparatorCol(Col) :- (Col =< 12, Col > 0) -> (write(' -----'), Col1 is Col - 1, printBoardRowSeparatorCol(Col1)).

printBoardRowSeparator :- write('   '), printBoardRowSeparatorCol(12).

% print the whole board (similar to the row printing)
printBoard(_, _, []).
printBoard(IvoryQueenStack, CigarQueenStack, [Row|OtherRows], [AlphaCol|OtherAlphaCols]) :- printAlphaCol(AlphaCol), printBoardRow(IvoryQueenStack, CigarQueenStack, Row), nl, printBoardRowSeparator, nl, printBoard(IvoryQueenStack, CigarQueenStack, OtherRows, OtherAlphaCols).

printFancyBoard(IvoryQueenStack, CigarQueenStack, Board) :-  rows(Rows), printAlphaRows(Rows), nl, printBoardRowSeparator, cols(Cols), nl, printBoard(IvoryQueenStack, CigarQueenStack, Board, Cols).

% print an empty board
printEmptyBoard :- Board^(emptyBoard(Board), printFancyBoard(20,20,Board)).
printHalfwayBoard :- Board^(halfwayBoard(Board), printFancyBoard(4, 6, Board)).
printFinalBoard :- Board^(finalBoard(Board), printFancyBoard(2, 2, Board)).
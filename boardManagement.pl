%%%%%%%%%%%%%%%%%%%%%%%%
%%% BOARD MANAGEMENT %%%
%%%%%%%%%%%%%%%%%%%%%%%%

dentroTabuleiro(X, Y) :- 	X >= 0, X =< 11,
							Y >= 0, Y =< 11.

naoSiPropria(X1,Y1,X2,Y2):- X1 \= X2 ; Y1 \= Y2.
							
vertical(X1, X2) :- 	X1 =:= X2.
vertical(X1, _, X2, _) :- vertical(X1, X2).

horizontal(Y1, Y2) :- 	Y1 =:= Y2.
horizontal(_, Y1, _, Y2) :- horizontal(Y1, Y2).

diagonal(X1, Y1, X2, Y2) :- abs(X1 - X2) =:= abs(Y1 - Y2).
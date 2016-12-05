%%%%%%%%%%%
%%PRINTER%%
%%%%%%%%%%%


printElement(Elem):-
	number(Elem),
	Elem > 0,
	write(Elem).
	
printElement(Elem):-
	number(Elem),
	Elem == -1,
	write(' ').
	
printElement(_).
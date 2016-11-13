
pMainMenu :-
	  clr,
	write('||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'), nl,
	write('||                        MONKEY QUEEN                          ||'), nl,
	write('||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'), nl,
	write('||                                                              ||'), nl,
	write('||                                                              ||'), nl,
	write('||                        1.Play                                ||'), nl,
	write('||                        2.How To Play                         ||'), nl,
	write('||                        3.About                               ||'), nl,
	write('||                        4.Exit                                ||'), nl,
	write('||                                                              ||'), nl,
	write('||                                                              ||'), nl,
	write('||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'), nl.
	
mainMenu :-
	pMainMenu,
	read(I),
	(
		I =:= 1 ->playMenu;
		I =:= 2 ->howToMenu;
		I =:= 3 ->aboutMenu;
		I =:= 4;
		
		mainMenu
	).
	
	
pPlayMenu :-
	 clr,
	write('||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'), nl,
	write('||                 MONKEY QUEEN - Play Modes                    ||'), nl,
	write('||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'), nl,
	write('||                                                              ||'), nl,
	write('||                                                              ||'), nl,
	write('||                    1.Player vs Player                        ||'), nl,
	write('||                    2.Player vs Computer                      ||'), nl,
	write('||                    3.Computer vs Computer                    ||'), nl,
	write('||                    4.Go Back                                 ||'), nl,
	write('||                                                              ||'), nl,
	write('||                                                              ||'), nl,
	write('||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'), nl.

pDifMenu1 :-
     clr,
	write('||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'), nl,
	write('||                 MONKEY QUEEN - Difficulty                    ||'), nl,
	write('||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'), nl,
	write('||                                                              ||'), nl,
	write('||                                                              ||'), nl,
	write('||                       1.Easy                                 ||'), nl,
	write('||                       2.Hard                                 ||'), nl,
	write('||                       4.Go Back                              ||'), nl,
	write('||                                                              ||'), nl,
	write('||                                                              ||'), nl,
	write('||                                                              ||'), nl,
	write('||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'), nl.
	
pDifMenu2 :-
     clr,
	write('||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'), nl,
	write('||                 MONKEY QUEEN - Difficulty                    ||'), nl,
	write('||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'), nl,
	write('||                                                              ||'), nl,
	write('||                                                              ||'), nl,
	write('||                       1.Easy                                 ||'), nl,
	write('||                       2.Hard                                 ||'), nl,
	write('||                       4.Go Back                              ||'), nl,
	write('||                                                              ||'), nl,
	write('||                                                              ||'), nl,
	write('||                                                              ||'), nl,
	write('||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'), nl.
	

playMenu :-
	pPlayMenu,
	read(I),
	(
		I =:= 1 ->bootPlay(20,human,human);
		I =:= 2 ->difMenu1;
		I =:= 3 ->difMenu2;
		I =:= 4 ->mainMenu;
		
		playMenu
	).

difMenu1 :-
	pDifMenu1,
	read(I),
	(
		I =:= 1 ->bootPlay(20,human,botDif1);
		I =:= 2 ->bootPlay(20,human,botDif2);
		I =:= 4 ->playMenu; 
		
		difMenu1
	).

difMenu2 :-
	pDifMenu2,
	read(I),
	(
		I =:= 1 ->bootPlay(20,botDif1,botDif1);
		I =:= 2 ->bootPlay(20,botDif2,botDif2);
		I =:= 4 ->playMenu; 
		
		difMenu2
	).
	
pHowToMenu :-
     clr,
	get_code(_),nl,
	write('||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'), nl,
	write('||                 MONKEY QUEEN - How To Play                   ||'), nl,
	write('||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'), nl,
	write('||                                                              ||'), nl,
	write('||      A monkey queen captures exactly like a Chess queen.     ||'), nl,
	write('||      Slides in any direction along a straight line of        ||'), nl,
	write('||      unoccupied squares ending with the capture of           ||'), nl,
	write('||                    an enemy piece.                           ||'), nl,
	write('||                                                              ||'), nl,
	write('||                                                              ||'), nl,
	write('||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'), nl,
	write('Press Enter to Continue'),nl,
	get_code(_),!,nl,
	
	clr,
	write('||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'), nl,
	write('||                 MONKEY QUEEN - How To Play                   ||'), nl,
	write('||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'), nl,
	write('||                                                              ||'), nl,
	write('||      When not capturing, a queen moves any distance          ||'), nl,
	write('||      in any direction, again like a Chess queen,             ||'), nl,
	write('||      except it leaves its bottom checker behind on the       ||'), nl,
	write('||      originating square,reducing its stack height by one.    ||'), nl,
	write('||      The queen monkey has thus given birth to a baby.        ||'), nl,
	write('||                                                              ||'), nl,
	write('||                                                              ||'), nl,
	write('||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'), nl,
	write('Press Enter to Continue'),nl,
	get_code(_),!,nl,
		
	 clr,
    write('||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'), nl,
	write('||                 MONKEY QUEEN - How To Play                   ||'), nl,
	write('||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'), nl,
	write('||                                                              ||'), nl,
	write('||      A baby monkey, like a monkey queen, captures            ||'), nl,
	write('||      exactly like a Chess queen. Babies also have            ||'), nl,
	write('||      a non-capture move. You win if you capture the          ||'), nl,
	write('||      enemy queen, or deprive your opponent of moves.         ||'), nl,
	write('||                                                              ||'), nl,	
	write('||                         4.Go Back                            ||'), nl,
	write('||                                                              ||'), nl,
	write('||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'), nl.
	

howToMenu :-
	pHowToMenu,
	read(I),
	(
		I =:= 4 ->mainMenu; 
		
		howToMenu
	).

	
pAboutMenu :-
	 clr,
	write('||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'), nl,
	write('||                   MONKEY QUEEN - About                       ||'), nl,
	write('||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'), nl,
	write('||                                                              ||'), nl,
	write('||                                                              ||'), nl,
	write('||                       Authors:                               ||'), nl,
	write('||                  - Ana Rita Torres                           ||'), nl,
	write('||                  - Rui Pedro Soares                          ||'), nl,
	write('||                                                              ||'), nl,
	write('||                       4.Go Back                              ||'), nl,
	write('||                                                              ||'), nl,
	write('||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'), nl.

aboutMenu :-
	pAboutMenu,
	read(I),
	(
		I =:= 4 ->mainMenu; 
		
		aboutMenu
	).
	
	


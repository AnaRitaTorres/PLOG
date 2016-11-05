%%%%%%%%%%%%%%%%%%%
%%% GAME CONFIG %%%
%%%%%%%%%%%%%%%%%%%

% Onde Jogador1 e Jogador2 podem ser humano ou computador, em qualquer combinacao possivel
presetGameMode(49,human,human).
presetGameMode(50,human,pc).
presetGameMode(51,pc,human).
presetGameMode(52,pc,pc).
	
setupGame :- 	abolish(currentGameMode/1),
				write('Welcome!'),
				nl,
				write('Choose Game Mode: 1 - Human/Human, 2 - Human/Pc, 3 - Pc/Human, 4 - Pc/Pc'),
				nl,
				get_code(Code),
				( 	Code == 49 -> assert(currentGameMode(Code));
					Code == 50 -> assert(currentGameMode(Code));
					Code == 51 -> assert(currentGameMode(Code));
					Code == 52 -> assert(currentGameMode(Code));
					write('Not an option. Aborting.'), abort
				).

runGame(State) :- 	(State == menu -> setupGame);
					(State == inGame -> playGame(State));
					(State == gameOver -> write('Game Over!'), abort).

playGame(State).

% joga(TipodeJogo, Dificuldade):- inicializaJogo(TipodeJogo), repeat, joga(Dificuldade), testaTermina, mostraPontuacao.
% assert(+ nº jogada, +Tabuleiro)
% avaliaPosicao(+ Tabuleiro, +Jogador,-Valor)
% geraMelhorTabuleiro(+Tabuleiro, +Jogador, -MelhorTabuleiro):- geraTabuleiro(Tabuleiro, PossiveisTabuleiros), avaliaTabuleiros(possiveisTabuleiros, MelhorTabuleiro).


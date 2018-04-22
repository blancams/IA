#include <stdio.h>
#include <stdlib.h>

#include "mancala.h"
#include "hextree.h"
#include "heuristic.h"
#include "globals.h"

// Depth 2
short testAgainstRegular (heuristic htest, short *heur_values) {
	struct result *winner;
	heuristic hreg = heuristicIARegular;

	winner = playMancala(0, htest, hreg, 2, 2, heur_values, NULL);
	if (winner->winner != 1) {
		free(winner);
		return ERR;
	}
	printf("Ha ganado el %d con un marcador de %d - %d.\n", winner->winner, winner->score1, winner->score2);
	free(winner);

	winner = playMancala(1, htest, hreg, 2, 2, heur_values, NULL);
	if (winner->winner != 1) {
		free(winner);
		return ERR;
	}
	printf("Ha ganado el %d con un marcador de %d - %d.\n", winner->winner, winner->score1, winner->score2);
	free(winner);

	winner = playMancala(0, hreg, htest, 2, 2, NULL, heur_values);
	if (winner->winner != 2) {
		free(winner);
		return ERR;
	}
	printf("Ha ganado el %d con un marcador de %d - %d.\n", winner->winner, winner->score1, winner->score2);
	free(winner);

	winner = playMancala(1, hreg, htest, 2, 2, NULL, heur_values);
	if (winner->winner != 2) {
		free(winner);
		return ERR;
	}
	printf("Ha ganado el %d con un marcador de %d - %d.\n", winner->winner, winner->score1, winner->score2);
	free(winner);

	return OK;
}

int main() {
	struct result *winner;
	heuristic h1 = heuristicIARegular;
	heuristic h2 = heuristicIABuena;

	printf("De momento un partidito de prueba entre el Regular y el Bueno.\n");

	winner = playMancala(0, h1, h2, 2, 3, NULL, NULL);
	if (winner == NULL) {
		printf("Algo paso wey.\n");
		return ERR;
	}

	printf("Ha ganado el %d con un marcador de %d - %d.\n", winner->winner, winner->score1, winner->score2);
	free(winner);

	return OK;
}

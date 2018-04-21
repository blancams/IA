#include <stdio.h>
#include <stdlib.h>

#include "mancala.h"
#include "hextree.h"
#include "heuristic.h"
#include "globals.h"

int main() {
	struct result *winner;
	heuristic h1 = heuristicIARegular;
	heuristic h2 = heuristicIABuena;

	printf("De momento un partidito de prueba entre el Regular y el Bueno.\n");

	winner = playMancala(0, h1, h2, 2, 3);
	if (winner == NULL) {
		printf("Algo paso wey.\n");
		return ERR;
	}

	printf("Ha ganado el %d con un marcador de %d - %d.\n", winner->winner, winner->score1, winner->score2);
	free(winner);
	
	return OK;
}

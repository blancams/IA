#include <stdio.h>
#include <stdlib.h>

#include "mancala.h"
#include "hextree.h"
#include "heuristic.h"
#include "globals.h"

int main() {
	short winner;
	heuristic h = heuristicIA;

	printf("De momento un partidito de prueba entre el Regular y el Bueno.\n");

	winner = playMancala(0, h, h, 2, 3);
	if (winner == ERR) {
		printf("Algo paso wey.\n");
		return ERR;
	}

	printf("Ha ganado el %d.\n");
	return OK;
}
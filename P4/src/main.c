#include <stdio.h>
#include <stdlib.h>

#include "mancala.h"
#include "hextree.h"
#include "heuristic.h"
#include "globals.h"
#include "whdb.h"

// All of the tests below are performed with depth 2. For other depths, other tests
// must be created.
short testAgainstRegular (heuristic htest, short *heur_values) {
	struct result *winner;
	heuristic hreg = heuristicIARegular;

	winner = playMancala(0, htest, hreg, 2, 2, heur_values, NULL);
	if (winner->winner != 1) {
		free(winner);
		return ERR;
	}
	free(winner);

	winner = playMancala(1, htest, hreg, 2, 2, heur_values, NULL);
	if (winner->winner != 1) {
		free(winner);
		return ERR;
	}
	free(winner);

	winner = playMancala(0, hreg, htest, 2, 2, NULL, heur_values);
	if (winner->winner != 2) {
		free(winner);
		return ERR;
	}
	free(winner);

	winner = playMancala(1, hreg, htest, 2, 2, NULL, heur_values);
	if (winner->winner != 2) {
		free(winner);
		return ERR;
	}
	free(winner);

	return OK;
}

short testAgainstGood (heuristic htest, short *heur_values) {
	struct result *winner;
	heuristic hreg = heuristicIABuena;

	winner = playMancala(0, htest, hreg, 2, 3, heur_values, NULL);
	if (winner->winner != 1) {
		free(winner);
		return ERR;
	}
	free(winner);

	winner = playMancala(1, htest, hreg, 2, 3, heur_values, NULL);
	if (winner->winner != 1) {
		free(winner);
		return ERR;
	}
	free(winner);

	winner = playMancala(0, hreg, htest, 3, 2, NULL, heur_values);
	if (winner->winner != 2) {
		free(winner);
		return ERR;
	}
	free(winner);

	winner = playMancala(1, hreg, htest, 3, 2, NULL, heur_values);
	if (winner->winner != 2) {
		free(winner);
		return ERR;
	}
	free(winner);

	return OK;
}

// Heuristics generated with heuristicWeight
short testWHAgainstWH (short player_turn, short *heur_values1, short *heur_values2) {
	struct result *winner;
	heuristic h = heuristicWeight;
	short ret;

	winner = playMancala(0, h, h, 2, 2, heur_values1, heur_values2);
	if (winner == NULL) {
		return ERR;
	}

	ret = winner->winner;
	free(winner);
	return ret;
}

// WHDB = weighted heuristics (generated through a 14-size vector of weights and the
// function heuristicWeight) data base. Returns win rate.
float testAgainstWHDB (heuristic htest, short *heur_values, struct whdb *whdb) {
	short i, wins, ret;
	float win_rate;

	if (whdb->cur_heur == 0) {
		return 0.0;
	}

	for (i=0, wins=0; i<whdb->cur_heur; i++) {
		ret = testWHAgainstWH(0, heur_values, whdb->weights[i]);
		if (ret == 1) {
			wins++;
		}

		ret = testWHAgainstWH(1, heur_values, whdb->weights[i]);
		if (ret == 1) {
			wins++;
		}

		ret = testWHAgainstWH(0, whdb->weights[i], heur_values);
		if (ret == 2) {
			wins++;
		}

		ret = testWHAgainstWH(1, whdb->weights[i], heur_values);
		if (ret == 2) {
			wins++;
		}
	}

	win_rate = (wins * 100.0) / (whdb->cur_heur * 4.0);

	return win_rate;
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

	printf("Ha ganado el %hi con un marcador de %hi - %hi.\n", winner->winner, winner->score1, winner->score2);
	free(winner);

	return OK;
}

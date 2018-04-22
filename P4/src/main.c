#include <stdio.h>
#include <stdlib.h>

#include "mancala.h"
#include "hextree.h"
#include "heuristic.h"
#include "globals.h"
#include "whdb.h"
#include "main.h"

// All of the tests below are performed with depth 2. For other depths, other tests
// must be created.
short testAgainstRegular (heuristic htest, short *heur_values) {
	struct result *winner;
	heuristic hreg = heuristicIARegular;

	winner = playMancala(0, htest, hreg, 2, 2, heur_values, NULL);
	if (winner->winner != 1) {
		free(winner);
		return F;
	}
	free(winner);

	winner = playMancala(1, htest, hreg, 2, 2, heur_values, NULL);
	if (winner->winner != 1) {
		free(winner);
		return F;
	}
	free(winner);

	winner = playMancala(0, hreg, htest, 2, 2, NULL, heur_values);
	if (winner->winner != 2) {
		free(winner);
		return F;
	}
	free(winner);

	winner = playMancala(1, hreg, htest, 2, 2, NULL, heur_values);
	if (winner->winner != 2) {
		free(winner);
		return F;
	}
	free(winner);

	return T;
}

short testAgainstGood (heuristic htest, short *heur_values) {
	struct result *winner;
	heuristic hreg = heuristicIABuena;

	winner = playMancala(0, htest, hreg, 2, 3, heur_values, NULL);
	if (winner->winner != 1) {
		free(winner);
		return F;
	}
	free(winner);

	winner = playMancala(1, htest, hreg, 2, 3, heur_values, NULL);
	if (winner->winner != 1) {
		free(winner);
		return F;
	}
	free(winner);

	winner = playMancala(0, hreg, htest, 3, 2, NULL, heur_values);
	if (winner->winner != 2) {
		free(winner);
		return F;
	}
	free(winner);

	winner = playMancala(1, hreg, htest, 3, 2, NULL, heur_values);
	if (winner->winner != 2) {
		free(winner);
		return F;
	}
	free(winner);

	return T;
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
float testWHAgainstWHDB (short *heur_values, struct whdb *whdb) {
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

// Assumes that a WHDB exists in 'filename' and tests each of the heuristics with
// the rest. It generates (n+4)^2 games where n is the number of heuristics, so be
// careful with the size of the database if you don't want to murder your computer.
// Returns the weighted heuristic as a vector of size 14 (pointer).
short* testSimpleWHDB (char *filename) {
	int i, cur_heur;
	short *heur_max, *heur_aux, heur_ret[14];
	float win_rate = -1.0, win_rate_aux;
	struct whdb *whdb;
	heuristic h = heuristicWeight;

	whdb = loadWHDB(filename);
	if (whdb == NULL) {
		return NULL;
	}

	cur_heur = getNumWHDB(whdb);
	for (i=0; i<cur_heur; i++) {
		//if (i%500==0) printf("%d: little control.\n", i);

		heur_aux = getWHDB(whdb, i);

		if (!testAgainstRegular(h, heur_aux)) {
			continue;
		}

		win_rate_aux = testWHAgainstWHDB(heur_aux, whdb);
		if (win_rate_aux > win_rate) {
			win_rate = win_rate_aux;
			heur_max = heur_aux;
		}
	}

	for (i=0; i<14; i++) {
		heur_ret[i] = heur_max[i];
	}

	freeWHDB(whdb);

	printf("Win rate of champion: %f\n", win_rate);
	return heur_ret;
}

int main() {
	struct result *winner;
	heuristic h1 = heuristicIARegular;
	heuristic h2 = heuristicIABuena;
	short i, *hwin;

	printf("De momento un partidito de prueba entre el Regular y el Bueno.\n");

	winner = playMancala(0, h1, h2, 2, 3, NULL, NULL);
	if (winner == NULL) {
		printf("Algo paso wey.\n");
		return ERR;
	}
	printf("Ha ganado el %hi con un marcador de %hi - %hi.\n", winner->winner, winner->score1, winner->score2);
	free(winner);

	createSimpleWHDB("dbs/whdb_simple");

	// hwin = testSimpleWHDB("dbs/whdb_simple");
	// printf("Champion: [ ");
	// for (i=0; i<14; i++) {
	// 	printf("%d ", hwin[i]);
	// }
	// printf("]\n");

	return OK;
}

// Recursive function to generate every possible 14-size vector with 1 and -1
short genSimpleWH (struct whdb *whdb, short *values, short index) {
	short ret;

	values[index] = 1;

	if (index == 13) {
		addWHDB(whdb, values);
	} else {
		ret = genSimpleWH(whdb, values, index+1);
		if (ret == ERR) {
			return ERR;
		}
	}

	values[index] = -1;

	if (index == 13) {
		addWHDB(whdb, values);
	} else {
		ret = genSimpleWH(whdb, values, index+1);
		if (ret == ERR) {
			return ERR;
		}
	}

	return OK;
}

// Creates a file in which to store the data base generated by the previous function
short createSimpleWHDB(char *filename) {
	struct whdb *whdb;
	short ret, values[14];

	whdb = loadWHDB(filename);
	if (whdb == NULL) {
		return ERR;
	}

	ret = genSimpleWH(whdb, values, 0);
	if (ret == ERR) {
		return ERR;
	}

	ret = saveWHDB(whdb, filename);
	if (ret == ERR) {
		return ERR;
	}

	return OK;
}

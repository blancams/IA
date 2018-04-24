#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "mancala.h"
#include "hextree.h"
#include "heuristic.h"
#include "globals.h"
#include "whdb.h"
#include "main.h"

int main() {
	struct result *winner;
	heuristic h1 = heuristicIARegular;
	heuristic h2 = heuristicIABuena;
	short ret;

	srand(time(NULL));

	printf("De momento un partidito de prueba entre el Regular y el Bueno.\n");

	winner = playMancala(0, h1, h2, 2, 3, NULL, NULL);
	if (winner == NULL) {
		printf("Algo paso wey.\n");
		return ERR;
	}
	printf("Ha ganado el %hi con un marcador de %hi - %hi.\n", winner->winner, winner->score1, winner->score2);
	free(winner);

	// ret = buildRandomWHDB("dbs/whdb_random", 400, -10.0, 10.0);
	// if (ret == ERR) {
	// 	printf("Algo mas paso wey.\n");
	// 	return ERR;
	// }

	ret = updateWHDB("dbs/whdb_random", 1000, -10.0, 10.0);
	if (ret == ERR) {
		printf("Algo mas mas paso wey.\n");
		return ERR;
	}

	return OK;
}

// All of the tests below are performed with depth 2. For other depths, other tests
// must be created.
short testAgainstRegular (heuristic htest, float *heur_values) {
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

short testAgainstGood (heuristic htest, float *heur_values) {
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
short testWHAgainstWH (short player_turn, float *heur_values1, float *heur_values2) {
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
float testWHAgainstWHDB (float *heur_values, struct whdb *whdb) {
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
// Prints the weighted heuristic as a vector of size 14 (pointer).
short testSimpleWHDB (char *filename) {
	int i, cur_heur;
	float *heur_max, *heur_aux, heur_ret[14];
	float win_rate = -1.0, win_rate_aux;
	struct whdb *whdb;
	heuristic h = heuristicWeight;

	whdb = loadWHDB(filename);
	if (whdb == NULL) {
		return ERR;
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

	printf("Champion: [ ");
	for (i=0; i<14; i++) {
		printf("%f ", heur_ret[i]);
	}
	printf("]\nWin rate of champion: %f\n", win_rate);

	return OK;
}

short buildRandomWHDB (char *filename, int num_heur, float min_h, float max_h) {
	struct whdb *whdb;
	float *heur_values;
	int i;

	whdb = createWHDB(num_heur);
	if (whdb == NULL) {
		return ERR;
	}

	for (i=0; i<num_heur; i++) {
		heur_values = generateRandomWH(min_h, max_h);
		if (heur_values == NULL) {
			freeWHDB(whdb);
			return ERR;
		}

		if (addWHDB(whdb, heur_values) == ERR) {
			free(heur_values);
			freeWHDB(whdb);
			return ERR;
		}

		free(heur_values);
	}

	return saveWHDB(whdb, filename);
}

// Assumes we have enabled testing on whdb
short iterationUpdateWHDB (struct whdb *whdb, float min_h, float max_h) {
	float *test_weights, win_rate;
	heuristic h = heuristicWeight;

	if (whdb == NULL) {
		return ERR;
	}

	test_weights = generateRandomWH(min_h, max_h);
	if (test_weights == NULL) {
		return ERR;
	}

	if (testAgainstRegular(h, test_weights) && testAgainstGood(h, test_weights)) {
		win_rate = testWHAgainstWHDB (test_weights, whdb);
		if (win_rate > getWinRateWHDB(whdb)) {
			printf("Yujus! Con %f en posicion %d\n", win_rate, whdb->win_index);
			updateRandomWHDB(whdb, test_weights);
		}
	}

	free(test_weights);
	return OK;

}

// Assumes there is a WHDB in 'filename', if there isn't it must be created with
// the function 'buildRandomWHDB'
short updateWHDB (char *filename, int iterations, float min_h, float max_h) {
	struct whdb *whdb;
	int i;
	short ret;

	whdb = loadWHDB(filename);
	if (whdb == NULL) {
		return ERR;
	}

	if (!isEnabledTestingWHDB(whdb)) {
		enableTestingWHDB(whdb, INIT_WR);
	}

	for (i=0; i<iterations; i++) {
		ret = iterationUpdateWHDB(whdb, min_h, max_h);
		if (ret == ERR) {
			break;
		}
	}

	printf("El win rate al final es: %f\n", getWinRateWHDB(whdb));

	ret = saveWHDB(whdb, filename);
	if (ret == ERR) {
		return ERR;
	}

	return OK;
}



/**********************************/
/*********** DEPRECATED ***********/
/**********************************/

/* Already tested. Results:
*    - The best heuristic had 49.95% win ratio. That means there was not a clear
*      winner, and therefore assigning similar weights to the seeds does not work.
*      That suggests we will have to assign different weights with high variance
*      in order to find the best heuristics.
*    - That said, it took around 32 hours to perform the test, which executed
*      between 10^8 and 10^9 games (3-30 million games an hour).

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
*/

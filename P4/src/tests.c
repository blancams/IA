#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "globals.h"
#include "tests.h"
#include "heuristic.h"
#include "mancala.h"
#include "whdb.h"
#include "genetic.h"

// All of the tests below are performed with depth 2. For other depths, other tests
// must be created.
short testAgainstRegular (heuristic htest, float *heur_values) {
	struct result *winner;
	heuristic hreg = heuristicIARegular;

	winner = playMancala(0, htest, hreg, 2, 2, heur_values, NULL, 0);
	if (winner->winner != 1) {
		free(winner);
		return F;
	}
	free(winner);

	winner = playMancala(1, htest, hreg, 2, 2, heur_values, NULL, 0);
	if (winner->winner != 1) {
		free(winner);
		return F;
	}
	free(winner);

	return T;
}

short testAgainstGood (heuristic htest, float *heur_values) {
	struct result *winner;
	heuristic hreg = heuristicIABuena;

	winner = playMancala(0, htest, hreg, 2, 3, heur_values, NULL, 0);
	if (winner->winner != 1) {
		free(winner);
		return F;
	}
	free(winner);

	winner = playMancala(1, htest, hreg, 2, 3, heur_values, NULL, 0);
	if (winner->winner != 1) {
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

	winner = playMancala(player_turn, h, h, 2, 2, heur_values1, heur_values2, 0);
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
	}

	win_rate = (wins * 100.0) / (whdb->cur_heur * 2.0);

	return win_rate;
}

// Tests each of the heuristics in the database 'whdb' with the rest. It generates
// 2*n^2 games where n is the number of heuristics, so be careful with the size of
// the database if you don't want to murder your computer.
// It also assumes that every heuristic fulfills the requirements, for example it
// doesn't check if those heuristics win against the Regular or the Good heuristics.
// Prints the weighted heuristic as a vector of size 14 (pointer).
short testTournamentWHDB (struct whdb *whdb, float *weights, float *win_rate) {
	int i, cur_heur;
	float *heur_max, *heur_aux;
	float win_rate_max = -1.0, win_rate_aux;

	if (whdb == NULL) {
		return ERR;
	}

	cur_heur = getNumWHDB(whdb);
	for (i=0; i<cur_heur; i++) {
		//if (i%50==49) printf("%d: little control.\n", i);

		heur_aux = getWHDB(whdb, i);

		win_rate_aux = testWHAgainstWHDB(heur_aux, whdb);
		if (win_rate_aux > win_rate_max) {
			win_rate_max = win_rate_aux;
			heur_max = heur_aux;
		}
	}

	*win_rate = win_rate_max;
	for (i=0; i<14; i++) {
		weights[i] = heur_max[i];
	}

	return OK;
}

// Assumes we have enabled testing on whdb
short iterationUpdateWHDB (struct whdb *whdb, generateWH gh, float *init_weights, float min_h, float max_h) {
	float *test_weights, win_rate;
	heuristic h = heuristicWeight;

	if (whdb == NULL) {
		return ERR;
	}

	test_weights = gh(init_weights, min_h, max_h);
	if (test_weights == NULL) {
		return ERR;
	}

	if (testAgainstRegular(h, test_weights) && testAgainstGood(h, test_weights)) {
		win_rate = testWHAgainstWHDB (test_weights, whdb);
		if (win_rate > getWinRateWHDB(whdb)) {
			printf("Success! Heuristic with win rate %f saved with index %d.\n", win_rate, whdb->win_index);
			updateAfterTestWHDB(whdb, test_weights);
		}
	}

	free(test_weights);
	return OK;

}

// Assumes there is a WHDB in 'filename', if there isn't it must be created with
// the function 'buildRandomWHDB'
short updateWHDB (char *filename, char *generateWHChoice, int iterations, float min_h, float max_h) {
	struct whdb *whdb;
	int i, pos;
	short ret;
	generateWH gh;
	float *init_weights;

	if (generateWHChoice == NULL || filename == NULL) {
		return ERR;
	}

	whdb = loadWHDB(filename);
	if (whdb == NULL) {
		return ERR;
	}

	if (!isEnabledTestingWHDB(whdb)) {
		enableTestingWHDB(whdb, INIT_WR);
	}

	if (!strcmp(generateWHChoice, "random")) {
		gh = generateRandomWH;
		init_weights = NULL;

		for (i=0; i<iterations; i++) {
			ret = iterationUpdateWHDB(whdb, gh, init_weights, min_h, max_h);
			if (ret == ERR) {
				break;
			}
		}
	} else if (!strcmp(generateWHChoice, "simple_mutation")) {
		gh = generateSimpleMutationWH;

		for (i=0; i<iterations; i++) {
			pos = rand() % (int)getNumWHDB(whdb);
			init_weights = getWHDB(whdb, pos);
			ret = iterationUpdateWHDB(whdb, gh, init_weights, min_h, max_h);
			if (ret == ERR) {
				break;
			}
		}
	} else {
		return ERR;
	}

	ret = saveWHDB(whdb, filename);
	if (ret == ERR) {
		return ERR;
	}

	return OK;
}

// This function doesn't check if heuristics win against regular and good.
short geneticUpdateWHDB (char *fn_gen, char *fn_test, short max_gen, float wrl, float cr, float mp, float mr, float min_h, float max_h) {
    short generation = 0;
    int i, num_gen, max_ind;
    float *win_rates, max_win_rate = 0.0, mx = mr/2, mm = mr/-2;
    struct whdb *whdb_gen, *whdb_test;

    if (wrl < 0.0 || wrl > 100.0 || cr < 0.0 || cr > 1.0 || mp < 0.0 || mp > 1.0) {
        return ERR;
    }

    whdb_gen = loadWHDB(fn_gen);
    if (whdb_gen == NULL) {
        return ERR;
    }

    whdb_test = loadWHDB(fn_test);
    if (whdb_test == NULL) {
        freeWHDB(whdb_gen);
        return ERR;
    }

    num_gen = getNumWHDB(whdb_gen);

    win_rates = (float*) malloc(num_gen * sizeof(float));
    if (win_rates == NULL) {
        freeWHDB(whdb_gen);
        freeWHDB(whdb_test);
        return ERR;
    }

    for (i=0; i<num_gen; i++) {
        win_rates[i] = testWHAgainstWHDB(getWHDB(whdb_gen, i), whdb_test);
        if (win_rates[i] > max_win_rate) {
            max_win_rate = win_rates[i];
            max_ind = i;
        }
    }

    while (generation < max_gen && max_win_rate < wrl) {
        generation++;
        printf("Creating generation %hi...\n", generation);

        whdb_gen = newGeneration(whdb_gen, win_rates, cr, mp, mm, mx, min_h, max_h);
        for (i=0; i<num_gen; i++) {
            win_rates[i] = testWHAgainstWHDB(getWHDB(whdb_gen, i), whdb_test);
            if (win_rates[i] > max_win_rate) {
                max_win_rate = win_rates[i];
                max_ind = i;
            }
        }
    }

    printf("After applying the genetic algorithm, the best heuristic lives in position %d.\n", max_ind);
    freeWHDB(whdb_test);
    free(win_rates);

    return saveWHDB(whdb_gen, fn_gen);
}

#ifndef _TESTS_H
#define _TESTS_H

#include "heuristic.h"

struct whdb;

/* Tests for Regular and Good IA strategies. */
short testAgainstRegular (heuristic htest, float *heur_values);
short testAgainstGood (heuristic htest, float *heur_values);

/* Test for strategies based on weights */
short testWHAgainstWH (short player_turn, float *heur_values1, float *heur_values2);

/* Tests a strategy based on weights against a data base of strategies based on weights. */
float testWHAgainstWHDB (float *heur_values, struct whdb *whdb);

/* Tests every heuristic in a WHDB against each other and prints the winner. */
short testTournamentWHDB (struct whdb *whdb, float *weights, float *win_rate);

/* Functions to update a weighted heuristic data base by generating new random heuristics */
short iterationUpdateWHDB (struct whdb *whdb, generateWH gh, float *init_weights, float min_h, float max_h);
short updateWHDB (char *filename, char *generateWHChoice, int iterations, float min_h, float max_h);

short geneticUpdateWHDB (char *fn_gen, char *fn_test, short max_gen, float wrl, float cr, float mp, float mr, float min_h, float max_h);

#endif

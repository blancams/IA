#ifndef _MAIN_H
#define _MAIN_H

#include "heuristic.h"

struct whdb;

/* Already tested */
// short genSimpleWH (struct whdb *whdb, short *values, short index);
// short createSimpleWHDB(char *filename);

/* Tests for Regular and Good IA strategies. */
short testAgainstRegular (heuristic htest, float *heur_values);
short testAgainstGood (heuristic htest, float *heur_values);

/* Test for strategies based on weights */
short testWHAgainstWH (short player_turn, float *heur_values1, float *heur_values2);

/* Tests a strategy based on weights against a data base of strategies based on weights. */
float testWHAgainstWHDB (float *heur_values, struct whdb *whdb);

/* Tests every heuristic in a WHDB against each other and prints the winner. */
short testSimpleWHDB (char *filename);

short buildRandomWHDB (char *filename, int num_heur, float min_h, float max_h);
short iterationUpdateWHDB (struct whdb *whdb, float min_h, float max_h);
short updateWHDB (char *filename, int iterations, float min_h, float max_h);

#endif

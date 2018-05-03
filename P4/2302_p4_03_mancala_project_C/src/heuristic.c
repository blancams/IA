#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#include "heuristic.h"
#include "mancala.h"

/**********************************/
/*********** HEURISTICS ***********/
/**********************************/

// Only with even depth
float heuristicIARegular(struct mancala_state ms, float *empty) {
	short sum1, sum2, cp, op;

	// With even depth, cp is the player who has the turn
	cp = currentPlayer(ms);
	op = oppositePlayer(ms);

	sum1 = getPts(ms, cp);
	sum2 = getPts(ms, op);

	return sum1-sum2;
}

// Only with odd depth
float heuristicIABuena(struct mancala_state ms, float *empty) {
	short sum1, sum2, cp, op;

	// With odd depth, op is the player who has the turn
	cp = currentPlayer(ms);
	op = oppositePlayer(ms);

	sum1 = getPts(ms, cp);
	sum2 = getPts(ms, op);

	return sum2-sum1;
}

// Only with even depth. Assumes 'weights' is of size 14, the first 7 weights
// are used for the current player and the other 7 for the opposite player.
// Also takes into account the existence of a winner.
float heuristicWeight(struct mancala_state ms, float *weights) {
	short i, cp, op, win;
	float sum;

	cp = currentPlayer(ms);
	op = oppositePlayer(ms);

	if (gameHasEnded(ms)) {
		win = gameWinner(ms);
		if (win == cp + 1) {
			return 361.0;
		} else if (win == op + 1){
			return -361.0;
		} else {
			return -361.0;
		}
	}

	for (i=0, sum=0.0; i<7; i++) {
		sum += weights[i] * getSeeds(ms, cp, i);
		sum += weights[i+7] * getSeeds(ms, op, i);
	}

	return sum;
}


/**********************************/
/*********** AUXILIARYF ***********/
/**********************************/

// Caller must free memory. Assumes empty == NULL.
float *generateRandomWH(float *empty, float min, float max) {
	float size = max-min;
	float *vector;
	short i;

	vector = (float*) malloc(14*sizeof(float));
	if (vector == NULL) {
		return NULL;
	}

	for (i=0; i<14; i++) {
		vector[i] = ((float)rand()/(float)(RAND_MAX))*size + min;
	}

	return vector;
}

// Caller must free memory.
float* generateSimpleMutationWH(float *init_weights, float min, float max) {
	float *vector, incr, size = MAX_INCR - MIN_INCR;
	short i;

	vector = (float*) malloc(14*sizeof(float));
	if (vector == NULL) {
		return NULL;
	}

	for (i=0; i<14; i++) {
		incr = ((float)rand()/(float)(RAND_MAX))*size + MIN_INCR + init_weights[i];
		if (incr <= max && incr >= min) {
			vector[i] = incr;
		} else {
			vector[i] = init_weights[i];
		}
	}

	return vector;
}

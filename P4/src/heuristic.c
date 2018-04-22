#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#include "heuristic.h"
#include "mancala.h"

// Only with even depth
short heuristicIARegular(struct mancala_state ms, short *empty) {
	short sum1, sum2, cp, op;

	// With even depth, cp is the player who has the turn
	cp = currentPlayer(ms);
	op = oppositePlayer(ms);

	sum1 = getPts(ms, cp);
	sum2 = getPts(ms, op);

	return sum1-sum2;
}

// Only with odd depth
short heuristicIABuena(struct mancala_state ms, short *empty) {
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
short heuristicWeight(struct mancala_state ms, short *weights) {
	short i, sum, cp, op, win;

	cp = currentPlayer(ms);
	op = oppositePlayer(ms);

	if (gameHasEnded(ms)) {
		win = gameWinner(ms);
		if (win == cp + 1) {
			return 50;
		} else if (win == op + 1){
			return -50;
		} else {
			return 0;
		}
	}

	for (i=0, sum=0; i<7; i++) {
		sum += weights[i] * getSeeds(ms, cp, i);
		sum += weights[i+7] * getSeeds(ms, op, i);
	}

	return sum;
}

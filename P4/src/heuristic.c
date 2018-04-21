#include <stdio.h>
#include <stdlib.h>

#include "heuristic.h"
#include "mancala.h"

// Only with even depth
short heuristicIARegular(struct mancala_state ms) {
	short sum1, sum2, cp, op;

	// With even depth, cp is the player who has the turn
	cp = currentPlayer(ms);
	op = oppositePlayer(ms);

	sum1 = getPts(ms, cp);
	sum2 = getPts(ms, op);

	return sum1-sum2;
}

// Only with odd depth
short heuristicIABuena(struct mancala_state ms) {
	short sum1, sum2, cp, op;

	// With odd depth, op is the player who has the turn
	cp = currentPlayer(ms);
	op = oppositePlayer(ms);

	sum1 = getPts(ms, cp);
	sum2 = getPts(ms, op);

	return sum2-sum1;
}

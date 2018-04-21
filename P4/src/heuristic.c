#include <stdio.h>
#include <stdlib.h>

#include "heuristic.h"
#include "mancala.h"

short heuristicIA(struct mancala_state ms) {
	short i, sum1, sum2, cp, op;

	cp = currentPlayer(ms);
	op = oppositePlayer(ms);

	for (i=0, sum1=0, sum2=0; i<7; i++) {
		sum1 += ms.hole[cp*7+i];
		sum2 += ms.hole[op*7+i];
	}

	return sum1-sum2;
}
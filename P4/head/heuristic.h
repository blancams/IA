#ifndef _HEURISTIC_H
#define _HEURISTIC_H

struct mancala_state;

typedef short (*heuristic)(struct mancala_state);

short heuristicIA(struct mancala_state ms);

#endif
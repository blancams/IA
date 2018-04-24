#ifndef _HEURISTIC_H
#define _HEURISTIC_H

struct mancala_state;

typedef float (*heuristic)(struct mancala_state, float*);

float heuristicIARegular(struct mancala_state ms, float *empty);
float heuristicIABuena(struct mancala_state ms, float *empty);
float heuristicWeight(struct mancala_state ms, float *weights);

float *generateRandomWH(float min, float max);

#endif

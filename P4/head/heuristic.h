#ifndef _HEURISTIC_H
#define _HEURISTIC_H

#define MAX_INCR 0.1
#define MIN_INCR -0.1

struct mancala_state;

typedef float (*heuristic)(struct mancala_state, float*);
typedef float* (*generateWH)(float*, float, float);

float heuristicIARegular(struct mancala_state ms, float *empty);
float heuristicIABuena(struct mancala_state ms, float *empty);
float heuristicWeight(struct mancala_state ms, float *weights);

float* generateRandomWH(float *empty, float min, float max);
float* generateSimilarWH(float *init_weights, float min, float max);

#endif

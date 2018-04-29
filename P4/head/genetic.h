#ifndef _GENETIC_H
#define _GENETIC_H

struct whdb;

void crossHeuristics(float* mask, short mask_flag, float* heur_values1, float* heur_values2, float* heur_ret);
void mutateHeuristic(float *weights, float min_incr, float max_incr, float min_h, float max_h);
struct whdb* newGeneration(struct whdb *old_whdb, float *win_rates, float cross_rate, float mut_rate, float mut_min, float mut_max, float min_h, float max_h);

#endif

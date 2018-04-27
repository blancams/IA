#ifndef _GENETIC_H
#define _GENETIC_H

struct whdb;

void crossHeuristics(short insert_flag, float* heur_values1, float* heur_values2, float* heur_ret);
short crossWHDB(struct whdb *whdb_from, struct whdb *whdb_to, int top_index, int bottom_index, int num_changed);
void mutateHeuristic(float *weights, float min_incr, float max_incr, float min_h, float max_h);
void applyMutation(struct whdb *whdb, float mutation_range, float mutation_prob, float min_h, float max_h);
struct whdb* createNewGeneration(struct whdb *whdb, float cross_rate, float mut_range, float mut_prob, float min_h, float max_h);

#endif

#include <stdio.h>
#include <stdlib.h>

#include "globals.h"
#include "genetic.h"
#include "whdb.h"

void crossHeuristics(short insert_flag, float* heur_values1, float* heur_values2, float* heur_ret) {
    short i;

    if (insert_flag == 0) {
        for (i=0; i<7; i++) {
            heur_ret[i] = heur_values1[i];
            heur_ret[i+7] = heur_values2[i+7];
        }
    } else if (insert_flag == 1) {
        for (i=0; i<7; i++) {
            heur_ret[i] = heur_values2[i];
            heur_ret[i+7] = heur_values1[i+7];
        }
    }

    return;
}

// Assumes num_changed is an even integer and that all of the arguments are what
// they are supposed to be (whdb != NULL, indexes inside range...)
short crossWHDB(struct whdb *whdb_from, struct whdb *whdb_to, int top_index, int bottom_index, int num_changed) {
    int i, num_heur;
    float *heur_values1, *heur_values2, *heur_ret1, *heur_ret2;

    if (num_changed == 0) {
        return OK;
    }

    heur_ret1 = (float*) malloc(14*sizeof(float));
    if (heur_ret1 == NULL) {
        return ERR;
    }

    heur_ret2 = (float*) malloc(14*sizeof(float));
    if (heur_ret2 == NULL) {
        free(heur_ret1);
        return ERR;
    }

    num_heur = getNumWHDB(whdb_from);
    for (i=0; i<num_changed; i += 2) {
        heur_values1 = getWHDB(whdb_from, top_index);
        heur_values2 = getWHDB(whdb_from, bottom_index);

        crossHeuristics(0, heur_values1, heur_values2, heur_ret1);
        crossHeuristics(1, heur_values1, heur_values2, heur_ret2);

        addWHDB(whdb_to, heur_ret1);
        addWHDB(whdb_to, heur_ret2);

        if (top_index > bottom_index) {
            top_index++;
            if (top_index == num_heur) {
                top_index = 0;
            }
            bottom_index--;
            if (bottom_index == -1) {
                bottom_index = num_heur - 1;
            }
        } else {
            top_index++;
            bottom_index--;
        }
    }

    free(heur_ret1);
    free(heur_ret2);

    return OK;
}

// Assumes weights != NULL
void mutateHeuristic(float *weights, float min_incr, float max_incr, float min_h, float max_h) {
    short i;
    float incr;

    for (i=0; i<14; i++) {
        incr = ((float)rand()/(float)(RAND_MAX))*(max_incr - min_incr) + min_incr + weights[i];
		if (incr <= max_h && incr >= min_h) {
			weights[i] = incr;
		}
    }

    return;
}

// Assumes whdb != NULL, that it exists and all that stuff
void applyMutation(struct whdb *whdb, float mutation_range, float mutation_prob, float min_h, float max_h) {
    int i, num_heur;
    float max_incr = mutation_range/2, min_incr = mutation_range/-2, rnd;

    num_heur = getNumWHDB(whdb);
    for (i=0; i<num_heur; i++) {
        rnd = ((float) rand()/(float)(RAND_MAX));
        if (rnd < mutation_prob) {
            mutateHeuristic(getWHDB(whdb, i), min_incr, max_incr, min_h, max_h);
        }
    }
}

struct whdb* createNewGeneration(struct whdb *whdb1, float cross_rate, float mut_range, float mut_prob, float min_h, float max_h) {
    struct whdb *whdb2;
    int num_heur, bottom_index, top_index, num_changed, num_unchanged, i;

    if (whdb1 == NULL || cross_rate < 0.0 || cross_rate > 1.0 || mut_range < 0.0 || mut_range > 0.5 || mut_prob < 0.0 || mut_prob > 1.0) {
        return NULL;
    }

    num_heur = getNumWHDB(whdb1);
    whdb2 = createWHDB(num_heur);
    if (whdb2 == NULL) {
        freeWHDB(whdb1);
        return NULL;
    }

    num_changed = (int)(num_heur * cross_rate + 0.5);
    if (num_changed % 2) {
        num_changed--;
    }
    num_unchanged = num_heur - num_changed;

    bottom_index = rand() % num_heur;
    top_index = bottom_index;

    for (i=0; i<num_unchanged; i++) {
        addWHDB(whdb2, getWHDB(whdb1, top_index));
        if (++top_index == num_heur) {
            top_index = 0;
        }
    }

    if (crossWHDB(whdb1, whdb2, top_index, bottom_index - 1, num_changed) == ERR) {
        freeWHDB(whdb1);
        freeWHDB(whdb2);
        return NULL;
    }

    applyMutation(whdb2, mut_range, mut_prob, min_h, max_h);

    freeWHDB(whdb1);
    return whdb2;
}

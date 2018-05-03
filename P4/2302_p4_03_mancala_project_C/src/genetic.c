#include <stdio.h>
#include <stdlib.h>

#include "globals.h"
#include "genetic.h"
#include "whdb.h"

// Assumes stuff is not NULL
void crossHeuristics(float* mask, short mask_flag, float* heur_values1, float* heur_values2, float* heur_ret) {
    short i;

    for (i=0; i<14; i++) {
        if (mask[i] == mask_flag) {
            heur_ret[i] = heur_values1[i];
        } else {
            heur_ret[i] = heur_values2[i];
        }
    }

    return;
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

struct whdb* newGeneration(struct whdb *old_whdb, float *win_rates, float cross_rate, float mut_rate, float mut_min, float mut_max, float min_h, float max_h) {
    struct whdb *new_whdb;
    short j;
    int i, cross_flag, num_heur, select_count, max_selected;
    float select_prob, rnd, *mask, *heur_child1, *heur_child2;

    if (old_whdb == NULL || win_rates == NULL) {
        return NULL;
    }

    mask = (float*) malloc(14 * sizeof(float));
    if (mask == NULL) {
        return NULL;
    }

    heur_child1 = (float*) malloc(14 * sizeof(float));
    if (heur_child1 == NULL) {
        free(mask);
        return NULL;
    }

    heur_child2 = (float*) malloc(14 * sizeof(float));
    if (heur_child2 == NULL) {
        free(mask);
        free(heur_child1);
        return NULL;
    }

    num_heur = getNumWHDB(old_whdb);

    new_whdb = createWHDB(num_heur);
    if (new_whdb == NULL) {
        free(mask);
        free(heur_child1);
        free(heur_child2);
        return NULL;
    }

    // This is a suboptimal way of implementing crossing, because it doesn't make
    // sure that (1-cr)*p heuristics are selected, it crosses consecutive heuristics
    // and has to do an ugly check in the end. The correct way would iterate until
    // selecting (1-cr)*p heuristics, and then cross random pairs. At least this way
    // doesn't break the program and can be implemented at 02:00AM.
    max_selected = (int)((1-cross_rate)*num_heur+0.5);
    for (i=0, cross_flag=-1, select_count=0; i<num_heur; i++) {
        select_prob = win_rates[i]*win_rates[i] / 100.0;
        rnd = ((float)rand()/(float)(RAND_MAX));
        // Alternative to always select good heuristics:
        // if ((select_prob > 0.70) || (rnd < select_prob && select_count < max_selected)) {
        if (rnd < select_prob && select_count < max_selected) {
            addWHDB(new_whdb, getWHDB(old_whdb, i));
            select_count++;
        } else {
            if (cross_flag != -1) {
                for (j=0; j<14; j++) {
                    mask[j] = rand() % 2;
                }
                crossHeuristics(mask, 0, getWHDB(old_whdb, cross_flag), getWHDB(old_whdb, i), heur_child1);
                crossHeuristics(mask, 1, getWHDB(old_whdb, cross_flag), getWHDB(old_whdb, i), heur_child2);
                addWHDB(new_whdb, heur_child1);
                addWHDB(new_whdb, heur_child2);
                cross_flag = -1;
            } else {
                cross_flag = i;
            }
        }
    }
    if (cross_flag != -1) {
        addWHDB(new_whdb, getWHDB(old_whdb, cross_flag));
    }

    // Mutation taking action.
    for (i=0; i<num_heur; i++) {
        rnd = ((float)rand()/(float)(RAND_MAX));
        // Alternative to avoid mutating good heuristics:
        // if (win_rates[i] < 75.0 && rnd < mut_rate) {
        if (rnd < mut_rate) {
            mutateHeuristic(getWHDB(new_whdb, i), mut_min, mut_max, min_h, max_h);
        }
    }

    free(mask);
    free(heur_child1);
    free(heur_child2);
    freeWHDB(old_whdb);

    return new_whdb;
}

#include <stdio.h>
#include <stdlib.h>

#include "globals.h"
#include "whdb.h"
#include "heuristic.h"
#include "tests.h"

/* WHDB management */

struct whdb* createWHDB(int max_heur) {
    struct whdb *whdb;
    int i, j;

    if (max_heur < 5 || max_heur > MAX_WHDB) {
        return NULL;
    }

    whdb = (struct whdb*) malloc(sizeof(struct whdb));
    if (whdb == NULL) {
        return NULL;
    }

    whdb->max_heur = max_heur;
    whdb->cur_heur = 0;
    whdb->win_index = -1;
    whdb->min_win_rate = 0.0;

    whdb->weights = (float **) malloc(max_heur*sizeof(float*));
    if (whdb->weights == NULL) {
        return NULL;
    }

    for (i=0; i<max_heur; i++) {
        whdb->weights[i] = (float *) malloc(14*sizeof(float));
        if (whdb->weights[i] == NULL) {
            for (j=i-1; j>=0; j--) {
                free(whdb->weights[j]);
            }
            free(whdb->weights);
            free(whdb);
            return NULL;
        }
    }

    return whdb;
}

void freeWHDB(struct whdb *whdb) {
    int i;

    for(i=0; i<whdb->max_heur; i++) {
        free(whdb->weights[i]);
    }

    free(whdb->weights);
    free(whdb);

    return;
}

short canAddWHDB(struct whdb *whdb) {

    return whdb->cur_heur < whdb->max_heur;
}

short addWHDB(struct whdb *whdb, float *weights) {
    short i;

    if (whdb == NULL || !canAddWHDB(whdb)) {
        return ERR;
    }

    for (i=0; i<14; i++) {
        whdb->weights[whdb->cur_heur][i] = weights[i];
    }

    whdb->cur_heur++;

    return OK;
}

// Size of database more than 5 at least, please.
float* getWHDB(struct whdb *whdb, int position) {
    int pos;

    if (whdb == NULL) {
        return NULL;
    }

    if (whdb->cur_heur < position) {
        return NULL;
    }

    if (position < 0) {
        pos = position + whdb->cur_heur;
    } else {
        pos = position;
    }

    return whdb->weights[pos];
}

int getNumWHDB(struct whdb *whdb) {
    if (whdb == NULL) {
        return ERR;
    }

    return whdb->cur_heur;
}

int getWindexWHDB(struct whdb *whdb) {
    if(whdb == NULL) {
        return ERR;
    }

    return whdb->win_index;
}

struct whdb* loadWHDB(char *filename) {
    struct whdb *whdb;
    int i, num_heur;
    short j;
    float weights[14];
    FILE *file;

    file = fopen(filename, "r");
    if (file == NULL) {
        return NULL;
    }

    fscanf(file, "%d ", &num_heur);
    if (num_heur > MAX_WHDB || num_heur < 0) {
        fclose(file);
        return NULL;
    }

    whdb = createWHDB(MAX_WHDB);
    if (whdb == NULL) {
        fclose(file);
        return NULL;
    }

    fscanf(file, "%d ", &whdb->win_index);
    if (whdb->win_index != -1) {
        fscanf(file, "%f ", &whdb->min_win_rate);
    }

    for (i=0; i<num_heur; i++) {
        for (j=0; j<14; j++) {
            fscanf(file, "%f ", &weights[j]);
        }
        addWHDB(whdb, weights);
    }

    fclose(file);

    return whdb;
}

short saveWHDB(struct whdb *whdb, char *filename) {
    int i;
    short j;
    FILE *file;

    if (whdb == NULL) {
        return ERR;
    }

    file = fopen(filename, "w");
    if (file == NULL) {
        freeWHDB(whdb);
        return ERR;
    }

    fprintf(file, "%d ", whdb->cur_heur);
    fprintf(file, "%d ", whdb->win_index);
    if (whdb->win_index != -1) {
        fprintf(file, "%f ", whdb->min_win_rate);
    }

    for (i=0; i<whdb->cur_heur; i++) {
        for (j=0; j<14; j++) {
            fprintf(file, "%f ", whdb->weights[i][j]);
        }
    }

    fclose(file);
    freeWHDB(whdb);
    return OK;
}

// Assumes whdb != NULL
float getWinRateWHDB(struct whdb *whdb) {

    return whdb->min_win_rate;
}


/* WHDB testing */

// Assumes whdb != NULL
short isEnabledTestingWHDB(struct whdb *whdb) {

    return whdb->win_index != -1;
}

void enableTestingWHDB (struct whdb *whdb, float min_win_rate) {
    if (whdb == NULL) {
        return;
    }

    if (whdb->win_index == -1) {
        whdb->win_index = 0;
        whdb->min_win_rate = min_win_rate;
    }

    return;
}

void disableTestingWHDB (struct whdb *whdb) {
    if (whdb == NULL) {
        return;
    }

    whdb->win_index = -1;
    whdb->min_win_rate = 0.0;
    return;
}

short updateAfterTestWHDB (struct whdb *whdb, float *new_weights) {
    short i;

    if (whdb == NULL || new_weights == NULL) {
        return ERR;
    }

    for (i=0; i<14; i++) {
        whdb->weights[whdb->win_index][i] = new_weights[i];
    }

    whdb->win_index++;
    if (whdb->win_index == whdb->cur_heur) {
        whdb->win_index = 0;
    }

    if (whdb->min_win_rate < 90.0) {
        whdb->min_win_rate += WR_INC;
    }

    return OK;
}


/* WHDB creation */

short buildRandomWHDB (char *filename, int num_heur, float min_h, float max_h) {
	struct whdb *whdb;
	float *heur_values;
	int i;
	generateWH gh = generateRandomWH;

	whdb = createWHDB(num_heur);
	if (whdb == NULL) {
		return ERR;
	}

	for (i=0; i<num_heur; i++) {
		heur_values = gh(NULL, min_h, max_h);
		if (heur_values == NULL) {
			freeWHDB(whdb);
			return ERR;
		}

		if (addWHDB(whdb, heur_values) == ERR) {
			free(heur_values);
			freeWHDB(whdb);
			return ERR;
		}

		free(heur_values);
	}

	return saveWHDB(whdb, filename);
}

short buildQualityRandomWHDB (char *filename, int num_heur, float min_h, float max_h) {
	struct whdb *whdb;
	float *heur_values;
	generateWH gh = generateRandomWH;
	heuristic h = heuristicWeight;

	whdb = createWHDB(num_heur);
	if (whdb == NULL) {
		return ERR;
	}

	while (getNumWHDB(whdb) < num_heur) {
		heur_values = gh(NULL, min_h, max_h);
		if (heur_values == NULL) {
			freeWHDB(whdb);
			return ERR;
		}

		if (!testAgainstRegular(h, heur_values) || !testAgainstGood(h, heur_values)) {
			free(heur_values);
			continue;
		}

		if (addWHDB(whdb, heur_values) == ERR) {
			free(heur_values);
			freeWHDB(whdb);
			return ERR;
		}

		free(heur_values);
	}

	return saveWHDB(whdb, filename);
}

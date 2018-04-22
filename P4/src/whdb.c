#include <stdio.h>
#include <stdlib.h>

#include "globals.h"
#include "whdb.h"

struct whdb* createWHDB(int max_heur) {
    struct whdb *whdb;
    int i, j;

    if (max_heur < 0 || max_heur > MAX_WHDB) {
        return NULL;
    }

    whdb = (struct whdb*) malloc(sizeof(struct whdb));
    if (whdb == NULL) {
        return NULL;
    }

    whdb->max_heur = max_heur;
    whdb->cur_heur = 0;

    whdb->weights = (short **) malloc(max_heur*sizeof(short*));
    if (whdb->weights == NULL) {
        return NULL;
    }

    for (i=0; i<max_heur; i++) {
        whdb->weights[i] = (short *) malloc(14*sizeof(short));
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

short addWHDB(struct whdb *whdb, short *weights) {
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

short* getWHDB(struct whdb *whdb, int position) {
    if (whdb == NULL) {
        return NULL;
    }

    return whdb->weights[position];
}

short getNumWHDB(struct whdb *whdb) {
    if (whdb == NULL) {
        return ERR;
    }

    return whdb->cur_heur;
}

struct whdb* loadWHDB(char *filename) {
    struct whdb *whdb;
    int i, num_heur;
    short weights[14], j;
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

    for (i=0; i<num_heur; i++) {
        for (j=0; j<14; j++) {
            fscanf(file, "%hi ", &weights[j]);
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

    for (i=0; i<whdb->cur_heur; i++) {
        for (j=0; j<14; j++) {
            fprintf(file, "%hi ", whdb->weights[i][j]);
        }
    }

    fclose(file);
    freeWHDB(whdb);
    return OK;
}

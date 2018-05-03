#ifndef _WHDB_H
#define _WHDB_H

#define MAX_WHDB 50000
#define INIT_WR 70.0
#define WR_INC 0.05

struct whdb {
    int max_heur;
    int cur_heur;
    float **weights;
    int win_index;
    float min_win_rate;
};

struct whdb* createWHDB(int max_heur);
void freeWHDB(struct whdb *whdb);
short canAddWHDB(struct whdb *whdb);
short addWHDB(struct whdb *whdb, float *weights);
float* getWHDB(struct whdb *whdb, int position);
int getNumWHDB(struct whdb *whdb);
int getWindexWHDB(struct whdb *whdb);
struct whdb* loadWHDB(char *filename);
short saveWHDB(struct whdb *whdb, char *filename);
float getWinRateWHDB(struct whdb *whdb);

short buildRandomWHDB (char *filename, int num_heur, float min_h, float max_h);
short buildQualityRandomWHDB (char *filename, int num_heur, float min_h, float max_h);

short isEnabledTestingWHDB(struct whdb *whdb);
void enableTestingWHDB (struct whdb *whdb, float min_win_rate);
void disableTestingWHDB (struct whdb *whdb);
short updateAfterTestWHDB (struct whdb *whdb, float *new_weights);

#endif

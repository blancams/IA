#ifndef _WHDB_H
#define _WHDB_H

#define MAX_WHDB 50000

struct whdb {
    int max_heur;
    int cur_heur;
    short **weights;
};

struct whdb* createWHDB(int max_heur);
void freeWHDB(struct whdb *whdb);
short canAddWHDB(struct whdb *whdb);
short addWHDB(struct whdb *whdb, short *weights);
short* getWHDB(struct whdb *whdb, int position);
struct whdb* loadWHDB();
short saveWHDB(struct whdb *whdb);

#endif

#ifndef _MAIN_H
#define _MAIN_H

#include "heuristic.h"

void printArgumentInfo(char *instruction);
void printArgumentDetail(char *instruction);
void printArgumentError(char *instruction);
void printHelp(char *instruction);
void printListFlags();
void printStart();
void printOptionError();
void printCriticalError();
heuristic parseHeuristic(char *hname);
short parseDepth(char *hname, short init_depth);
short parseWeights(char *hname, char **argv, short pos, float *weights);

/* Already tested */
// short genSimpleWH (struct whdb *whdb, short *values, short index);
// short createSimpleWHDB(char *filename);
#endif

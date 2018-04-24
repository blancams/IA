#ifndef _MANCALA_H
#define _MANCALA_H

#include "heuristic.h"

#define WIN1 1
#define WIN2 2
#define DRAW 0

struct hextree_node;

struct strategy {
	heuristic h;
	short depth;
	float *heur_values;
};

struct mancala_state {
	short hole[14];
	short player_turn;
	short kalaha_flag;
	short number_turn;
	struct strategy *str[2];
};

struct result {
	short winner;
	short score1;
	short score2;
};

// Creation and deletion of a game
struct mancala_state* createMancalaGame(short *player_1, short *player_2, short player_turn, struct strategy *str1, struct strategy *str2);
void freeMancalaGame(struct mancala_state *ms);

// Creation and deletion of a strategy
struct strategy* createStrategy(heuristic h, short depth, float *heur_values);
void freeStrategy(struct strategy *str);

// Obtaining of useful information
short oppositeHole(short hole);
short currentPlayer(struct mancala_state ms);
short oppositePlayer(struct mancala_state ms);
short playerOne();
short playerTwo();
short getSeeds(struct mancala_state ms, short player, short hole);
short countSeeds(struct mancala_state ms, short player, short hole_from);
short* holesWithSeeds(struct mancala_state ms, short player, short hole_from);
short getPts(struct mancala_state ms, short player);

// Control of states and movements
short gameHasEnded(struct mancala_state ms);
short isValidMove(struct mancala_state ms, short move);
short gameWinner(struct mancala_state ms);

// Making of a move
short isCapture(struct mancala_state ms, short hole_end);
short isSteal(struct mancala_state ms, short hole_end);
short mustRepeatTurn(struct mancala_state ms, short hole_end);
short makeMove(struct mancala_state *ms, short move);

// Decision of a move based on a strategy
short buildHextree(struct hextree_node *node, short depth, struct mancala_state ms, struct strategy *str);
short chooseMove(struct mancala_state ms);

// Play the game! (and return the winner)
struct result* playMancala(short player_turn, heuristic h1, heuristic h2, short depth1, short depth2, float *heur_values1, float *heur_values2);

#endif

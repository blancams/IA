#include <stdio.h>
#include <stdlib.h>

#include "globals.h"
#include "mancala.h"
#include "hextree.h"
#include "heuristic.h"

struct mancala_state* createMancalaGame(short *player_1, short *player_2, short player_turn, struct strategy *str1, struct strategy *str2) {
	short i;
	struct mancala_state *ms;

	ms = (struct mancala_state*) malloc(sizeof(struct mancala_state));

	if (ms == NULL) {
		return NULL;
	}

	for (i=0; i<7; i++) {
		ms->hole[i] = player_1[i];
		ms->hole[i+7] = player_2[i];
	}

	ms->player_turn = player_turn;
	ms->kalaha_flag = F;
	ms->number_turn = 0;

	ms->str[0] = str1;
	ms->str[1] = str2;

	return ms;
}

struct strategy* createStrategy(heuristic h, short depth, short *heur_values) {
	struct strategy *str;

	str = (struct strategy*) malloc(sizeof(struct strategy));
	if (str == NULL) {
		return NULL;
	}

	str->h = h;
	str->depth = depth;
	str->heur_values = heur_values;

	return str;
}

void freeStrategy(struct strategy *str) {
	if (str != NULL) {
		free(str);
	}

	return;
}

void freeMancalaGame(struct mancala_state *ms) {
	freeStrategy(ms->str[1]);
	freeStrategy(ms->str[0]);

	if (ms != NULL) {
		free(ms);
	}

	return;
}

void printMancala(struct mancala_state ms) {
	printf("\n");
	printf("J2: %d\t\t\t  J1: %d\n", ms.hole[13], ms.hole[6]);
	printf("%d     %d     %d     %d     %d     %d\n", ms.hole[12], ms.hole[11], ms.hole[10], ms.hole[9], ms.hole[8], ms.hole[7]);
	printf("-------------------------------\n");
	printf("%d     %d     %d     %d     %d     %d\n", ms.hole[0], ms.hole[1], ms.hole[2], ms.hole[3], ms.hole[4], ms.hole[5]);
	printf("\n");
}

short oppositeHole(short hole) {
	if (hole < 0 || hole > 13) {
		return ERR;
	}

	return 12-hole;
}

short currentPlayer(struct mancala_state ms) {

	return ms.player_turn;
}

short oppositePlayer(struct mancala_state ms) {
	if (ms.player_turn == 0) {
		return 1;
	}

	return 0;
}

short playerOne() {

	return 0;
}

short playerTwo() {

	return 1;
}

short getSeeds(struct mancala_state ms, short player, short hole) {
	if (player < 0 || player > 1 || hole < 0 || hole > 6) {
		return ERR;
	}

	return ms.hole[player*7 + hole];
}

short countSeeds(struct mancala_state ms, short player, short hole_from) {
	short i, sum;

	if (player < 0 || player > 1 || hole_from < 0 || hole_from > 6) {
		return ERR;
	}

	for (i=hole_from, sum=0; i<7; i++) {
		sum += ms.hole[player*7 + i];
	}

	return sum;
}

short* holesWithSeeds(struct mancala_state ms, short player, short hole_from) {
	short *holes;
	short i, j;

	for (i=hole_from, j=0; i<6; i++) {
		if (ms.hole[player*7 + i] != 0) {
			j++;
		}
	}

	// Must be freed by the caller
	holes = (short*) malloc(j*sizeof(short));
	if (holes == NULL) {
		return NULL;
	}

	for (i=hole_from, j=0; i<6; i++) {
		if (ms.hole[player*7 + i] != 0) {
			holes[j] = i;
		}
	}

	return holes;
}

short getPts(struct mancala_state ms, short player) {
	return countSeeds(ms, player, 0);
}

short gameHasEnded(struct mancala_state ms) {
	short i, test1, test2;

	for (i=0, test1=T; i<6; i++) {
		if (ms.hole[i] != 0) {
			test1 = F;
			break;
		}
	}

	for (i=0, test2=T; i<6; i++) {
		if (ms.hole[i+7] != 0) {
			test2 = F;
			break;
		}
	}

	return test1 | test2;
}

short isValidMove(struct mancala_state ms, short move) {
	if (move < 0 || move > 5) {
		return F;
	}

	if (ms.hole[currentPlayer(ms)*7 + move] == 0) {
		return F;
	}

	return T;
}

short gameWinner(struct mancala_state ms) {
	short i, res;

	if (!gameHasEnded(ms)) {
		return ERR;
	}

	for (i=0, res=0; i<7; i++) {
		res += ms.hole[i];
		res -= ms.hole[i+7];
	}

	if (res == 0) {
		return DRAW;
	} else if (res > 0) {
		return WIN1;
	} else {
		return WIN2;
	}
}

short isCapture(struct mancala_state ms, short hole_end) {
	short cp, oph;

	cp = currentPlayer(ms);
	oph = oppositeHole(hole_end);

	return (hole_end >= cp*7) && (hole_end <= cp*7 + 5) && (ms.hole[hole_end] == 1) && (ms.hole[oph] != 0);
}

short isSteal(struct mancala_state ms, short hole_end) {
	short op, oph;

	op = oppositePlayer(ms);
	oph = oppositeHole(hole_end);

	return (hole_end >= op*7) && (hole_end <= op*7 + 5) && (ms.hole[hole_end] > 6) && (ms.hole[oph] == 0);
}

short mustRepeatTurn(struct mancala_state ms, short hole_end) {

	return (hole_end == currentPlayer(ms)*7 + 6) && (ms.hole[hole_end] > 1);
}

short makeMove(struct mancala_state *ms, short move) {
	short i, seeds, hole_start, hole_end, ophole_end, cp;

	if (ms->kalaha_flag == F) {
		if (!isValidMove(*ms, move)) {
			return ERR;
		}

		cp = currentPlayer(*ms);

		hole_start = cp*7 + move;
		seeds = ms->hole[hole_start];
		ms->hole[hole_start] = 0;
		for (i=0, hole_end=hole_start; i<seeds; i++) {
			hole_end++;
			if (hole_end == 14) {
				hole_end = 0;
			}
			ms->hole[hole_end]++;
		}
		ophole_end = oppositeHole(hole_end);

		if (isCapture(*ms, hole_end)) {
			ms->hole[hole_end] = 0;
			ms->hole[cp*7+6] += ms->hole[ophole_end] + 1;
			ms->hole[ophole_end] = 0;
		}

		/* They are such fucking sons of a b****
		if (isSteal(*ms, hole_end)) {
			ms->hole[ophole_end] = ms->hole[hole_end];
			ms->hole[hole_end] = 0;
		} */

		if (mustRepeatTurn(*ms, hole_end)) {
			ms->kalaha_flag = T;
		}

		ms->number_turn++;
	} else {
		ms->kalaha_flag = F;
	}

	if (ms->player_turn == 0) {
		ms->player_turn = 1;
	} else {
		ms->player_turn = 0;
	}

	return OK;
}

short buildHextree(struct hextree_node *node, short depth, struct mancala_state ms, struct strategy *str) {
	short i, value, ret, flag = 0;
	struct mancala_state ms_child;
	struct hextree_node *child;

	if (depth == 0) {
		value = str->h(ms, str->heur_values);
		ret = setValue(node, value);
		if (ret == ERR) {
			return ERR;
		}
		return OK;
	}

	for (i=0; i<6; i++) {
		if (isValidMove(ms, i)) {
			if (flag == 0) {
				flag = 1;
			}

			ret = getIndex(node);
			if (ret == ERR) {
				return ERR;
			} else if (ret == -1) {
				ret = addHextreeNode(node, i, i);
				if (ret == ERR) {
					return ERR;
				}
			} else {
				ret = addHextreeNode(node, i, ret);
				if (ret == ERR) {
					return ERR;
				}
			}

			child = getChildren(node, i);

			ms_child = ms;
			ret = makeMove(&ms_child, i);
			if (ret == ERR) {
				return ERR;
			}
			ret = buildHextree(child, depth-1, ms_child, str);
			if (ret == ERR) {
				return ERR;
			}
		}
	}

	if (flag == 0) {
		value = str->h(ms, str->heur_values);
		ret = setValue(node, value);
		if (ret == ERR) {
			return ERR;
		}
	}

	return OK;
}

short chooseMove(struct mancala_state ms) {
	struct hextree_node *root, *node_nega;
	short cp, ret;

	cp = currentPlayer(ms);

	root = createHextree();
	if (root == NULL) {
		return ERR;
	}

	ret = buildHextree(root, ms.str[cp]->depth, ms, ms.str[cp]);
	if (ret == ERR) {
		freeHextree(root);
		return ERR;
	}

	node_nega = negaMax(root, 1);
	if (node_nega == NULL) {
		freeHextree(root);
		return ERR;
	}

	ret = getIndex(node_nega);
	freeHextree(root);

	return ret;
}

struct result* playMancala(short player_turn, heuristic h1, heuristic h2, short depth1, short depth2, short *heur_values1, short *heur_values2) {
	struct mancala_state *ms;
	struct strategy *str1, *str2;
	struct result *res;
	short init[7] = {3, 3, 3, 3, 3, 3, 0};
	short move, ret;

	res = (struct result*) malloc(sizeof(struct result));
	if (res == NULL) {
		return NULL;
	}

	str1 = createStrategy(h1, depth1, heur_values1);
	if (str1 == NULL) {
		return NULL;
	}

	str2 = createStrategy(h2, depth2, heur_values2);
	if (str2 == NULL) {
		return NULL;
	}

	ms = createMancalaGame(init, init, player_turn, str1, str2);
	if (ms == NULL) {
		return NULL;
	}

	while(!gameHasEnded(*ms)) {
		//printMancala(*ms);
		move = chooseMove(*ms);
		if (move == ERR) {
			return NULL;
		}
		//printf("Choice from %d has been %d.\n", ms->player_turn, move);
		ret = makeMove(ms, move);
		if (ret == ERR) {
			return NULL;
		}
	}

	ret = gameWinner(*ms);
	printMancala(*ms);

	res->winner = ret;
	res->score1 = countSeeds(*ms, 0, 0);
	res->score2 = countSeeds(*ms, 1, 0);

	freeMancalaGame(ms);

	return res;
}

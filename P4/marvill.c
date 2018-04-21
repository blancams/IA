#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <limits.h>

#include "marvill.h"

// Structure that represents the state of the game
typedef struct mancala_state {
	short state[14];
	short pl_turn;				// player1: 1, player2: 2
	short turn;
} mancala_state_t;

typedef struct heuristic {
	short number_operations;
	short *operations;
	float win_rate;
} heuristic_t;

typedef struct hdb {
	int max_h;
	struct heuristic *list_h;
	int cur_h;
} hdb_t;

typedef struct nmx {
	short value;
	short i;
	short j;
	short k;
} nmx_t;

// Prints the state of the game (it doesn't print who has the turn, should be added if needed)
void printState(struct mancala_state t) {
	printf("\n");
	printf("J2: %d\t\t\t  J1: %d\n", t.state[13], t.state[6]);
	printf("%d     %d     %d     %d     %d     %d\n", t.state[12], t.state[11], t.state[10], t.state[9], t.state[8], t.state[7]);
	printf("-------------------------------\n");
	printf("%d     %d     %d     %d     %d     %d\n", t.state[0], t.state[1], t.state[2], t.state[3], t.state[4], t.state[5]);
	printf("\n");
}

// Sets initial state from all of those boring arguments, pturn must be 1 or 2
void setInitialState(struct mancala_state *t, short i0, short i1, short i2, short i3, short i4, short i5, short i6, short i7, short i8, short i9, short i10, short i11, short i12, short i13, short pturn) {
	short i;

	t->state[0] = i0;
	t->state[1] = i1;
	t->state[2] = i2;
	t->state[3] = i3;
	t->state[4] = i4;
	t->state[5] = i5;
	t->state[6] = i6;
	t->state[7] = i7;
	t->state[8] = i8;
	t->state[9] = i9;
	t->state[10] = i10;
	t->state[11] = i11;
	t->state[12] = i12;
	t->state[13] = i13;
	t->pl_turn = pturn;
	t->turn = 0;
}

// This basically checks if we're trying to make a move from a place with value 0
// It is coded so that you can check with "if (invalidMove(t,choice))"
short invalidMove(struct mancala_state t, int choice) {
	if (t.pl_turn == 1 && t.state[choice] == 0) return 1;
	if (t.pl_turn == 2 && t.state[choice+7] == 0) return 1;
	return 0;
}

// Performs a move and takes everything into account. Do not touch this please.
void move(struct mancala_state *t, int choice) {
	short incr = 0, i, j, v;

	if (choice < 0 || choice > 6) return;
	if (invalidMove(*t, choice)) return;

	if (t->pl_turn == 2) {
		incr += 7;
	}

	j = choice+incr;
	v = t->state[j];
	for (i=0; i < v; i++) {
		j++;
		if (j == 14) j = 0;
		t->state[j]++;
	}
	t->state[choice+incr] = 0;

	// Turno
	if (!(t->pl_turn == 1 && j == 6 && t->state[6] != 1) && !(t->pl_turn == 2 && j == 13 && t->state[13] != 1)) {
		if (t->pl_turn == 1) t->pl_turn = 2;
		else if (t->pl_turn == 2) t->pl_turn = 1;
	}

	// Captura 1
	if (t->pl_turn == 2 && j <= 5 && j >= 0 && t->state[j] == 1 && t->state[12-j] != 0) {
		t->state[j] = 0;
		t->state[6] += t->state[12-j] + 1;
		t->state[12-j] = 0;
	}

	// Captura 2
	if (t->pl_turn == 1 && j <= 12 && j >= 7 && t->state[j] == 1 && t->state[12-j] != 0) {
		t->state[j] = 0;
		t->state[13] += t->state[12-j] + 1;
		t->state[12-j] = 0;
	}

	// Robo 1
	if (t->pl_turn == 2 && j <= 12 && j >= 7 && t->state[j] >= 7 && t->state[12-j] == 0) {
		t->state[12-j] = t->state[j];
		t->state[j] = 0;
	}

	// Robo 2
	if (t->pl_turn == 1 && j <= 5 && j >= 0 && t->state[j] >= 7 && t->state[12-j] == 0) {
		t->state[12-j] = t->state[j];
		t->state[j] = 0;
	}

	t->turn++;

	return;
}

// Checks if the game has come to an end
short end(struct mancala_state t) {

	if ((t.state[0] == 0 && t.state[1] == 0 && t.state[2] == 0 && t.state[3] == 0 && t.state[4] == 0 && t.state[5] == 0) || 
		(t.state[7] == 0 && t.state[8] == 0 && t.state[9] == 0 && t.state[10] == 0 && t.state[11] == 0 && t.state[12] == 0)) {
		return 0;
	}

	return 1;
}

// Returns all of the states after making one move (depth=1) through states
void nextStates(struct mancala_state t, struct mancala_state states[6]) {
	short i;

	for (i=0; i<6; i++) {
		states[i] = t;
		move(&states[i], i);
	}

}

// Returns all of the states after making two moves (depth=2) through states
void nextNextStates(struct mancala_state t, struct mancala_state states[6][6]) {
	short i, j;

	for (i=0; i<6; i++) {
		for (j=0; j<6; j++) {
			states[i][j] = t;
			move(&states[i][j], i);
			move(&states[i][j], j);
		}
	}

}

// Returns all of the states after making three moves (depth=3) through states
void nextNextNextStates(struct mancala_state t, struct mancala_state states[6][6][6]) {
	short i, j, k;

	for (i=0; i<6; i++) {
		for (j=0; j<6; j++) {
			for (k=0; k<6; k++) {
				states[i][j][k] = t;
				move(&states[i][j][k], i);
				move(&states[i][j][k], j);
				move(&states[i][j][k], k);
			}
		}
	}
}

short getMinFromList(short choice, short values[6], struct mancala_state st0, struct mancala_state st1[6]) {
	short i, min = SHRT_MAX;

	for(i=0; i<6; i++) {
		if (values[i] < min && !invalidMove(st0, choice) && (!end(st1[choice]) || !invalidMove(st1[choice], i))) {
			min = values[i];
		}
	}

	return min;
}

short getIndexFromList(short choice, short values[6], struct mancala_state st0, struct mancala_state st1[6]) {
	short i, min = SHRT_MAX, index = -1;

	for(i=0; i<6; i++) {
		if (values[i] < min && !invalidMove(st0, choice) && (!end(st1[choice]) || !invalidMove(st1[choice], i))) {
			min = values[i];
			index = i;
		}
	}

	return index;
}

// From a two-dimensional array, gets the index of the maximum value, taking into account
// that illegal moves are, oddly enough, illegal
short getNegamax(short values[6][6], struct mancala_state st0, struct mancala_state st1[6]) {
	short a = SHRT_MIN, b = -1, i, j, value, index;

	for(i=0;i<6;i++) {
		/*if (st0.pl_turn == st1[i].pl_turn && !invalidMove(st0, i)) value = 0;
		else */value = getMinFromList(i, values[i], st0, st1);
		if (value == SHRT_MAX) continue;
		index = getIndexFromList(i, values[i], st0, st1);
		if (value > a) {
			a = value;
			b = i;
		}
	}

	return b;
}

// Same as Negamax but for three-dimensional arrays
struct nmx getNegamaxGood(short node[3], short depth, short color, short values[6][6][6], struct mancala_state st0, struct mancala_state st1[6], struct mancala_state st2[6][6]) {
	short new_node[6][3], i;
	struct nmx info, aux;

	if (depth == 0 || (node[0] != -1 && node[1] != -1 && node[2] != -1)) {
		if (!invalidMove(st0, node[0]) && (!end(st1[node[0]]) || !invalidMove(st1[node[0]], node[1])) && (!end(st2[node[0]][node[1]]) || !invalidMove(st2[node[0]][node[1]], node[2]))) {
			info.value = color * values[node[0]][node[1]][node[2]];
			info.i = node[0];
			info.j = node[1];
			info.k = node[2];
			return info;
		} else {
			info.value = color * SHRT_MIN;
			info.i = node[0];
			info.j = node[1];
			info.k = node[2];
			return info;
		}
	}

	info.value = SHRT_MIN;
	info.i = -1;
	info.j = -1;
	info.k = -1;

	if (node[0] == -1) {
		for (i=0; i<6; i++) {
			new_node[i][0] = i;
			new_node[i][1] = -1;
			new_node[i][2] = -1;
		}
	} else if (node[1] == -1) {
		for (i=0; i<6; i++) {
			new_node[i][0] = node[0];
			new_node[i][1] = i;
			new_node[i][2] = -1;
		}
	} else if (node[2] == -1) {
		for (i=0; i<6; i++) {
			new_node[i][0] = node[0];
			new_node[i][1] = node[1];
			new_node[i][2] = i;
		}
	}

	for (i=0; i<6; i++) {
		aux = getNegamaxGood(new_node[i], depth-1, -1 * color, values, st0, st1, st2);
		aux.value *= -1;
		if (aux.value > info.value) {
			info.value = aux.value;
			info.i = aux.i;
			info.j = aux.j;
			info.k = aux.k;
		}
	}
	
	return info;
}

short getNegamaxGoodIndex(struct nmx info) {

	return info.i;
}

short createHeuristic(struct heuristic *h, short number_operations) {
	short size;

	h->number_operations = number_operations;

	size = number_operations*2 + 1;
	h->operations = (short*) malloc(size*sizeof(short));

	if (h->operations == NULL) return -1;

	return 0;
}

void freeHeuristic(struct heuristic *h) {
	free(h->operations);
	return;
}

void printHeuristic(struct heuristic h) {
	short i;

	for (i=0; i<h.number_operations*2+1; i++) {
		printf("%d ", h.operations[i]);
	}

	return;
}

short createHDB(struct hdb *h, int max_h) {
	h->max_h = max_h;
	h->cur_h = 0;
	h->list_h = (struct heuristic*) malloc(max_h*sizeof(struct heuristic));
	if (h->list_h == NULL) return -1;

	return 0;
}

void freeHDB(struct hdb *h) {
	int i;

	for (i=0; i<h->cur_h; i++) {
		freeHeuristic(&(h->list_h[i]));
	}
	free(h->list_h);
	return;
}

short addHDB(struct hdb *hdb, struct heuristic h) {
	if (hdb->cur_h == hdb->max_h) {
		return -1;
	}

	hdb->list_h[hdb->cur_h] = h;
	hdb->cur_h++;
	return 0;
}

short generateRandomHeuristic(struct heuristic *h, short number_operations) {
	short i;

	i = createHeuristic(h, number_operations);
	if (i == -1) return -1;

	h->operations[0] = rand() % 4;
	h->operations[1] = rand() % 14;
	h->operations[2] = rand() % 14;
	for (i=3; i<h->number_operations*2+1; i += 2) {
		h->operations[i] = rand() % 4;
		h->operations[i+1] = rand() % 14;
	}

	return 0;
}

short applyOperation(short operator, short op1, short op2) {
	if (operator == _SUM) {
		return op1 + op2;
	} else if (operator == _SUB) {
		return op1 - op2;
	} else if (operator == _MUL) {
		return op1 * op2;
	} else if (operator == _DIV) {
		if (op2 == 0) return op1;
		return op1 / op2;
	}
}

short heuristicRandom(struct heuristic h, struct mancala_state t) {
	short i, value;

	value = applyOperation(h.operations[0], t.state[h.operations[1]], t.state[h.operations[2]]);

	for (i=3; i<h.number_operations*2+1; i = i+2) {
		value = applyOperation(h.operations[i], value, t.state[h.operations[i+1]]);
	}

	return value;
}

short strategyRandom(struct heuristic h, struct mancala_state t) {
	short values[6][6], i, j;
	struct mancala_state st2[6][6], st1[6];

	nextStates(t, st1);
	nextNextStates(t, st2);

	for(i=0; i<6; i++) {
		for(j=0; j<6; j++) {
			if (t.pl_turn == st1[i].pl_turn && !invalidMove(t, i)) values[i][j] = heuristicRandom(h, st1[i]);
			else values[i][j] = heuristicRandom(h, st2[i][j]);
		}
	}

	return getNegamax(values, t, st1);
}

// Heuristic for Regular strategy described below
short heuristicRegularAndGood(struct mancala_state t, short turn) {
	short a = 0, b = 0, i;

	for (i=0; i<7; i++) {
		a += t.state[i];
		b += t.state[i+7];
	}

	if (turn == 1) return a-b;
	else return b-a;
}

// Makes a choice with the "Regular" strategy, this means: take into account all of the
// possible states after two moves, calculate sum(first row)-sum(second row) for all of them
// and choose the moves that maximize that value
short strategyRegular(struct mancala_state t) {
	short values[6][6], i, j;
	struct mancala_state st2[6][6], st1[6];

	nextStates(t, st1);
	nextNextStates(t, st2);

	for(i=0; i<6; i++) {
		for(j=0; j<6; j++) {
			if (t.pl_turn == st1[i].pl_turn && !invalidMove(t, i)) values[i][j] = heuristicRegularAndGood(st1[i], t.pl_turn);
			else values[i][j] = heuristicRegularAndGood(st2[i][j], t.pl_turn);
		}
			
	}

	return getNegamax(values, t, st1);
}

// Makes a choice with the "Good" strategy, this means: do exactly the same as in the "Regular"
// strategy but taking into account all of the possible states after three moves, not just two (because potato)
// This is the only strategy with depth 3, the rest are all depth 2
short strategyGood(struct mancala_state t) {
	short values[6][6][6], i, j, k, l, a, b, node[3] = {-1, -1, -1};
	struct mancala_state st3[6][6][6], st2[6][6], st1[6];

	nextStates(t, st1);
	nextNextStates(t, st2);
	nextNextNextStates(t, st3);

	for(i=0; i<6; i++) {
		for(j=0; j<6; j++) {
			for(k=0; k<6; k++) {
				if (end(st2[i][j])) {
					values[i][j][k] = heuristicRegularAndGood(st3[i][j][k], t.pl_turn);
				} else {
					values[i][j][k] = -50;
				}
			}
		}
	}

	return getNegamaxGoodIndex(getNegamaxGood(node, 3, 1, values, t, st1, st2));
}

// Returns the winner, 0 if there is a draw
short winner(struct mancala_state t) {
	short i, a1 = 0, a2 = 0;

	for(i=0; i<7; i++) {
		a1 += t.state[i];
		a2 += t.state[i+7];
	}

	if (a1 > a2) {
		return 1;
	} else if (a1 < a2) {
		return 2;
	} else {
		return 0;
	}
}

// Here is where stuff has to be done for the first player
short strategyOne(struct heuristic h, struct mancala_state t) {
	return strategyRegular(t);
}

// Here is where stuff has to be done for the second player
short strategyTwo(struct heuristic h, struct mancala_state t) {
	return strategyRandom(h, t);
}

short testRegularAndGood(struct mancala_state *t, struct heuristic *h) {
	short choice, max=100, g;

	setInitialState(t, 3, 3, 3, 3, 3, 3, 0, 3, 3, 3, 3, 3, 3, 0, 1);
	g = 0;
	while (end(*t) && g < max) {
		if (t->pl_turn == 1) {
			choice = strategyRandom(*h, *t);
		} else {
			choice = strategyRegular(*t);
		}
		move(t, choice);
		g++;
	}
	
	if (winner(*t) == 2 || g == max) return 1;

	setInitialState(t, 3, 3, 3, 3, 3, 3, 0, 3, 3, 3, 3, 3, 3, 0, 2);
	g = 0;
	while (end(*t) && g < max) {
		if (t->pl_turn == 1) {
			choice = strategyRandom(*h, *t);
		} else {
			choice = strategyRegular(*t);
		}
		move(t, choice);
		g++;
	}
	
	if (winner(*t) == 2 || g == max) return 1;

	setInitialState(t, 3, 3, 3, 3, 3, 3, 0, 3, 3, 3, 3, 3, 3, 0, 1);
	g = 0;
	while (end(*t) && g < max) {
		if (t->pl_turn == 1) {
			choice = strategyRegular(*t);
		} else {
			choice = strategyRandom(*h, *t);
		}
		move(t, choice);
		g++;
	}
	
	if (winner(*t) == 1 || g == max) return 1;

	setInitialState(t, 3, 3, 3, 3, 3, 3, 0, 3, 3, 3, 3, 3, 3, 0, 2);
	g = 0;
	while (end(*t) && g < max) {
		if (t->pl_turn == 1) {
			choice = strategyRegular(*t);
		} else {
			choice = strategyRandom(*h, *t);
		}
		move(t, choice);
		g++;
	}
	
	if (winner(*t) == 1 || g == max) return 1;

	setInitialState(t, 3, 3, 3, 3, 3, 3, 0, 3, 3, 3, 3, 3, 3, 0, 1);
	g = 0;
	while (end(*t) && g < max) {
		if (t->pl_turn == 1) {
			choice = strategyRandom(*h, *t);
		} else {
			choice = strategyGood(*t);
		}
		move(t, choice);
		g++;
	}
	
	if (winner(*t) == 2 || g == max) return 1;

	setInitialState(t, 3, 3, 3, 3, 3, 3, 0, 3, 3, 3, 3, 3, 3, 0, 2);
	g = 0;
	while (end(*t) && g < max) {
		if (t->pl_turn == 1) {
			choice = strategyRandom(*h, *t);
		} else {
			choice = strategyGood(*t);
		}
		move(t, choice);
		g++;
	}
	
	if (winner(*t) == 2 || g == max) return 1;

	setInitialState(t, 3, 3, 3, 3, 3, 3, 0, 3, 3, 3, 3, 3, 3, 0, 1);
	g = 0;
	while (end(*t) && g < max) {
		if (t->pl_turn == 1) {
			choice = strategyGood(*t);
		} else {
			choice = strategyRandom(*h, *t);
		}
		move(t, choice);
		g++;
	}
	
	if (winner(*t) == 1 || g == max) return 1;

	setInitialState(t, 3, 3, 3, 3, 3, 3, 0, 3, 3, 3, 3, 3, 3, 0, 2);
	g = 0;
	while (end(*t) && g < max) {
		if (t->pl_turn == 1) {
			choice = strategyGood(*t);
		} else {
			choice = strategyRandom(*h, *t);
		}
		move(t, choice);
		g++;
	}
	
	if (winner(*t) == 1 || g == max) return 1;

	return 0;
}

short testRandoms(struct mancala_state *t, struct heuristic *h1, struct heuristic *h2) {
	short wins=0, choice, max=100, g;

	setInitialState(t, 3, 3, 3, 3, 3, 3, 0, 3, 3, 3, 3, 3, 3, 0, 1);
	g = 0;
	while (end(*t) && g < max) {
		if (t->pl_turn == 1) {
			choice = strategyRandom(*h1, *t);
		} else {
			choice = strategyRandom(*h2, *t);
		}
		move(t, choice);
		g++;
	}
	if (g == max) return -1;
	if (winner(*t) == 2) wins++;

	setInitialState(t, 3, 3, 3, 3, 3, 3, 0, 3, 3, 3, 3, 3, 3, 0, 2);
	g = 0;
	while (end(*t) && g < max) {
		if (t->pl_turn == 1) {
			choice = strategyRandom(*h1, *t);
		} else {
			choice = strategyRandom(*h2, *t);
		}
		move(t, choice);
		g++;
	}
	if (g == max) return -1;
	if (winner(*t) == 2) wins++;

	setInitialState(t, 3, 3, 3, 3, 3, 3, 0, 3, 3, 3, 3, 3, 3, 0, 1);
	g = 0;
	while (end(*t) && g < max) {
		if (t->pl_turn == 1) {
			choice = strategyRandom(*h2, *t);
		} else {
			choice = strategyRandom(*h1, *t);
		}
		move(t, choice);
		g++;
	}
	if (g == max) return -1;
	if (winner(*t) == 1) wins++;

	setInitialState(t, 3, 3, 3, 3, 3, 3, 0, 3, 3, 3, 3, 3, 3, 0, 2);
	g = 0;
	while (end(*t) && g < max) {
		if (t->pl_turn == 1) {
			choice = strategyRandom(*h2, *t);
		} else {
			choice = strategyRandom(*h1, *t);
		}
		move(t, choice);
		g++;
	}
	if (g == max) return -1;
	if (winner(*t) == 1) wins++;

	return wins;
}

short testLoops(struct mancala_state *t, struct heuristic *h, struct hdb *hdb, struct hdb *champions, int db_size, int top_size, int db_tests, int he_tests, short number_operations, float win_rate) {
	short ret, wins, r;
	int i, j;

	ret = createHDB(hdb, db_size);
	if (ret == -1) return -1;

	ret = createHDB(champions, top_size);
	if (ret == -1) return -1;

	for(j=3; j<12; j++) {
		for(i=0; i<db_tests; i++) {
			ret = generateRandomHeuristic(h, j);
			if (ret == -1) {
				printf("Errorsito.\n");
				return -1;
			}

			ret = testRegularAndGood(t, h);
			if (ret == -1) return -1;
			else if (ret == 1) {
				freeHeuristic(h);
				continue; 
			}

			if (addHDB(hdb, *h)) {
				freeHeuristic(h);
				break;
			}
		}
	}
	
	printf("Hay %d elementos en la base de datos.\n", hdb->cur_h);

	for (i=1; i<he_tests+1; i++) {
		if (i%10000 == 0) printf("Iteration %d\n", i);
		r = (rand() % (number_operations - 3)) + 3;
		ret = generateRandomHeuristic(h, r);
		if (ret == -1) {
			printf("Errorsito.\n");
			return -1;
		}
		
		ret = testRegularAndGood(t, h);
		if (ret == -1) return -1;
		else if (ret == 1) {
			freeHeuristic(h);
			continue; 
		}

		// After this tests we have discarded those who do not win against regular and good

		for (j=0, wins=0; j<hdb->cur_h; j++) {
			ret = testRandoms(t, h, &(hdb->list_h[j]));
			if (ret == -1) {
				printf("Fallo en iteracion %d, se descarta todo.\n", i);
				wins = 0;
				break;
			} else {
				wins += ret;
			}
		}

		if (hdb->cur_h == 0) {
			h->win_rate = 0.0;
		} else {
			h->win_rate = (wins * 100.0) / (hdb->cur_h*4);
		}

		if (h->win_rate > win_rate) {
			if(addHDB(champions, *h)) {
				freeHeuristic(h);
				break;
			}
		}

	}

	printf("\nCampeones:\n");
	for (i=0; i<champions->cur_h; i++) {
		printf("%d: ", i+1);
		printHeuristic(champions->list_h[i]);
		printf("  - %f\n", champions->list_h[i].win_rate);
	}

	freeHDB(hdb);
	freeHDB(champions);

	return 0;
}

// Pretty little main
int main() {
	struct mancala_state init;
	struct heuristic h, win;
	struct hdb hdb, champions;
	struct nmx info;
	short ret, operations[11] = {0, 1, 8, 3, 0, 3, 1, 0, 9, 3, 0}, i, choice;
	clock_t start, finish;

	srand(time(NULL));
	start = clock();

	//ret = testLoops(&init, &h, &hdb, &champions, DB_SIZE, TOP_SIZE, DB_TESTS, HE_TESTS, 20, 80.0);
	//if (ret == -1) return -1;

	ret = createHeuristic(&win, 5);
	if (ret == -1) return -1;
	for (i=0; i<11; i++) {
		win.operations[i] = operations[i];
	}

	setInitialState(&init, 3, 3, 3, 3, 3, 3, 0, 3, 3, 3, 3, 3, 3, 0, 1);

	while (end(init)) {
		printState(init);
		if (init.pl_turn == 1) {
			choice = strategyRandom(win, init);
		} else {
			choice = strategyRegular(init);
		}
		printf("Choice from %d is %d", init.pl_turn, choice);
		move(&init, choice);
	}

	printf("\nAnd the winner, who is supposed to be 1, is actually %d!\n", winner(init));
	printState(init);

	finish = clock();

	printf("It took %lf seconds!", (double) (finish-start)/CLOCKS_PER_SEC);

	return 0;
}
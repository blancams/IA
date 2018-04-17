#include <stdlib.h>
#include <stdio.h>
#include <time.h>

// Structure that represents the state of the game
typedef struct mancala_state {
	short state[14];
	short pl_turn;				// player1: 1, player2: 2
	short max;
	short turn;
} mancala_state_t;

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
void setInitialState(struct mancala_state *t, short i0, short i1, short i2, short i3, short i4, short i5, short i6,
					 short i7, short i8, short i9, short i10, short i11, short i12, short i13, short pturn) {
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
	t->max = 0;
	for(i=0; i<14; i++) {
		t->max += t->state[i];
	}
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
	if (!(t->pl_turn == 1 && j == 6 && t->state[6] != 0) && !(t->pl_turn == 2 && j == 13 && t->state[13] != 0)) {
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
	if (t->pl_turn == 2 && j <= 12 && j >= 7 && t->state[j] >= 6 && t->state[12-j] == 0) {
		t->state[12-j] = t->state[j];
		t->state[j] = 0;
	}

	// Robo 2
	if (t->pl_turn == 1 && j <= 5 && j >= 0 && t->state[j] >= 6 && t->state[12-j] == 0) {
		t->state[12-j] = t->state[j];
		t->state[j] = 0;
	}

	t->turn++;

	return;
}

// Checks if the game has come to an end
short end(struct mancala_state t) {

	if ((t.state[0] == 0 && t.state[1] == 0 && t.state[2] == 0 && t.state[3] == 0 && t.state[4] == 0 && t.state[5] == 0) || 
		(t.state[7] == 0 && t.state[8] == 0 && t.state[9] == 0 && t.state[10] == 0 && t.state[11] == 0 && t.state[12] == 0) ||
		t.state[6] > t.max/2 || t.state[13] > t.max/2) {
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

// Makes a choice with the "Regular" strategy, this means: take into account all of the
// possible states after two moves, calculate sum(first row)-sum(second row) for all of them
// and choose the moves that maximize that value
short strategyRegular(struct mancala_state t) {
	short values[6][6], i, j, k, a, b;
	struct mancala_state st2[6][6], st1[6];

	nextStates(t, st1);
	nextNextStates(t, st2);

	for(i=0; i<6; i++) {
		for(j=0; j<6; j++) {
			a = 0;
			b = 0;
			for(k=0; k<7; k++) {
				a += st2[i][j].state[k];
				b += st2[i][j].state[k+7];
			}
			if (t.pl_turn == 1) values[i][j] = a-b;
			if (t.pl_turn == 2) values[i][j] = b-a;
		}
			
	}

	b = -1;
	a = t.max * -1;
	for(i=0; i<6; i++) {
		for(j=0; j<6; j++) {
			if (values[i][j] > a && !invalidMove(t, i) && !invalidMove(st1[i], j)) {
				a = values[i][j];
				b = i;
			}
		}
		
	}

	return b;
}

// Makes a choice with the "Good" strategy, this means: do exactly the same as in the "Regular"
// strategy but taking into account all of the possible states after three moves, not just two (because potato)
short strategyGood(struct mancala_state t) {
	short values[6][6][6], i, j, k, l, a, b;
	struct mancala_state st3[6][6][6], st2[6][6], st1[6];

	nextStates(t, st1);
	nextNextStates(t, st2);
	nextNextNextStates(t, st3);

	for (i=0; i<6; i++) {
		for (j=0; j<6; j++) {
			for (k=0; k<6; k++) {
				a = 0;
				b = 0;
				for(l=0; l<7; l++) {
					a += st3[i][j][k].state[l];
					b += st3[i][j][k].state[l+7];
				}
				if (t.pl_turn == 1) values[i][j][k] = a-b;
				if (t.pl_turn == 2) values[i][j][k] = b-a;
			}
		}
	}

	b = -1;
	a = t.max * -1;
	for (i=0; i<6; i++) {
		for (j=0; j<6; j++) {
			for (k=0; k<6; k++) {
				if (values[i][j][k] > a && !invalidMove(t, i) && !invalidMove(st1[i], j) && !invalidMove(st2[i][j], k)) {
					a = values[i][j][k];
					b = i;
				}
			}
		}
	}

	return b;
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
short strategyOne(struct mancala_state t) {
	return strategyGood(t);
}

// Here is where stuff has to be done for the second player
short strategyTwo(struct mancala_state t) {
	return strategyRegular(t);
}

// Pretty little main
int main() {
	struct mancala_state init;
	short choice;

	setInitialState(&init, 3, 3, 3, 3, 3, 3, 0, 3, 3, 3, 3, 3, 3, 0, 1);

	while (end(init)) {
		//printState(init);
		if (init.pl_turn == 1) {
			choice = strategyOne(init);
		} else {
			choice = strategyTwo(init);
		}
		move(&init, choice);
	}

	printf("\nFIN!!! El ganador es: %d", winner(init));
	printState(init);


	return 0;
}
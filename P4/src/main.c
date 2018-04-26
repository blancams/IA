#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

#include "mancala.h"
#include "hextree.h"
#include "heuristic.h"
#include "globals.h"
#include "whdb.h"
#include "main.h"

void printArgumentInfo(char *instruction) {
	if (!strcmp(instruction, "-g")) {
		printf("-g <player_turn> <depth> <see_flag> <heuristic_player_1> <heuristic_player_2> <weights_player_1[14]> <weights_player_2[14]>\n");
	} else if (!strcmp(instruction, "-b")) {
		printf("-b <file_name> <number_heuristics> <min_weight> <max_weight>\n");
	} else if (!strcmp(instruction, "-u")) {
		printf("-u <file_name> <type_update> <num_iterations> <min_weight> <max_weight>\n");
	} else if (!strcmp(instruction, "-p")) {
		printf("-p <file_name> <position>\n");
	} else if (!strcmp(instruction, "-ia")) {
		printf("-ia <file_name> <group> <team> <number> <password> <day> <month> <alias>");
	}

	return;
}

void printArgumentError(char *instruction) {
	printf("Error in arguments. Please follow the structure:\n");
	printArgumentInfo(instruction);

	return;
}

void printHelp() {
	printArgumentInfo("-g");
	printf("\tPlays a Mancala game with the desired options:\n");
	printf("\t\tplayer_turn:        0 -> first player starts the game\n");
	printf("\t\t                    1 -> second player starts the game\n");
	printf("\t\tdepth:              desired depth for searching, must be even\n");
	printf("\t\tsee_flag:           1 -> prints the development of the game\n");
	printf("\t\t                    0 -> only prints the result\n");
	printf("\t\theuristic_player_1: name of the heuristic for player 1 (heuristicIARegular, heuristicIABuena or heuristicWeight)\n");
	printf("\t\theuristic_player_2: name of the heuristic for player 2 (heuristicIARegular, heuristicIABuena or heuristicWeight)\n");
	printf("\t\tweights_player_1:   fourteen float numbers, weights to apply heuristicWeight for player 1\n");
	printf("\t\tweights_player_2:   fourteen float numbers, weights to apply heuristicWeight for player 2\n");
	printf("\n");
	printArgumentInfo("-b");
	printf("\tGenerates a data base of random heuristics with the desired options:\n");
	printf("\t\tfile_name:          name of the file in which to save the data base.\n");
	printf("\t\tnumber_heuristics:  number of heuristics to save\n");
	printf("\t\tmin_weight:         minimum possible weight as a float number\n");
	printf("\t\tmax_weight:         maximum possible weight as a float number\n");
	printf("\n");
	printArgumentInfo("-u");
	printf("\tUploads the data base in a file through with the desired options:\n");
	printf("\t\tfile_name:          name of the file from which to load the data base (it must exist)\n");
	printf("\t\ttype_update:        type of heuristic generation (random, genetic)\n");
	printf("\t\titerations:         number of heuristics generated to upload the data base\n");
	printf("\t\tmin_weight:         minimum possible weight as a float number\n");
	printf("\t\tmax_weight:         maximum possible weight as a float number\n");
	printf("\n");
	printArgumentInfo("-p");
	printf("\tPrints one specific heuristic (its 14 weights) from a data base with the desired options:\n");
	printf("\t\tfile_name:          name of the file from which to load the heuristic (it must exist)\n");
	printf("\t\tposition:           index of the heuristic in the data base (if -1, prints the last one generated)\n");
	printf("\n");
	printArgumentInfo("-ia");
	printf("\tGenerates the text of the .cl file required for Artificial Intelligence uploads:\n");
	printf("\t\tfile_name:          name of the file from which to load the heuristic (it must exist)\n");
	printf("\t\tgroup:              group of the team/pair (always two digits)\n");
	printf("\t\tteam:               number of the team/pair (always two digits)\n");
	printf("\t\tnumber:             number of the file (1, 2 or 3)\n");
	printf("\t\tpassword:           two character string that identifies your group\n");
	printf("\t\tday:                day in which it is going to be uploaded (always two digits)\n");
	printf("\t\tmonth:              month in which it is going to be uploaded (always two digits)\n");
	printf("\t\talias:              name of the bot (for example, \"Marvill-1.1.0\")\n");
	printf("\n");

	return;
}

void printStart() {
	printf("Welcome to the Marvill Mancala Project. To run the program, execute the command \"./main\" with one of the following flags:\n\n");
	printHelp();
}

void printOptionError() {
	printf("You have not selected a correct flag. Please run the program with one of the following flags:\n");
	printHelp();
}

void printCriticalError() {
	printf("Critical failure. Restart the program.\n");

	return;
}

heuristic parseHeuristic(char *hname) {
	if (!strcmp(hname, "heuristicIARegular")) {
		return heuristicIARegular;
	} else if (!strcmp(hname, "heuristicIABuena")) {
		return heuristicIABuena;
	} else if (!strcmp(hname, "heuristicWeight")) {
		return heuristicWeight;
	}

	return NULL;
}

short parseDepth(char *hname, short init_depth) {
	if (init_depth % 2 != 0) {
		return ERR;
	} else if (!strcmp(hname, "heuristicIABuena")) {
		return init_depth+1;
	}

	return init_depth;
}

short parseWeights(char *hname, char **argv, short pos, float *weights) {
	short i;

	if (strcmp(hname, "heuristicIARegular") && strcmp(hname, "heuristicIABuena")) {
		for (i=0; i<14; i++) {
			weights[i] = atof(argv[pos+i]);
		}
	} else {
		return F;
	}

	return T;
}

int main(int argc, char **argv) {
	srand(time(NULL));

	if (argc == 1) {
		printStart();
	} else if (!strcmp(argv[1], "-g")) {
		// Processing of arguments for Mancala games
		struct result *winner;
		short pl_turn, d1, d2, flag;
		float *w1, *w2;
		heuristic h1, h2;

		// Arguments must be 7 (no weighted heuristics involved), 21 (one) or 35 (two)
		if (argc != 7 && argc != 21 && argc != 35) {
			printArgumentError(argv[1]);
			return ERR;
		}

		// Sets player turn
		pl_turn = (short) atoi(argv[2]);
		if (pl_turn != 0 && pl_turn != 1) {
			printArgumentError(argv[1]);
			return ERR;
		}

		// Parses heuristics depending on the first two arguments, strings equal to heuristic names
		h1 = parseHeuristic(argv[5]);
		if (h1 == NULL) {
			printArgumentError(argv[1]);
			return ERR;
		}

		h2 = parseHeuristic(argv[6]);
		if (h2 == NULL) {
			printArgumentError(argv[1]);
			return ERR;
		}

		// Sets depths
		d1 = parseDepth(argv[5], (short) atoi(argv[3]));
		if (d1 == ERR) {
			printArgumentError(argv[1]);
			return ERR;
		}

		d2 = parseDepth(argv[6], (short) atoi(argv[3]));
		if (d2 == ERR) {
			printArgumentError(argv[1]);
			return ERR;
		}

		// Sets see flag
		flag = (short) atoi(argv[4]);
		if (flag != 0 && flag != 1) {
			printArgumentError(argv[1]);
			return ERR;
		}

		// Processes weights.
		// If there are 21 arguments, one of the heuristics is weighted and the other is not
		if (argc == 21) {
			// Memory allocation for the first heuristic
			w1 = (float*) malloc(14*sizeof(float));
			if (w1 == NULL) {
				printCriticalError();
				return ERR;
			}

			// If parseWeights returns false, the first heuristic is regular/good
			if (!parseWeights(argv[5], argv, 7, w1)) {
				// Frees and sets to NULL the vector for the first heuristic
				free(w1);
				w1 = NULL;

				// Memory allocation for the second heuristic
				w2 = (float*) malloc(14*sizeof(float));
				if (w2 == NULL) {
					printCriticalError();
					return ERR;
				}

				// If parseWeights returns false, the second heuristic is regular/good
				// so there has been a mistake while introducing arguments
				if (!parseWeights(argv[6], argv, 21, w2)) {
					// Frees the vector for the second heuristic and returns error
					free(w2);
					printArgumentError(argv[1]);
					return ERR;
				}
			// If parseWeights return true, the first heuristic is weighted and the other regular/good
			} else {
				// Sets the vector for the second heuristic to NULL
				w2 = NULL;
			}
		// If there are 35 arguments, both of the heuristics are weighted
		} else if (argc == 35) {
			// Memory allocation for both vectors
			w1 = (float*) malloc(14*sizeof(float));
			if (w1 == NULL) {
				printCriticalError();
				return ERR;
			}

			w2 = (float*) malloc(14*sizeof(float));
			if (w2 == NULL) {
				printCriticalError();
				return ERR;
			}

			// After parsing, if one or the other returns false, one of them at least is regular/good
			// so there has been a mistake while introducing arguments
			if (!parseWeights(argv[5], argv, 7, w1) | !parseWeights(argv[6], argv, 21, w2)) {
				// Frees memory for both vectors and returns error
				free(w1);
				free(w2);
				printArgumentError(argv[1]);
				return ERR;
			}
		// Otherwise, there are 6 arguments and no vectors are needed
		} else {
			// Sets both vectors to NULL
			w1 = NULL;
			w2 = NULL;
		}

		winner = playMancala(pl_turn, h1, h2, d1, d2, w1, w2, flag);
		if (winner == NULL) {
			printArgumentError(argv[1]);
			free(w1);
			free(w2);
			free(winner);
			return ERR;
		}
		printf("\nPlayer %hi has won with the score %hi-%hi.\n", winner->winner, winner->score1, winner->score2);

		free(w1);
		free(w2);
		free(winner);
	} else if (!strcmp(argv[1], "-b")) {
		short ret;

		if (argc != 6) {
			printArgumentError(argv[1]);
			return ERR;
		}

		ret = buildRandomWHDB(argv[2], atoi(argv[3]), atof(argv[4]), atof(argv[5]));
		if (ret == ERR) {
			printArgumentError(argv[1]);
			return ERR;
		}

		printf("Database created successfully in %s.\n", argv[2]);
	} else if (!strcmp(argv[1], "-u")) {
		short ret;

		if (argc != 7) {
			printArgumentError(argv[1]);
			return ERR;
		}

		ret = updateWHDB(argv[2], argv[3], atoi(argv[4]), atof(argv[5]), atof(argv[6]));
		if (ret == ERR) {
			printArgumentError(argv[1]);
			return ERR;
		}

		printf("Update completed succesfully!\n");
	} else if (!strcmp(argv[1], "-p")) {
		struct whdb *whdb;
		float* weights;
		int pos;
		short i;

		if (argc != 4) {
			printArgumentError(argv[1]);
			return ERR;
		}

		whdb = loadWHDB(argv[2]);
		if (whdb == NULL) {
			printArgumentError(argv[1]);
			return ERR;
		}

		pos = atoi(argv[3]);
		if (pos == -1) {
			weights = getWHDB(whdb, getWindexWHDB(whdb)-1);
		} else {
			weights = getWHDB(whdb, pos);
			if (weights == NULL) {
				printArgumentError(argv[1]);
				freeWHDB(whdb);
				return ERR;
			}
		}

		printf("Weights of heuristic: [ ");
		for (i=0; i<14; i++) {
			printf("%f ", weights[i]);
		}
		printf("]\n");

		freeWHDB(whdb);
	} else if (!strcmp(argv[1], "-ia")) {
		char buffer[2000], file_name[23], group[3], team[3], password[3], day[3], month[3], alias[31];
		char file[29];
		struct whdb *whdb;
		float *weights;
		int number, buffer_aux=0;
		FILE *fp;

		if (argc != 10) {
			printArgumentError(argv[1]);
			return ERR;
		}

		strcpy(file_name, argv[2]);
		strcpy(group, argv[3]);
		strcpy(team, argv[4]);
		number = atoi(argv[5]);
		strcpy(password, argv[6]);
		strcpy(day, argv[7]);
		strcpy(month, argv[8]);
		strcpy(alias, argv[9]);

		if (number < 1 || number > 3) {
			printArgumentError(argv[1]);
			return ERR;
		}

		whdb = loadWHDB(file_name);
		if (whdb == NULL) {
			printArgumentError(argv[1]);
			return ERR;
		}

		weights = getWHDB(whdb, getWindexWHDB(whdb)-number);

		sprintf(file_name, "grupo%spareja%s%d%s%s%s", group, team, number, password, day, month);
		buffer_aux += sprintf(buffer+buffer_aux, "(defpackage :%s            ; se declara un paquete lisp que usa common-lisp\n", file_name);
		buffer_aux += sprintf(buffer+buffer_aux, "  (:use :common-lisp :mancala)                 ; y mancala, y exporta la funcion de evaluacion\n");
		buffer_aux += sprintf(buffer+buffer_aux, "  (:export :heuristica :*alias*))              ; heuristica y un alias para el torneo\n\n\n");
		buffer_aux += sprintf(buffer+buffer_aux, "(in-package %s)\n\n", file_name);
		buffer_aux += sprintf(buffer+buffer_aux, "(defun heuristica (estado)\n");
		buffer_aux += sprintf(buffer+buffer_aux, "  (let ((tablero (estado-tablero estado))\n");
		buffer_aux += sprintf(buffer+buffer_aux, "        (mi-jugador (estado-lado-sgte-jugador estado))\n");
		buffer_aux += sprintf(buffer+buffer_aux, "        (oponente (lado-contrario (estado-lado-sgte-jugador estado))))\n");
		buffer_aux += sprintf(buffer+buffer_aux, "    (if (juego-terminado-p estado)\n");
		buffer_aux += sprintf(buffer+buffer_aux, "        (if (> (suma-fila tablero mi-jugador)\n");
		buffer_aux += sprintf(buffer+buffer_aux, "               (suma-fila tablero oponente))\n");
		buffer_aux += sprintf(buffer+buffer_aux, "            361\n");
		buffer_aux += sprintf(buffer+buffer_aux, "          -361)\n");
		buffer_aux += sprintf(buffer+buffer_aux, "      (+ (* (get-fichas tablero mi-jugador 0) %.2f)\n", weights[0]);
		buffer_aux += sprintf(buffer+buffer_aux, "         (* (get-fichas tablero mi-jugador 1) %.2f)\n", weights[1]);
		buffer_aux += sprintf(buffer+buffer_aux, "         (* (get-fichas tablero mi-jugador 2) %.2f)\n", weights[2]);
		buffer_aux += sprintf(buffer+buffer_aux, "         (* (get-fichas tablero mi-jugador 3) %.2f)\n", weights[3]);
		buffer_aux += sprintf(buffer+buffer_aux, "         (* (get-fichas tablero mi-jugador 4) %.2f)\n", weights[4]);
		buffer_aux += sprintf(buffer+buffer_aux, "         (* (get-fichas tablero mi-jugador 5) %.2f)\n", weights[5]);
		buffer_aux += sprintf(buffer+buffer_aux, "         (* (get-fichas tablero mi-jugador 6) %.2f)\n", weights[6]);
		buffer_aux += sprintf(buffer+buffer_aux, "         (* (get-fichas tablero oponente 0) %.2f)\n", weights[7]);
		buffer_aux += sprintf(buffer+buffer_aux, "         (* (get-fichas tablero oponente 1) %.2f)\n", weights[8]);
		buffer_aux += sprintf(buffer+buffer_aux, "         (* (get-fichas tablero oponente 2) %.2f)\n", weights[9]);
		buffer_aux += sprintf(buffer+buffer_aux, "         (* (get-fichas tablero oponente 3) %.2f)\n", weights[10]);
		buffer_aux += sprintf(buffer+buffer_aux, "         (* (get-fichas tablero oponente 4) %.2f)\n", weights[11]);
		buffer_aux += sprintf(buffer+buffer_aux, "         (* (get-fichas tablero oponente 5) %.2f)\n", weights[12]);
		buffer_aux += sprintf(buffer+buffer_aux, "         (* (get-fichas tablero oponente 6) %.2f)))))\n\n", weights[13]);
		buffer_aux += sprintf(buffer+buffer_aux, "(defvar *alias* '|%s|) ; alias que aparecera en el ranking\n", alias);

		sprintf(file, "cl/%s.cl", file_name);
		fp = fopen(file, "w");
		fprintf(fp, "%s", buffer);
		fclose(fp);

		freeWHDB(whdb);

		printf("File %s.cl generated successfully!\n", file_name);
	} else {
		printOptionError();
	}

	return OK;
}

// All of the tests below are performed with depth 2. For other depths, other tests
// must be created.
short testAgainstRegular (heuristic htest, float *heur_values) {
	struct result *winner;
	heuristic hreg = heuristicIARegular;

	winner = playMancala(0, htest, hreg, 2, 2, heur_values, NULL, 0);
	if (winner->winner != 1) {
		free(winner);
		return F;
	}
	free(winner);

	winner = playMancala(1, htest, hreg, 2, 2, heur_values, NULL, 0);
	if (winner->winner != 1) {
		free(winner);
		return F;
	}
	free(winner);

	return T;
}

short testAgainstGood (heuristic htest, float *heur_values) {
	struct result *winner;
	heuristic hreg = heuristicIABuena;

	winner = playMancala(0, htest, hreg, 2, 3, heur_values, NULL, 0);
	if (winner->winner != 1) {
		free(winner);
		return F;
	}
	free(winner);

	winner = playMancala(1, htest, hreg, 2, 3, heur_values, NULL, 0);
	if (winner->winner != 1) {
		free(winner);
		return F;
	}
	free(winner);

	return T;
}

// Heuristics generated with heuristicWeight
short testWHAgainstWH (short player_turn, float *heur_values1, float *heur_values2) {
	struct result *winner;
	heuristic h = heuristicWeight;
	short ret;

	winner = playMancala(0, h, h, 2, 2, heur_values1, heur_values2, 0);
	if (winner == NULL) {
		return ERR;
	}

	ret = winner->winner;
	free(winner);
	return ret;
}

// WHDB = weighted heuristics (generated through a 14-size vector of weights and the
// function heuristicWeight) data base. Returns win rate.
float testWHAgainstWHDB (float *heur_values, struct whdb *whdb) {
	short i, wins, ret;
	float win_rate;

	if (whdb->cur_heur == 0) {
		return 0.0;
	}

	for (i=0, wins=0; i<whdb->cur_heur; i++) {
		ret = testWHAgainstWH(0, heur_values, whdb->weights[i]);
		if (ret == 1) {
			wins++;
		}

		ret = testWHAgainstWH(1, heur_values, whdb->weights[i]);
		if (ret == 1) {
			wins++;
		}
	}

	win_rate = (wins * 100.0) / (whdb->cur_heur * 2.0);

	return win_rate;
}

// Assumes that a WHDB exists in 'filename' and tests each of the heuristics with
// the rest. It generates (n+4)^2 games where n is the number of heuristics, so be
// careful with the size of the database if you don't want to murder your computer.
// Prints the weighted heuristic as a vector of size 14 (pointer).
short testSimpleWHDB (char *filename) {
	int i, cur_heur;
	float *heur_max, *heur_aux, heur_ret[14];
	float win_rate = -1.0, win_rate_aux;
	struct whdb *whdb;
	heuristic h = heuristicWeight;

	whdb = loadWHDB(filename);
	if (whdb == NULL) {
		return ERR;
	}

	cur_heur = getNumWHDB(whdb);
	for (i=0; i<cur_heur; i++) {
		//if (i%500==0) printf("%d: little control.\n", i);

		heur_aux = getWHDB(whdb, i);

		if (!testAgainstRegular(h, heur_aux)) {
			continue;
		}

		win_rate_aux = testWHAgainstWHDB(heur_aux, whdb);
		if (win_rate_aux > win_rate) {
			win_rate = win_rate_aux;
			heur_max = heur_aux;
		}
	}

	for (i=0; i<14; i++) {
		heur_ret[i] = heur_max[i];
	}

	freeWHDB(whdb);

	printf("Champion: [ ");
	for (i=0; i<14; i++) {
		printf("%f ", heur_ret[i]);
	}
	printf("]\nWin rate of champion: %f\n", win_rate);

	return OK;
}

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

// Assumes we have enabled testing on whdb
short iterationUpdateWHDB (struct whdb *whdb, generateWH gh, float *init_weights, float min_h, float max_h) {
	float *test_weights, win_rate;
	heuristic h = heuristicWeight;

	if (whdb == NULL) {
		return ERR;
	}

	test_weights = gh(init_weights, min_h, max_h);
	if (test_weights == NULL) {
		return ERR;
	}

	if (testAgainstRegular(h, test_weights) && testAgainstGood(h, test_weights)) {
		win_rate = testWHAgainstWHDB (test_weights, whdb);
		if (win_rate > getWinRateWHDB(whdb)) {
			printf("Éxito! Heurística con %f guardada en posicion %d.\n", win_rate, whdb->win_index);
			updateAfterTestWHDB(whdb, test_weights);
		}
	}

	free(test_weights);
	return OK;

}

// Assumes there is a WHDB in 'filename', if there isn't it must be created with
// the function 'buildRandomWHDB'
short updateWHDB (char *filename, char *generateWHChoice, int iterations, float min_h, float max_h) {
	struct whdb *whdb;
	int i;
	short ret;
	generateWH gh;
	float *init_weights;

	if (generateWHChoice == NULL || filename == NULL) {
		return ERR;
	}

	whdb = loadWHDB(filename);
	if (whdb == NULL) {
		return ERR;
	}

	if (!strcmp(generateWHChoice, "random")) {
		gh = generateRandomWH;
		init_weights = NULL;
	} else if (!strcmp(generateWHChoice, "genetic")) {
		gh = generateSimilarWH;
		i = rand() % (int)getNumWHDB(whdb);
		init_weights = getWHDB(whdb, i);
	} else {
		return ERR;
	}

	if (!isEnabledTestingWHDB(whdb)) {
		enableTestingWHDB(whdb, INIT_WR);
	}

	for (i=0; i<iterations; i++) {
		ret = iterationUpdateWHDB(whdb, gh, init_weights, min_h, max_h);
		if (ret == ERR) {
			break;
		}
	}

	ret = saveWHDB(whdb, filename);
	if (ret == ERR) {
		return ERR;
	}

	return OK;
}



/**********************************/
/*********** DEPRECATED ***********/
/**********************************/

/* Already tested. Results:
*    - The best heuristic had 49.95% win ratio. That means there was not a clear
*      winner, and therefore assigning similar weights to the seeds does not work.
*      That suggests we will have to assign different weights with high variance
*      in order to find the best heuristics.
*    - That said, it took around 32 hours to perform the test, which executed
*      between 10^8 and 10^9 games (3-30 million games an hour).

// Recursive function to generate every possible 14-size vector with 1 and -1
short genSimpleWH (struct whdb *whdb, short *values, short index) {
	short ret;

	values[index] = 1;

	if (index == 13) {
		addWHDB(whdb, values);
	} else {
		ret = genSimpleWH(whdb, values, index+1);
		if (ret == ERR) {
			return ERR;
		}
	}

	values[index] = -1;

	if (index == 13) {
		addWHDB(whdb, values);
	} else {
		ret = genSimpleWH(whdb, values, index+1);
		if (ret == ERR) {
			return ERR;
		}
	}

	return OK;
}

// Creates a file in which to store the data base generated by the previous function
short createSimpleWHDB(char *filename) {
	struct whdb *whdb;
	short ret, values[14];

	whdb = loadWHDB(filename);
	if (whdb == NULL) {
		return ERR;
	}

	ret = genSimpleWH(whdb, values, 0);
	if (ret == ERR) {
		return ERR;
	}

	ret = saveWHDB(whdb, filename);
	if (ret == ERR) {
		return ERR;
	}

	return OK;
}
*/

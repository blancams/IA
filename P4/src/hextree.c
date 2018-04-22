#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#include "globals.h"
#include "hextree.h"

struct hextree_node* createHextree() {
	short i;
	struct hextree_node *root;

	root = (struct hextree_node*) malloc(sizeof(struct hextree_node));

	if (root == NULL) {
		return NULL;
	}

	for (i=0; i<6; i++) {
		root->children[i] = NULL;
	}

	root->index = -1;

	return root;
}

void freeHextree(struct hextree_node *node) {
	short i;

	if (node == NULL) {
		return;
	}

	for (i=0; i<6; i++) {
		if (node->children[i] != NULL) {
			freeHextree(node->children[i]);
		}
	}

	free(node);

	return;
}

short addHextreeNode(struct hextree_node *parent, short position, short index) {
	short i;
	struct hextree_node *child;

	if (parent == NULL || parent->children[position] != NULL) {
		return ERR;
	}

	child = (struct hextree_node*) malloc(sizeof(struct hextree_node));

	if (child == NULL) {
		return ERR;
	}

	for (i=0; i<6; i++) {
		child->children[i] = NULL;
	}

	parent->children[position] = child;

	child->index = index;

	return OK;
}

short isHextreeLeaf(struct hextree_node *node) {
	short i;

	if (node == NULL) {
		return F;
	}

	for (i=0; i<6; i++) {
		if (node->children[i] != NULL) {
			return F;
		}
	}

	return T;
}

struct hextree_node* getChildren(struct hextree_node *parent, short position) {

	return parent->children[position];
}

short getIndex(struct hextree_node *node) {
	if (node == NULL) {
		return ERR;
	}

	return node->index;
}

short setValue(struct hextree_node *node, short value) {
	if (node == NULL) {
		return ERR;
	}

	node->value = value;

	return OK;
}

struct hextree_node* negaMax(struct hextree_node *node, short color) {
	short i;
	struct hextree_node *node_aux;

	if (node == NULL) {
		return NULL;
	}

	if (isHextreeLeaf(node)) {
		node->value *= color;
		return node;
	}

	node->value = SHRT_MIN;
	for (i=0; i<6; i++) {
		if (node->children[i] == NULL) {
			continue;
		}
		node_aux = negaMax(node->children[i], -1*color);
		//printf("Valores: %d, %d, %p\n", node->value, node_aux->value, node_aux);
		node_aux->value *= -1;
		if (node_aux->value > node->value) {
			node->value = node_aux->value;
			node->index = node_aux->index;
		}
	}

	return node;
}

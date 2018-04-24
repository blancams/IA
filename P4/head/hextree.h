#ifndef _HEXTREE_H
#define _HEXTREE_H

struct hextree_node {
	float value;
	short index;
	struct hextree_node *children[6];
};

struct hextree_node* createHextree();
void freeHextree(struct hextree_node *node);
short addHextreeNode(struct hextree_node *parent, short position, short index);
short isHextreeLeaf(struct hextree_node *node);
struct hextree_node* getChildren(struct hextree_node *parent, short position);
short getIndex(struct hextree_node *node);
short setValue(struct hextree_node *node, float value);
struct hextree_node* negaMax(struct hextree_node *node, short color);

#endif

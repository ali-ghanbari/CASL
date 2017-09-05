#include <stdlib.h>
#include <string.h>
#include "centipd.h"

centipede_node_t* construct_centipede ();
centipede_node_t* add_node (centipede_node_t* head, const void* data, size_t szdata);
void* get_node (centipede_node_t* head, int index, void* data, size_t szdata);
int get_count (centipede_node_t* head);
centipede_node_t* destruct_centipede (centipede_node_t* head);

centipede_node_t* construct_centipede () {
	return NULL;
}

centipede_node_t* add_node (centipede_node_t* head, const void* data, size_t szdata) {
	centipede_node_t* node;
	
	node = malloc (sizeof (centipede_node_t));
	if (!node) return 0;
	node->next = head;
	node->szdata = szdata;
	node->data = malloc (szdata);
	if (!(node->data)) {
		free (node);
		return 0;
	}
	memcpy (node->data, data, szdata);
	return node;
}

void* get_node (centipede_node_t* head, int index, void* data, size_t szdata) {
	while (head && index) {
		head = head->next;
		index --;
	}
	if (head) if (head->szdata <= szdata) memcpy (data, head->data, szdata);
	return head;
}

int get_count (centipede_node_t* head) {
	int count = 0;
	
	while (head) {
		head = head->next;
		count ++;
	}
	return count;
}

centipede_node_t* destruct_centipede (centipede_node_t* head) {
	centipede_node_t* p;
	
	while (head) {
		p = head->next;
		if (head->data) free (head->data);
		free (head);
		head = p;
	}
	return 0;
}

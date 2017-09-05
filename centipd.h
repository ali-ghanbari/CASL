#ifndef __CENTIPEDE__
#define __CENTIPEDE__

typedef struct _centipede_node_t {
	void* data;
	size_t szdata;
	struct _centipede_node_t* next;
} centipede_node_t;

extern centipede_node_t* construct_centipede ();
extern centipede_node_t* add_node (centipede_node_t* head, const void* data, size_t szdata);
extern void* get_node (centipede_node_t* head, int index, void* data, size_t szdata);
extern int get_count (centipede_node_t* head);
extern centipede_node_t* destruct_centipede (centipede_node_t* head);

#endif

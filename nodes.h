#ifndef __NODES__
#define __NODES__

typedef enum _opr_type_t {
	ASSIGN_OP,
	IF_THEN_ELSE_OP,
	WHILE_DO_OP,
	SKIP_OP,
	RETURN_OP,
	SEQUENCE_OP,
	ADD_OP,
	SUB_OP,
	MUL_OP,
	DIV_OP,
	AND_OP,
	OR_OP,
	EQ_OP,
	GT_OP,
	LT_OP,
	NOT_OP,
	NEG_OP,
	SNN_OP,
	SMN_OP
} opr_type_t;

typedef enum _node_type_t {
	VAR_NODE,
	NUM_NODE,
	ID_NODE,
	OPR_NODE
} node_type_t;

typedef struct _var_node_t {
	int ndex;
} var_node_t;

typedef struct _num_node_t {
	int value;
} num_node_t;

typedef struct _id_node_t {
	int ndex;
} id_node_t;

typedef struct _opr_node_t {
	opr_type_t opr;
	/*int cnt;*/
	struct _node_t* ops [1]; /*extendable*/
} opr_node_t;

typedef union _node_data_t {
	var_node_t var_node;
	num_node_t num_node;
	id_node_t id_node;
	opr_node_t opr_node;
} node_data_t;

typedef struct _node_t {
	node_type_t type;
	node_data_t data;
} node_t;

#endif

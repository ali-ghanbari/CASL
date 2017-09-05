%{
	#include <stdio.h>
	#include <stdarg.h>
	#include "centipd.h"
	#include "nodes.h"
	
	typedef enum _term_type_t {INTEGER, STATE, UNIT, ILL} term_type_t;
	
	extern int yylex ();
	void yyerror (char* str);
	term_type_t do_type_check (node_t* ast);
	void do_compilation (const char* name, centipede_node_t* dim, centipede_node_t* states,
		node_t* trans_ast, node_t* final_ast);
	void do_code_generation (node_t* ast, FILE* fout, centipede_node_t* id_lst, centipede_node_t* dm_lst);
	
	extern FILE* yyin;
	extern int yylineno;
	extern char* yytext;
	centipede_node_t* machine_dim;
	centipede_node_t* machine_states;
	node_t* trans_root;
	node_t* final_root;
	node_t* new_var_node (int ndex);
	node_t* new_num_node (int val);
	node_t* new_id_node (const char* idtext);
	node_t* new_opr_node (opr_type_t opr, int cnt, ...);
%}

%union {
	int value;
	int ndex;
	node_t* node;
};

%token MACHINE DIM STATES TRANSITION FINAL ENDMACHINE IF THEN ELSE ENDIF WHILE DO ENDWHILE SKIP RETURN SNN SMN IDENTIFIER
%token <value> NUMERAL
%token <ndex> VARIABLE

%type <node> expr stmt_lst stmt func

%start machine

%left ';'
%left '|'
%left '&'
%left '=' '<' '>'
%left '-' '+'
%left '*' '/'
%nonassoc NEG NOT

%%

machine:
	MACHINE IDENTIFIER DIM int_tuple STATES iden_tuple TRANSITION func FINAL expr ENDMACHINE
		{trans_root = $8; final_root = $10;}
	|
	;
int_tuple:
	NUMERAL 					{machine_dim = add_node (machine_dim, &yylval, sizeof (yylval));}
	| int_tuple ',' NUMERAL 	{machine_dim = add_node (machine_dim, &yylval, sizeof (yylval));}
	;
iden_tuple:
	IDENTIFIER 					{machine_states = add_node (machine_states, yytext, strlen (yytext) + 1);}
	| iden_tuple ',' IDENTIFIER {machine_states = add_node (machine_states, yytext, strlen (yytext) + 1);}
	;
func:
	stmt 					{$$ = $1;}
	;

stmt:
	'{' stmt_lst '}' 						{$$ = $2;}
	| expr ':' '=' expr 					{$$ = new_opr_node (ASSIGN_OP, 2, $1, $4);}
	| IF expr THEN stmt ELSE stmt ENDIF 	{$$ = new_opr_node (IF_THEN_ELSE_OP, 3, $2, $4, $6);}
	| WHILE expr DO stmt ENDWHILE 			{$$ = new_opr_node (WHILE_DO_OP, 2, $2, $4);}
	| SKIP 									{$$ = new_opr_node (SKIP_OP, 0);}
	| RETURN expr 							{$$ = new_opr_node (RETURN_OP, 1, $2);}
	;
	
stmt_lst:
	stmt_lst ';' stmt 		{$$ = new_opr_node (SEQUENCE_OP, 2, $1, $3);}
	| stmt 					{$$ = $1;}
	;
	
expr:
	expr '+' expr 			{$$ = new_opr_node (ADD_OP, 2, $1, $3);}
	| expr '-' expr 		{$$ = new_opr_node (SUB_OP, 2, $1, $3);}
	| expr '*' expr 		{$$ = new_opr_node (MUL_OP, 2, $1, $3);}
	| expr '/' expr 		{$$ = new_opr_node (DIV_OP, 2, $1, $3);}
	| expr '&' expr 		{$$ = new_opr_node (AND_OP, 2, $1, $3);}
	| expr '|' expr 		{$$ = new_opr_node (OR_OP, 2, $1, $3);}
	| expr '=' expr 		{$$ = new_opr_node (EQ_OP, 2, $1, $3);}
	| expr '>' expr 		{$$ = new_opr_node (GT_OP, 2, $1, $3);}
	| expr '<' expr 		{$$ = new_opr_node (LT_OP, 2, $1, $3);}
	| '(' expr ')' 			{$$ = $2;}
	| '~' expr %prec NOT 	{$$ = new_opr_node (NOT_OP, 1, $2);}
	| '-' expr %prec NEG 	{$$ = new_opr_node (NEG_OP, 1, $2);}
	| SNN '(' expr ')' 		{$$ = new_opr_node (SNN_OP, 1, $3);}
	| SMN '(' expr ')' 		{$$ = new_opr_node (SMN_OP, 1, $3);}
	| VARIABLE 				{$$ = new_var_node ($1);}
	| NUMERAL 				{$$ = new_num_node ($1);}
	| IDENTIFIER 			{$$ = new_id_node (yytext);}
	;

%%

node_t* new_var_node (int ndex) {
	node_t* node;
	
	node = malloc (sizeof (node_t));
	if (!node) {
		yyerror ("out of memory!");
		return NULL;
	}
	node->type = VAR_NODE;
	(node->data).var_node.ndex = ndex;
	return node;
}

node_t* new_num_node (int val) {
	node_t* node;
	
	node = malloc (sizeof (node_t));
	if (!node) {
		yyerror ("out of memory!");
		return NULL;
	}
	node->type = NUM_NODE;
	(node->data).num_node.value = val;
	return node;
}

node_t* new_id_node (const char* idtext) {
	char iden_str [256];
	node_t* node;
	int i;
	
	node = malloc (sizeof (node_t));
	if (!node) {
		yyerror ("out of memory!");
		return NULL;
	}
	node->type = ID_NODE;
	for (i = 0; i < get_count (machine_states); i ++) {
		if (!get_node (machine_states, i, iden_str, 256)) {
			yyerror ("unexpected error!");
			return NULL;
		}
		if (!strcmp (iden_str, idtext)) {
			(node->data).id_node.ndex = i;
			return node;
		}
	}
	free (node);
	printf ("undefined symbol \'%s\'", idtext);
	yyerror ("");
	return NULL;
}

node_t* new_opr_node (opr_type_t opr, int cnt, ...) {
	const size_t mem_sz = sizeof (node_t) + (cnt - 1) * sizeof (node_t*);
	node_t* node;
	va_list oprs;
	int i;
	
	node = malloc (mem_sz);
	if (!node) {
		yyerror ("unexpected error!");
		return NULL;
	}
	node->type = OPR_NODE;
	(node->data).opr_node.opr = opr;
	/*(node->data).opr_node.cnt = cnt;*/
	va_start (oprs, cnt);
	for (i = 0; i < cnt; i ++)
		(node->data).opr_node.ops [i] = va_arg (oprs, node_t*);
	va_end (oprs);
	return node;
}

term_type_t do_type_check (node_t* ast) {
	switch (ast->type) {
		case VAR_NODE:
		case NUM_NODE:
			return INTEGER;
		case ID_NODE:
			return STATE;
	}
	switch ((ast->data).opr_node.opr) {
		case ASSIGN_OP:
			if (((ast->data).opr_node.ops [0])->type != VAR_NODE ||
				do_type_check ((ast->data).opr_node.ops [1]) != INTEGER) {
				yyerror ("type mismatch in assignment!");
				return ILL;
			}
			return UNIT;
		case IF_THEN_ELSE_OP:
			if (do_type_check ((ast->data).opr_node.ops [0]) != INTEGER ||
				do_type_check ((ast->data).opr_node.ops [1]) != UNIT	||
				do_type_check ((ast->data).opr_node.ops [2]) != UNIT) {
				yyerror ("type mismatch in decision!");
				return ILL;
			}
			return UNIT;
		case WHILE_DO_OP:
			if (do_type_check ((ast->data).opr_node.ops [0]) != INTEGER ||
				do_type_check ((ast->data).opr_node.ops [1]) != UNIT) {
				yyerror ("type mismatch in while loop!");
				return ILL;
			}
			return UNIT;
		case SKIP_OP: return UNIT;
		case RETURN_OP:
			if (do_type_check ((ast->data).opr_node.ops [0]) != STATE) {
				yyerror ("type mismatch in return statement!");
				return ILL;
			}
			return UNIT;
		case SEQUENCE_OP:
			if (do_type_check ((ast->data).opr_node.ops [0]) != UNIT ||
				do_type_check ((ast->data).opr_node.ops [1]) != UNIT) {
				yyerror ("type mismatch while composing a sequence!");
				return ILL;
			}
			return UNIT;
		case ADD_OP:
			if (do_type_check ((ast->data).opr_node.ops [0]) != INTEGER ||
				do_type_check ((ast->data).opr_node.ops [1]) != INTEGER) {
				yyerror ("type mismatch while addition!");
				return ILL;
			}
			return INTEGER;
		case SUB_OP:
			if (do_type_check ((ast->data).opr_node.ops [0]) != INTEGER ||
				do_type_check ((ast->data).opr_node.ops [1]) != INTEGER) {
				yyerror ("type mismatch while subtraction!");
				return ILL;
			}
			return INTEGER;
		case MUL_OP:
			if (do_type_check ((ast->data).opr_node.ops [0]) != INTEGER ||
				do_type_check ((ast->data).opr_node.ops [1]) != INTEGER) {
				yyerror ("type mismatch while multiplication!");
				return ILL;
			}
			return INTEGER;
		case DIV_OP:
			if (do_type_check ((ast->data).opr_node.ops [0]) != INTEGER ||
				do_type_check ((ast->data).opr_node.ops [1]) != INTEGER) {
				yyerror ("type mismatch while division!");
				return ILL;
			}
			return INTEGER;
		case AND_OP:
			if (do_type_check ((ast->data).opr_node.ops [0]) != INTEGER ||
				do_type_check ((ast->data).opr_node.ops [1]) != INTEGER) {
				yyerror ("type mismatch while conjunction!");
				return ILL;
			}
			return INTEGER;
		case OR_OP:
			if (do_type_check ((ast->data).opr_node.ops [0]) != INTEGER ||
				do_type_check ((ast->data).opr_node.ops [1]) != INTEGER) {
				yyerror ("type mismatch while disjunction!");
				return ILL;
			}
			return INTEGER;
		case EQ_OP:
			if (do_type_check ((ast->data).opr_node.ops [0])
				!= do_type_check ((ast->data).opr_node.ops [1])) {
				yyerror ("type mismatch while comparison (EQ)!");
				return ILL;
			}
			return INTEGER;
		case GT_OP:
			if (do_type_check ((ast->data).opr_node.ops [0]) != INTEGER ||
				do_type_check ((ast->data).opr_node.ops [1]) != INTEGER) {
				yyerror ("type mismatch while comparison (GT)!");
				return ILL;
			}
			return INTEGER;
		case LT_OP:
			if (do_type_check ((ast->data).opr_node.ops [0]) != INTEGER ||
				do_type_check ((ast->data).opr_node.ops [1]) != INTEGER) {
				yyerror ("type mismatch while comparison (LT)!");
				return ILL;
			}
			return INTEGER;
		case NOT_OP:
			if (do_type_check ((ast->data).opr_node.ops [0]) != INTEGER) {
				yyerror ("type mismatch while logical negation!");
				return ILL;
			}
			return INTEGER;
		case NEG_OP:
			if (do_type_check ((ast->data).opr_node.ops [0]) != INTEGER) {
				yyerror ("type mismatch while arithmetic negation!");
				return ILL;
			}
			return INTEGER;
		case SNN_OP:
			if (do_type_check ((ast->data).opr_node.ops [0]) != INTEGER) {
				yyerror ("type mismatch while computing SNN!");
				return ILL;
			}
			return STATE;
		case SMN_OP:
			if (do_type_check ((ast->data).opr_node.ops [0]) != INTEGER) {
				yyerror ("type mismatch while computing SMN!");
				return ILL;
			}
			return STATE;
		default:
			return ILL;
	}
}

void do_compilation (const char* name, centipede_node_t* dim, centipede_node_t* states,
	node_t* trans_ast, node_t* final_ast) {
	char str [34];
	FILE* fout;
	int idat;
	int i;

	if (!trans_ast || !final_ast) return;
	if (do_type_check (trans_ast) != UNIT) {
		yyerror ("unable to compile the machine due to an ill-typedness in transition clause!");
		return;
	}
	if (do_type_check (final_ast) != INTEGER) {
		yyerror ("unable to compile the machine due to an ill-typedness in final clause!");
		return;
	}
	if (strlen (name) > 32) {
		yyerror ("machine name is too long!");
		return;
	}
	sprintf (str, "%s.c", name);
	fout = fopen (str, "wb");
	if (!fout) {
		yyerror ("fatal: unable to create output file.");
		return;
	}
	fprintf (fout, "typedef enum _state_t {");
	for (i = 0; i < get_count (states) - 1; i ++) {		
		if(! get_node (states, i, str, 34)) {
			yyerror ("too long name for an identifier!");
			fclose (fout);
			return;
		}
		fprintf (fout, "%s, ", str);
	}
	if(! get_node (states, get_count (states) - 1, str, 34)) {
			yyerror ("too long name for an identifier!");
			fclose (fout);
			return;
		}
	fprintf (fout, "%s} state_t;\n", str);
	fprintf (fout, "typedef state_t array_t ");
	for (i = get_count (dim) - 1; i >= 0; i --) {
		get_node (dim, i, &idat, sizeof (idat));
		fprintf (fout, "[%d]", idat);
	}
	fprintf (fout, ";\n\n");
	fprintf (fout, "static int turn = 0;\n");
	fprintf (fout, "static array_t ca [2] = {{0}, {0}};\n");
	fprintf (fout, "static const int base [3] = {-1, 0, 1};\n");
	fprintf (fout, "static int var_pool [16] = {0};\n\n");
	fprintf (fout, "void compute (array_t* out);\n");
	fprintf (fout, "int final ();\n");
	fprintf (fout, "void do_transition ();\n");
	fprintf (fout, "state_t transition (");
	for (i = 0; i < get_count (dim) - 1; i ++)
		fprintf (fout, "int c%d, ", i);
	fprintf (fout, "int c%d);\n", get_count (dim) - 1);
	fprintf (fout, "state_t get_cell (");
	for (i = 0; i < get_count (dim) - 1; i ++)
		fprintf (fout, "int c%d, ", i);
	fprintf (fout, "int c%d);\n", get_count (dim) - 1);
	fprintf (fout, "void set_cell (");
	for (i = 0; i < get_count (dim) - 1; i ++)
		fprintf (fout, "int c%d, ", i);
	fprintf (fout, "int c%d, state_t st);\n", get_count (dim) - 1);
	fprintf (fout, "state_t snn (");
	for (i = 0; i < get_count (dim) - 1; i ++)
		fprintf (fout, "int c%d, ", i);
	fprintf (fout, "int c%d, int ndex);\n", get_count (dim) - 1);
	fprintf (fout, "state_t smn (");
	for (i = 0; i < get_count (dim) - 1; i ++)
		fprintf (fout, "int c%d, ", i);
	fprintf (fout, "int c%d, int ndex);\n", get_count (dim) - 1);
	fprintf (fout, "\n");
	fprintf (fout, "state_t get_cell (");
	for (i = 0; i < get_count (dim) - 1; i ++)
		fprintf (fout, "int c%d, ", i);
	fprintf (fout, "int c%d) {\n", get_count (dim) - 1);
	for (i = 0; i < get_count (dim); i ++) {
		get_node (dim, (get_count (dim) - 1) - i, &idat, sizeof (idat));
		fprintf (fout, "\tc%d = c%d < 0 ? c%d + %d : c%d %% %d;\n", i, i, i, idat, i, idat);
	}
	fprintf (fout, "\n");
	fprintf (fout, "\treturn ca [turn] ");
	for (i = 0; i < get_count (dim); i ++)
		fprintf (fout, "[c%d]", i);
	fprintf (fout, ";\n}\n");
	fprintf (fout, "void set_cell (");
	for (i = 0; i < get_count (dim) - 1; i ++)
		fprintf (fout, "int c%d, ", i);
	fprintf (fout, "int c%d, state_t st) {\n", get_count (dim) - 1);
	for (i = 0; i < get_count (dim); i ++) {
		get_node (dim, (get_count (dim) - 1) - i, &idat, sizeof (idat));
		fprintf (fout, "\tc%d = c%d < 0 ? c%d + %d : c%d %% %d;\n", i, i, i, idat, i, idat);
	}
	fprintf (fout, "\n");
	fprintf (fout, "\tca [(turn + 1) % 2] ");
	for (i = 0; i < get_count (dim); i ++)
		fprintf (fout, "[c%d]", i);
	fprintf (fout, " = st;\n}\n");
	fprintf (fout, "state_t snn (");
	for (i = 0; i < get_count (dim) - 1; i ++)
		fprintf (fout, "int c%d, ", i);
	fprintf (fout, "int c%d, int ndex) {\n", get_count (dim) - 1);
	fprintf (fout, "\tint n [%d] = {", get_count (dim));
	for (i = 0; i < get_count (dim) - 1; i ++)
		fprintf (fout, "c%d, ", i);
	fprintf (fout, "c%d};\n\n", get_count (dim) - 1);
	fprintf (fout, "\tn [ndex / 3] += base [ndex % 3];\n");
	fprintf (fout, "\treturn get_cell (");
	for (i = 0; i < get_count (dim) - 1; i ++)
		fprintf (fout, "n [%d], ", i);
	fprintf (fout, "n [%d]);\n", get_count (dim) - 1);
	fprintf (fout, "}\n");
	fprintf (fout, "state_t smn (");
	for (i = 0; i < get_count (dim) - 1; i ++)
		fprintf (fout, "int c%d, ", i);
	fprintf (fout, "int c%d, int ndex) {\n", get_count (dim) - 1);
	fprintf (fout, "\tint n [%d] = {", get_count (dim));
	for (i = 0; i < get_count (dim) - 1; i ++)
		fprintf (fout, "c%d, ", i);
	fprintf (fout, "c%d};\n", get_count (dim) - 1);
	fprintf (fout, "\tint bvec [%d];\n\tint len;\n\tint i;\n\n", get_count (dim));
	fprintf (fout, "\tfor (len = 0; ndex >= 3; len ++, ndex /= 3)\n");
	fprintf (fout, "\t\tbvec [len] = base [ndex % 3];\n");
	fprintf (fout, "\tbvec [len] = base [ndex];\n");
	fprintf (fout, "\tfor (i = 0; len >= 0; i ++, len --)\n");
	fprintf (fout, "\t\tn [i] += bvec [len];\n");
	fprintf (fout, "\treturn get_cell (");
	for (i = 0; i < get_count (dim) - 1; i ++)
		fprintf (fout, "n [%d], ", i);
	fprintf (fout, "n [%d]);\n", get_count (dim) - 1);
	fprintf (fout, "}\n");
	fprintf (fout, "state_t transition (");
	for (i = 0; i < get_count (dim) - 1; i ++)
		fprintf (fout, "int c%d, ", i);
	fprintf (fout, "int c%d) {\n", get_count (dim) - 1);
	do_code_generation (trans_ast, fout, states, dim);
	fprintf (fout, "\nreturn (state_t) 0; /*default behavior, to be safe!*/\n}\n");
	fprintf (fout, "void do_transition () {\n");
	for (i = 0; i < get_count (dim); i ++)
		fprintf (fout, "\tint c%d;\n", i);
	fprintf (fout, "\n");
	for (i = 0; i < get_count (dim); i ++) {		
		for (idat = 0; idat < i; idat ++) fprintf (fout, "\t");
		get_node (dim, (get_count (dim) - 1) - i, &idat, sizeof (int));
		fprintf (fout, "\tfor (c%d = 0; c%d < %d; c%d ++)\n", i, i, idat, i);
	}
	for (idat = 0; idat < i; idat ++) fprintf (fout, "\t");
	fprintf (fout, "\tset_cell (");
	for (i = 0; i < get_count (dim); i ++)
		fprintf (fout, "c%d, ", i);
	fprintf (fout, "transition (");
	for (i = 0; i < get_count (dim) - 1; i ++)
		fprintf (fout, "c%d, ", i);
	fprintf (fout, "c%d);\n", get_count (dim) - 1);
	fprintf (fout, "\tturn = (turn + 1) % 2;\n}\n");
	fprintf (fout, "int final () {return ");
	do_code_generation (final_ast, fout, states, dim);
	fprintf (fout, ";}\n");
	fprintf (fout, "void compute (array_t* out) {\n");
	fprintf (fout, "\twhile (!final ()) {do_transition ();}\n");
	fprintf (fout, "\tmemcpy (out, &ca [turn], sizeof (array_t);\n");
	fprintf (fout, "}\n");
	fclose (fout);
}

void do_code_generation (node_t* ast, FILE* fout, centipede_node_t* id_lst, centipede_node_t* dm_lst) {
	char* temp_buf;
	int i;
	
	switch (ast->type) {
		case VAR_NODE:
			fprintf (fout, "var_pool [%d]", (ast->data).var_node.ndex);
			return;
		case NUM_NODE:
			fprintf (fout, "%d", (ast->data).num_node.value);
			return;
		case ID_NODE:
			temp_buf = malloc (33);
			if (!temp_buf) {
				yyerror ("an unexpected error occured due to memory shortage!");
				return;
			}
			if (!get_node (id_lst, (ast->data).id_node.ndex, temp_buf, 33)) {
				yyerror ("too long the identifier name is!");
				free (temp_buf);
				return;
			}
			fprintf (fout, "%s", temp_buf);
			free (temp_buf);
			return;
	}
	switch ((ast->data).opr_node.opr) {
		case ASSIGN_OP:
			do_code_generation ((ast->data).opr_node.ops [0], fout, id_lst, dm_lst);
			fprintf (fout, " = ");
			do_code_generation ((ast->data).opr_node.ops [1], fout, id_lst, dm_lst);
			fprintf (fout, ";\n");
			return;
		case IF_THEN_ELSE_OP:
			fprintf (fout, "if (");
			do_code_generation ((ast->data).opr_node.ops [0], fout, id_lst, dm_lst);
			fprintf (fout, ") {\n");
			do_code_generation ((ast->data).opr_node.ops [1], fout, id_lst, dm_lst);
			fprintf (fout, "} else {\n");
			do_code_generation ((ast->data).opr_node.ops [2], fout, id_lst, dm_lst);
			fprintf (fout, "}\n");
			return;
		case WHILE_DO_OP:
			fprintf (fout, "while (");
			do_code_generation ((ast->data).opr_node.ops [0], fout, id_lst, dm_lst);
			fprintf (fout, ") {\n");
			do_code_generation ((ast->data).opr_node.ops [1], fout, id_lst, dm_lst);
			fprintf (fout, "}\n");
			return;
		case SKIP_OP: return;
		case RETURN_OP:
			fprintf (fout, "return ");
			do_code_generation ((ast->data).opr_node.ops [0], fout, id_lst, dm_lst);
			fprintf (fout, ";\n");
			return;
		case SEQUENCE_OP:
			do_code_generation ((ast->data).opr_node.ops [0], fout, id_lst, dm_lst);
			do_code_generation ((ast->data).opr_node.ops [1], fout, id_lst, dm_lst);
			return;
		case ADD_OP:
			do_code_generation ((ast->data).opr_node.ops [0], fout, id_lst, dm_lst);
			fprintf (fout, " + ");
			do_code_generation ((ast->data).opr_node.ops [1], fout, id_lst, dm_lst);
			return;
		case SUB_OP:
			do_code_generation ((ast->data).opr_node.ops [0], fout, id_lst, dm_lst);
			fprintf (fout, " - ");
			do_code_generation ((ast->data).opr_node.ops [1], fout, id_lst, dm_lst);
			return;
		case MUL_OP:
			do_code_generation ((ast->data).opr_node.ops [0], fout, id_lst, dm_lst);
			fprintf (fout, " * ");
			do_code_generation ((ast->data).opr_node.ops [1], fout, id_lst, dm_lst);
			return;
		case DIV_OP:
			do_code_generation ((ast->data).opr_node.ops [0], fout, id_lst, dm_lst);
			fprintf (fout, " / ");
			do_code_generation ((ast->data).opr_node.ops [1], fout, id_lst, dm_lst);
			return;
		case AND_OP:
			do_code_generation ((ast->data).opr_node.ops [0], fout, id_lst, dm_lst);
			fprintf (fout, " && ");
			do_code_generation ((ast->data).opr_node.ops [1], fout, id_lst, dm_lst);
			return;
		case OR_OP:
			do_code_generation ((ast->data).opr_node.ops [0], fout, id_lst, dm_lst);
			fprintf (fout, " || ");
			do_code_generation ((ast->data).opr_node.ops [1], fout, id_lst, dm_lst);
			return;
		case EQ_OP:
			do_code_generation ((ast->data).opr_node.ops [0], fout, id_lst, dm_lst);
			fprintf (fout, " == ");
			do_code_generation ((ast->data).opr_node.ops [1], fout, id_lst, dm_lst);
			return;
		case GT_OP:
			do_code_generation ((ast->data).opr_node.ops [0], fout, id_lst, dm_lst);
			fprintf (fout, " > ");
			do_code_generation ((ast->data).opr_node.ops [1], fout, id_lst, dm_lst);
			return;
		case LT_OP:
			do_code_generation ((ast->data).opr_node.ops [0], fout, id_lst, dm_lst);
			fprintf (fout, " < ");
			do_code_generation ((ast->data).opr_node.ops [1], fout, id_lst, dm_lst);
			return;
		case NOT_OP:
			fprintf (fout, "! ");
			do_code_generation ((ast->data).opr_node.ops [0], fout, id_lst, dm_lst);
			return;
		case NEG_OP:
			fprintf (fout, "- ");
			do_code_generation ((ast->data).opr_node.ops [0], fout, id_lst, dm_lst);
			return;
		case SNN_OP:
			fprintf (fout, "snn (");
			for (i = 0; i < get_count (dm_lst); i ++)
				fprintf (fout, "c%d, ", i);
			do_code_generation ((ast->data).opr_node.ops [0], fout, id_lst, dm_lst);
			fprintf (fout, ")");
			return;
		case SMN_OP:
			fprintf (fout, "smn (");
			for (i = 0; i < get_count (dm_lst); i ++)
				fprintf (fout, "c%d, ", i);
			do_code_generation ((ast->data).opr_node.ops [0], fout, id_lst, dm_lst);
			fprintf (fout, ")");
			return;
		default:
			yyerror ("a malshaped abstract syntax tree encountered!");
			return;
	}	
}

void yyerror (char* str) {
	printf ("%4d: %s\n", yylineno, str);
}

int main (int argc, char* argv []) {
	int i;
	char str [80];
	
	if (argc != 2) {
		printf ("usage: %s file-name\n", argv [0]);
		return 0;
	}
	yyin = fopen (argv [1], "rb");
	if (!yyin) {
		printf ("fatal: unable to open file \'%s\'.\n", argv [1]);
		printf ("usage: %s file-name\n", argv [0]);
		return 0;
	}
	machine_dim = construct_centipede ();
	machine_states = construct_centipede ();
	trans_root = final_root = NULL;
	yyparse ();
	fclose (yyin);
	do_compilation ("hello", machine_dim, machine_states, trans_root, final_root);
	destruct_centipede (machine_dim);
	destruct_centipede (machine_states);
	return 0;
}

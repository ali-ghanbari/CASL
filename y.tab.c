#ifndef lint
static const char yysccsid[] = "@(#)yaccpar	1.9 (Berkeley) 02/21/93";
#endif

#include <stdlib.h>
#include <string.h>

#define YYBYACC 1
#define YYMAJOR 1
#define YYMINOR 9
#define YYPATCH 20100216

#define YYEMPTY        (-1)
#define yyclearin      (yychar = YYEMPTY)
#define yyerrok        (yyerrflag = 0)
#define YYRECOVERING() (yyerrflag != 0)

#define YYPREFIX "yy"

/* compatibility with bison */
#ifdef YYPARSE_PARAM
/* compatibility with FreeBSD */
#ifdef YYPARSE_PARAM_TYPE
#define YYPARSE_DECL() yyparse(YYPARSE_PARAM_TYPE YYPARSE_PARAM)
#else
#define YYPARSE_DECL() yyparse(void *YYPARSE_PARAM)
#endif
#else
#define YYPARSE_DECL() yyparse(void)
#endif /* YYPARSE_PARAM */

extern int YYPARSE_DECL();

#line 2 "bas.y"
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
#line 29 "bas.y"
typedef union {
	int value;
	int ndex;
	node_t* node;
} YYSTYPE;
#line 66 "y.tab.c"
#define MACHINE 257
#define DIM 258
#define STATES 259
#define TRANSITION 260
#define FINAL 261
#define ENDMACHINE 262
#define IF 263
#define THEN 264
#define ELSE 265
#define ENDIF 266
#define WHILE 267
#define DO 268
#define ENDWHILE 269
#define SKIP 270
#define RETURN 271
#define SNN 272
#define SMN 273
#define IDENTIFIER 274
#define NUMERAL 275
#define VARIABLE 276
#define NEG 277
#define NOT 278
#define YYERRCODE 256
static const short yylhs[] = {                           -1,
    0,    0,    5,    5,    6,    6,    4,    3,    3,    3,
    3,    3,    3,    2,    2,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,
};
static const short yylen[] = {                            2,
   11,    0,    1,    3,    1,    3,    1,    3,    4,    7,
    5,    1,    2,    3,    1,    3,    3,    3,    3,    3,
    3,    3,    3,    3,    3,    2,    2,    4,    4,    1,
    1,    1,
};
static const short yydefred[] = {                         0,
    0,    0,    0,    0,    3,    0,    0,    0,    5,    0,
    4,    0,    0,    0,    0,   12,    0,    0,    0,   32,
   31,   30,    0,    0,    0,    0,    0,    7,    0,    6,
    0,    0,    0,    0,    0,   27,    0,   15,    0,   26,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    8,   25,    0,    0,
    0,    0,    0,    0,    0,   18,   19,    0,    0,    0,
    0,   28,   29,   14,    0,    1,    0,   11,    0,   10,
};
static const short yydgoto[] = {                          2,
   27,   37,   28,   29,    6,   10,
};
static const short yysindex[] = {                      -255,
 -261,    0, -243, -259,    0,  -41, -257, -256,    0,  -43,
    0,  -40, -253,  -34,  -34,    0,  -34,  -22,  -16,    0,
    0,    0,  -34,  -40,  -34,  -34,  134,    0, -239,    0,
   82,   51,  327,  -34,  -34,    0,  -45,    0,  273,    0,
  -34,  -34,  -34,  -34,  -34,  -34,  -34,  -34,  -34,  -33,
  -34,  -40,  -40,  298,  306,  -40,    0,    0,  160,  378,
  -35,  -35,  -35,  -38,  -38,    0,    0,  -34,   92, -235,
 -238,    0,    0,    0,  327,    0,  -40,    0, -240,    0,
};
static const short yyrindex[] = {                        33,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    1,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   63,   32,
    7,   16,   41,  -18,   -9,    0,    0,    0,    0,    0,
    0,    0,    0,    0,   22,    0,    0,    0,    0,    0,
};
static const short yygindex[] = {                         0,
  367,    0,   93,    0,    0,    0,
};
#define YYTABLESIZE 451
static const short yytable[] = {                         25,
   13,    1,    8,   48,   23,   25,   48,   47,   49,   46,
   23,   49,    3,   56,    4,    5,    9,   34,   11,   17,
   30,   51,   17,   35,   17,   80,   17,   68,   16,   77,
   78,   16,    2,   16,    0,   16,    0,    0,    0,   17,
   17,   17,   17,   17,   22,    0,    0,   22,   16,   16,
   16,   16,   16,   24,    0,    0,   24,    0,    0,   13,
    0,    0,    0,    0,   22,   22,   22,   22,   22,   20,
    0,    0,   20,   24,   24,   24,   24,   24,   23,   57,
    9,   23,   24,    0,    0,   26,    0,    0,   42,   20,
   20,   26,   48,   47,    0,   46,    0,   49,   23,   23,
   23,   23,   23,   21,    0,   17,   17,    0,    0,    0,
   44,   43,   45,    0,   16,   16,   38,    0,    0,   42,
   21,   21,    0,   48,   47,   13,   46,    0,   49,   42,
   22,   22,    0,   48,   47,    0,   46,    0,   49,   24,
   24,   44,   43,   45,   70,   71,    9,    0,   74,    0,
    0,   44,   43,   45,    0,   20,   20,    0,    0,    0,
    0,    0,    0,    0,   23,   23,    0,    0,    0,   79,
    0,   42,    0,    0,   41,   48,   47,    0,   46,    0,
   49,    0,    0,    0,    0,    0,   21,   21,    0,    0,
    0,   50,    0,   44,   43,   45,    0,   42,    0,    0,
    0,   48,   47,    0,   46,   41,   49,    0,    0,    0,
    0,    0,    0,    0,    0,   41,   12,    7,    0,   44,
   43,   45,   14,    0,    0,    0,   15,    0,    0,   16,
   17,   18,   19,   20,   21,   22,    0,   18,   19,   20,
   21,   22,   17,   17,    0,   17,   17,   17,    0,   17,
   17,   16,   16,    0,   16,   16,   16,   41,   16,   16,
    0,   13,    0,    0,    0,   13,   13,   22,   22,   13,
   22,   22,   22,    0,   22,   22,   24,   24,    0,   24,
   24,   24,    9,   24,   24,    0,    9,    9,    0,    0,
    9,    0,   20,   20,    0,   20,   20,   20,    0,   20,
   20,   23,   23,    0,   23,   23,   23,    0,   23,   23,
   42,    0,    0,   58,   48,   47,    0,   46,   53,   49,
    0,    0,    0,   21,   21,    0,   21,   21,   21,    0,
   21,   21,   44,   43,   45,   42,    0,    0,   72,   48,
   47,    0,   46,   42,   49,   52,   73,   48,   47,    0,
   46,    0,   49,   76,    0,    0,    0,   44,   43,   45,
    0,    0,    0,    0,   42,   44,   43,   45,   48,   47,
    0,   46,    0,   49,    0,    0,    0,    0,    0,    0,
   31,   32,    0,   33,    0,    0,   44,   43,   45,   36,
    0,   39,   40,    0,    0,    0,   41,    0,    0,    0,
   54,   55,    0,    0,    0,    0,    0,   59,   60,   61,
   62,   63,   64,   65,   66,   67,    0,   69,    0,   48,
   47,   41,   46,    0,   49,    0,    0,    0,    0,   41,
    0,    0,    0,    0,   75,    0,    0,   44,   43,   45,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   41,
};
static const short yycheck[] = {                         40,
   44,  257,   44,   42,   45,   40,   42,   43,   47,   45,
   45,   47,  274,   59,  258,  275,  274,   40,  275,   38,
  274,  261,   41,   40,   43,  266,   45,   61,   38,  265,
  269,   41,    0,   43,   -1,   45,   -1,   -1,   -1,   58,
   59,   60,   61,   62,   38,   -1,   -1,   41,   58,   59,
   60,   61,   62,   38,   -1,   -1,   41,   -1,   -1,   59,
   -1,   -1,   -1,   -1,   58,   59,   60,   61,   62,   38,
   -1,   -1,   41,   58,   59,   60,   61,   62,   38,  125,
   59,   41,  123,   -1,   -1,  126,   -1,   -1,   38,   58,
   59,  126,   42,   43,   -1,   45,   -1,   47,   58,   59,
   60,   61,   62,   41,   -1,  124,  125,   -1,   -1,   -1,
   60,   61,   62,   -1,  124,  125,   24,   -1,   -1,   38,
   58,   59,   -1,   42,   43,  125,   45,   -1,   47,   38,
  124,  125,   -1,   42,   43,   -1,   45,   -1,   47,  124,
  125,   60,   61,   62,   52,   53,  125,   -1,   56,   -1,
   -1,   60,   61,   62,   -1,  124,  125,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  124,  125,   -1,   -1,   -1,   77,
   -1,   38,   -1,   -1,  124,   42,   43,   -1,   45,   -1,
   47,   -1,   -1,   -1,   -1,   -1,  124,  125,   -1,   -1,
   -1,   58,   -1,   60,   61,   62,   -1,   38,   -1,   -1,
   -1,   42,   43,   -1,   45,  124,   47,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  124,  260,  259,   -1,   60,
   61,   62,  263,   -1,   -1,   -1,  267,   -1,   -1,  270,
  271,  272,  273,  274,  275,  276,   -1,  272,  273,  274,
  275,  276,  261,  262,   -1,  264,  265,  266,   -1,  268,
  269,  261,  262,   -1,  264,  265,  266,  124,  268,  269,
   -1,  261,   -1,   -1,   -1,  265,  266,  261,  262,  269,
  264,  265,  266,   -1,  268,  269,  261,  262,   -1,  264,
  265,  266,  261,  268,  269,   -1,  265,  266,   -1,   -1,
  269,   -1,  261,  262,   -1,  264,  265,  266,   -1,  268,
  269,  261,  262,   -1,  264,  265,  266,   -1,  268,  269,
   38,   -1,   -1,   41,   42,   43,   -1,   45,  268,   47,
   -1,   -1,   -1,  261,  262,   -1,  264,  265,  266,   -1,
  268,  269,   60,   61,   62,   38,   -1,   -1,   41,   42,
   43,   -1,   45,   38,   47,  264,   41,   42,   43,   -1,
   45,   -1,   47,  262,   -1,   -1,   -1,   60,   61,   62,
   -1,   -1,   -1,   -1,   38,   60,   61,   62,   42,   43,
   -1,   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,   -1,
   14,   15,   -1,   17,   -1,   -1,   60,   61,   62,   23,
   -1,   25,   26,   -1,   -1,   -1,  124,   -1,   -1,   -1,
   34,   35,   -1,   -1,   -1,   -1,   -1,   41,   42,   43,
   44,   45,   46,   47,   48,   49,   -1,   51,   -1,   42,
   43,  124,   45,   -1,   47,   -1,   -1,   -1,   -1,  124,
   -1,   -1,   -1,   -1,   68,   -1,   -1,   60,   61,   62,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  124,
};
#define YYFINAL 2
#ifndef YYDEBUG
#define YYDEBUG 0
#endif
#define YYMAXTOKEN 278
#if YYDEBUG
static const char *yyname[] = {

"end-of-file",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,"'&'",0,"'('","')'","'*'","'+'","','","'-'",0,"'/'",0,0,0,0,0,0,0,0,0,0,
"':'","';'","'<'","'='","'>'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"'{'",
"'|'","'}'","'~'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"MACHINE","DIM","STATES","TRANSITION",
"FINAL","ENDMACHINE","IF","THEN","ELSE","ENDIF","WHILE","DO","ENDWHILE","SKIP",
"RETURN","SNN","SMN","IDENTIFIER","NUMERAL","VARIABLE","NEG","NOT",
};
static const char *yyrule[] = {
"$accept : machine",
"machine : MACHINE IDENTIFIER DIM int_tuple STATES iden_tuple TRANSITION func FINAL expr ENDMACHINE",
"machine :",
"int_tuple : NUMERAL",
"int_tuple : int_tuple ',' NUMERAL",
"iden_tuple : IDENTIFIER",
"iden_tuple : iden_tuple ',' IDENTIFIER",
"func : stmt",
"stmt : '{' stmt_lst '}'",
"stmt : expr ':' '=' expr",
"stmt : IF expr THEN stmt ELSE stmt ENDIF",
"stmt : WHILE expr DO stmt ENDWHILE",
"stmt : SKIP",
"stmt : RETURN expr",
"stmt_lst : stmt_lst ';' stmt",
"stmt_lst : stmt",
"expr : expr '+' expr",
"expr : expr '-' expr",
"expr : expr '*' expr",
"expr : expr '/' expr",
"expr : expr '&' expr",
"expr : expr '|' expr",
"expr : expr '=' expr",
"expr : expr '>' expr",
"expr : expr '<' expr",
"expr : '(' expr ')'",
"expr : '~' expr",
"expr : '-' expr",
"expr : SNN '(' expr ')'",
"expr : SMN '(' expr ')'",
"expr : VARIABLE",
"expr : NUMERAL",
"expr : IDENTIFIER",

};
#endif
#if YYDEBUG
#include <stdio.h>
#endif

/* define the initial stack-sizes */
#ifdef YYSTACKSIZE
#undef YYMAXDEPTH
#define YYMAXDEPTH  YYSTACKSIZE
#else
#ifdef YYMAXDEPTH
#define YYSTACKSIZE YYMAXDEPTH
#else
#define YYSTACKSIZE 500
#define YYMAXDEPTH  500
#endif
#endif

#define YYINITSTACKSIZE 500

int      yydebug;
int      yynerrs;

typedef struct {
    unsigned stacksize;
    short    *s_base;
    short    *s_mark;
    short    *s_last;
    YYSTYPE  *l_base;
    YYSTYPE  *l_mark;
} YYSTACKDATA;

#define YYPURE 0

int      yyerrflag;
int      yychar;
YYSTYPE  yyval;
YYSTYPE  yylval;

/* variables for the parser stack */
static YYSTACKDATA yystack;
#line 105 "bas.y"

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
#line 872 "y.tab.c"
/* allocate initial stack or double stack size, up to YYMAXDEPTH */
static int yygrowstack(YYSTACKDATA *data)
{
    int i;
    unsigned newsize;
    short *newss;
    YYSTYPE *newvs;

    if ((newsize = data->stacksize) == 0)
        newsize = YYINITSTACKSIZE;
    else if (newsize >= YYMAXDEPTH)
        return -1;
    else if ((newsize *= 2) > YYMAXDEPTH)
        newsize = YYMAXDEPTH;

    i = data->s_mark - data->s_base;
    newss = (data->s_base != 0)
          ? (short *)realloc(data->s_base, newsize * sizeof(*newss))
          : (short *)malloc(newsize * sizeof(*newss));
    if (newss == 0)
        return -1;

    data->s_base  = newss;
    data->s_mark = newss + i;

    newvs = (data->l_base != 0)
          ? (YYSTYPE *)realloc(data->l_base, newsize * sizeof(*newvs))
          : (YYSTYPE *)malloc(newsize * sizeof(*newvs));
    if (newvs == 0)
        return -1;

    data->l_base = newvs;
    data->l_mark = newvs + i;

    data->stacksize = newsize;
    data->s_last = data->s_base + newsize - 1;
    return 0;
}

#if YYPURE || defined(YY_NO_LEAKS)
static void yyfreestack(YYSTACKDATA *data)
{
    free(data->s_base);
    free(data->l_base);
    memset(data, 0, sizeof(*data));
}
#else
#define yyfreestack(data) /* nothing */
#endif

#define YYABORT  goto yyabort
#define YYREJECT goto yyabort
#define YYACCEPT goto yyaccept
#define YYERROR  goto yyerrlab

int
YYPARSE_DECL()
{
    int yym, yyn, yystate;
#if YYDEBUG
    const char *yys;

    if ((yys = getenv("YYDEBUG")) != 0)
    {
        yyn = *yys;
        if (yyn >= '0' && yyn <= '9')
            yydebug = yyn - '0';
    }
#endif

    yynerrs = 0;
    yyerrflag = 0;
    yychar = YYEMPTY;
    yystate = 0;

#if YYPURE
    memset(&yystack, 0, sizeof(yystack));
#endif

    if (yystack.s_base == NULL && yygrowstack(&yystack)) goto yyoverflow;
    yystack.s_mark = yystack.s_base;
    yystack.l_mark = yystack.l_base;
    yystate = 0;
    *yystack.s_mark = 0;

yyloop:
    if ((yyn = yydefred[yystate]) != 0) goto yyreduce;
    if (yychar < 0)
    {
        if ((yychar = yylex()) < 0) yychar = 0;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("%sdebug: state %d, reading %d (%s)\n",
                    YYPREFIX, yystate, yychar, yys);
        }
#endif
    }
    if ((yyn = yysindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
#if YYDEBUG
        if (yydebug)
            printf("%sdebug: state %d, shifting to state %d\n",
                    YYPREFIX, yystate, yytable[yyn]);
#endif
        if (yystack.s_mark >= yystack.s_last && yygrowstack(&yystack))
        {
            goto yyoverflow;
        }
        yystate = yytable[yyn];
        *++yystack.s_mark = yytable[yyn];
        *++yystack.l_mark = yylval;
        yychar = YYEMPTY;
        if (yyerrflag > 0)  --yyerrflag;
        goto yyloop;
    }
    if ((yyn = yyrindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
        yyn = yytable[yyn];
        goto yyreduce;
    }
    if (yyerrflag) goto yyinrecovery;

    yyerror("syntax error");

    goto yyerrlab;

yyerrlab:
    ++yynerrs;

yyinrecovery:
    if (yyerrflag < 3)
    {
        yyerrflag = 3;
        for (;;)
        {
            if ((yyn = yysindex[*yystack.s_mark]) && (yyn += YYERRCODE) >= 0 &&
                    yyn <= YYTABLESIZE && yycheck[yyn] == YYERRCODE)
            {
#if YYDEBUG
                if (yydebug)
                    printf("%sdebug: state %d, error recovery shifting\
 to state %d\n", YYPREFIX, *yystack.s_mark, yytable[yyn]);
#endif
                if (yystack.s_mark >= yystack.s_last && yygrowstack(&yystack))
                {
                    goto yyoverflow;
                }
                yystate = yytable[yyn];
                *++yystack.s_mark = yytable[yyn];
                *++yystack.l_mark = yylval;
                goto yyloop;
            }
            else
            {
#if YYDEBUG
                if (yydebug)
                    printf("%sdebug: error recovery discarding state %d\n",
                            YYPREFIX, *yystack.s_mark);
#endif
                if (yystack.s_mark <= yystack.s_base) goto yyabort;
                --yystack.s_mark;
                --yystack.l_mark;
            }
        }
    }
    else
    {
        if (yychar == 0) goto yyabort;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("%sdebug: state %d, error recovery discards token %d (%s)\n",
                    YYPREFIX, yystate, yychar, yys);
        }
#endif
        yychar = YYEMPTY;
        goto yyloop;
    }

yyreduce:
#if YYDEBUG
    if (yydebug)
        printf("%sdebug: state %d, reducing by rule %d (%s)\n",
                YYPREFIX, yystate, yyn, yyrule[yyn]);
#endif
    yym = yylen[yyn];
    if (yym)
        yyval = yystack.l_mark[1-yym];
    else
        memset(&yyval, 0, sizeof yyval);
    switch (yyn)
    {
case 1:
#line 55 "bas.y"
	{trans_root = yystack.l_mark[-3].node; final_root = yystack.l_mark[-1].node;}
break;
case 3:
#line 59 "bas.y"
	{machine_dim = add_node (machine_dim, &yylval, sizeof (yylval));}
break;
case 4:
#line 60 "bas.y"
	{machine_dim = add_node (machine_dim, &yylval, sizeof (yylval));}
break;
case 5:
#line 63 "bas.y"
	{machine_states = add_node (machine_states, yytext, strlen (yytext) + 1);}
break;
case 6:
#line 64 "bas.y"
	{machine_states = add_node (machine_states, yytext, strlen (yytext) + 1);}
break;
case 7:
#line 67 "bas.y"
	{yyval.node = yystack.l_mark[0].node;}
break;
case 8:
#line 71 "bas.y"
	{yyval.node = yystack.l_mark[-1].node;}
break;
case 9:
#line 72 "bas.y"
	{yyval.node = new_opr_node (ASSIGN_OP, 2, yystack.l_mark[-3].node, yystack.l_mark[0].node);}
break;
case 10:
#line 73 "bas.y"
	{yyval.node = new_opr_node (IF_THEN_ELSE_OP, 3, yystack.l_mark[-5].node, yystack.l_mark[-3].node, yystack.l_mark[-1].node);}
break;
case 11:
#line 74 "bas.y"
	{yyval.node = new_opr_node (WHILE_DO_OP, 2, yystack.l_mark[-3].node, yystack.l_mark[-1].node);}
break;
case 12:
#line 75 "bas.y"
	{yyval.node = new_opr_node (SKIP_OP, 0);}
break;
case 13:
#line 76 "bas.y"
	{yyval.node = new_opr_node (RETURN_OP, 1, yystack.l_mark[0].node);}
break;
case 14:
#line 80 "bas.y"
	{yyval.node = new_opr_node (SEQUENCE_OP, 2, yystack.l_mark[-2].node, yystack.l_mark[0].node);}
break;
case 15:
#line 81 "bas.y"
	{yyval.node = yystack.l_mark[0].node;}
break;
case 16:
#line 85 "bas.y"
	{yyval.node = new_opr_node (ADD_OP, 2, yystack.l_mark[-2].node, yystack.l_mark[0].node);}
break;
case 17:
#line 86 "bas.y"
	{yyval.node = new_opr_node (SUB_OP, 2, yystack.l_mark[-2].node, yystack.l_mark[0].node);}
break;
case 18:
#line 87 "bas.y"
	{yyval.node = new_opr_node (MUL_OP, 2, yystack.l_mark[-2].node, yystack.l_mark[0].node);}
break;
case 19:
#line 88 "bas.y"
	{yyval.node = new_opr_node (DIV_OP, 2, yystack.l_mark[-2].node, yystack.l_mark[0].node);}
break;
case 20:
#line 89 "bas.y"
	{yyval.node = new_opr_node (AND_OP, 2, yystack.l_mark[-2].node, yystack.l_mark[0].node);}
break;
case 21:
#line 90 "bas.y"
	{yyval.node = new_opr_node (OR_OP, 2, yystack.l_mark[-2].node, yystack.l_mark[0].node);}
break;
case 22:
#line 91 "bas.y"
	{yyval.node = new_opr_node (EQ_OP, 2, yystack.l_mark[-2].node, yystack.l_mark[0].node);}
break;
case 23:
#line 92 "bas.y"
	{yyval.node = new_opr_node (GT_OP, 2, yystack.l_mark[-2].node, yystack.l_mark[0].node);}
break;
case 24:
#line 93 "bas.y"
	{yyval.node = new_opr_node (LT_OP, 2, yystack.l_mark[-2].node, yystack.l_mark[0].node);}
break;
case 25:
#line 94 "bas.y"
	{yyval.node = yystack.l_mark[-1].node;}
break;
case 26:
#line 95 "bas.y"
	{yyval.node = new_opr_node (NOT_OP, 1, yystack.l_mark[0].node);}
break;
case 27:
#line 96 "bas.y"
	{yyval.node = new_opr_node (NEG_OP, 1, yystack.l_mark[0].node);}
break;
case 28:
#line 97 "bas.y"
	{yyval.node = new_opr_node (SNN_OP, 1, yystack.l_mark[-1].node);}
break;
case 29:
#line 98 "bas.y"
	{yyval.node = new_opr_node (SMN_OP, 1, yystack.l_mark[-1].node);}
break;
case 30:
#line 99 "bas.y"
	{yyval.node = new_var_node (yystack.l_mark[0].ndex);}
break;
case 31:
#line 100 "bas.y"
	{yyval.node = new_num_node (yystack.l_mark[0].value);}
break;
case 32:
#line 101 "bas.y"
	{yyval.node = new_id_node (yytext);}
break;
#line 1198 "y.tab.c"
    }
    yystack.s_mark -= yym;
    yystate = *yystack.s_mark;
    yystack.l_mark -= yym;
    yym = yylhs[yyn];
    if (yystate == 0 && yym == 0)
    {
#if YYDEBUG
        if (yydebug)
            printf("%sdebug: after reduction, shifting from state 0 to\
 state %d\n", YYPREFIX, YYFINAL);
#endif
        yystate = YYFINAL;
        *++yystack.s_mark = YYFINAL;
        *++yystack.l_mark = yyval;
        if (yychar < 0)
        {
            if ((yychar = yylex()) < 0) yychar = 0;
#if YYDEBUG
            if (yydebug)
            {
                yys = 0;
                if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
                if (!yys) yys = "illegal-symbol";
                printf("%sdebug: state %d, reading %d (%s)\n",
                        YYPREFIX, YYFINAL, yychar, yys);
            }
#endif
        }
        if (yychar == 0) goto yyaccept;
        goto yyloop;
    }
    if ((yyn = yygindex[yym]) && (yyn += yystate) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yystate)
        yystate = yytable[yyn];
    else
        yystate = yydgoto[yym];
#if YYDEBUG
    if (yydebug)
        printf("%sdebug: after reduction, shifting from state %d \
to state %d\n", YYPREFIX, *yystack.s_mark, yystate);
#endif
    if (yystack.s_mark >= yystack.s_last && yygrowstack(&yystack))
    {
        goto yyoverflow;
    }
    *++yystack.s_mark = (short) yystate;
    *++yystack.l_mark = yyval;
    goto yyloop;

yyoverflow:
    yyerror("yacc stack overflow");

yyabort:
    yyfreestack(&yystack);
    return (1);

yyaccept:
    yyfreestack(&yystack);
    return (0);
}

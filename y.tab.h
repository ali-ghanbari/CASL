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
typedef union {
	int value;
	int ndex;
	node_t* node;
} YYSTYPE;
extern YYSTYPE yylval;

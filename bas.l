alpha [A-Za-z]
digit [0-9]

%{
	#include <stdlib.h>
	#include "nodes.h"
	#include "y.tab.h"
	
	extern void yyerror (char* str);
%}

int is_reserved (const char* str);

%%

0|[1-9][0-9]*				{
								yylval.value = atoi (yytext);
								return NUMERAL;
							}
{alpha}({alpha}|{digit})+	{
								int res = is_reserved (yytext);
								
								if (!res)
									return IDENTIFIER;
								return res;
							}
[-+*/()=;,]					return *yytext;
['{''}'':''&''|']			return *yytext;
['~''>''<']					return *yytext;
[a-z]						{
								yylval.ndex = *yytext - 'a';
								return VARIABLE;
							}
[ \t\n]						; /*skip whitespace*/
.							yyerror ("invalid character!");

%%

int yywrap () {
	return 1;
}

int is_reserved (const char* str) {
	const static struct {
		char* s;
		int v;
	} res_tab [] = {{"machine", MACHINE},
					{"dim", DIM},
					{"states", STATES},
					{"transition", TRANSITION},
					{"final", FINAL},
					{"endmachine", ENDMACHINE},
					{"if", IF},
					{"then", THEN},
					{"else", ELSE},
					{"endif", ENDIF},
					{"while", WHILE},
					{"do", DO},
					{"endwhile", ENDWHILE},
					{"skip", SKIP},
					{"return", RETURN},
					{"snn", SNN},
					{"smn", SMN},
					{NULL, 0}};

	int i;
	
	for (i = 0; res_tab [i].s; i ++)
		if (!strcmp (res_tab [i].s, str)) return res_tab [i].v;
	return res_tab [i].v;
}

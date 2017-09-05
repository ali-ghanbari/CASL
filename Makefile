a.out : y.tab.o lex.yy.o
	cc centipd.c y.tab.c lex.yy.c
lex.yy.o y.tab.o : lex.yy.c "y.tab.h
y.tab.c "y.tab.h : bas.y
	yacc -d bas.y
lex.yy.c : bas.l
	lex bas.l

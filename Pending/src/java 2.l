%{
#include <iostream>
#include <cstdlib>
#include <string.h>
#include "help.h"
#include "y.tab.h"
void yyerror(const char *s);
using namespace std;
%}

%%



%%
int yywrap()
{
    return 1;
}

void yyerror(const char *s) {
    /*fflush(stdout);*/
    /*printf("\n%*s\n%*s\n", column, "*", column, s);*/
    //fprintf(stderr, "lexical error on line %d: %s\n", yylineno, s);
    char sr[100];
    sprintf(sr,"line %d %s\n", yylineno, s);
    throw logic_error(sr);
}


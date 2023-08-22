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

([/][*][^*]*[*]+([^*/][^*]*[*]+)*[/])|([/][/].*) {yylineno++;}

"(" {yylval.id = strdup(yytext);return LEFTBRACKET;}
")" {yylval.id = strdup(yytext);return RIGHTBRACKET;}
"{" {yylval.id = strdup(yytext);return LEFTCURLYBRACKET;}
"}" {yylval.id = strdup(yytext);return RIGHTCURLYBRACKET;}
"[" {yylval.id = strdup(yytext);return LEFTSQUAREBRACKET;}
"]" {yylval.id = strdup(yytext);return RIGHTSQUAREBRACKET;}
"..." {yylval.id = strdup(yytext);return THREEDOT;}
";" {yylval.id = strdup(yytext);return SEMICOLON;}
"," {yylval.id = strdup(yytext);return COMMA;}
"." {yylval.id = strdup(yytext);return DOT;}
"@" {yylval.id = strdup(yytext);return ATTHERATE;}
"::" {yylval.id = strdup(yytext);return DOUBLECOLON;}
">>>=" {yylval.id = strdup(yytext);return TRIPLEGREATEREQUAL;}
">>>" {yylval.id = strdup(yytext);return TRIPLEGREATER;}
"<<=" {yylval.id = strdup(yytext);return DOUBLELESSEQUAL;}
">>=" {yylval.id = strdup(yytext);return DOUBLEGREATEREQUAL;}
"<<" {yylval.id = strdup(yytext);return DOUBLELESS;}
">>" {yylval.id = strdup(yytext);return DOUBLEGREATER;}
"+=" {yylval.id = strdup(yytext);return ADDEQUAL;}
"-=" {yylval.id = strdup(yytext);return SUBEQUAL;}
"*=" {yylval.id = strdup(yytext);return MULEQUAL;}
"/=" {yylval.id = strdup(yytext);return DIVEQUAL;}
"&=" {yylval.id = strdup(yytext);return ANDEQUAL;}
"|=" {yylval.id = strdup(yytext);return OREQUAL;}
"^=" {yylval.id = strdup(yytext);return POWEREQUAL;}
"%=" {yylval.id = strdup(yytext);return MODEQUAL;}
"->" {yylval.id = strdup(yytext);return ARROW;}
"==" {yylval.id = strdup(yytext);return EQUALEQUAL;}
">=" {yylval.id = strdup(yytext);return GREATEREQUAL;}
"<=" {yylval.id = strdup(yytext);return LESSEQUAL;}
"!=" {yylval.id = strdup(yytext);return NOTEQUAL;}
"&&" {yylval.id = strdup(yytext);return AND;}
"||" {yylval.id = strdup(yytext);return OR;}
"++" {yylval.id = strdup(yytext);return ADDADD;}
"--" {yylval.id = strdup(yytext);return SUBSUB;}
"=" {yylval.id = strdup(yytext);return EQUAL;}
"<" {yylval.id = strdup(yytext);return LESS;}
">" {yylval.id = strdup(yytext);return GREATER;}
"!" {yylval.id = strdup(yytext);return NOT;}
"~" {yylval.id = strdup(yytext);return TILDE;}
"?" {yylval.id = strdup(yytext);return QUESTION;}
":" {yylval.id = strdup(yytext);return COLON;}
"+" {yylval.id = strdup(yytext);return ADD;}
"-" {yylval.id = strdup(yytext);return SUB;}
"*" {yylval.id = strdup(yytext);return MUL;}
"/" {yylval.id = strdup(yytext);return DIV;}
"&" {yylval.id = strdup(yytext);return ANDBIT;}
"|" {yylval.id = strdup(yytext);return ORBIT;}
"^" {yylval.id = strdup(yytext);return POWER;}
"%" {yylval.id = strdup(yytext);return MOD;}

(["]([^"\n]*)["])|["]["]["][ ]*[\n]+[^"]*(["]["]?([^"])+)*["]["]["]|([']([^'\n]*)[']) { yylval.id = strdup(yytext); return STRINGLITERAL;}

(['].[']) { yylval.id = strdup(yytext); return CHARLITERAL;}

[0][x][01a-fA-F_]*((([pP][+-][01a-fA-F_]+)?[lL]?)|(([.][01a-fA-F_]*([pP][+-][01a-fA-F_]+)?[fFdD]?)))? { yylval.num = atof(yytext); return HEXLITERAL;}

[0][b][01_]*((([pP][+-][01_]+)?[lL]?)|(([.][01_]*([pP][+-][01_]+)?[fFdD]?)))? { yylval.num = atof(yytext); return BINARYLITERAL;}

[0-9_]*[.][0-9_]*([eE][+-][0-9_]+)?[fFdD]? { yylval.num = atof(yytext); return FLOATLITERAL;}

[0-9_]*(([eE][+-][0-9_]+)?[lL]?) { yylval.num = atof(yytext); return INTLITERAL;}

"abstract" {yylval.id = strdup(yytext);return ABSTRACT;}
"continue" {yylval.id = strdup(yytext);return CONTINUE;}
"for" {yylval.id = strdup(yytext);return FOR;}
"new" {yylval.id = strdup(yytext);return NEW;}
"switch" {yylval.id = strdup(yytext);return SWITCH;}
"assert" {yylval.id = strdup(yytext);return ASSERT;}
"default" {yylval.id = strdup(yytext);return DEFAULT;}
"if" {yylval.id = strdup(yytext);return IF;}
"package" {yylval.id = strdup(yytext);return PACKAGE;}
"synchronized" {yylval.id = strdup(yytext);return SYNCHRONIZED;}
"boolean" {yylval.id = strdup(yytext);return BOOLEAN;}
"do" {yylval.id = strdup(yytext);return DO;}
"goto" {yylval.id = strdup(yytext);return GOTO;}
"private" {yylval.id = strdup(yytext);return PRIVATE;}
"this" {yylval.id = strdup(yytext);return THIS;}
"break" {yylval.id = strdup(yytext);return BREAK;}
"double" {yylval.id = strdup(yytext);return DOUBLE;}
"implements" {yylval.id = strdup(yytext);return IMPLEMENTS;}
"protected" {yylval.id = strdup(yytext);return PROTECTED;}
"throws" {yylval.id = strdup(yytext);return THROWS;}
"byte" {yylval.id = strdup(yytext);return BYTE;}
"else" {yylval.id = strdup(yytext);return ELSE;}
"import" {yylval.id = strdup(yytext);return IMPORT;}
"public" {yylval.id = strdup(yytext);return PUBLIC;}
"throw" {yylval.id = strdup(yytext);return THROW;}
"case" {yylval.id = strdup(yytext);return CASE;}
"enum" {yylval.id = strdup(yytext);return ENUM;}
"instanceof" {yylval.id = strdup(yytext);return INSTANCE;}
"return" {yylval.id = strdup(yytext);return RETURN;}
"transient" {yylval.id = strdup(yytext);return TRANSIENT;}
"catch" {yylval.id = strdup(yytext);return CATCH;}
"extends" {yylval.id = strdup(yytext);return EXTENDS;}
"int" {yylval.id = strdup(yytext);return INT;}
"short" {yylval.id = strdup(yytext);return SHORT;}
"try" {yylval.id = strdup(yytext);return TRY;}
"char" {yylval.id = strdup(yytext);return CHAR;}
"final" {yylval.id = strdup(yytext);return FINAL;}
"interface" {yylval.id = strdup(yytext);return INTERFACE;}
"static" {yylval.id = strdup(yytext);return STATIC;}
"void" {yylval.id = strdup(yytext);return VOID;}
"class" {yylval.id = strdup(yytext);return CLASS;}
"finally" {yylval.id = strdup(yytext);return FINALLY;}
"long" {yylval.id = strdup(yytext);return LONG;}
"strictfp" {yylval.id = strdup(yytext);return STRICTFP;}
"volatile" {yylval.id = strdup(yytext);return VOLATILE;}
"const" {yylval.id = strdup(yytext);return CONST;}
"float" {yylval.id = strdup(yytext);return FLOAT;}
"native" {yylval.id = strdup(yytext);return NATIVE;}
"super" {yylval.id = strdup(yytext);return SUPER;}
"while" {yylval.id = strdup(yytext);return WHILE;}
"exports" {yylval.id = strdup(yytext);return EXPORTS;}
"opens" {yylval.id = strdup(yytext);return OPENS;}
"requires" {yylval.id = strdup(yytext);return REQUIRES;}
"uses" {yylval.id = strdup(yytext);return USES;}
"module" {yylval.id = strdup(yytext);return MODULE;}
"permits" {yylval.id = strdup(yytext);return PERMITS;}
"sealed" {yylval.id = strdup(yytext);return SEALED;}
"var" {yylval.id = strdup(yytext);return VAR;}
"non-sealed" {yylval.id = strdup(yytext);return NONSEALED;}
"provides" {yylval.id = strdup(yytext);return PROVIDES;}
"to" {yylval.id = strdup(yytext);return TO;}
"with" {yylval.id = strdup(yytext);return WITH;}
"open" {yylval.id = strdup(yytext);return OPEN;}
"record" {yylval.id = strdup(yytext);return RECORD;}
"transitive" {yylval.id = strdup(yytext);return TRANSITIVE;}
"yield" {yylval.id = strdup(yytext);return YIELD;}
"suspend" {yylval.id = strdup(yytext);return SUSPEND;}
"true"|"false" { yylval.id = strdup(yytext); return BOOLEANLITERAL;}
"null" {yylval.id = strdup(yytext);return NULLLITERAL;}
[a-zA-Z$_][a-zA-Z0-9$_]* { yylval.id = strdup(yytext); return IDENTIFIER;}
"System.out.println" { yylval.id = strdup(yytext);return SYSTEMOUTPRINTLN;}

[ ]|\t|\r {  }
\n {  yylineno++; }
. { yyerror("Error: Unknown Character"); }


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


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

"(" {yylval.str = strdup(yytext);return LEFTBRACKET;}
")" {yylval.str = strdup(yytext);return RIGHTBRACKET;}
"{" {yylval.str = strdup(yytext);return LEFTCURLYBRACKET;}
"}" {yylval.str = strdup(yytext);return RIGHTCURLYBRACKET;}
"[" {yylval.str = strdup(yytext);return LEFTSQUAREBRACKET;}
"]" {yylval.str = strdup(yytext);return RIGHTSQUAREBRACKET;}
"..." {yylval.str = strdup(yytext);return THREEDOT;}
";" {yylval.str = strdup(yytext);return SEMICOLON;}
"," {yylval.str = strdup(yytext);return COMMA;}
"." {yylval.str = strdup(yytext);return DOT;}
"@" {yylval.str = strdup(yytext);return ATTHERATE;}
"::" {yylval.str = strdup(yytext);return DOUBLECOLON;}
">>>=" {yylval.str = strdup(yytext);return TRIPLEGREATEREQUAL;}
">>>" {yylval.str = strdup(yytext);return TRIPLEGREATER;}
"<<=" {yylval.str = strdup(yytext);return DOUBLELESSEQUAL;}
">>=" {yylval.str = strdup(yytext);return DOUBLEGREATEREQUAL;}
"<<" {yylval.str = strdup(yytext);return DOUBLELESS;}
">>" {yylval.str = strdup(yytext);return DOUBLEGREATER;}
"+=" {yylval.str = strdup(yytext);return ADDEQUAL;}
"-=" {yylval.str = strdup(yytext);return SUBEQUAL;}
"*=" {yylval.str = strdup(yytext);return MULEQUAL;}
"/=" {yylval.str = strdup(yytext);return DIVEQUAL;}
"&=" {yylval.str = strdup(yytext);return ANDEQUAL;}
"|=" {yylval.str = strdup(yytext);return OREQUAL;}
"^=" {yylval.str = strdup(yytext);return POWEREQUAL;}
"%=" {yylval.str = strdup(yytext);return MODEQUAL;}
"->" {yylval.str = strdup(yytext);return ARROW;}
"==" {yylval.str = strdup(yytext);return EQUALEQUAL;}
">=" {yylval.str = strdup(yytext);return GREATEREQUAL;}
"<=" {yylval.str = strdup(yytext);return LESSEQUAL;}
"!=" {yylval.str = strdup(yytext);return NOTEQUAL;}
"&&" {yylval.str = strdup(yytext);return AND;}
"||" {yylval.str = strdup(yytext);return OR;}
"++" {yylval.str = strdup(yytext);return ADDADD;}
"--" {yylval.str = strdup(yytext);return SUBSUB;}
"=" {yylval.str = strdup(yytext);return EQUAL;}
"<" {yylval.str = strdup(yytext);return LESS;}
">" {yylval.str = strdup(yytext);return GREATER;}
"!" {yylval.str = strdup(yytext);return NOT;}
"~" {yylval.str = strdup(yytext);return TILDE;}
"?" {yylval.str = strdup(yytext);return QUESTION;}
":" {yylval.str = strdup(yytext);return COLON;}
"+" {yylval.str = strdup(yytext);return ADD;}
"-" {yylval.str = strdup(yytext);return SUB;}
"*" {yylval.str = strdup(yytext);return MUL;}
"/" {yylval.str = strdup(yytext);return DIV;}
"&" {yylval.str = strdup(yytext);return ANDBIT;}
"|" {yylval.str = strdup(yytext);return ORBIT;}
"^" {yylval.str = strdup(yytext);return POWER;}
"%" {yylval.str = strdup(yytext);return MOD;}

(["]([^"\n]*)["])|["]["]["][ ]*[\n]+[^"]*(["]["]?([^"])+)*["]["]["]|([']([^'\n]*)[']) { yylval.str = strdup(yytext); return STRINGLITERAL;}

(['].[']) { yylval.str = strdup(yytext); return CHARLITERAL;}

[0][x][01a-fA-F_]*((([pP][+-][01a-fA-F_]+)?[lL]?)|(([.][01a-fA-F_]*([pP][+-][01a-fA-F_]+)?[fFdD]?)))? { yylval.str = strdup(yytext); return HEXLITERAL;}

[0][b][01_]*((([pP][+-][01_]+)?[lL]?)|(([.][01_]*([pP][+-][01_]+)?[fFdD]?)))? { yylval.str = strdup(yytext); return BINARYLITERAL;}

[0-9_]*[.][0-9_]*([eE][+-][0-9_]+)?[fFdD]? { yylval.str = strdup(yytext); return FLOATLITERAL;}

[0-9_]*(([eE][+-][0-9_]+)?[lL]?) { yylval.str = strdup(yytext); return INTLITERAL;}

"abstract" {yylval.str = strdup(yytext);return ABSTRACT;}
"continue" {yylval.str = strdup(yytext);return CONTINUE;}
"for" {yylval.loop_var.st = strdup(yytext);return FOR;}
"new" {yylval.str = strdup(yytext);return NEW;}
"switch" {yylval.str = strdup(yytext);return SWITCH;}
"assert" {yylval.str = strdup(yytext);return ASSERT;}
"default" {yylval.str = strdup(yytext);return DEFAULT;}
"if" {yylval.str = strdup(yytext);return IF;}
"package" {yylval.str = strdup(yytext);return PACKAGE;}
"synchronized" {yylval.str = strdup(yytext);return SYNCHRONIZED;}
"boolean" {yylval.str = strdup(yytext);return BOOLEAN;}
"String" {yylval.str = strdup(yytext);return STRING;}
"do" {yylval.str = strdup(yytext);return DO;}
"goto" {yylval.str = strdup(yytext);return GOTO;}
"private" {yylval.str = strdup(yytext);return PRIVATE;}
"this" {yylval.str = strdup(yytext);return THIS;}
"break" {yylval.str = strdup(yytext);return BREAK;}
"double" {yylval.str = strdup(yytext);return DOUBLE;}
"implements" {yylval.str = strdup(yytext);return IMPLEMENTS;}
"protected" {yylval.str = strdup(yytext);return PROTECTED;}
"throws" {yylval.str = strdup(yytext);return THROWS;}
"byte" {yylval.str = strdup(yytext);return BYTE;}
"else" {yylval.str = strdup(yytext);return ELSE;}
"import" {yylval.str = strdup(yytext);return IMPORT;}
"public" {yylval.str = strdup(yytext);return PUBLIC;}
"throw" {yylval.str = strdup(yytext);return THROW;}
"case" {yylval.str = strdup(yytext);return CASE;}
"enum" {yylval.str = strdup(yytext);return ENUM;}
"instanceof" {yylval.str = strdup(yytext);return INSTANCE;}
"return" {yylval.str = strdup(yytext);return RETURN;}
"transient" {yylval.str = strdup(yytext);return TRANSIENT;}
"catch" {yylval.str = strdup(yytext);return CATCH;}
"extends" {yylval.str = strdup(yytext);return EXTENDS;}
"int" {yylval.str = strdup(yytext);return INT;}
"short" {yylval.str = strdup(yytext);return SHORT;}
"try" {yylval.str = strdup(yytext);return TRY;}
"char" {yylval.str = strdup(yytext);return CHAR;}
"final" {yylval.str = strdup(yytext);return FINAL;}
"interface" {yylval.str = strdup(yytext);return INTERFACE;}
"static" {yylval.str = strdup(yytext);return STATIC;}
"void" {yylval.str = strdup(yytext);return VOID;}
"class" {yylval.str = strdup(yytext);return CLASS;}
"finally" {yylval.str = strdup(yytext);return FINALLY;}
"long" {yylval.str = strdup(yytext);return LONG;}
"strictfp" {yylval.str = strdup(yytext);return STRICTFP;}
"volatile" {yylval.str = strdup(yytext);return VOLATILE;}
"const" {yylval.str = strdup(yytext);return CONST;}
"float" {yylval.str = strdup(yytext);return FLOAT;}
"native" {yylval.str = strdup(yytext);return NATIVE;}
"super" {yylval.str = strdup(yytext);return SUPER;}
"while" {yylval.loop_var.st = strdup(yytext);return WHILE;}
"exports" {yylval.str = strdup(yytext);return EXPORTS;}
"opens" {yylval.str = strdup(yytext);return OPENS;}
"requires" {yylval.str = strdup(yytext);return REQUIRES;}
"uses" {yylval.str = strdup(yytext);return USES;}
"module" {yylval.str = strdup(yytext);return MODULE;}
"permits" {yylval.str = strdup(yytext);return PERMITS;}
"sealed" {yylval.str = strdup(yytext);return SEALED;}
"var" {yylval.str = strdup(yytext);return VAR;}
"non-sealed" {yylval.str = strdup(yytext);return NONSEALED;}
"provides" {yylval.str = strdup(yytext);return PROVIDES;}
"to" {yylval.str = strdup(yytext);return TO;}
"with" {yylval.str = strdup(yytext);return WITH;}
"open" {yylval.str = strdup(yytext);return OPEN;}
"record" {yylval.str = strdup(yytext);return RECORD;}
"transitive" {yylval.str = strdup(yytext);return TRANSITIVE;}
"yield" {yylval.str = strdup(yytext);return YIELD;}
"suspend" {yylval.str = strdup(yytext);return SUSPEND;}
"true"|"false" { yylval.str = strdup(yytext); return BOOLEANLITERAL;}
"null" {yylval.str = strdup(yytext);return NULLLITERAL;}
[a-zA-Z$_][a-zA-Z0-9$_]* { yylval.str = strdup(yytext); return IDENTIFIER;}
"System.out.println" { yylval.str = strdup(yytext);return SYSTEMOUTPRINTLN;}

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


%{
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include <iostream>
#include <string>
#include <list>

#include "help.h"

using namespace std;

extern char yytext[];
extern int column;
extern int yylineno;
int cnt;

int yylex(void);
void yyerror(const char *s);
%}

%union {
        double num;
    node *nd;
    nodes *ndl;
    char *id;
}

%token LEFTBRACKET RIGHTBRACKET LEFTCURLYBRACKET RIGHTCURLYBRACKET LEFTSQUAREBRACKET RIGHTSQUAREBRACKET THREEDOT SEMICOLON COMMA DOT ATTHERATE DOUBLECOLON TRIPLEGREATEREQUAL TRIPLEGREATER DOUBLELESSEQUAL DOUBLEGREATEREQUAL DOUBLELESS DOUBLEGREATER ADDEQUAL SUBEQUAL MULEQUAL DIVEQUAL ANDEQUAL OREQUAL POWEREQUAL MODEQUAL ARROW EQUALEQUAL GREATEREQUAL LESSEQUAL NOTEQUAL AND OR ADDADD SUBSUB EQUAL LESS GREATER NOT TILDE QUESTION COLON ADD SUB MUL DIV ANDBIT ORBIT POWER MOD


%token ABSTRACT CONTINUE FOR NEW SWITCH ASSERT DEFAULT IF PACKAGE SYNCHRONIZED BOOLEAN DO GOTO PRIVATE THIS BREAK DOUBLE IMPLEMENTS PROTECTED THROWS BYTE ELSE IMPORT PUBLIC THROW CASE ENUM INSTANCE RETURN TRANSIENT CATCH EXTENDS INT SHORT TRY CHAR FINAL INTERFACE STATIC VOID CLASS FINALLY LONG STRICTFP VOLATILE CONST FLOAT NATIVE SUPER WHILE UNDERSEMICOLONORE EXPORTS OPENS REQUIRES USES MODULE PERMITS SEALED VAR NONSEALED PROVIDES TO WITH OPEN RECORD TRANSITIVE YIELD NULLLITERAL SUSPEND

%token <num> INTLITERAL FLOATLITERAL BINARYLITERAL HEXLITERAL

%token <id> IDENTIFIER STRINGLITERAL CHARLITERAL BOOLEANLITERAL

%type <nd> Start CompilationUnit Literal Type PrimitiveType NonPrimitiveType NumericType IntegralType FloatingPointType ArrayType Name SimpleName QualifiedName ImportDeclarationOpt TypeDeclarationOpt ImportDeclaration TypeDeclaration SingleTypeImportDeclaration TypeImportOnDemandDeclaration ClassDeclaration Modifier ModifiersOpt ClassBody ClassBodyDeclarationsOpt ClassBodyDeclaration ClassMemberDeclaration StaticInitializer ConstructorDeclaration FieldDeclaration MethodDeclaration VarDeclarator VarDeclaratorId Expression ArrayInitializer MethodBody MethodHeader MethodDeclarator FormalParameterListOpt FormalParameter Block ConstructorDeclarator ConstructorBody ExplicitConstructorInvocationOpt ExplicitConstructorInvocation BlockStatementOpt BlockStatement ArgumentListOpt ArgumentList VarInitializersOpt VarInitializer CommaOpt LocalVariableDeclarationStatement LocalVariableDeclaration Statement StatementWithoutTrailingSubstatement LabeledStatement IfThenStatement IfThenElseStatement WhileStatement ForStatement StatementNoShortIf LabeledStatementNoShortIf IfThenElseStatementNoShortIf WhileStatementNoShortIf ForStatementNoShortIf EmptyStatement ExpressionStatement BreakStatement ContinueStatement ReturnStatement Assignment PreIncrementExpression PreDecrementExpression PostDecrementExpression PostIncrementExpression ClassInstanceCreationExpression ForInitOpt ForInit ExpressionOpt ForUpdateOpt ForUpdate StatementExpression IDENTOpt Primary PrimaryNoNewArray ArrayCreationExpression FieldAccess MethodInvocation ArrayAccess DimsOpt Dims DimExpr PostFixExpression UnaryExpression UnaryExpressionNotPlusMinus CastExpression MultiplicativeExpression AdditiveExpression ShiftExpression RelationalExpression EqualityExpression AndExpression ExclusiveOrExpression InclusiveOrExpression ConditionalAndExpression ConditionalOrExpression ConditionalExpression AssignmentExpression LeftHandSide AssignmentOperator

%type <ndl> ImportDeclarations TypeDeclarations Modifiers ClassBodyDeclarations VarDeclarators FormalParameterList BlockStatements VarInitializers StatementExpressionList DimExprs


%start Start


%%
Start:
        CompilationUnit
    ;
Literal:
             INTLITERAL         {$$ = new node(++cnt,"literals"); nodtostr($$,$1);}
	|    FLOATLITERAL       {$$ = new node(++cnt,"literals"); nodtostr($$,$1);}
	|     BOOLEANLITERAL        {$$ = new node(++cnt,"literals"); nodtostr($$,$1);}
	|    STRINGLITERAL      {$$ = new node(++cnt,"literals"); nodtostr($$,$1);}
	|    CHARLITERAL        {$$ = new node(++cnt,"literals"); nodtostr($$,$1);}
	|    NULLLITERAL        {$$ = new node(++cnt,"literals"); nodtostr($$,$1);}
	;
Type:
        PrimitiveType           {$$ = new node(++cnt,"type"); nodjoin($$,$1);}
    |   NonPrimitiveType        {$$ = new node(++cnt,"type"); nodjoin($$,$1);}
    ;
PrimitiveType:
        NumericType             {$$ = new node(++cnt,"PrimitiveType"); nodjoin($$,$1);}
        BOOLEAN                 {$$ = new node(++cnt,"PrimitiveType"); nodtostr($$,$1);}
    ;
NumericType:
        IntegralType            {$$ = new node(++cnt,"NumericType"); nodjoin($$,$1);}
    |   FloatingPointType       {$$ = new node(++cnt,"NumericType"); nodjoin($$,$1);}
    ;
IntegralType:
        INT                     {$$ = new node(++cnt,"IntegralType"); nodtostr($$,$1);}
    |   LONG                    {$$ = new node(++cnt,"IntegralType"); nodtostr($$,$1);}
    |   CHAR                    {$$ = new node(++cnt,"IntegralType"); nodtostr($$,$1);}
    ;
FloatingPointType:
        FLOAT                   {$$ = new node(++cnt,"FloatingPointType"); nodtostr($$,$1);}
    |   DOUBLE                  {$$ = new node(++cnt,"FloatingPointType"); nodtostr($$,$1);}
    ;
NonPrimitiveType:
        ArrayType               {$$ = new node(++cnt,"NonPrimitiveType"); nodjoin($$,$1);}
    ;
ArrayType:
        PrimitiveType IDENTIFIER LEFTSQUAREBRACKET RIGHTSQUAREBRACKET  {
                $$ = new node(++cnt,"ArrayType");
                nodjoin($$,$1);
                nodtostr($$,$2);
                nodtostr($$,$3);
                nodtostr($$,$4);
        }
    |   Name LEFTSQUAREBRACKET RIGHTSQUAREBRACKET    {
                $$ = new node(++cnt,"ArrayType");
                nodjoin($$,$1);
                nodtostr($$,$2);
                nodtostr($$,$3);
        }
    |   ArrayType LEFTSQUAREBRACKET RIGHTSQUAREBRACKET       {
                $$ = $1;
                nodtostr($$,$2);
                nodtostr($$,$3);
    }
    ;
Name:
        SimpleName      {
                $$ = new node(++cnt,"Name");
                nodjoin($$,$1);
        }
    |   QualifiedName   {
                $$ = new node(++cnt,"Name");
                nodjoin($$,$1);
        }
    ;
SimpleName:
        IDENTIFIER      {
                $$ = new node(++cnt,"SimpleName");
                nodtostr($$,$1);
        }
    ;
QualifiedName:
        Name DOT IDENTIFIER {
                $$ = new node(++cnt,"QualifiedName");
                nodjoin($$,$1);
                nodtostr($$,$2);
                nodtostr($$,$3);
        }
    ;
CompilationUnit:
        ImportDeclarationOpt TypeDeclarationOpt {
                $$ = new node(++cnt,"CompilationUnit");
                nodjoin($$,$1);
                nodjoin($$,$2);
        }
    ;
ImportDeclarationOpt:
        ImportDeclarations      {
                $$ = new node(++cnt,"ImportDeclarationOpt");
                nodtolis($$,$1);
        }
    |   {   $$ = new node();}
    ;
TypeDeclarationOpt:
        TypeDeclarations        {
                $$ = new node(++cnt,"TypeDeclarationOpt");
                nodtolis($$,$1);
        }
    |   {       $$ = new node();}
    ;
ImportDeclarations:
        ImportDeclaration       {
                $$ = new node();
                $$->push_back($1);
        }
    |   ImportDeclarations ImportDeclaration    {
                $$ = $1 ;
                $$->push_back($2);
    }
    ;
TypeDeclarations:
        TypeDeclaration         {
                $$ = new node();
                $$->push_back($1);
        }
    |   TypeDeclarations TypeDeclaration    {
                $$ = $1 ;
                $$->push_back($2);
    }
    ;
ImportDeclaration:
        SingleTypeImportDeclaration     {
                $$ = new node(++cnt,"ImportDeclaration");
                nodjoin($$,$1);
        }
    |   TypeImportOnDemandDeclaration     {
                $$ = new node(++cnt,"ImportDeclaration");
                nodjoin($$,$1);
        }
    ;
SingleTypeImportDeclaration:
        IMPORT Name DOT IDENTIFIER SEMICOLON   {
                $$ = new node(++cnt,"SingleTypeImportDeclaration");
                nodtostr($$,$1);
                nodjoin($$,$2);
                nodtostr($$,$3);
                nodtostr($$,$4);
                nodtostr($$,$5);
        }
    ;
TypeImportOnDemandDeclaration:
        IMPORT Name DOT MUL SEMICOLON   {
                $$ = new node(++cnt,"TypeImportOnDemandDeclaration");
                nodtostr($$,$1);
                nodjoin($$,$2);
                nodtostr($$,$3);
                nodtostr($$,$4);
                nodtostr($$,$5);
        }
    ;
TypeDeclaration:
        ClassDeclaration        {
                $$ = new node(++cnt,"TypeDeclaration");
                nodjoin($$,$1);
        }
    ;
Modifiers:
        Modifier        {
                $$ = new node();
                $$->push_back($1);
        }
    |   Modifiers Modifier      {
                $$ = $1;
                $1->push_back($2);
    }
    ;
Modifier:
        PUBLIC          {
                $$ = new node(++cnt,"Modifier");
                nodtostr($$,$1);
        }
    |   PRIVATE         {
                $$ = new node(++cnt,"Modifier");
                nodtostr($$,$1);
        }
    ;
ClassDeclaration:
        ModifiersOpt CLASS IDENTIFIER ClassBody {
                $$ = new node(++cnt,"ClassDeclaration");
                nodjoin($$,$1);
                nodtostr($$,$2);
                nodtostr($$,$3);
                nodjoin($$,$4);
        }
    |   ModifiersOpt CLASS IDENTIFIER EXTENDS IDENTIFIER ClassBody      {
                $$ = new node(++cnt,"ClassDeclaration");
                nodjoin($$,$1);
                nodtostr($$,$2);
                nodtostr($$,$3);
                nodtostr($$,$4);
                nodtostr($$,$5);
                nodjoin($$,$6);
        }
    ;
ModifiersOpt:
        Modifiers       {
                $$ = new node(++cnt,"ModifiersOpt");
                nodtolis($$,$1);
        }
    |   { $$ = new node() ;}
    ;
ClassBody:
        LEFTCURLYBRACKET ClassBodyDeclarationsOpt RIGHTCURLYBRACKET        {
                $$ = new node(++cnt,"ClassBody");
                nodtostr($$,$1);
                nodjoin($$,$2);
                nodtostr($$.$3);
        }
    ;
ClassBodyDeclarationsOpt:
        ClassBodyDeclarations   {
                $$ = new node(++cnt,"ClassBodyDeclarationsOpt");
                nodtolis($$,$1);
        }
    |   { $$ = new node(); }
    ;
ClassBodyDeclarations:
        ClassBodyDeclaration    {
                $$ = new node();
                $$->push_back($1);
        }
    |   ClassBodyDeclarations ClassBodyDeclaration      {
        $$ = $1;
        $1->push_back($2);
    }
    ;
ClassBodyDeclaration:
        ClassMemberDeclaration          {
                $$ = new node(++cnt,"ClassBodyDeclaration");
                nodjoin($$,$1);
        }
    |   StaticInitializer       {
                $$ = new node(++cnt,"ClassBodyDeclaration");
                nodjoin($$,$1);
    }
    |   ConstructorDeclaration       {
                $$ = new node(++cnt,"ClassBodyDeclaration");
                nodjoin($$,$1);
    }
    ;
ClassMemberDeclaration:
        FieldDeclaration          {
                $$ = new node(++cnt,"ClassMemberDeclaration");
                nodjoin($$,$1);
        }
    |   MethodDeclaration          {
                $$ = new node(++cnt,"ClassMemberDeclaration");
                nodjoin($$,$1);
        }
    ;
FieldDeclaration:
        ModifiersOpt Type VarDeclarators SEMICOLON     {
                $$ = new node(++cnt,"FieldDeclaration");
                nodjoin($$,$1);
                nodjoin($$,$2);
                nodtolis($$,$3);
                nodtostr($$,$4);
        }
    ;
VarDeclarators:
        VarDeclarator   {
                $$ = new node();
                $$->push_back($1);
        }
    |   VarDeclarators COMMA VarDeclarator      {
                $$ = $1;                               //here
                node* new1 = new node(++cnt,$2);
                $1->push_back(new1);
    }
    ;
VarDeclarator:
        VarDeclaratorId        {
                $$  = new node(++cnt,"VarDeclarator");
                nodjoin($$,$1);
        }
    |   VarDeclaratorId EQUAL  VarInitializer     {
                $$  = new node(++cnt,"VarDeclarator");
                nodjoin($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
    }
		;
VarDeclaratorId:
        IDENTIFIER      {
                $$ = new node(++cnt,"VarDeclaratorId");
                nodtostr($$,$1);
        }
    |   VarDeclaratorId LEFTSQUAREBRACKET RIGHTSQUAREBRACKET {
                $$ = $1;
                node* new1 = new node(++cnt,$2);
                node* new2 = new node(++cnt,$3);     //here
                $1->push_back(new1);
                $1->push_back(new2);
    }
    ;
VarInitializer:
        Expression      {
                $$ = new node(++cnt,"VarInitializer");
                nodjoin($$,$1);
        }
	|   ArrayInitializer    {
                $$ = new node(++cnt,"VarInitializer");
                nodjoin($$,$1);
        }
	;
MethodDeclaration:
        MethodHeader MethodBody {
                $$ = new node(++cnt,"MethodDeclaration");
                nodjoin($$,$1);
                nodjoin($$,$2);
        }
    ;
MethodHeader:
        ModifiersOpt Type MethodDeclarator      {
                $$ = new node(++cnt,"MethodHeader");
                nodjoin($$,$1);
                nodjoin($$,$2);
                nodjoin($$,$3);
        }
	|   ModifiersOpt VOID MethodDeclarator      {
                $$ = new node(++cnt,"MethodHeader");
                nodjoin($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
        }
	;

FormalParameterListOpt:
        FormalParameterList     {
                $$ = new node(++cnt,"FormalParameterListOpt");
                nodjoin($$,$1);
        }
    |   {
        $$ = new node();
    }
    ;
MethodDeclarator:
        IDENTIFIER LEFTBRACKET FormalParameterListOpt RIGHTBRACKET         {
                $$ = new node(++cnt,"MethodDeclarator");
                nodtostr($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
        }
    |   MethodDeclarator LEFTSQUAREBRACKET RIGHTSQUAREBRACKET        {
                $$ = $1;
                node* new1 = new node(++cnt,$2);      //here
                node* new2 = new node(++cnt,$3);
                $1->push_back(new1);
                $1->push_back(new2);
    }
    ;
FormalParameterList:
        FormalParameter         {
                $$ = new node(++cnt,$1);

        }
	|   FormalParameterList COMMA FormalParameter   {
                $$ = $1;
                node* new1 = new node(++cnt,$2);          // here
                $1->push_back(new1);
                $1->push_back($3);
        }
	;
FormalParameter:
        Type VarDeclaratorId    {
                $$ = new node(++cnt,"FormalParameter");
                nodjoin($$,$1);
                nodjoin($$,$2);
        }
	;
MethodBody:
        Block   {
                $$ = new node(++cnt,"MethodBody");
                nodjoin($$,$1);
        }
    ;
StaticInitializer:
        STATIC Block    {
                $$ = new node(++cnt,"StaticInitializer");
                nodtostr($$,$1);
                nodjoin($$,$2);
        }
    ;
ConstructorDeclaration:
        ModifiersOpt ConstructorDeclarator ConstructorBody      {
                $$ = new node(++cnt,"ConstructorDeclaration");
                nodjoin($$,$1);
                nodjoin($$,$2);
                nodjoin($$,$3);
        }
	;
ConstructorDeclarator:
        SimpleName LEFTBRACKET FormalParameterListOpt RIGHTBRACKET         {
                $$ = new node(++cnt,"ConstructorDeclarator");
                nodjoin($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
        }
	;
ExplicitConstructorInvocationOpt:
        ExplicitConstructorInvocation           {
                $$ = new node(++cnt,"ExplicitConstructorInvocationOpt");
                nodjoin($$,$1);
        }
    |   { $$ = new node(); }
    ;
BlockStatementOpt:
        BlockStatements {
                $$ = new node(++cnt,"BlockStatementOpt");
                nodtolis($$,$1);
        }
    |   { $$ = new node();}
    ;
ArgumentListOpt:
        ArgumentList    {
                $$ = new node(++cnt,"ArgumentListOpt");
                nodtolis($$,$1);
        }
    |   { $$ = new node(); }
    ;
ConstructorBody:
        LEFTCURLYBRACKET ExplicitConstructorInvocationOpt BlockStatementOpt RIGHTCURLYBRACKET {
                $$ = new node(++cnt,"ContructorBody");
                nodtostr($$,$1);
                nodjoin($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
        }
    ;
ExplicitConstructorInvocation:
        THIS LEFTBRACKET ArgumentListOpt RIGHTBRACKET SEMICOLON   {
                $$ = new node(++cnt,"ExplicitConstructorInvocation");
                nodtostr($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
                nodtostr($$,$5);
        }
    |    SUPER LEFTBRACKET ArgumentListOpt RIGHTBRACKET SEMICOLON {
                $$ = new node(++cnt,"ExplicitConstructorInvocation");
                nodtostr($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
                nodtostr($$,$5);
    }
	;
VarInitializersOpt:
       VarInitializers         {
                $$ = new node(++cnt,"VarInitializersOpt");
                nodtolis($$,$1);
        }
    |   { $$ = new node(); }
    ;
CommaOpt:
        COMMA           {
                $$ = new node(++cnt,"CommaOpt");
        }
    |   { $$ = new node() ; }
    ;
ArrayInitializer:
        LEFTCURLYBRACKET VarInitializersOpt CommaOpt RIGHTCURLYBRACKET     {
                $$ = new node(++cnt,"ArrayInitializer");
                nodtostr($$,$1);
                nodjoin($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
        }
    ;
VarInitializers:
        VarInitializer  {
                $$ = new node();
                $$->push_back($1);
        }
	|   VarInitializers COMMA VarInitializer  {
                $$ = $1;
                $1->push_back($3);
        }
    ;
Block:
        LEFTCURLYBRACKET BlockStatementOpt RIGHTCURLYBRACKET       {
                $$ = new node(++cnt,"Block");
                nodtostr($$,$1);
                nodjoin($$,$2);
                nodtostr($$,$3);
        }
    ;
BlockStatements:
        BlockStatement          {
                $$ = new node();
                $$->push_back($1);
        }
    |   BlockStatements BlockStatement  {
                $$ = $1;
                $1->push_back($2);
    }
    ;
BlockStatement:
        LocalVariableDeclarationStatement       {
                $$ = new node(++cnt,"BlockStatement");
                nodjoin($$,$1);
        }
    |   Statement       {
                $$ = new node(++cnt,"BlockStatement");
                nodjoin($$,$1);
    }
    ;
LocalVariableDeclarationStatement:
        LocalVariableDeclaration SEMICOLON        {
                $$ = new node(++cnt,"LocalVariableDeclarationStatement");
                nodjoin($$,$1);
                nodtostr($$,$2);
        }
    ;
LocalVariableDeclaration: Type VariableDeclarators {
                $$ = new node(++cnt,"LocalVariableDeclaration");
                nodjoin($$,$1);
                nodtolis($$,$2);
                }
	;
VariableDeclarators: VariableDeclarator {$$ = new node(); $$->push_back($1);}
		| VariableDeclarators COMMA VariableDeclarator {$$=$1; $1->push_back($3);}
		;

VariableDeclarator: VariableDeclaratorId { $$ = new node(++cnt,"VariableDeclarator"); nodjoin($$,$1);}
		| VariableDeclaratorId EQUAL VariableInitializer { $$ = new node(++cnt,"VariableDeclarator"); nodjoin($$,$1); nodtostr($$,$2); nodjoin($$,$3);}
		;

VariableInitializer: Expression { $$ = new node(++cnt,"VariableInitializer"); nodjoin($$,$1);}
		| ArrayInitializer { $$ = new node(++cnt,"VariableInitializer"); nodjoin($$,$1);}
		;

VariableDeclaratorId: IDENTIFIER {$$ = new node(++cnt,"VariableDeclaratorId"); nodtostr($$,$1);}
		| VariableDeclaratorId LEFTSQUAREBRACKET RIGHTSQUAREBRACKET {$$ = $1; node* n1 = new node(++cnt,$2); node* n2 = new node(++cnt,$3); nodjoin($$,n1); nodjoin($$,n2);}
		;

Statement:
        StatementWithoutTrailingSubstatement    {
                $$ = new node(++cnt,"Statement");
                nodjoin($$,$1);
        }
	|   LabeledStatement    {
                $$ = new node(++cnt,"Statement");
                nodjoin($$,$1);
        }
	|   IfThenStatement    {
                $$ = new node(++cnt,"Statement");
                nodjoin($$,$1);
        }
	|   IfThenElseStatement    {
                $$ = new node(++cnt,"Statement");
                nodjoin($$,$1);
        }
	|   WhileStatement    {
                $$ = new node(++cnt,"Statement");
                nodjoin($$,$1);
        }
	|   ForStatement    {
                $$ = new node(++cnt,"Statement");
                nodjoin($$,$1);
        }
	;
StatementNoShortIf:
        StatementWithoutTrailingSubstatement    {
                $$ = new node(++cnt,"StatementNoShortIf");
                nodjoin($$,$1);
        }
	|   LabeledStatementNoShortIf    {
                $$ = new node(++cnt,"StatementNoShortIf");
                nodjoin($$,$1);
        }
	|   IfThenElseStatementNoShortIf    {
                $$ = new node(++cnt,"StatementNoShortIf");
                nodjoin($$,$1);
        }
	|   WhileStatementNoShortIf    {
                $$ = new node(++cnt,"StatementNoShortIf");
                nodjoin($$,$1);
        }
	|   ForStatementNoShortIf    {
                $$ = new node(++cnt,"StatementNoShortIf");
                nodjoin($$,$1);
        }
	;

StatementWithoutTrailingSubstatement:
        Block    {
                $$ = new node(++cnt,"StatementWithoutTrailingSubstatement");
                nodjoin($$,$1);
        }
	|   EmptyStatement    {
                $$ = new node(++cnt,"StatementWithoutTrailingSubstatement");
                nodjoin($$,$1);
        }
	|   ExpressionStatement    {
                $$ = new node(++cnt,"StatementWithoutTrailingSubstatement");
                nodjoin($$,$1);
        }
	|   BreakStatement    {
                $$ = new node(++cnt,"StatementWithoutTrailingSubstatement");
                nodjoin($$,$1);
        }
	|   ContinueStatement    {
                $$ = new node(++cnt,"StatementWithoutTrailingSubstatement");
                nodjoin($$,$1);
        }
	|   ReturnStatement    {
                $$ = new node(++cnt,"StatementWithoutTrailingSubstatement");
                nodjoin($$,$1);
        }
	;
EmptyStatement:
        SEMICOLON    {
                $$ = new node(++cnt,"EmptyStatement");
                nodtostr($$,$1);
        }
    ;
LabeledStatement:
        IDENTIFIER COLON Statement    {
                $$ = new node(++cnt,"LabeledStatement");
                nodtostr($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
        }
    ;
LabeledStatementNoShortIf:
        IDENTIFIER COLON StatementNoShortIf    {
                $$ = new node(++cnt,"LabeledStatementNoShortIf");
                nodtostr($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
        }
    ;
ExpressionStatement:
        StatementExpression SEMICOLON    {
                $$ = new node(++cnt,"ExpressionStatement");
                nodjoin($$,$1);
                nodtostr($$,$2);
        }
    ;
StatementExpression:
        Assignment    {
                $$ = new node(++cnt,"StatementExpression");
                nodjoin($$,$1);
        }
    |   PreIncrementExpression    {
                $$ = new node(++cnt,"StatementExpression");
                nodjoin($$,$1);
        }
    |   PreDecrementExpression    {
                $$ = new node(++cnt,"StatementExpression");
                nodjoin($$,$1);
        }
    |   PostIncrementExpression    {
                $$ = new node(++cnt,"StatementExpression");
                nodjoin($$,$1);
        }
    |   PostDecrementExpression    {
                $$ = new node(++cnt,"StatementExpression");
                nodjoin($$,$1);
        }
    |   MethodInvocation    {
                $$ = new node(++cnt,"StatementExpression");
                nodjoin($$,$1);
        }
    |   ClassInstanceCreationExpression    {
                $$ = new node(++cnt,"StatementExpression");
                nodjoin($$,$1);
        }
    ;

ClassInstanceCreationExpression: NEW ClassType LEFTBRACKET ArgumentListOpt RIGHTBRACKET {
        $$=new node(++cnt,"ClassInstanceCreationExpression");
        nodtostr($$,$1);
        nodjoin($$,$2);
        nodtostr($$,$3);
        nodjoin($$,$4);
        nodtostr($$,$5);
}
;

ClassType:	  ClassOrInterfaceType{$$=new node(++cnt,"ClassType"); nodjoin($$,$1);}
		;

ClassOrInterfaceType: Name {$$=new node(++cnt,"ClassOrInterfaceType"); nodjoin($$,$1);}
		;

IfThenStatement:
        IF LEFTBRACKET Expression RIGHTBRACKET Statement    {
                $$ = new node(++cnt,"IfThenStatement");
                nodtostr($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
                nodjoin($$,$5);
        }
    ;
IfThenElseStatement:
        IF LEFTBRACKET Expression RIGHTBRACKET  StatementNoShortIf ELSE Statement    {
                $$ = new node(++cnt,"IfThenElseStatement");
                nodtostr($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
                nodjoin($$,$5);
                nodtostr($$,$6);
                nodjoin($$,$7);
        }
    ;
IfThenElseStatementNoShortIf:
        IF LEFTBRACKET Expression RIGHTBRACKET StatementNoShortIf ELSE StatementNoShortIf    {
                $$ = new node(++cnt,"IfThenElseStatementNoShortIf");
                nodtostr($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
                nodjoin($$,$5);
                nodtostr($$,$6);
                nodjoin($$,$7);
        }
    ;
WhileStatement:
        WHILE LEFTBRACKET Expression RIGHTBRACKET Statement    {
                $$ = new node(++cnt,"WhileStatement");
                nodtostr($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
                nodjoin($$,$5);
        }
	;

WhileStatementNoShortIf:
        WHILE LEFTBRACKET Expression RIGHTBRACKET StatementNoShortIf    {
                $$ = new node(++cnt,"WhileStatementNoShortIf");
                nodtostr($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
                nodjoin($$,$5);
        }
    ;
ForInitOpt:
        ForInit     {
                $$ = new node(++cnt,"ForInitOpt");
                nodjoin($$,$1);
        }
    |  { $$ = new node(); } ;

ExpressionOpt:
        Expression     {
                $$ = new node(++cnt,"ExpressionOpt");
                nodjoin($$,$1);
        }
    |   { $$ = new node(); } ;

ForUpdateOpt:
        ForUpdate     {
                $$ = new node(++cnt,"ForUpdateOpt");
                nodjoin($$,$1);
        }
    |   { $$ = new node(); } ;

ForStatement:
        FOR LEFTBRACKET ForInitOpt SEMICOLON ExpressionOpt SEMICOLON ForUpdateOpt RIGHTBRACKET Statement     {
                $$ = new node(++cnt,"ForStatement");
                nodtostr($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
                nodjoin($$,$5);
                nodtostr($$,$6);
                nodjoin($$,$7);
                nodtostr($$,$8);
                nodjoin($$,$9);
        }
	;

ForStatementNoShortIf:
        FOR LEFTBRACKET ForInitOpt SEMICOLON ExpressionOpt SEMICOLON ForUpdateOpt RIGHTBRACKET StatementNoShortIf{
                $$ = new node(++cnt,"ForStatement");
                nodtostr($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
                nodjoin($$,$5);
                nodtostr($$,$6);
                nodjoin($$,$7);
                nodtostr($$,$8);
                nodjoin($$,$9);
        }
	;


ForInit:
         StatementExpressionList     {        //dekhna hai
                $$ = new node(++cnt,"ForInit");
                nodtolis($$,$1);
        }
	|    LocalVariableDeclaration     {
                $$ = new node(++cnt,"ForInit");
                nodjoin($$,$1);
        }
	;

ForUpdate:
        StatementExpressionList     {
                $$ = new node(++cnt,"ForInitOpt");
                nodtolis($$,$1);
        }
	;

StatementExpressionList:
        StatementExpression     {
                $$ = new node();
                $$->push_back($1);
        }
	|   StatementExpressionList COMMA StatementExpression   {
                $$ = $1;
                $1->push_back($3);
        }
	;

IDENTOpt: IDENTIFIER    {
                $$ = new node(++cnt,"IDENTOpt");
                nodtostr($$,$1);
        }
    |     { $$ = new node(); } ;

BreakStatement:
        BREAK IDENTOpt SEMICOLON    {
                $$ = new node(++cnt,"BreakStatement");
                nodtostr($$,$1);
                nodjoin($$,$2);
                nodtostr($$,$3);
        }
	;

ContinueStatement:
        CONTINUE IDENTOpt SEMICOLON    {
                $$ = new node(++cnt,"ContinueStatement");
                nodtostr($$,$1);
                nodjoin($$,$2);
                nodtostr($$,$3);
        }
	;

ReturnStatement:
        RETURN ExpressionOpt SEMICOLON    {
                $$ = new node(++cnt,"ReturnStatement");
                nodtostr($$,$1);
                nodjoin($$,$2);
                nodtostr($$,$3);
        }
	|   SUSPEND ExpressionOpt SEMICOLON    {
                $$ = new node(++cnt,"ReturnStatement");
                nodtostr($$,$1);
                nodjoin($$,$2);
                nodtostr($$,$3);
        }
	;
Primary:
        PrimaryNoNewArray    {
                $$ = new node(++cnt,"Primary");
                nodjoin($$,$1);
        }
    |   ArrayCreationExpression    {
                $$ = new node(++cnt,"Primary");
                nodjoin($$,$1);
        }
    ;
PrimaryNoNewArray:
        Literal    {
                $$ = new node(++cnt,"PrimaryNoNewArray");
                nodjoin($$,$1);
        }
    |   THIS    {
                $$ = new node(++cnt,"PrimaryNoNewArray");
                nodtostr($$,$1);
        }
    |   LEFTBRACKET Expression RIGHTBRACKET    {
                $$ = new node(++cnt,"PrimaryNoNewArray");
                nodtostr($$,$1);
                nodjoin($$,$2);
                nodtostr($$,$3);
        }
    |   FieldAccess    {
                $$ = new node(++cnt,"PrimaryNoNewArray");
                nodjoin($$,$1);
        }
    |   MethodInvocation    {
                $$ = new node(++cnt,"PrimaryNoNewArray");
                nodjoin($$,$1);
        }
    |   ArrayAccess    {
                $$ = new node(++cnt,"PrimaryNoNewArray");
                nodjoin($$,$1);
        }
    ;
ArgumentList:
        Expression    {
                $$ = new node(++cnt,"ArgumentList");
                nodjoin($$,$1);
        }
	|   ArgumentList COMMA Expression       {
                $$ = $1;
                $1->push_back($3);
        }
	;
DimsOpt: Dims   {
                $$ = new node(++cnt,"DimsOpt");
                nodtolis($$,$1);
        }
    |   { $$ = new node(); }
    ;
ArrayCreationExpression:
        NEW PrimitiveType DimExprs DimsOpt      {
                $$ = new node(++cnt,"ArrayCreationExpression");
                nodtolis($$,$1);
                nodjoin($$,$2);
                nodtolis($$,$3);
                nodjoin($$,$4);
        }
	;
DimExprs:
        DimExpr {
                $$ = new node();
                $$->$1;
        }
	|   DimExprs DimExpr    {
                $$ = $1;
                $1->push_back($2);
        }
	;
DimExpr:
        LEFTSQUAREBRACKET Expression RIGHTSQUAREBRACKET      {
                $$ = new node(++cnt,"DimExpr");
                nodtostr($$,$1);
                nodjoin($$,$2);
                nodtostr($$,$3);
        }
    ;
Dims:
        LEFTSQUAREBRACKET RIGHTSQUAREBRACKET         {
                $$ = new node();
                node* n1 = new node(++cnt,$2);
                node* n2 = new node(++cnt,$3);                          //here
                $$->push_back(n1);
                $$->push_back(n2);
        }
    |   Dims LEFTSQUAREBRACKET RIGHTSQUAREBRACKET    {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                node* n2 = new node(++cnt,$3);
                $1->push_back(n1);
                $1->push_back(n2);

    }
    ;
FieldAccess:
        Primary DOT IDENTIFIER          {
                $$ = new node(++cnt,"FieldAccess");
                nodjoin($$,$1);
                nodtostr($$,$2);
                nodtostr($$,$3);
        }
	|   SUPER DOT IDENTIFIER        {
                $$ = new node(++cnt,"FieldAccess");
                nodtostr($$,$1);
                nodtostr($$,$2);
                nodtostr($$,$3);
        }
	;
MethodInvocation:
        Name LEFTBRACKET ArgumentListOpt RIGHTBRACKET      {
                $$ = new node(++cnt,"MethodInvocation");
                nodjoin($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
        }
	|   Primary DOT IDENTIFIER LEFTBRACKET ArgumentListOpt RIGHTBRACKET      {
                $$ = new node(++cnt,"MethodInvocation");
                nodjoin($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
                nodjoin($$,$5);
                nodtostr($$,$6);
        }
	|   SUPER DOT IDENTIFIER LEFTBRACKET ArgumentListOpt RIGHTBRACKET      {
                $$ = new node(++cnt,"MethodInvocation");
                nodtostr($$,$1);
                nodtostr($$,$2);
                nodtostr($$,$3);
                nodtostr($$,$4);
                nodjoin($$,$5);
                nodtostr($$,$6);
        }
	|   Name LEFTCURLYBRACKET ArgumentListOpt RIGHTCURLYBRACKET      {
                $$ = new node(++cnt,"MethodInvocation");
                nodjoin($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
        }
	|   Primary DOT IDENTIFIER LEFTCURLYBRACKET ArgumentListOpt RIGHTCURLYBRACKET      {
                $$ = new node(++cnt,"MethodInvocation");
                nodjoin($$,$1);
                nodtostr($$,$2);
                nodtostr($$,$3);
                nodtostr($$,$4);
                nodjoin($$,$5);
                nodtostr($$,$6);
        }
	|   SUPER DOT IDENTIFIER LEFTCURLYBRACKET ArgumentListOpt RIGHTCURLYBRACKET      {
                $$ = new node(++cnt,"MethodInvocation");
                nodtostr($$,$1);
                nodtostr($$,$2);
                nodtostr($$,$3);
                nodtostr($$,$4);
                nodjoin($$,$5);
                nodtostr($$,$6);
        }
	;
ArrayAccess:
        Name LEFTSQUAREBRACKET Expression RIGHTSQUAREBRACKET      {
                $$ = new node(++cnt,"ArrayAcces");
                nodjoin($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
        }
	|   PrimaryNoNewArray LEFTSQUAREBRACKET Expression RIGHTSQUAREBRACKET      {
                $$ = new node(++cnt,"ArrayAcces");
                nodjoin($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
        }
	;

PostFixExpression:
        Primary      {
                $$ = new node(++cnt,"PostFixExpression");
                nodjoin($$,$1);
        }
	|   Name      {
                $$ = new node(++cnt,"PostFixExpression");
                nodjoin($$,$1);
        }
	|   PostIncrementExpression      {
                $$ = new node(++cnt,"PostFixExpression");
                nodjoin($$,$1);
        }
	|   PostDecrementExpression      {
                $$ = new node(++cnt,"PostFixExpression");
                nodjoin($$,$1);
        }
	;

PostIncrementExpression:
        PostFixExpression ADDADD      {
                $$ = new node(++cnt,"PostIncrementExpression");
                nodjoin($$,$1);
                nodtostr($$,$2);
        }
	;

PostDecrementExpression:
        PostFixExpression SUBSUB      {
                $$ = new node(++cnt,"PostDecrementExpression");
                nodjoin($$,$1);
                nodtostr($$,$2);
        }
	;

UnaryExpression:
        PreIncrementExpression      {
                $$ = new node(++cnt,"UnaryExpression");
                nodjoin($$,$1);
        }
        |   PreDecrementExpression      {
                $$ = new node(++cnt,"UnaryExpression");
                nodjoin($$,$1);
        }
	|   ADD UnaryExpression      {
                $$ = new node(++cnt,"UnaryExpression");
                nodtostr($$,$1);
                nodjoin($$,$2);
        }
	|   SUB UnaryExpression      {
                $$ = new node(++cnt,"UnaryExpression");
                nodtostr($$,$1);
                nodjoin($$,$2);
        }
	|   UnaryExpressionNotPlusMinus      {
                $$ = new node(++cnt,"UnaryExpression");
                nodjoin($$,$1);
        }
	;

PreIncrementExpression:
        ADDADD UnaryExpression      {
                $$ = new node(++cnt,"PreIncrementExpression");
                nodtostr($$,$1);
                nodjoin($$,$2);
        }
	;

PreDecrementExpression:
        SUBSUB UnaryExpression      {
                $$ = new node(++cnt,"PreDecrementExpression");
                nodtostr($$,$1);
                nodjoin($$,$2);
        }
	;

UnaryExpressionNotPlusMinus:
        PostFixExpression      {
                $$ = new node(++cnt,"UnaryExpressionNotPlusMinus");
                nodjoin($$,$1);
        }
	|   TILDE UnaryExpression      {
                $$ = new node(++cnt,"UnaryExpressionNotPlusMinus");
                nodtostr($$,$1);
                nodjoin($$,$2);
        }
	|   NOT UnaryExpression      {
                $$ = new node(++cnt,"UnaryExpressionNotPlusMinus");
                nodtostr($$,$1);
                nodjoin($$,$2);
        }
	|   CastExpression      {
                $$ = new node(++cnt,"UnaryExpressionNotPlusMinus");
                nodjoin($$,$1);
        }
	;

CastExpression:
        LEFTBRACKET PrimitiveType DimsOpt RIGHTBRACKET UnaryExpression      {
                $$ = new node(++cnt,"CastExpression");
                nodtostr($$,$1);
                nodjoin($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
                nodjoin($$,$5);
        }
	|   LEFTBRACKET Expression RIGHTBRACKET UnaryExpressionNotPlusMinus      {
                $$ = new node(++cnt,"CastExpression");
                nodtostr($$,$1);
                nodjoin($$,$2);
                nodtostr($$,$4);
                nodjoin($$,$5);
        }
	|   LEFTBRACKET Name Dims RIGHTBRACKET UnaryExpressionNotPlusMinus      {
                $$ = new node(++cnt,"CastExpression");
                nodtostr($$,$1);
                nodjoin($$,$2);
                nodtolis($$,$3);
                nodtostr($$,$4);
                nodjoin($$,$5);
        }
	;

MultiplicativeExpression:
        UnaryExpression      {
                $$ = new node(++cnt,"MultiplicativeExpression");
                nodjoin($$,$1);
        }
	|   MultiplicativeExpression MUL UnaryExpression        {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                $1->push_back(n1);
                $1->push_back($3);
        }
	|   MultiplicativeExpression DIV UnaryExpression        {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                $1->push_back(n1);
                $1->push_back($3);
        }
	|   MultiplicativeExpression MOD UnaryExpression        {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                $1->push_back(n1);
                $1->push_back($3);
        }
	;

AdditiveExpression:
        MultiplicativeExpression      {
                $$ = new node(++cnt,"AdditiveExpression");
                nodjoin($$,$1);
        }
	|   AdditiveExpression ADD MultiplicativeExpression        {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                $1->push_back(n1);
                $1->push_back($3);
        }
	|   AdditiveExpression SUB  MultiplicativeExpression        {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                $1->push_back(n1);
                $1->push_back($3);
        }
	;

ShiftExpression:
        AdditiveExpression      {
                $$ = new node(++cnt,"ShiftExpression");
                nodjoin($$,$1);
        }
	|   ShiftExpression DOUBLELESS AdditiveExpression        {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                $1->push_back(n1);
                $1->push_back($3);
        }
	|   ShiftExpression DOUBLEGREATER AdditiveExpression        {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                $1->push_back(n1);
                $1->push_back($3);
        }
	|   ShiftExpression TRIPLEGREATER AdditiveExpression        {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                $1->push_back(n1);
                $1->push_back($3);
        }
	;

RelationalExpression:
        ShiftExpression      {
                $$ = new node(++cnt,"RelationalExpression");
                nodjoin($$,$1);
        }
	|   RelationalExpression LESS ShiftExpression        {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                $1->push_back(n1);
                $1->push_back($3);
        }
	|   RelationalExpression GREATER ShiftExpression        {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                $1->push_back(n1);
                $1->push_back($3);
        }
	|   RelationalExpression LESSEQUAL ShiftExpression        {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                $1->push_back(n1);
                $1->push_back($3);
        }
	|   RelationalExpression GREATEREQUAL ShiftExpression        {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                $1->push_back(n1);
                $1->push_back($3);
        }


EqualityExpression:
        RelationalExpression      {
                $$ = new node(++cnt,"EqualityExpression");
                nodjoin($$,$1);
        }
	|   EqualityExpression EQUALEQUAL RelationalExpression        {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                $1->push_back(n1);
                $1->push_back($3);
        }
	|   EqualityExpression NOTEQUAL RelationalExpression        {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                $1->push_back(n1);
                $1->push_back($3);
        }
	;

AndExpression:
        EqualityExpression      {
                $$ = new node(++cnt,"AndExpression");
                nodjoin($$,$1);
        }
	|   AndExpression '&' EqualityExpression        {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                $1->push_back(n1);
                $1->push_back($3);
        }
	;

ExclusiveOrExpression:
        AndExpression      {
                $$ = new node(++cnt,"ExclusiveOrExpression");
                nodjoin($$,$1);
        }
	|   ExclusiveOrExpression POWER AndExpression        {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                $1->push_back(n1);
                $1->push_back($3);
        }
	;

InclusiveOrExpression:
        ExclusiveOrExpression      {
                $$ = new node(++cnt,"InclusiveOrExpression");
                nodjoin($$,$1);
        }
	|   InclusiveOrExpression ORBIT ExclusiveOrExpression        {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                $1->push_back(n1);
                $1->push_back($3);
        }
	;

ConditionalAndExpression:
        InclusiveOrExpression      {
                $$ = new node(++cnt,"conditionalAndExpression");
                nodjoin($$,$1);
        }
	|   ConditionalAndExpression AND InclusiveOrExpression        {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                $1->push_back(n1);
                $1->push_back($3);
        }
	;

ConditionalOrExpression:
        ConditionalAndExpression      {
                $$ = new node(++cnt,"ConditionalOrExpression");
                nodjoin($$,$1);
        }
	|   ConditionalOrExpression OR ConditionalAndExpression        {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                $1->push_back(n1);
                $1->push_back($3);
        }
	;

ConditionalExpression: ConditionalOrExpression      {
                $$ = new node(++cnt,"ConditionalExpression");
                nodjoin($$,$1);
        }
	|   ConditionalOrExpression QUESTION Expression COLON ConditionalExpression        {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                $1->push_back(n1);
                $1->push_back($3);
                node* n2 = new node(++cnt,$3);
                $1->push_back(n2);
                $1->push_back($5);

        }
	;

AssignmentExpression:
        ConditionalExpression      {
                $$ = new node(++cnt,"AssignmentExpression");
                nodjoin($$,$1);
        }
	|   Assignment      {
                $$ = new node(++cnt,"AssignmentExpression");
                nodjoin($$,$1);
        }
	;

Assignment:
        LeftHandSide AssignmentOperator AssignmentExpression      {
                $$ = new node(++cnt,"Assignment");
                nodjoin($$,$1);
                nodjoin($$,$2);
                nodjoin($$,$3);
        }
	;

LeftHandSide:
        Name          {
                $$ = new node(++cnt,"LeftHandSide");
                nodjoin($$,$1);
        }
	|   FieldAccess          {
                $$ = new node(++cnt,"LeftHandSide");
                nodjoin($$,$1);
        }
	|   ArrayAccess          {
                $$ = new node(++cnt,"LeftHandSide");
                nodjoin($$,$1);
        }
	;

 AssignmentOperator:
        EQUAL          {
                $$ = new node(++cnt,"AssignmentOperator");
                nodtostr($$,$1);
        }
    |   MULEQUAL          {
                $$ = new node(++cnt,"AssignmentOperator");
                nodtostr($$,$1);
        }
    |   DIVEQUAL          {
                $$ = new node(++cnt,"AssignmentOperator");
                nodtostr($$,$1);
        }
    |   SUBEQUAL          {
                $$ = new node(++cnt,"AssignmentOperator");
                nodtostr($$,$1);
        }
    |   ANDEQUAL          {
                $$ = new node(++cnt,"AssignmentOperator");
                nodtostr($$,$1);
        }
    |   ADDEQUAL          {
                $$ = new node(++cnt,"AssignmentOperator");
                nodtostr($$,$1);
        }
    ;

Expression:
        AssignmentExpression          {
                $$ = new node(++cnt,"Expression");
                nodjoin($$,$1);
        }
	;

%%

int main(void) {
    yyparse();
    return 0;
}

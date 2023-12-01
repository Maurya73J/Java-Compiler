%{
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include <iostream>
#include <string>
#include <list>

using namespace std;

extern char yytext[];
extern int column;
extern int yylineno;
int cnt;

int yylex(void);
void yyerror(const char *s);

%}

%union {
    node *nd;
    nodes *ndl;
    char *id;
}


%token INTLITERAL FLOATLITERAL BOOLLITERAL STRINGLITERAL CHARLITERAL NULLLITERAL VOID STATIC PUBLIC PRIVATE CLASS EXTENDS THIS SUPER IF ELSE WHILE BREAK CONTINUE RETURN SUSPEND NEW 
%token LSB RSB DOT SC MUL LCB RCB COMMA ASN LP RP COLON INC DNC PLUS MINUS TILDE EXCLAMATION DIV MOD LS RS URS LT GT GE LE EQ NOTEQ PIPE POWER AND OR QUES MULASN DIVASN MINUSASN ANDASN PLUSASN 
%token BOOLEAN INT LONG CHAR FLOAT DOUBLE
%token <id> Identifier


%type <nd> Start CompilationUnit Literal Type PrimitiveType NonPrimitiveType NumericType IntegralType FloatingPointType ArrayType Name SimpleName QualifiedName ImportDeclarationOpt TypeDeclarationOpt ImportDeclaration TypeDeclaration SingleTypeImportDeclaration TypeImportOnDemandDeclaration ClassDeclaration Modifier ModifiersOpt ClassBody ClassBodyDeclarationsOpt ClassBodyDeclaration ClassMemberDeclaration StaticInitializer ConstructorDeclaration FieldDeclaration MethodDeclaration VarDeclarator VarDeclaratorId Expression ArrayInitializer MethodBody MethodHeader MethodDeclarator FormalParameterListOpt FormalParameter Block ConstructorDeclarator ConstructorBody ExplicitConstructorInvocationOpt ExplicitConstructorInvocation BlockStatementOpt BlockStatement ArgumentListOpt ArgumentList VarInitializersOpt VarInitializer CommaOpt LocalVariableDeclarationStatement LocalVariableDeclaration statement StatementWithoutTrailingSubstatement LabeledStatement IfThenStatement IfThenElseStatement WhileStatement ForStatement StatementNoShortIf LabeledStatementNoShortIf IfThenElseStatementNoShortIf WhileStatementNoShortIf ForStatementNoShortIf EmptyStatement ExpressionStatement BreakStatement ContinueStatement ReturnStatement Assignment PreIncrementExpression PreDecrementExpression PostDecrementExpression PostIncrementExpression ClassInstanceCreationExpression ForInitOpt ForInit ExpressionOpt ForUpdateOpt ForUpdate StatementExpression IDENTOpt Primary PrimaryNoNewArray ArrayCreationExpression FieldAccess MethodInvocation ArrayAccess DimsOpt Dims DimExpr PostFixExpression UnaryExpression UnaryExpressionNotPlusMinus CastExpression MultiplicativeExpression AdditiveExpression ShiftExpression RelationalExpression EqualityExpression AndExpression ExclusiveOrExpression InclusiveOrExpression ConditionalAndExpression ConditionalOrExpression ConditionalExpression AssignmentExpression LeftHandSide AssignmentOperator 
%type <ndl> ImportDeclarations TypeDeclarations Modifiers ClassBodyDeclarations VarDeclarators FormalParameterList BlockStatements VarInitializers StatementExpressionList DimExprs


%start Start


%%
Start:
        CompilationUnit
    ;
Literal:    
             INTLITERAL         {$$ = new node(++cnt,"literals"); nodtostr($$,$1);}
	|    FLOATLITERAL       {$$ = new node(++cnt,"literals"); nodtostr($$,$1);}
	|    BOOLLITERAL        {$$ = new node(++cnt,"literals"); nodtostr($$,$1);}
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
        PrimitiveType Identifier LSB RSB  {
                $$ = new node(++cnt,"ArrayType"); 
                nodjoin($$,$1);
                nodtostr($$,$2);
                nodtostr($$,$3);
                nodtostr($$,$4);
        }
    |   Name LSB RSB    {
                $$ = new node(++cnt,"ArrayType"); 
                nodjoin($$,$1);
                nodtostr($$,$2);
                nodtostr($$,$3);
        }
    |   ArrayType LSB RSB       {
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
        Identifier      {
                $$ = new node(++cnt,"SimpleName");
                nodtostr($$,$1);
        }
    ;
QualifiedName:
        Name DOT Identifier {
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
    |   {       $$ = new node();}
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
        IMPORT Name DOT Identifier SC   {
                $$ = new node(++cnt,"SingleTypeImportDeclaration");
                nodtostr($$,$1);
                nodjoin($$,$2);
                nodtostr($$,$3);
                nodtostr($$,$4);
                nodtostr($$,$5);
        }
    ;
TypeImportOnDemandDeclaration:
        IMPORT Name DOT MUL SC   {
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
        ModifiersOpt CLASS Identifier ClassBody {
                $$ = new node(++cnt,"ClassDeclaration");
                nodjoin($$,$1);
                nodtostr($$,$2);
                nodtostr($$,$3);
                nodjoin($$,$4);
        }
    |   ModifiersOpt CLASS Identifier EXTENDS Identifier ClassBody      {
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
        LCB ClassBodyDeclarationsOpt RCB        {
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
        ModifiersOpt Type VarDeclarators SC     {
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
    |   VarDeclaratorId ASN  VarInitializer     {
                $$  = new node(++cnt,"VarDeclarator");
                nodjoin($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
    }
		;  
VarDeclaratorId:
        Identifier      {
                $$ = new node(++cnt,"VarDeclaratorId");
                nodtostr($$,$1);
        }
    |   VarDeclaratorId LSB RSB {
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
        Identifier LP FormalParameterListOpt RP         {
                $$ = new node(++cnt,"MethodDeclarator");
                nodtostr($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
        }
    |   MethodDeclarator LSB RSB        {
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
        SimpleName LP FormalParameterListOpt RP         {
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
        LCB ExplicitConstructorInvocationOpt BlockStatementOpt RCB {
                $$ = new node(++cnt,"ContructorBody");
                nodtostr($$,$1);
                nodjoin($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
        }
    ;
ExplicitConstructorInvocation: 
        THIS LP ArgumentListOpt RP SC   {
                $$ = new node(++cnt,"ExplicitConstructorInvocation");
                nodtostr($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
                nodtostr($$,$5);
        }
    |    SUPER LP ArgumentListOpt RP SC {
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
        LCB VarInitializersOpt CommaOpt RCB     {
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
        LCB BlockStatementOpt RCB       {
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
        LocalVariableDeclaration SC        {
                $$ = new node(++cnt,"LocalVariableDeclarationStatement");
                nodjoin($$,$1);
                nodtostr($$,$2);
        }
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
        SC    {
                $$ = new node(++cnt,"EmptyStatement");
                nodtostr($$,$1);
        }
    ;
LabeledStatement:
        Identifier COLON Statement    {
                $$ = new node(++cnt,"LabeledStatement");
                nodtostr($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
        }
    ;
LabeledStatementNoShortIf:
        Identifier COLON StatementNoShortIf    {
                $$ = new node(++cnt,"LabeledStatementNoShortIf");
                nodtostr($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
        }
    ;
ExpressionStatement:
        StatementExpression SC    {
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
IfThenStatement:
        IF LP Expression RP Statement    {
                $$ = new node(++cnt,"IfThenStatement");
                nodtostr($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
                nodjoin($$,$5);
        }
    ;
IfThenElseStatement:
        IF LP Expression RP  StatementNoShortIf ELSE Statement    {
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
        IF LP Expression RP StatementNoShortIf ELSE StatementNoShortIf    {
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
        WHILE LP Expression RP Statement    {
                $$ = new node(++cnt,"WhileStatement");
                nodtostr($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
                nodjoin($$,$5);
        }
	;

WhileStatementNoShortIf:  
        WHILE LP Expression RP StatementNoShortIf    {
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
        FOR LP ForInitOpt SC ExpressionOpt SC ForUpdateOpt RP Statement     {
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
        FOR LP ForInitOpt SC ExpressionOpt SC ForUpdateOpt RP StatementNoShortIf{
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

IDENTOpt: Identifier    {
                $$ = new node(++cnt,"IDENTOpt");
                nodtostr($$,$1);
        } 
    |     { $$ = new node(); } ;

BreakStatement:	  
        BREAK IDENTOpt SC    {
                $$ = new node(++cnt,"BreakStatement");
                nodtostr($$,$1);
                nodjoin($$,$2);
                nodtostr($$,$3);
        }       
	;

ContinueStatement: 
        CONTINUE IDENTOpt SC    {
                $$ = new node(++cnt,"ContinueStatement");
                nodtostr($$,$1);
                nodjoin($$,$2);
                nodtostr($$,$3);
        } 
	;

ReturnStatement:    
        RETURN ExpressionOpt SC    {
                $$ = new node(++cnt,"ReturnStatement");
                nodtostr($$,$1);
                nodjoin($$,$2);
                nodtostr($$,$3);
        } 
	|   SUSPEND ExpressionOpt SC    {
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
    |   LP Expression RP    {
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
        LSB Expression RSB      {
                $$ = new node(++cnt,"DimExpr");
                nodtostr($$,$1);
                nodjoin($$,$2);
                nodtostr($$,$3);
        }
    ;
Dims:
        LSB RSB         {
                $$ = new node();
                node* n1 = new node(++cnt,$2);
                node* n2 = new node(++cnt,$3);                          //here
                $$->push_back(n1);
                $$->push_back(n2);
        }
    |   Dims LSB RSB    {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                node* n2 = new node(++cnt,$3);
                $1->push_back(n1);
                $1->push_back(n2);

    }
    ;
FieldAccess:	  
        Primary DOT Identifier          {
                $$ = new node(++cnt,"FieldAccess");
                nodjoin($$,$1);
                nodtostr($$,$2);
                nodtostr($$,$3);
        }
	|   SUPER DOT Identifier        {
                $$ = new node(++cnt,"FieldAccess");
                nodtostr($$,$1);
                nodtostr($$,$2);
                nodtostr($$,$3);
        }
	;
MethodInvocation: 
        Name LP ArgumentListOpt RP      {
                $$ = new node(++cnt,"MethodInvocation");
                nodjoin($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
        }
	|   Primary DOT Identifier LP ArgumentListOpt RP      {
                $$ = new node(++cnt,"MethodInvocation");
                nodjoin($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
                nodjoin($$,$5);
                nodtostr($$,$6);
        }
	|   SUPER DOT Identifier LP ArgumentListOpt RP      {
                $$ = new node(++cnt,"MethodInvocation");
                nodtostr($$,$1);
                nodtostr($$,$2);
                nodtostr($$,$3);
                nodtostr($$,$4);
                nodjoin($$,$5);
                nodtostr($$,$6);
        }
	|   Name LCB ArgumentListOpt RCB      {
                $$ = new node(++cnt,"MethodInvocation");
                nodjoin($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
        }
	|   Primary DOT Identifier LCB ArgumentListOpt RCB      {
                $$ = new node(++cnt,"MethodInvocation");
                nodjoin($$,$1);
                nodtostr($$,$2);
                nodtostr($$,$3);
                nodtostr($$,$4);
                nodjoin($$,$5);
                nodtostr($$,$6);
        }
	|   SUPER DOT Identifier LCB ArgumentListOpt RCB      {
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
        Name LSB Expression RSB      {
                $$ = new node(++cnt,"ArrayAcces");
                nodjoin($$,$1);
                nodtostr($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
        }
	|   PrimaryNoNewArray LSB Expression RSB      {
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
        PostFixExpression INC      {
                $$ = new node(++cnt,"PostIncrementExpression");
                nodjoin($$,$1);
                nodtostr($$,$2);
        }
	;

PostDecrementExpression: 
        PostFixExpression DNC      {
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
	|   PLUS UnaryExpression      {
                $$ = new node(++cnt,"UnaryExpression");
                nodtostr($$,$1);
                nodjoin($$,$2);
        }
	|   MINUS UnaryExpression      {
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
        INC UnaryExpression      {
                $$ = new node(++cnt,"PreIncrementExpression");
                nodtostr($$,$1);
                nodjoin($$,$2);
        }
	;

PreDecrementExpression: 
        DNC UnaryExpression      {
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
	|   EXCLAMATION UnaryExpression      {
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
        LP PrimitiveType DimsOpt RP UnaryExpression      {
                $$ = new node(++cnt,"CastExpression");
                nodtostr($$,$1);
                nodjoin($$,$2);
                nodjoin($$,$3);
                nodtostr($$,$4);
                nodjoin($$,$5);
        }
	|   LP Expression RP UnaryExpressionNotPlusMinus      {
                $$ = new node(++cnt,"CastExpression");
                nodtostr($$,$1);
                nodjoin($$,$2);
                nodtostr($$,$4);
                nodjoin($$,$5);
        }
	|   LP Name Dims RP UnaryExpressionNotPlusMinus      {
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
	|   AdditiveExpression PLUS MultiplicativeExpression        {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                $1->push_back(n1);
                $1->push_back($3);
        }
	|   AdditiveExpression MINUS MultiplicativeExpression        {
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
	|   ShiftExpression LS AdditiveExpression        {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                $1->push_back(n1);
                $1->push_back($3);
        }
	|   ShiftExpression RS AdditiveExpression        {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                $1->push_back(n1);
                $1->push_back($3);
        }
	|   ShiftExpression URS AdditiveExpression        {
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
	|   RelationalExpression LT ShiftExpression        {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                $1->push_back(n1);
                $1->push_back($3);
        }
	|   RelationalExpression GT ShiftExpression        {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                $1->push_back(n1);
                $1->push_back($3);
        }
	|   RelationalExpression LE ShiftExpression        {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                $1->push_back(n1);
                $1->push_back($3);
        }
	|   RelationalExpression GE ShiftExpression        {
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
	|   EqualityExpression EQ RelationalExpression        {
                $$ = $1;
                node* n1 = new node(++cnt,$2);
                $1->push_back(n1);
                $1->push_back($3);
        }
	|   EqualityExpression NOTEQ RelationalExpression        {
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
	|   InclusiveOrExpression PIPE ExclusiveOrExpression        {
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
	|   ConditionalOrExpression QUES Expression COLON ConditionalExpression        {
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
        ASN          {
                $$ = new node(++cnt,"AssignmentOperator");
                nodtostr($$,$1);
        }
    |   MULASN          {
                $$ = new node(++cnt,"AssignmentOperator");
                nodtostr($$,$1);
        }
    |   DIVASN          {
                $$ = new node(++cnt,"AssignmentOperator");
                nodtostr($$,$1);
        }
    |   MINUSASN          {
                $$ = new node(++cnt,"AssignmentOperator");
                nodtostr($$,$1);
        }
    |   ANDASN          {
                $$ = new node(++cnt,"AssignmentOperator");
                nodtostr($$,$1);
        }
    |   PLUSASN          {
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
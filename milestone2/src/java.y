%{
#include<bits/stdc++.h>
#include "help.h"

using namespace std;

extern char yytext[];
extern int column;
extern int yylineno;
char * type;
char * t1="";
char * t2="";
int cnt;
string localClassId = "";
string localFuncId = "";
int scope = 0;
struct symtabentry{
    string lexeme;
    string synCat;
    string dataType;
    string class_id;
    string func_id;
    vector<string> arguements;
    int lineno;
    int scope;
};
vector<symtabentry> symbolTable;
void insert_entry(string _lexeme, string _synCat, string _dataType ,string _class_id,string _func_id, int _lineno,int _scope) ;
bool check(string _lexeme,string _class_id,string _func_id,int _scope);
char * getType(string _lexeme,string _class_id,string _func_id, int _scope);
char* strConcat(char * s1, char * s2);
bool typeCheck(string s1,string s2);
char * getReturnType(string _class_id,string _func_id);
void print();
int yylex(void);
void yyerror(const char *s);
%}


%verbose

%union {
        double num;
    char *nd;

    char *id;
}

%token< id> SYSTEMOUTPRINTLN LEFTBRACKET RIGHTBRACKET LEFTCURLYBRACKET RIGHTCURLYBRACKET LEFTSQUAREBRACKET RIGHTSQUAREBRACKET THREEDOT SEMICOLON COMMA DOT ATTHERATE DOUBLECOLON TRIPLEGREATEREQUAL TRIPLEGREATER DOUBLELESSEQUAL DOUBLEGREATEREQUAL DOUBLELESS DOUBLEGREATER ADDEQUAL SUBEQUAL MULEQUAL DIVEQUAL ANDEQUAL OREQUAL POWEREQUAL MODEQUAL ARROW EQUALEQUAL GREATEREQUAL LESSEQUAL NOTEQUAL AND OR ADDADD SUBSUB EQUAL LESS GREATER NOT TILDE QUESTION COLON ADD SUB MUL DIV ANDBIT ORBIT POWER MOD


%token <id> ABSTRACT CONTINUE FOR STRING NEW SWITCH ASSERT DEFAULT IF PACKAGE SYNCHRONIZED BOOLEAN DO GOTO PRIVATE THIS BREAK DOUBLE IMPLEMENTS PROTECTED THROWS BYTE ELSE IMPORT PUBLIC THROW CASE ENUM INSTANCE RETURN TRANSIENT CATCH EXTENDS INT SHORT TRY CHAR FINAL INTERFACE STATIC VOID CLASS FINALLY LONG STRICTFP VOLATILE CONST FLOAT NATIVE SUPER WHILE UNDERSEMICOLONORE EXPORTS OPENS REQUIRES USES MODULE PERMITS SEALED VAR NONSEALED PROVIDES TO WITH OPEN RECORD TRANSITIVE YIELD NULLLITERAL SUSPEND

%token <num> INTLITERAL FLOATLITERAL BINARYLITERAL HEXLITERAL

%token <id> IDENTIFIER STRINGLITERAL CHARLITERAL BOOLEANLITERAL

%type <nd> Start PrintlnStatement CompilationUnit Literal Type PrimitiveType NonPrimitiveType NumericType IntegralType FloatingPointType ArrayType Name SimpleName QualifiedName ImportDeclarationOpt TypeDeclarationOpt ImportDeclaration TypeDeclaration SingleTypeImportDeclaration TypeImportOnDemandDeclaration ClassDeclaration Modifier ModifiersOpt ClassBody ClassBodyDeclarationsOpt ClassBodyDeclaration ClassMemberDeclaration StaticInitializer ConstructorDeclaration FieldDeclaration MethodDeclaration VarDeclarator VarDeclaratorId Expression ArrayInitializer MethodBody MethodHeader MethodDeclarator FormalParameterListOpt FormalParameter Block ConstructorDeclarator ConstructorBody ExplicitConstructorInvocationOpt ExplicitConstructorInvocation BlockStatementOpt BlockStatement ArgumentListOpt VarInitializersOpt VarInitializer CommaOpt LocalVariableDeclarationStatement LocalVariableDeclaration Statement StatementWithoutTrailingSubstatement LabeledStatement IfThenStatement IfThenElseStatement WhileStatement ForStatement StatementNoShortIf LabeledStatementNoShortIf IfThenElseStatementNoShortIf WhileStatementNoShortIf ForStatementNoShortIf EmptyStatement ExpressionStatement BreakStatement ContinueStatement ReturnStatement Assignment PreIncrementExpression PreDecrementExpression PostDecrementExpression PostIncrementExpression ClassInstanceCreationExpression ForInitOpt ForInit ExpressionOpt ForUpdateOpt ForUpdate StatementExpression IDENTOpt Primary PrimaryNoNewArray ArrayCreationExpression FieldAccess MethodInvocation ArrayAccess DimsOpt DimExpr PostFixExpression UnaryExpression UnaryExpressionNotPlusMinus CastExpression MultiplicativeExpression AdditiveExpression ShiftExpression RelationalExpression EqualityExpression AndExpression ExclusiveOrExpression InclusiveOrExpression ConditionalAndExpression ConditionalOrExpression ConditionalExpression AssignmentExpression LeftHandSide AssignmentOperator ClassType ClassOrInterfaceType VariableDeclarator VariableDeclaratorId VariableInitializer

%type <nd> ImportDeclarations TypeDeclarations Modifiers ClassBodyDeclarations VariableDeclarators VarDeclarators FormalParameterList BlockStatements VarInitializers StatementExpressionList DimExprs Dims ArgumentList VarDeclaratorIds MethodDeclarators MultiplicativeExpressions AdditiveExpressions ShiftExpressions RelationalExpressions EqualityExpressions AndExpressions ExclusiveOrExpressions InclusiveOrExpressions ConditionalAndExpressions ConditionalOrExpressions ConditionalExpressions ImportDeclarationOpts TypeDeclarationOpts ModifiersOpts ClassBodyDeclarationsOpts FormalParameterListOpts ExplicitConstructorInvocationOpts BlockStatementOpts ArgumentListOpts VarInitializersOpts CommaOpts ForInitOpts IDENTOpts ExpressionOpts ForUpdateOpts DimsOpts

%start Start


%%
Start:
        CompilationUnit
    ;
Literal:
             INTLITERAL       {$$="int";}
	|    FLOATLITERAL       {$$ = "float";}
	|     BOOLEANLITERAL     {$$="boolean";}   
	|    STRINGLITERAL      {$$="string";}
	|    CHARLITERAL        {$$="char";}
	|    NULLLITERAL        {$$="Null";}
	;
Type:
        PrimitiveType           
     |   NonPrimitiveType     
    ;
PrimitiveType:
        NumericType          
     |   BOOLEAN               {$$ = $1;type = $1;}  
     | STRING                   {$$ = $1;type = $1;}
    ;
NumericType:
        IntegralType        
    |   FloatingPointType   
    ;
IntegralType:
        INT                     {$$ = $1;type = $1;}  
    |   LONG                    {$$ = $1;type = $1;}  
    |   CHAR                    {$$ = $1;type = $1;}  
    ;
FloatingPointType:
        FLOAT                   {$$ = $1;type = $1;}  
    |   DOUBLE                  {$$ = $1;type = $1;}  
    ;
NonPrimitiveType:
        ArrayType               
    ;
ArrayType:
        PrimitiveType  LEFTSQUAREBRACKET RIGHTSQUAREBRACKET {type =$1;}
    |   Name LEFTSQUAREBRACKET RIGHTSQUAREBRACKET    {insert_entry($1,"array",type,localClassId, localFuncId ,yylineno,scope);}
    |   ArrayType LEFTSQUAREBRACKET RIGHTSQUAREBRACKET       
    ;
Name:
        SimpleName    {$$=$1;}
    |   QualifiedName   {$$=$1;}
    ;
SimpleName:
        IDENTIFIER   {$$=$1;}    
    ;
QualifiedName:
        Name DOT IDENTIFIER { $$= strConcat($1,strConcat($2,$3));}
    ;
CompilationUnit:
        ImportDeclarationOpt TypeDeclarationOpt 
    ;
ImportDeclarationOpt: ImportDeclarationOpts
;

ImportDeclarationOpts:
        ImportDeclarations      
        | 

    ;

TypeDeclarationOpt: TypeDeclarationOpts
;

TypeDeclarationOpts:
        TypeDeclarations        
        | 

    ;
ImportDeclarations:
        ImportDeclaration       
    |   ImportDeclarations ImportDeclaration    
    ;
TypeDeclarations:
        TypeDeclaration         
    |   TypeDeclarations TypeDeclaration    
    ;
ImportDeclaration:
        SingleTypeImportDeclaration     
    |   TypeImportOnDemandDeclaration     
    ;
SingleTypeImportDeclaration:
        IMPORT Name DOT IDENTIFIER SEMICOLON   {char * temp= strConcat($1,strConcat($2,strConcat($3,$4)));insert_entry(temp,"headerfile","N/A","N/A","N/A",yylineno,scope);}
    ;
TypeImportOnDemandDeclaration :
        IMPORT Name DOT MUL SEMICOLON    {char * temp= strConcat($1,strConcat($2,strConcat($3,$4)));insert_entry(temp,"headerfile","N/A","N/A","N/A",yylineno,scope);}
    ;
TypeDeclaration:
        ClassDeclaration        
    ;
Modifiers:
        Modifier        
    |   Modifiers Modifier      
    ;
Modifier:
        PUBLIC         
    |   PRIVATE        
    |   STATIC        
    ;
ClassDeclaration:
        ModifiersOpt CLASS IDENTIFIER {localClassId=$3;insert_entry($3,"class","N/A",localClassId,localFuncId,yylineno,scope);scope++;} ClassBody { scope--;}
    |   ModifiersOpt CLASS IDENTIFIER {localClassId=$3;insert_entry($3,"class","N/A",localClassId,localFuncId,yylineno,scope);scope++;} EXTENDS IDENTIFIER ClassBody    {scope--;} 
    ;
ModifiersOpt: ModifiersOpts 
;
ModifiersOpts:
        Modifiers       
        | 

    ;
    
ClassBody:
        LEFTCURLYBRACKET ClassBodyDeclarationsOpt RIGHTCURLYBRACKET      
    ;

ClassBodyDeclarationsOpt: ClassBodyDeclarationsOpts
;

ClassBodyDeclarationsOpts:
        ClassBodyDeclarations   
        |  

    ;
ClassBodyDeclarations:
        ClassBodyDeclaration    
    |   ClassBodyDeclarations ClassBodyDeclaration      
    ;
ClassBodyDeclaration:
        ClassMemberDeclaration          
    |   StaticInitializer       
    |   ConstructorDeclaration       
    ;
ClassMemberDeclaration:
        FieldDeclaration          
    |   MethodDeclaration        
    ;
FieldDeclaration:
        ModifiersOpt Type VarDeclarators SEMICOLON    {type = $2;} 
    ;
VarDeclarators:
        VarDeclarator   
    |   VarDeclarators COMMA VarDeclarator      
    ;
VarDeclarator:
        VarDeclaratorId       
    |   VarDeclaratorId EQUAL  VarInitializer     
		;
VarDeclaratorId: VarDeclaratorIds 
;

VarDeclaratorIds:
        IDENTIFIER      {insert_entry($1,"variable",type,localClassId,localFuncId,yylineno,scope);}
    |   VarDeclaratorId LEFTSQUAREBRACKET RIGHTSQUAREBRACKET 
    ;
VarInitializer:
        Expression      
	|   ArrayInitializer    
	;
MethodDeclaration:
        MethodHeader MethodBody 
    ;
MethodHeader:
        ModifiersOpt Type MethodDeclarator    {type=$2;} 
	|   ModifiersOpt VOID MethodDeclarator     {type="void";} 
	;

FormalParameterListOpt: {scope++;} FormalParameterListOpts {scope--;}
;

FormalParameterListOpts:
        FormalParameterList     
        | 

    ;
MethodDeclarator: MethodDeclarators 
;
MethodDeclarators:
        IDENTIFIER {localFuncId=$1;} LEFTBRACKET FormalParameterListOpt RIGHTBRACKET        { insert_entry($1,"function",type,localClassId,localFuncId,yylineno,scope);}
    |   MethodDeclarator LEFTSQUAREBRACKET RIGHTSQUAREBRACKET       
    ;
FormalParameterList:
        FormalParameter        
	|   FormalParameterList COMMA FormalParameter  
	;
FormalParameter:
        Type VarDeclaratorId   {type=$1;} 
	;
MethodBody:
        Block   
    ;
StaticInitializer:
        STATIC Block    
    ;
ConstructorDeclaration:
        ModifiersOpt ConstructorDeclarator ConstructorBody      
	;
ConstructorDeclarator:
        SimpleName LEFTBRACKET FormalParameterListOpt RIGHTBRACKET     {localFuncId = $1 ;insert_entry($1,"function","N/A",localClassId,localFuncId,yylineno,scope);}   
	;

ExplicitConstructorInvocationOpt: ExplicitConstructorInvocationOpts
;
ExplicitConstructorInvocationOpts:
        ExplicitConstructorInvocation          
        |  

    ;

BlockStatementOpt: BlockStatementOpts 
;
BlockStatementOpts:
        BlockStatements 
        | 

    ;

ArgumentListOpt: ArgumentListOpts 

ArgumentListOpts:
        ArgumentList    
        | 

    ;
ConstructorBody:
        LEFTCURLYBRACKET ExplicitConstructorInvocationOpt BlockStatementOpt RIGHTCURLYBRACKET 
    ;
ExplicitConstructorInvocation:
        THIS LEFTBRACKET ArgumentListOpt RIGHTBRACKET SEMICOLON   
    |    SUPER LEFTBRACKET ArgumentListOpt RIGHTBRACKET SEMICOLON 
	;

VarInitializersOpt: VarInitializersOpts
;

VarInitializersOpts:
       VarInitializers        
        | 

    ;

CommaOpt: CommaOpts 
;
CommaOpts:
        COMMA          
        | 

    ;
ArrayInitializer:
        LEFTCURLYBRACKET VarInitializersOpt CommaOpt RIGHTCURLYBRACKET     
    ;
VarInitializers:
        VarInitializer  
	|   VarInitializers COMMA VarInitializer  
    ;
Block:
        LEFTCURLYBRACKET {scope++;} BlockStatementOpt RIGHTCURLYBRACKET    {scope--;}  
    ;
BlockStatements:
        BlockStatement          
    |   BlockStatements BlockStatement  
    ;
BlockStatement:
        LocalVariableDeclarationStatement       
    |   Statement       
    ;
LocalVariableDeclarationStatement:
        LocalVariableDeclaration SEMICOLON       
    ;
LocalVariableDeclaration: Type VariableDeclarators {type = $1;}
	;
VariableDeclarators: VariableDeclarator 
		| VariableDeclarators COMMA VariableDeclarator 
		;

VariableDeclarator: VariableDeclaratorId 
		| VariableDeclaratorId EQUAL VariableInitializer {if(!typeCheck($1,$3)){cout<<"Type Mismatch:"<<yylineno<<endl; exit(2);}} 
		;

VariableInitializer: Expression {$$=$1;}
		| ArrayInitializer 
		;

VariableDeclaratorId: IDENTIFIER {insert_entry($1,"variable",type,localClassId,localFuncId,yylineno,scope);$$=type;}
		| VariableDeclaratorId LEFTSQUAREBRACKET RIGHTSQUAREBRACKET {$$=type;}
		;

Statement:
        StatementWithoutTrailingSubstatement  
	|   LabeledStatement   
	|   IfThenStatement    
	|   IfThenElseStatement    
	|   WhileStatement    
	|   ForStatement   
        |   PrintlnStatement    
	;
PrintlnStatement:
                SYSTEMOUTPRINTLN LEFTBRACKET Expression RIGHTBRACKET SEMICOLON    
	;
StatementNoShortIf:
        StatementWithoutTrailingSubstatement    
	|   LabeledStatementNoShortIf   
	|   IfThenElseStatementNoShortIf    
	|   WhileStatementNoShortIf    
	|   ForStatementNoShortIf    
	;

StatementWithoutTrailingSubstatement:
        Block    
	|   EmptyStatement
	|   ExpressionStatement    
	|   BreakStatement   
	|   ContinueStatement    
	|   ReturnStatement    {char * ch = getReturnType(localClassId,localFuncId);if(!typeCheck($1,ch)){cout<<"Return type mismatch"<<yylineno<<endl;exit(56);}}
	;
EmptyStatement:
        SEMICOLON    
    ;
LabeledStatement:
        IDENTIFIER COLON Statement    
    ;
LabeledStatementNoShortIf:
        IDENTIFIER COLON StatementNoShortIf   
        
    ;
ExpressionStatement:
        StatementExpression SEMICOLON    
    ;
StatementExpression:
        Assignment    
    |   PreIncrementExpression    
    |   PreDecrementExpression    
    |   PostIncrementExpression    
    |   PostDecrementExpression    
    |   MethodInvocation    
    |   ClassInstanceCreationExpression    
    ;

ClassInstanceCreationExpression: NEW ClassType LEFTBRACKET ArgumentListOpt RIGHTBRACKET //yha baaki hai new class
;

ClassType:	  ClassOrInterfaceType 
		;

ClassOrInterfaceType: Name 
		;

IfThenStatement:
        IF LEFTBRACKET Expression RIGHTBRACKET Statement    
    ;
IfThenElseStatement:
        IF LEFTBRACKET Expression RIGHTBRACKET  StatementNoShortIf ELSE Statement    
    ;
IfThenElseStatementNoShortIf:
        IF LEFTBRACKET Expression RIGHTBRACKET StatementNoShortIf ELSE StatementNoShortIf    
    ;
WhileStatement:
        WHILE LEFTBRACKET Expression RIGHTBRACKET Statement    
	;

WhileStatementNoShortIf:
        WHILE LEFTBRACKET Expression RIGHTBRACKET StatementNoShortIf    
    ;
ForInitOpt: ForInitOpts 
;

ForInitOpts:
        ForInit    
        | 

;

ExpressionOpt: ExpressionOpts {$$=$1;}
;
ExpressionOpts:
        Expression    {$$=$1;}
        |       

     ;

ForUpdateOpt: ForUpdateOpts 
;
ForUpdateOpts:
        ForUpdate     
        | 

    ;

ForStatement:
        FOR LEFTBRACKET ForInitOpt SEMICOLON ExpressionOpt SEMICOLON ForUpdateOpt RIGHTBRACKET Statement     
	;

ForStatementNoShortIf:
        FOR LEFTBRACKET ForInitOpt SEMICOLON ExpressionOpt SEMICOLON ForUpdateOpt RIGHTBRACKET StatementNoShortIf
        
	;


ForInit:
         StatementExpressionList     
	|    LocalVariableDeclaration     
	;

ForUpdate:
        StatementExpressionList     
	;

StatementExpressionList:
        StatementExpression     
	|   StatementExpressionList COMMA StatementExpression   
	;

IDENTOpt: IDENTOpts 

IDENTOpts: IDENTIFIER   {insert_entry($1,"variable",type,localClassId,"N/A",yylineno,scope);}
        |  

     ;

BreakStatement:
        BREAK IDENTOpt SEMICOLON    
	;

ContinueStatement:
        CONTINUE IDENTOpt SEMICOLON    
	;

ReturnStatement:
        RETURN ExpressionOpt SEMICOLON    {$$=$2;}
	|   SUSPEND ExpressionOpt SEMICOLON    
	;
Primary:
        PrimaryNoNewArray    {$$=$1;}
    |   ArrayCreationExpression    
    ;
PrimaryNoNewArray:
        Literal   {$$=$1;}
    |   THIS    
    |   LEFTBRACKET Expression RIGHTBRACKET    
    |   FieldAccess    {$$=$1;}
    |   MethodInvocation   {$$=$1;} 
    |   ArrayAccess    {$$=$1;}
    ;
ArgumentList:
        Expression    
	|   ArgumentList COMMA Expression       
	;

DimsOpt: DimsOpts 
;

DimsOpts: Dims   
        | 

    ;
ArrayCreationExpression:
        NEW PrimitiveType DimExprs 
        |  NEW PrimitiveType DimExprs Dims           
	;
DimExprs:
        DimExpr 
	|   DimExprs DimExpr    
	;
DimExpr:
        LEFTSQUAREBRACKET Expression RIGHTSQUAREBRACKET      
    ;
Dims:
        LEFTSQUAREBRACKET RIGHTSQUAREBRACKET         
    |   Dims LEFTSQUAREBRACKET RIGHTSQUAREBRACKET    
    ;
FieldAccess:
        Primary DOT IDENTIFIER         
	|   SUPER DOT IDENTIFIER   {char *temp = strConcat($1,strConcat($2,$3));insert_entry(temp,"fieldaccess","N/A",localClassId,"N/A",yylineno,scope); $$ = temp;}          
	;
MethodInvocation:
        Name LEFTBRACKET ArgumentListOpt RIGHTBRACKET      {$$ = getType($1,localClassId,localFuncId,scope);}
	|   Primary DOT IDENTIFIER LEFTBRACKET ArgumentListOpt RIGHTBRACKET      {$$ = getType($3,localClassId,localFuncId,scope);}
	|   SUPER DOT IDENTIFIER LEFTBRACKET ArgumentListOpt RIGHTBRACKET     {$$ = getType($3,localClassId,localFuncId,scope);}
	|   Name LEFTCURLYBRACKET ArgumentListOpt RIGHTCURLYBRACKET      {$$ = getType($1,localClassId,localFuncId,scope);}
	|   Primary DOT IDENTIFIER LEFTCURLYBRACKET ArgumentListOpt RIGHTCURLYBRACKET   {$$ = getType($3,localClassId,localFuncId,scope);}  
	|   SUPER DOT IDENTIFIER LEFTCURLYBRACKET ArgumentListOpt RIGHTCURLYBRACKET     {$$ = getType($3,localClassId,localFuncId,scope);} 
	;
ArrayAccess:
        Name LEFTSQUAREBRACKET Expression RIGHTSQUAREBRACKET    {$$ = getType($1,localClassId,localFuncId,scope);}  
	|   PrimaryNoNewArray LEFTSQUAREBRACKET Expression RIGHTSQUAREBRACKET     
	;

PostFixExpression:
        Primary      {$$=$1;}
	|   Name      {$$ = getType($1,localClassId,localFuncId,scope);}
	|   PostIncrementExpression   {$$=$1;}   
	|   PostDecrementExpression     {$$=$1;} 
	;

PostIncrementExpression:
        PostFixExpression ADDADD    {if(typeCheck($1,"int")){$$=$1;} else {cout<<"Type Mismatch"<<yylineno<<endl;exit(3);}}  
	;

PostDecrementExpression:
        PostFixExpression SUBSUB     {if(typeCheck($1,"int"))$$=$1; else {cout<<"Type Mismatch"<<yylineno<<endl;exit(4);}}
	;

UnaryExpression:
        PreIncrementExpression    {$$=$1;}  
        |   PreDecrementExpression   {$$=$1;}   
	|   ADD UnaryExpression     {if(typeCheck($1,"int"))$$=$2; else {cout<<"Type Mismatch"<<yylineno<<endl;exit(5);}} 
	|   SUB UnaryExpression     {if(typeCheck($1,"int"))$$=$2; else {cout<<"Type Mismatch"<<yylineno<<endl;exit(6);}} 
	|   UnaryExpressionNotPlusMinus     {$$=$1;} 
	;

PreIncrementExpression:
        ADDADD UnaryExpression    {if(typeCheck($1,"int"))$$=$2; else {cout<<"Type Mismatch"<<yylineno<<endl;exit(7);}}   
	;

PreDecrementExpression:
        SUBSUB UnaryExpression    {if(typeCheck($1,"int"))$$=$2; else {cout<<"Type Mismatch"<<yylineno<<endl;exit(8);}} 
	;

UnaryExpressionNotPlusMinus:
        PostFixExpression      {$$=$1;}
	|   TILDE UnaryExpression     {$$=$2;}
	|   NOT UnaryExpression      {if(typeCheck($1,"boolean"))$$="boolean"; else {cout<<"Type Mismatch"<<yylineno<<endl;exit(9);}}
	|   CastExpression      {$$="int";}
	;

CastExpression:
        LEFTBRACKET PrimitiveType DimsOpt RIGHTBRACKET UnaryExpression      
	|   LEFTBRACKET Expression RIGHTBRACKET UnaryExpressionNotPlusMinus      
	|   LEFTBRACKET Name Dims RIGHTBRACKET UnaryExpressionNotPlusMinus     
	;

MultiplicativeExpression:  MultiplicativeExpressions {$$=$1;}
;

MultiplicativeExpressions:
        UnaryExpression     {$$=$1;} 
	|   MultiplicativeExpression MUL UnaryExpression     {if(typeCheck($1,$3)){$$=$1;}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(10);}}    
	|   MultiplicativeExpression DIV UnaryExpression     {if(typeCheck($1,$3)){$$=$1;}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(11);}}    
	|   MultiplicativeExpression MOD UnaryExpression      {if(typeCheck($1,$3)){$$=$1;}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(12);}}  
	;

AdditiveExpression: AdditiveExpressions{$$=$1;}
;

AdditiveExpressions:
        MultiplicativeExpression  {$$=$1;}   
	|   AdditiveExpression ADD MultiplicativeExpression    {if(typeCheck($1,$3)){$$=$1;}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(13);}}   
	|   AdditiveExpression SUB  MultiplicativeExpression   {if(typeCheck($1,$3)){$$=$1;}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(14);}}      
	;

ShiftExpression: ShiftExpressions{$$=$1;}
;

ShiftExpressions:
        AdditiveExpression    {$$=$1;} 
	|   ShiftExpression DOUBLELESS AdditiveExpression     {if(typeCheck($1,$3)){$$=$1;}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(15);}}   
	|   ShiftExpression DOUBLEGREATER AdditiveExpression    {if(typeCheck($1,$3)){$$=$1;}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(16);}}  
	|   ShiftExpression TRIPLEGREATER AdditiveExpression     {if(typeCheck($1,$3)){$$=$1;}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(17);}}  
	;

RelationalExpression: RelationalExpressions{$$=$1;}
;
// here
RelationalExpressions:
        ShiftExpression     {$$=$1;}
	|   RelationalExpression LESS ShiftExpression        {if(typeCheck($1,$3)){$$="boolean";}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(18);}} 
	|   RelationalExpression GREATER ShiftExpression      {if(typeCheck($1,$3)){$$="boolean";}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(19);}} 
	|   RelationalExpression LESSEQUAL ShiftExpression     {if(typeCheck($1,$3)){$$="boolean";}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(20);}}  
	|   RelationalExpression GREATEREQUAL ShiftExpression    {if(typeCheck($1,$3)){$$="boolean";}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(21);}}    
;

EqualityExpression: EqualityExpressions{$$=$1;}
;

EqualityExpressions:
        RelationalExpression   {$$=$1;}   
	|   EqualityExpression EQUALEQUAL RelationalExpression     {if(typeCheck($1,$3)){$$="boolean";}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(22);}}   
	|   EqualityExpression NOTEQUAL RelationalExpression       {if(typeCheck($1,$3)){$$="boolean";}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(23);}}  
	;
AndExpression: AndExpressions   {$$=$1;}
;
AndExpressions:
        EqualityExpression     {$$=$1;} 
	|   AndExpression ANDBIT EqualityExpression        {if(typeCheck($1,$3)){$$="boolean";}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(24);}} 
	;

ExclusiveOrExpression: ExclusiveOrExpressions{$$=$1;}
;
ExclusiveOrExpressions:
        AndExpression     {$$=$1;}
	|   ExclusiveOrExpression POWER AndExpression        {if(typeCheck($1,$3)){$$=$1;}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(25);}} 
	;

InclusiveOrExpression: InclusiveOrExpressions {$$=$1;}
;

InclusiveOrExpressions:
        ExclusiveOrExpression {$$=$1;}
	|   InclusiveOrExpression ORBIT ExclusiveOrExpression    {if(typeCheck($1,$3)){$$=$1;}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(26);}} 
	;

ConditionalAndExpression: ConditionalAndExpressions {$$=$1;}
;

ConditionalAndExpressions:
        InclusiveOrExpression     {$$=$1;}
	|   ConditionalAndExpression AND InclusiveOrExpression     {if(typeCheck($1,$3)){$$="boolean";}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(27);}} 
	;

ConditionalOrExpression: ConditionalOrExpressions {$$=$1;}
;

ConditionalOrExpressions:
        ConditionalAndExpression     {$$=$1;}
	|   ConditionalOrExpression OR ConditionalAndExpression     {if(typeCheck($1,$3)){$$="boolean";}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(28);}} 
	;

ConditionalExpression : ConditionalExpressions {$$=$1;}
;

ConditionalExpressions: ConditionalOrExpression      {$$=$1;}
	|   ConditionalOrExpression QUESTION Expression COLON ConditionalExpression      {if(typeCheck($1,"boolean")){$$=$3;}else{$$=$5;}}
	;

AssignmentExpression:
        ConditionalExpression    {$$=$1;}
	|   Assignment      {$$=$1;}
	;

Assignment:
        LeftHandSide {t1 = $1;}  AssignmentOperator AssignmentExpression {t2 = $3;if(typeCheck(t1,t2)){$$=t1;} else{cout<<"type Mismatch:"<<yylineno<<endl;exit(29);}} 
	;

LeftHandSide:
        Name          {char *t = getType($1,localClassId,localFuncId,scope);$$=t;}
	|   FieldAccess     {$$ = $1;} 
	|   ArrayAccess       {$$=$1;} 
	;

 AssignmentOperator:
        EQUAL          
    |   MULEQUAL
    |   DIVEQUAL         
    |   SUBEQUAL          
    |   ANDEQUAL         
    |   ADDEQUAL        
    ;

Expression:
        AssignmentExpression         {$$=$1;}
	;

%%

int main(void) {
    yyparse();
    print();
    return 0;
}


void insert_entry(string _lexeme, string _synCat, string _dataType ,string _class_id,string _func_id,int _lineno,int _scope) {
    if(!check(_lexeme,_class_id,_func_id,_scope)){
        symtabentry temp;
        temp.lexeme = _lexeme;
        temp.synCat = _synCat;
        temp.dataType = _dataType;
        temp.class_id = _class_id;
        temp.func_id=_func_id;
        temp.lineno = _lineno;
        temp.scope = _scope;
        symbolTable.push_back(temp);
    }
    else{
        cout<<_lexeme<<": redeclaration at line no:"<<yylineno<<endl;exit(30);
    }
    
}
bool check(string _lexeme,string _class_id,string _func_id, int _scope){
    for(auto i:symbolTable){
        if((i.lexeme==_lexeme&&i.class_id==_class_id&&i.scope==_scope&&i.func_id==_func_id)) return true;
    }
    return false;
}
void print(void){
        for(auto i: symbolTable){
                cout<<i.lexeme<<" "<<i.synCat<<" "<<i.dataType<<" "<<i.class_id<<" "<<i.func_id<<" "<<i.lineno<<" "<<i.scope<<endl;
        }
}

char * strConcat(char *s1, char*s2){
        char * str3 = (char *) malloc(1 + strlen(s1)+ strlen(s2) );
      strcpy(str3, s1);
      strcat(str3, s2);
      return str3;
}

bool typeCheck(string s1, string s2){
        if(s1==s2){
                return true;
        }
        return false;
}
char * getType(string _lexeme,string _class_id,string _func_id, int _scope){
        while(_scope>-1){
                        for(auto i:symbolTable){
                        if((i.lexeme==_lexeme&&i.class_id==_class_id&&i.scope==_scope)) { return strdup(i.dataType.c_str());}
                }
                _scope--;
        }
    cout<<_lexeme<<": this is not declared in this scope"<<yylineno<<endl;
    exit(31);
    
}
char * getReturnType(string _class_id,string _func_id){
        for(auto i:symbolTable){
        if((i.class_id==_class_id&&i.func_id==_func_id)) { return strdup(i.dataType.c_str());}
    }
    exit(31);
    
}
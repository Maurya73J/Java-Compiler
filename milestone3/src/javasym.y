%{
#include<bits/stdc++.h>
#include "help.h"

using namespace std;

extern char yytext[];
extern int column;
extern int yylineno;
char * type;
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
    string dimentions;
};
vector<symtabentry> symbolTable;
void insert_entry(string _lexeme, string _synCat, string _dataType ,string _class_id,string _func_id, int _lineno,int _scope,string _dimentions) ;
bool check(string _lexeme,string _class_id,string _func_id,int _scope);
char* strConcat(char * s1, char * s2);
char * temp="";
void print();
int yylex(void);
void yyerror(const char *s);
%}


%verbose

%union {
        char * num;
    char *nd;
    list<char*> *ndl;
    char *id;
}

%token< id> SYSTEMOUTPRINTLN LEFTBRACKET RIGHTBRACKET LEFTCURLYBRACKET RIGHTCURLYBRACKET LEFTSQUAREBRACKET RIGHTSQUAREBRACKET THREEDOT SEMICOLON COMMA DOT ATTHERATE DOUBLECOLON TRIPLEGREATEREQUAL TRIPLEGREATER DOUBLELESSEQUAL DOUBLEGREATEREQUAL DOUBLELESS DOUBLEGREATER ADDEQUAL SUBEQUAL MULEQUAL DIVEQUAL ANDEQUAL OREQUAL POWEREQUAL MODEQUAL ARROW EQUALEQUAL GREATEREQUAL LESSEQUAL NOTEQUAL AND OR ADDADD SUBSUB EQUAL LESS GREATER NOT TILDE QUESTION COLON ADD SUB MUL DIV ANDBIT ORBIT POWER MOD


%token <id> ABSTRACT CONTINUE STRING FOR NEW SWITCH ASSERT DEFAULT IF PACKAGE SYNCHRONIZED BOOLEAN DO GOTO PRIVATE THIS BREAK DOUBLE IMPLEMENTS PROTECTED THROWS BYTE ELSE IMPORT PUBLIC THROW CASE ENUM INSTANCE RETURN TRANSIENT CATCH EXTENDS INT SHORT TRY CHAR FINAL INTERFACE STATIC VOID CLASS FINALLY LONG STRICTFP VOLATILE CONST FLOAT NATIVE SUPER WHILE UNDERSEMICOLONORE EXPORTS OPENS REQUIRES USES MODULE PERMITS SEALED VAR NONSEALED PROVIDES TO WITH OPEN RECORD TRANSITIVE YIELD NULLLITERAL SUSPEND

%token <num> INTLITERAL FLOATLITERAL BINARYLITERAL HEXLITERAL

%token <id> IDENTIFIER STRINGLITERAL CHARLITERAL BOOLEANLITERAL

%type <nd> Start PrintlnStatement CompilationUnit Literal Type PrimitiveType NonPrimitiveType NumericType IntegralType FloatingPointType ArrayType Name SimpleName QualifiedName ImportDeclarationOpt TypeDeclarationOpt ImportDeclaration TypeDeclaration SingleTypeImportDeclaration TypeImportOnDemandDeclaration ClassDeclaration Modifier ModifiersOpt ClassBody ClassBodyDeclarationsOpt ClassBodyDeclaration ClassMemberDeclaration StaticInitializer ConstructorDeclaration FieldDeclaration MethodDeclaration VarDeclarator VarDeclaratorId Expression ArrayInitializer MethodBody MethodHeader MethodDeclarator FormalParameterListOpt FormalParameter Block ConstructorDeclarator ConstructorBody ExplicitConstructorInvocationOpt ExplicitConstructorInvocation BlockStatementOpt BlockStatement ArgumentListOpt VarInitializersOpt VarInitializer CommaOpt LocalVariableDeclarationStatement LocalVariableDeclaration Statement StatementWithoutTrailingSubstatement LabeledStatement IfThenStatement IfThenElseStatement WhileStatement ForStatement StatementNoShortIf LabeledStatementNoShortIf IfThenElseStatementNoShortIf WhileStatementNoShortIf ForStatementNoShortIf EmptyStatement ExpressionStatement BreakStatement ContinueStatement ReturnStatement Assignment PreIncrementExpression PreDecrementExpression PostDecrementExpression PostIncrementExpression ClassInstanceCreationExpression ForInitOpt ForInit ExpressionOpt ForUpdateOpt ForUpdate StatementExpression IDENTOpt Primary PrimaryNoNewArray ArrayCreationExpression FieldAccess MethodInvocation ArrayAccess DimsOpt DimExpr PostFixExpression UnaryExpression UnaryExpressionNotPlusMinus CastExpression MultiplicativeExpression AdditiveExpression ShiftExpression RelationalExpression EqualityExpression AndExpression ExclusiveOrExpression InclusiveOrExpression ConditionalAndExpression ConditionalOrExpression ConditionalExpression AssignmentExpression LeftHandSide AssignmentOperator ClassType ClassOrInterfaceType VariableDeclarator VariableDeclaratorId VariableInitializer

%type <ndl> ImportDeclarations TypeDeclarations Modifiers ClassBodyDeclarations VariableDeclarators VarDeclarators FormalParameterList BlockStatements VarInitializers StatementExpressionList DimExprs Dims ArgumentList VarDeclaratorIds MethodDeclarators MultiplicativeExpressions AdditiveExpressions ShiftExpressions RelationalExpressions EqualityExpressions AndExpressions ExclusiveOrExpressions InclusiveOrExpressions ConditionalAndExpressions ConditionalOrExpressions ConditionalExpressions ImportDeclarationOpts TypeDeclarationOpts ModifiersOpts ClassBodyDeclarationsOpts FormalParameterListOpts BlockStatementOpts ArgumentListOpts  CommaOpts ForInitOpts IDENTOpts ExpressionOpts ForUpdateOpts DimsOpts

%start Start


%%
Start:
        CompilationUnit
    ;
Literal:
             INTLITERAL       {$$=$1;}
	|    FLOATLITERAL       
	|     BOOLEANLITERAL     {$$=$1;}   
	|    STRINGLITERAL      {$$=$1;}
	|    CHARLITERAL        {$$=$1;}
	|    NULLLITERAL        {$$=$1;}
	;
Type:
        PrimitiveType           
     |   NonPrimitiveType     
    ;
PrimitiveType:
        NumericType          
     |   BOOLEAN               {$$ = $1;type = $1;}
     |   STRING              {$$ = $1;type = $1;}  

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
    |   Name LEFTSQUAREBRACKET RIGHTSQUAREBRACKET    {insert_entry($1,"array",type,localClassId, localFuncId ,yylineno,scope,"N/A");}
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
        IMPORT Name DOT IDENTIFIER SEMICOLON   {char * temp= strConcat($1,strConcat($2,strConcat($3,$4)));insert_entry(temp,"headerfile","N/A","N/A","N/A",yylineno,scope,"N/A");}
    ;
TypeImportOnDemandDeclaration :
        IMPORT Name DOT MUL SEMICOLON    {char * temp= strConcat($1,strConcat($2,strConcat($3,$4)));insert_entry(temp,"headerfile","N/A","N/A","N/A",yylineno,scope,"N/A");}
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
        ModifiersOpt CLASS IDENTIFIER {localClassId=$3;insert_entry($3,"class","N/A",localClassId,localFuncId,yylineno,scope,"N/A");scope++;} ClassBody { scope--;}
    |   ModifiersOpt CLASS IDENTIFIER {localClassId=$3;insert_entry($3,"class","N/A",localClassId,localFuncId,yylineno,scope,"N/A");scope++;} EXTENDS IDENTIFIER ClassBody    {scope--;} 
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
        IDENTIFIER      {insert_entry($1,"variable",type,localClassId,localFuncId,yylineno,scope,"N/A");}
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
        IDENTIFIER {localFuncId=$1;} LEFTBRACKET FormalParameterListOpt RIGHTBRACKET        { insert_entry($1,"function",type,localClassId,localFuncId,yylineno,scope,"N/A");}
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
        SimpleName LEFTBRACKET FormalParameterListOpt RIGHTBRACKET     {localFuncId = $1 ;insert_entry($1,"function","N/A",localClassId,localFuncId,yylineno,scope,"N/A");}   
	;


ExplicitConstructorInvocationOpt:
        ExplicitConstructorInvocation          
//        |  {here changes}

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


VarInitializersOpt:
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
	|   VarInitializersOpt COMMA VarInitializer  
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

VariableDeclarator: VariableDeclaratorId {insert_entry($1,"variable",type,localClassId,localFuncId,yylineno,scope,"N/A");}
		| VariableDeclaratorId EQUAL VariableInitializer         {string s = $3;if(s=="new"){insert_entry($1,"array",type,localClassId,localFuncId,yylineno,scope,temp);}else{ {insert_entry($1,"variable",type,localClassId,localFuncId,yylineno,scope,"N/A");}}}











		;

VariableInitializer: Expression {$$=$1;}
		| ArrayInitializer 
		;

VariableDeclaratorId: IDENTIFIER 
		| VariableDeclaratorId LEFTSQUAREBRACKET RIGHTSQUAREBRACKET 
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
	|   ReturnStatement    
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

ClassInstanceCreationExpression: NEW ClassType LEFTBRACKET ArgumentList RIGHTBRACKET //yha baaki hai new class
| NEW ClassType LEFTBRACKET RIGHTBRACKET
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

ExpressionOpt: ExpressionOpts 
;
ExpressionOpts:
        Expression    
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

IDENTOpts: IDENTIFIER   {insert_entry($1,"variable",type,localClassId,"N/A",yylineno,scope,"N/A");}
        |  

     ;

BreakStatement:
        BREAK IDENTOpt SEMICOLON    
	;

ContinueStatement:
        CONTINUE IDENTOpt SEMICOLON    
	;

ReturnStatement:
        RETURN ExpressionOpt SEMICOLON    
	|   SUSPEND ExpressionOpt SEMICOLON    
	;
Primary:
        PrimaryNoNewArray    
    |   ArrayCreationExpression    
    ;
PrimaryNoNewArray:
        Literal   
    |   THIS    
    |   LEFTBRACKET Expression RIGHTBRACKET    
    |   FieldAccess    
    |   MethodInvocation    
    |   ArrayAccess    
    ;
ArgumentList:
        Expression    
	|   ArgumentList COMMA Expression       
	;

DimsOpt: DimsOpt 
;

DimsOpts: Dims   
        | 

    ;
ArrayCreationExpression:
        NEW PrimitiveType DimExprs 
        |  NEW PrimitiveType DimExprs Dims           
	;
DimExprs:
        DimExpr {temp = $1;}
	|   DimExprs DimExpr    {char *temp1=strConcat(temp,strConcat(":",$2));temp = temp1;}
	;
DimExpr:
        LEFTSQUAREBRACKET Expression RIGHTSQUAREBRACKET     {$$ = $2;} 
    ;
Dims:
        LEFTSQUAREBRACKET RIGHTSQUAREBRACKET         
    |   Dims LEFTSQUAREBRACKET RIGHTSQUAREBRACKET    
    ;
FieldAccess:
        Primary DOT IDENTIFIER         
	|   SUPER DOT IDENTIFIER   {char *temp = strConcat($1,strConcat($2,$3));insert_entry(temp,"fieldaccess","N/A",localClassId,"N/A",yylineno,scope,"N/A");}          
	;
MethodInvocation:
        Name LEFTBRACKET ArgumentListOpt RIGHTBRACKET      
	|   Primary DOT IDENTIFIER LEFTBRACKET ArgumentListOpt RIGHTBRACKET      
	|   SUPER DOT IDENTIFIER LEFTBRACKET ArgumentListOpt RIGHTBRACKET     
	|   Name LEFTCURLYBRACKET ArgumentListOpt RIGHTCURLYBRACKET      
	|   Primary DOT IDENTIFIER LEFTCURLYBRACKET ArgumentListOpt RIGHTCURLYBRACKET     
	|   SUPER DOT IDENTIFIER LEFTCURLYBRACKET ArgumentListOpt RIGHTCURLYBRACKET      
	;
ArrayAccess:
        Name LEFTSQUAREBRACKET Expression RIGHTSQUAREBRACKET      
	|   PrimaryNoNewArray LEFTSQUAREBRACKET Expression RIGHTSQUAREBRACKET     
	;

PostFixExpression:
        Primary      
	|   Name      
	|   PostIncrementExpression      
	|   PostDecrementExpression      
	;

PostIncrementExpression:
        PostFixExpression ADDADD      
	;

PostDecrementExpression:
        PostFixExpression SUBSUB     
	;

UnaryExpression:
        PreIncrementExpression      
        |   PreDecrementExpression      
	|   ADD UnaryExpression     
	|   SUB UnaryExpression     
	|   UnaryExpressionNotPlusMinus      
	;

PreIncrementExpression:
        ADDADD UnaryExpression      
	;

PreDecrementExpression:
        SUBSUB UnaryExpression      
	;

UnaryExpressionNotPlusMinus:
        PostFixExpression      
	|   TILDE UnaryExpression     
	|   NOT UnaryExpression      
	|   CastExpression      
	;

CastExpression:
        LEFTBRACKET PrimitiveType DimsOpt RIGHTBRACKET UnaryExpression      
	|   LEFTBRACKET Expression RIGHTBRACKET UnaryExpressionNotPlusMinus      
	|   LEFTBRACKET Name Dims RIGHTBRACKET UnaryExpressionNotPlusMinus     
	;

MultiplicativeExpression:  MultiplicativeExpressions 
;

MultiplicativeExpressions:
        UnaryExpression      
	|   MultiplicativeExpression MUL UnaryExpression        
	|   MultiplicativeExpression DIV UnaryExpression        
	|   MultiplicativeExpression MOD UnaryExpression       
	;

AdditiveExpression: AdditiveExpressions
;

AdditiveExpressions:
        MultiplicativeExpression     
	|   AdditiveExpression ADD MultiplicativeExpression      
	|   AdditiveExpression SUB  MultiplicativeExpression        
	;

ShiftExpression: ShiftExpressions
;

ShiftExpressions:
        AdditiveExpression     
	|   ShiftExpression DOUBLELESS AdditiveExpression       
	|   ShiftExpression DOUBLEGREATER AdditiveExpression     
	|   ShiftExpression TRIPLEGREATER AdditiveExpression      
	;

RelationalExpression: RelationalExpressions
;

RelationalExpressions:
        ShiftExpression     
	|   RelationalExpression LESS ShiftExpression        
	|   RelationalExpression GREATER ShiftExpression      
	|   RelationalExpression LESSEQUAL ShiftExpression      
	|   RelationalExpression GREATEREQUAL ShiftExpression       
;

EqualityExpression: EqualityExpressions
;

EqualityExpressions:
        RelationalExpression      
	|   EqualityExpression EQUALEQUAL RelationalExpression       
	|   EqualityExpression NOTEQUAL RelationalExpression        
	;
AndExpression: AndExpressions
;
AndExpressions:
        EqualityExpression      
	|   AndExpression ANDBIT EqualityExpression        
	;

ExclusiveOrExpression: ExclusiveOrExpressions
;
ExclusiveOrExpressions:
        AndExpression     
	|   ExclusiveOrExpression POWER AndExpression        
	;

InclusiveOrExpression: InclusiveOrExpressions
;

InclusiveOrExpressions:
        ExclusiveOrExpression
	|   InclusiveOrExpression ORBIT ExclusiveOrExpression       
	;

ConditionalAndExpression: ConditionalAndExpressions
;

ConditionalAndExpressions:
        InclusiveOrExpression     
	|   ConditionalAndExpression AND InclusiveOrExpression      
	;

ConditionalOrExpression: ConditionalOrExpressions
;

ConditionalOrExpressions:
        ConditionalAndExpression     
	|   ConditionalOrExpression OR ConditionalAndExpression      
	;

ConditionalExpression : ConditionalExpressions 
;

ConditionalExpressions: ConditionalOrExpression      
	|   ConditionalOrExpression QUESTION Expression COLON ConditionalExpression      
	;

AssignmentExpression:
        ConditionalExpression    
	|   Assignment      
	;

Assignment:
        LeftHandSide AssignmentOperator AssignmentExpression     
	;

LeftHandSide:
        Name          
	|   FieldAccess         
	|   ArrayAccess        
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
        AssignmentExpression         
	;

%%

int main(void) {
    yyparse();
    print();
    return 0;
}


void insert_entry(string _lexeme, string _synCat, string _dataType ,string _class_id,string _func_id,int _lineno,int _scope,string _dimentions = "N/A") {
    if(!check(_lexeme,_class_id,_func_id,_scope)){
        symtabentry temp;
        temp.lexeme = _lexeme;
        temp.synCat = _synCat;
        temp.dataType = _dataType;
        temp.class_id = _class_id;
        temp.func_id=_func_id;
        temp.lineno = _lineno;
        temp.scope = _scope;
        temp.dimentions = "{"+_dimentions+"}";
        symbolTable.push_back(temp);
    }
    else{
        cout<<_lexeme<<": redeclaration at line no:"<<yylineno<<endl;exit(10);
    }
    
}
bool check(string _lexeme,string _class_id,string _func_id, int _scope){
    for(auto i:symbolTable){
        if(i.lexeme==_lexeme&&i.class_id==_class_id&&i.scope==_scope&&i.func_id==_func_id) return true;
    }
    return false;
}
void print(void){
        ofstream fout("symbolTable.csv");
        for(auto i: symbolTable){
                fout<<i.lexeme<<","<<i.synCat<<","<<i.dataType<<","<<i.class_id<<","<<i.func_id<<","<<i.lineno<<","<<i.scope<<","<<i.dimentions<< endl;
        }
}

char * strConcat(char *s1, char*s2){
        char * str3 = (char *) malloc(1 + strlen(s1)+ strlen(s2) );
      strcpy(str3, s1);
      strcat(str3, s2);
      return str3;
}
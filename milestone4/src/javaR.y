%{
#include<bits/stdc++.h>
// #include"runtime.h"
#include"codeGen.h"


using namespace std;

extern char yytext[];
extern int column;
extern int yylineno;
int cnt,temp;
int label=0;
vector<quad> ir;
int yylex(void);
void yyerror(const char *s);
int check=0;
char buff[100];
char * strConcat(char *s1, char*s2);
vector<string> find(string _lexeme);
int typesize(string s);
string getType(string s);
vector<vector<string>> SymbolTable;
map<string,Klass> mp;
void SymTab();
string array3ac(string s);
string word;
void ch_clear(quad &ch);
ofstream code_file;
int line=1;
int breakContinue=0;
int param = 0,flag=0 ;
string currClass ="",currMethod="";
%}

%verbose

%union {
        double num;
    char* str;
    struct loop{
        char *st;
        int num;
    }loop_var;
}

%token< str> SYSTEMOUTPRINTLN LEFTBRACKET RIGHTBRACKET LEFTCURLYBRACKET RIGHTCURLYBRACKET LEFTSQUAREBRACKET RIGHTSQUAREBRACKET THREEDOT SEMICOLON COMMA DOT ATTHERATE DOUBLECOLON TRIPLEGREATEREQUAL TRIPLEGREATER DOUBLELESSEQUAL DOUBLEGREATEREQUAL DOUBLELESS DOUBLEGREATER ADDEQUAL SUBEQUAL MULEQUAL DIVEQUAL ANDEQUAL OREQUAL POWEREQUAL MODEQUAL ARROW EQUALEQUAL GREATEREQUAL LESSEQUAL NOTEQUAL AND OR ADDADD SUBSUB EQUAL LESS GREATER NOT TILDE QUESTION COLON ADD SUB MUL DIV ANDBIT ORBIT POWER MOD


%token <str> ABSTRACT STRING CONTINUE NEW SWITCH ASSERT DEFAULT IF PACKAGE SYNCHRONIZED BOOLEAN DO GOTO PRIVATE THIS BREAK DOUBLE IMPLEMENTS PROTECTED THROWS BYTE ELSE IMPORT PUBLIC THROW CASE ENUM INSTANCE RETURN TRANSIENT CATCH EXTENDS INT SHORT TRY CHAR FINAL INTERFACE STATIC VOID CLASS FINALLY LONG STRICTFP VOLATILE CONST FLOAT NATIVE SUPER UNDERSEMICOLONORE EXPORTS OPENS REQUIRES USES MODULE PERMITS SEALED VAR NONSEALED PROVIDES TO WITH OPEN RECORD TRANSITIVE YIELD NULLLITERAL SUSPEND
%token <loop_var> FOR WHILE
%token <str> INTLITERAL FLOATLITERAL BINARYLITERAL HEXLITERAL

%token <str> IDENTIFIER STRINGLITERAL CHARLITERAL BOOLEANLITERAL

%type <str> Start ArrayAccesses PrintlnStatement CompilationUnit Literal Type PrimitiveType NonPrimitiveType NumericType IntegralType FloatingPointType ArrayType Name SimpleName QualifiedName ImportDeclarationOpt TypeDeclarationOpt ImportDeclaration TypeDeclaration SingleTypeImportDeclaration TypeImportOnDemandDeclaration ClassDeclaration Modifier ModifiersOpt ClassBody ClassBodyDeclarationsOpt ClassBodyDeclaration ClassMemberDeclaration StaticInitializer ConstructorDeclaration FieldDeclaration MethodDeclaration VarDeclarator VarDeclaratorId Expression ArrayInitializer MethodBody MethodHeader MethodDeclarator FormalParameterListOpt FormalParameter Block ConstructorDeclarator ConstructorBody ExplicitConstructorInvocation BlockStatementOpt BlockStatement ArgumentListOpt VarInitializersOpt VarInitializer CommaOpt LocalVariableDeclarationStatement LocalVariableDeclaration Statement StatementWithoutTrailingSubstatement LabeledStatement IfThenStatement IfThenElseStatement WhileStatement ForStatement StatementNoShortIf LabeledStatementNoShortIf IfThenElseStatementNoShortIf WhileStatementNoShortIf ForStatementNoShortIf EmptyStatement ExpressionStatement BreakStatement ContinueStatement ReturnStatement Assignment PreIncrementExpression PreDecrementExpression PostDecrementExpression PostIncrementExpression ClassInstanceCreationExpression ForInitOpt ForInit ExpressionOpt ForUpdateOpt ForUpdate StatementExpression IDENTOpt Primary PrimaryNoNewArray ArrayCreationExpression FieldAccess MethodInvocation ArrayAccess DimsOpt DimExpr PostFixExpression UnaryExpression UnaryExpressionNotPlusMinus CastExpression MultiplicativeExpression AdditiveExpression ShiftExpression RelationalExpression EqualityExpression AndExpression ExclusiveOrExpression InclusiveOrExpression ConditionalAndExpression ConditionalOrExpression ConditionalExpression AssignmentExpression LeftHandSide AssignmentOperator ClassType ClassOrInterfaceType   

%type <str> ImportDeclarations TypeDeclarations Modifiers ClassBodyDeclarations  VarDeclarators FormalParameterList BlockStatements VarInitializers StatementExpressionList DimExprs Dims ArgumentList VarDeclaratorIds MethodDeclarators MultiplicativeExpressions AdditiveExpressions ShiftExpressions RelationalExpressions EqualityExpressions AndExpressions ExclusiveOrExpressions InclusiveOrExpressions ConditionalAndExpressions ConditionalOrExpressions ConditionalExpressions ImportDeclarationOpts TypeDeclarationOpts ModifiersOpts ClassBodyDeclarationsOpts FormalParameterListOpts  BlockStatementOpts  CommaOpts ForInitOpts IDENTOpts ExpressionOpts ForUpdateOpts 

%start Start


%%
Start:
        CompilationUnit
    ;
Literal:
             INTLITERAL         
	|    FLOATLITERAL       
	|     BOOLEANLITERAL        
	|    STRINGLITERAL      
	|    CHARLITERAL        
	|    NULLLITERAL        
	;
Type:
        PrimitiveType           
     |   NonPrimitiveType        
    ;
PrimitiveType:
        NumericType             
     |   BOOLEAN   
     | STRING              
    ;
NumericType:
        IntegralType            
    |   FloatingPointType       
    ;
IntegralType:
        INT                     
    |   LONG                    
    |   CHAR                    
    ;
FloatingPointType:
        FLOAT                   
    |   DOUBLE                  
    ;
NonPrimitiveType:
        ArrayType               
    ;
ArrayType:
        PrimitiveType  LEFTSQUAREBRACKET RIGHTSQUAREBRACKET 
    |   Name LEFTSQUAREBRACKET RIGHTSQUAREBRACKET    
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
        IMPORT Name DOT IDENTIFIER SEMICOLON   
    ;
TypeImportOnDemandDeclaration :
        IMPORT Name DOT MUL SEMICOLON   
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
        ModifiersOpt CLASS IDENTIFIER {Klass obj; mp[$3] = obj;currClass=$3;quad ch;ch.idx=to_string(line++);ch.arg1=$3; ir.push_back(ch);} ClassBody         {currClass="";quad ch;ch.idx=to_string(line++);ch.arg1 = "End Class";ir.push_back(ch);}
    |   ModifiersOpt CLASS IDENTIFIER {Klass obj; mp[$3] = obj;currClass=$3;} EXTENDS IDENTIFIER ClassBody     {currClass="";}
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
        ModifiersOpt Type VarDeclarators SEMICOLON     
    ;
VarDeclarators:
        VarDeclarator   
    |   VarDeclarators COMMA VarDeclarator      
    ;
VarDeclarator:
        VarDeclaratorId  {if(currClass!=""&&currMethod==""){
                        mp[currClass].local.push($1);
                }
                else if(currClass!=""&&currMethod!=""){
                        mp[currClass]._vtable.methods[currMethod].local.push($1);
                        mp[currClass]._vtable.methods[currMethod].storage+=typesize(getType($1));
                }
        }     
    |   VarDeclaratorId EQUAL  VarInitializer    { if(currClass!=""&&currMethod==""){
                        mp[currClass].local.push($1);
                }
                else if(currClass!=""&&currMethod!=""){
                        mp[currClass]._vtable.methods[currMethod].local.push($1);
                        mp[currClass]._vtable.methods[currMethod].storage+=typesize(getType($1));
                }
        quad ch;ch.idx=to_string(line++);ch.arg1=$1;ch.res=$3; ch.op = $2;ir.push_back(ch);} 
		;
VarDeclaratorId: VarDeclaratorIds 
;

VarDeclaratorIds:
        IDENTIFIER      
    |   VarDeclaratorId LEFTSQUAREBRACKET RIGHTSQUAREBRACKET 
    ;
VarInitializer:
        Expression      
	|   ArrayInitializer    
	;
MethodDeclaration:
        MethodHeader MethodBody {currMethod="";quad ch;ch.idx=to_string(line++); ch.op="Func_"+currClass+ " end";ir.push_back(ch);}
    ;
MethodHeader:
        ModifiersOpt Type MethodDeclarator     
	|   ModifiersOpt VOID MethodDeclarator      
	;

FormalParameterListOpt: FormalParameterListOpts
;

FormalParameterListOpts:
        FormalParameterList     
        | 

    ;
MethodDeclarator: MethodDeclarators 
;
MethodDeclarators:
        IDENTIFIER {act_rec obj; mp[currClass]._vtable.methods[currClass+"."+$1] = obj;currMethod = currClass+"."+$1; quad ch;ch.idx=to_string(line++);string temp=$1; ch.op="Func_"+temp+" start :";ir.push_back(ch);}  LEFTBRACKET FormalParameterListOpt RIGHTBRACKET      //{cout<<mp[currClass]._vtable.methods[currMethod].param.top();}   
    |   MethodDeclarator LEFTSQUAREBRACKET RIGHTSQUAREBRACKET       
    ;
FormalParameterList:
        FormalParameter        
	|   FormalParameterList COMMA FormalParameter  
	;
FormalParameter:
        Type VarDeclaratorId    {mp[currClass]._vtable.methods[currMethod].param.push($2); mp[currClass]._vtable.methods[currMethod].storage+=typesize($1);}
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
        SimpleName LEFTBRACKET FormalParameterListOpt RIGHTBRACKET        
	;




BlockStatementOpt: BlockStatementOpts 
;
BlockStatementOpts:
        BlockStatements 
        | 

    ;


ArgumentListOpt:
        ArgumentList    
        | 

    ;
ConstructorBody:
        LEFTCURLYBRACKET ExplicitConstructorInvocation BlockStatementOpt RIGHTCURLYBRACKET
        | LEFTCURLYBRACKET BlockStatementOpt RIGHTCURLYBRACKET   
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
        LEFTCURLYBRACKET BlockStatementOpt RIGHTCURLYBRACKET      
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
LocalVariableDeclaration: Type VarDeclarators 
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
        |        SYSTEMOUTPRINTLN LEFTBRACKET RIGHTBRACKET SEMICOLON   
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
	|   BreakStatement   {quad ch;ch.idx=to_string(line++);ch.arg1="GOTO" ;ch.idx="label"+to_string(breakContinue+1);ir.push_back(ch);}
	|   ContinueStatement   {quad ch;ch.idx=to_string(line++); ch.arg1="GOTO" ;ch.idx="label"+to_string(breakContinue);ir.push_back(ch);} 
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

ClassInstanceCreationExpression: NEW ClassType LEFTBRACKET ArgumentListOpt RIGHTBRACKET 
;

ClassType:	  ClassOrInterfaceType 
		;

ClassOrInterfaceType: Name 
		;

IfThenStatement:
        IF {temp=check;check=3;} LEFTBRACKET Expression RIGHTBRACKET {check = temp;} Statement   {quad ch;ch.idx=to_string(line++);ch.arg1="Label"+to_string(label);ir.push_back(ch);}
    ;
IfThenElseStatement:
        IF LEFTBRACKET {temp=check;check=3;} Expression RIGHTBRACKET {check = temp;} StatementNoShortIf ELSE {quad ch;ch.idx=to_string(line++);ch.arg1="Label"+to_string(label);ir.push_back(ch);} Statement {quad ch;ch.idx=to_string(line++);ch.arg1="next";ir.push_back(ch);}    
    ;
IfThenElseStatementNoShortIf:
        IF {temp=check;check=3;} LEFTBRACKET Expression RIGHTBRACKET {check = temp;} StatementNoShortIf ELSE {quad ch;ch.idx=to_string(line++);ch.arg1="Label"+to_string(label);ir.push_back(ch);} StatementNoShortIf {quad ch;ch.idx=to_string(line++);ch.arg1="next";ir.push_back(ch);}   
    ;
WhileStatement:
        WHILE {temp=check;check=2;quad ch;ch.idx=to_string(line++); ch.arg1="Label"+to_string(++label); breakContinue=label;$1.num=label;ir.push_back(ch);} LEFTBRACKET Expression RIGHTBRACKET {check = temp;} Statement
            {quad ch;ch.idx=to_string($1.num);ch.op="GOTO";quad ch1;ch1.idx=to_string(line++);ch1.arg1="Label"+to_string($1.num+1);breakContinue--;ir.push_back(ch);ir.push_back(ch1);} 
	;

WhileStatementNoShortIf:
        WHILE {temp=check;check=2;quad ch;ch.idx=to_string(line++); ch.arg1="Label"+to_string(++label); breakContinue=label;$1.num=label;ir.push_back(ch);} LEFTBRACKET Expression RIGHTBRACKET {check = temp;} StatementNoShortIf
            {quad ch;ch.idx=to_string($1.num);ch.op="GOTO";quad ch1;ch1.idx=to_string(line++);ch1.arg1="Label"+to_string($1.num+1);breakContinue--;ir.push_back(ch);ir.push_back(ch1);} 
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
        FOR LEFTBRACKET ForInitOpt SEMICOLON {temp=check;check=1;quad ch;ch.idx=to_string(line++); ch.arg1="Label"+to_string(++label); breakContinue=label;$1.num=label;ir.push_back(ch);} ExpressionOpt {check = temp;} SEMICOLON ForUpdateOpt RIGHTBRACKET Statement    {
                quad ch;ch.idx=to_string($1.num);ch.op="GOTO";quad ch1;ch1.idx=to_string(line++);ch1.arg1="Label"+to_string($1.num+1);breakContinue--;ir.push_back(ch);ir.push_back(ch1);
                }
	;

ForStatementNoShortIf:
        FOR LEFTBRACKET ForInitOpt SEMICOLON {temp=check;check=1;quad ch;ch.idx=to_string(line++); ch.arg1="Label"+to_string(++label); breakContinue=label;$1.num=label;ir.push_back(ch);} ExpressionOpt {check = temp;} SEMICOLON ForUpdateOpt RIGHTBRACKET StatementNoShortIf   {
                quad ch;ch.idx=to_string($1.num);ch.op="GOTO";quad ch1;ch1.idx=to_string(line++);ch1.arg1="Label"+to_string($1.num+1);breakContinue--;ir.push_back(ch);ir.push_back(ch1);
                }
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

IDENTOpts: IDENTIFIER 
        |  

     ;

BreakStatement:
        BREAK IDENTOpt SEMICOLON    
	;

ContinueStatement:
        CONTINUE IDENTOpt SEMICOLON    
	;

ReturnStatement:
        RETURN ExpressionOpt SEMICOLON    {mp[currClass]._vtable.methods[currMethod].returnVal=$2;}
	|   SUSPEND ExpressionOpt SEMICOLON    
	;
Primary:
        PrimaryNoNewArray    
    |   ArrayCreationExpression    
    ;
PrimaryNoNewArray:
        Literal   {if(flag){mp[currClass]._vtable.methods[currMethod].param.push($1);}}
    |   THIS    
    |   LEFTBRACKET Expression RIGHTBRACKET    {$$ = strConcat($1,strConcat($2,$3));}
    |   FieldAccess    
    |   MethodInvocation    
    |   ArrayAccesses   {if(flag){mp[currClass]._vtable.methods[currMethod].param.push($1);}string ch = word+"[" + array3ac($1) + "]";$$=strdup(ch.c_str());} 
    ;
ArrayAccesses:
    ArrayAccess {$$=$1;}
    ;
ArgumentList:
        Expression    {quad ch;ch.idx=to_string(line++);param++;ch.op="param";ch.arg1= $1;ir.push_back(ch);}
	|   ArgumentList COMMA Expression       {quad ch;ch.idx=to_string(line++);param++;ch.op="param";ch.arg1= $1;ir.push_back(ch);}
	;


DimsOpt: Dims   
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
	|   SUPER DOT IDENTIFIER       
	;
MethodInvocation:
        Name {param=0;} LEFTBRACKET {flag=1;} ArgumentListOpt {flag=0;} RIGHTBRACKET      {
                vector<string> temp = find($1);
                mp[temp[3]]._vtable.methods[temp[3]+"."+temp[4]].controlLink = &mp[currClass]._vtable.methods[currMethod]; 
                // cout<<mp[temp[3]]._vtable.methods[temp[3]+"."+temp[4]].controlLink->param.top();
                quad ch;ch.idx=to_string(line++);ch.op= "pushparam"; ch.arg1=to_string(mp[temp[3]]._vtable.methods[temp[3]+"."+temp[4]].storage);ir.push_back(ch);ch.op="call";ch.res="t"+cnt; ch.arg1=$1;ch.arg2=to_string(param); ir.push_back(ch); ch.op = "popparam" ; ch.arg1 =to_string(mp[temp[3]]._vtable.methods[temp[3]+"."+temp[4]].storage);ir.push_back(ch);sprintf($$,"t%d",cnt++);                
        }
	|   Primary DOT IDENTIFIER LEFTBRACKET ArgumentListOpt RIGHTBRACKET      
	|   SUPER DOT IDENTIFIER LEFTBRACKET ArgumentListOpt RIGHTBRACKET     
	|   Name LEFTCURLYBRACKET ArgumentListOpt RIGHTCURLYBRACKET      
	|   Primary DOT IDENTIFIER LEFTCURLYBRACKET ArgumentListOpt RIGHTCURLYBRACKET     
	|   SUPER DOT IDENTIFIER LEFTCURLYBRACKET ArgumentListOpt RIGHTCURLYBRACKET      
	;
ArrayAccess:
        Name LEFTSQUAREBRACKET Expression RIGHTSQUAREBRACKET      {sprintf($$,strConcat($1,strConcat("[",strConcat($3,"]"))));}
	| ArrayAccesses LEFTSQUAREBRACKET Expression RIGHTSQUAREBRACKET    {sprintf($$,strConcat($1,strConcat("[",strConcat($3,"]"))));} 
	;

PostFixExpression:
        Primary      
	|   Name     {$$=$1;if(flag){mp[currClass]._vtable.methods[currMethod].param.push($1);}} 
	|   PostIncrementExpression {$$=$1;}     
	|   PostDecrementExpression      {$$=$1;}
	;

PostIncrementExpression:
        PostFixExpression ADDADD    {quad ch;ch.idx=to_string(line++);ch.op="+";ch.res="t"+to_string(cnt);ch.arg1=$1;ch.arg2="1";ir.push_back(ch);ch_clear(ch); ch.op="=";ch.idx=to_string(line++);ch.res=$1;ch.arg1="t"+to_string(cnt);ir.push_back(ch); sprintf($$,"t%d",cnt);cnt++;}  
	;

PostDecrementExpression:
        PostFixExpression SUBSUB      {quad ch;ch.idx=to_string(line++);ch.op="-";ch.res="t"+to_string(cnt);ch.arg1=$1;ch.arg2="1";ir.push_back(ch); (ch); ch.op="=";ch.idx=to_string(line++);ch.res=$1;ch.arg1="t"+to_string(cnt);ir.push_back(ch); sprintf($$,"t%d",cnt);cnt++;}  
	;

UnaryExpression:
        PreIncrementExpression      {$$=$1;}
        |   PreDecrementExpression      {$$=$1;}
	|   ADD UnaryExpression      {quad ch;ch.idx=to_string(line++);ch.op="unary+";ch.res="t"+to_string(cnt);ch.arg1=$1;ch.arg2=$2;ir.push_back(ch);ch_clear(ch);ch.op="=";ch.idx=to_string(line++);ch.res=$2;ch.arg1="t"+to_string(cnt);ir.push_back(ch);  sprintf($$,"t%d",cnt);cnt++;}
	|   SUB UnaryExpression     {quad ch;ch.idx=to_string(line++);ch.op="unary-";ch.res="t"+to_string(cnt);ch.arg1=$1;ch.arg2=$2;ir.push_back(ch);ch_clear(ch);ch.op="=";ch.idx=to_string(line++);ch.res=$2;ch.arg1="t"+to_string(cnt);ir.push_back(ch);  sprintf($$,"t%d",cnt);cnt++;}
	|   UnaryExpressionNotPlusMinus   {$$=$1;}   
	;

PreIncrementExpression:
        ADDADD UnaryExpression       {quad ch;ch.idx=to_string(line++);ch.op="+";ch.res="t"+to_string(cnt);ch.arg1=$1;ch.arg2="1";ir.push_back(ch); ch.op="=";ch.idx=to_string(line++);ch.res=$1;ch.arg1="t"+to_string(cnt);ir.push_back(ch); sprintf($$,"t%d",cnt);cnt++;}   
	;

PreDecrementExpression:
        SUBSUB UnaryExpression       {quad ch;ch.idx=to_string(line++);ch.op="-";ch.res="t"+to_string(cnt);ch.arg1=$1;ch.arg2="1";ir.push_back(ch); ch.op="=";ch.idx=to_string(line++);ch.res=$1;ch.arg1="t"+to_string(cnt);ir.push_back(ch); sprintf($$,"t%d",cnt);cnt++;}  
	;

UnaryExpressionNotPlusMinus:
        PostFixExpression      {$$=$1;}
	|   TILDE UnaryExpression      
	|   NOT UnaryExpression      
	|   CastExpression      
	;

CastExpression:
        LEFTBRACKET PrimitiveType DimsOpt RIGHTBRACKET UnaryExpression      
	|   LEFTBRACKET Expression RIGHTBRACKET UnaryExpressionNotPlusMinus      
	|   LEFTBRACKET Name Dims RIGHTBRACKET UnaryExpressionNotPlusMinus     
	;

MultiplicativeExpression:  MultiplicativeExpressions {$$=$1;}
;

MultiplicativeExpressions:
        UnaryExpression      {$$=$1;}
	|   MultiplicativeExpression MUL UnaryExpression       {quad ch;ch.idx=to_string(line++);ch.op=$2;ch.arg1=$1;ch.arg2=$3;ch.res="t"+to_string(cnt);sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);} 
	|   MultiplicativeExpression DIV UnaryExpression       {quad ch;ch.idx=to_string(line++);ch.op=$2;ch.arg1=$1;ch.arg2=$3;ch.res="t"+to_string(cnt);sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);} 
	|   MultiplicativeExpression MOD UnaryExpression      {quad ch;ch.idx=to_string(line++);ch.op=$2;ch.arg1=$1;ch.arg2=$3;ch.res="t"+to_string(cnt);sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}
	;

AdditiveExpression: AdditiveExpressions {$$=$1;}
;

AdditiveExpressions:
        MultiplicativeExpression     {$$=$1;}
	|   AdditiveExpression ADD MultiplicativeExpression     {quad ch;ch.idx=to_string(line++);ch.op=$2;ch.arg1=$1;ch.arg2=$3;ch.res="t"+to_string(cnt);sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}
	|   AdditiveExpression SUB  MultiplicativeExpression    {quad ch;ch.idx=to_string(line++);ch.op=$2;ch.arg1=$1;ch.arg2=$3;ch.res="t"+to_string(cnt);sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}   
	;

ShiftExpression: ShiftExpressions {$$=$1;}
;

ShiftExpressions:
        AdditiveExpression     {$$=$1;}
	|   ShiftExpression DOUBLELESS AdditiveExpression    {quad ch;ch.idx=to_string(line++);ch.op=$2;ch.arg1=$1;ch.arg2=$3;ch.res="t"+to_string(cnt);sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}  
	|   ShiftExpression DOUBLEGREATER AdditiveExpression  {quad ch;ch.idx=to_string(line++);ch.op=$2;ch.arg1=$1;ch.arg2=$3;ch.res="t"+to_string(cnt);sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}  
	|   ShiftExpression TRIPLEGREATER AdditiveExpression    {quad ch;ch.idx=to_string(line++);ch.op=$2;ch.arg1=$1;ch.arg2=$3;ch.res="t"+to_string(cnt);sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);} 
	;

RelationalExpression: RelationalExpressions     {$$=$1;}
;

RelationalExpressions:
        ShiftExpression     {$$=$1;}
	|   RelationalExpression LESS ShiftExpression      {if(check==1){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat($1,strConcat($2,$3)));ch.idx = to_string(label+1); label++;ir.push_back(ch);}
        else if(check==2){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat($1,strConcat($2,$3)));ch.idx = to_string(label+1); label++;ir.push_back(ch);;}
        else if(check==3){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat($1,strConcat($2,$3)));ch.idx = to_string(label+1); label++;ir.push_back(ch);;}
        else{{quad ch;ch.idx=to_string(line++);ch.op=$2;ch.res="t"+to_string(cnt);ch.arg1=$1;ch.arg2=$3; sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}}
        }   
	|   RelationalExpression GREATER ShiftExpression     {if(check==1){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat($1,strConcat($2,$3)));ch.idx = to_string(label+1); label++;ir.push_back(ch);}
        else if(check==2){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat($1,strConcat($2,$3)));ch.idx = to_string(label+1); label++;ir.push_back(ch);;}
        else if(check==3){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat($1,strConcat($2,$3)));ch.idx = to_string(label+1); label++;ir.push_back(ch);;}
        else{{quad ch;ch.idx=to_string(line++);ch.op=$2;ch.res="t"+to_string(cnt);ch.arg1=$1;ch.arg2=$3; sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}}
        }   
	|   RelationalExpression LESSEQUAL ShiftExpression      {if(check==1){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat($1,strConcat($2,$3)));ch.idx = to_string(label+1); label++;ir.push_back(ch);}
        else if(check==2){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat($1,strConcat($2,$3)));ch.idx = to_string(label+1); label++;ir.push_back(ch);;}
        else if(check==3){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat($1,strConcat($2,$3)));ch.idx = to_string(label+1); label++;ir.push_back(ch);;}
        else{{quad ch;ch.idx=to_string(line++);ch.op=$2;ch.res="t"+to_string(cnt);ch.arg1=$1;ch.arg2=$3; sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}}
        }   
	|   RelationalExpression GREATEREQUAL ShiftExpression      {if(check==1){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat($1,strConcat($2,$3)));ch.idx = to_string(label+1); label++;ir.push_back(ch);}
        else if(check==2){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat($1,strConcat($2,$3)));ch.idx = to_string(label+1); label++;ir.push_back(ch);;}
        else if(check==3){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat($1,strConcat($2,$3)));ch.idx = to_string(label+1); label++;ir.push_back(ch);;}
        else{{quad ch;ch.idx=to_string(line++);ch.op=$2;ch.res="t"+to_string(cnt);ch.arg1=$1;ch.arg2=$3; sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}}
        }   
;

EqualityExpression: EqualityExpressions {$$=$1;}
;

EqualityExpressions:
        RelationalExpression      {$$=$1;}
	|   EqualityExpression EQUALEQUAL RelationalExpression       {if(check==1){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat($1,strConcat($2,$3)));ch.idx = to_string(label+1); label++;ir.push_back(ch);}
        else if(check==2){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat($1,strConcat($2,$3)));ch.idx = to_string(label+1); label++;ir.push_back(ch);;}
        else if(check==3){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat($1,strConcat($2,$3)));ch.idx = to_string(label+1); label++;ir.push_back(ch);;}
        else{{quad ch;ch.idx=to_string(line++);ch.op=$2;ch.res="t"+to_string(cnt);ch.arg1=$1;ch.arg2=$3; sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}}
        }   
	|   EqualityExpression NOTEQUAL RelationalExpression         {if(check==1){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat($1,strConcat($2,$3)));ch.idx = to_string(label+1); label++;ir.push_back(ch);}
        else if(check==2){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat($1,strConcat($2,$3)));ch.idx = to_string(label+1); label++;ir.push_back(ch);;}
        else if(check==3){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat($1,strConcat($2,$3)));ch.idx = to_string(label+1); label++;ir.push_back(ch);;}
        else{{quad ch;ch.idx=to_string(line++);ch.op=$2;ch.res="t"+to_string(cnt);ch.arg1=$1;ch.arg2=$3; sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}}
        }   
	;
AndExpression: AndExpressions   {$$=$1;}
;
AndExpressions:
        EqualityExpression      {$$=$1;}
	|   AndExpression ANDBIT EqualityExpression{quad ch;ch.idx=to_string(line++);ch.op=$2;ch.arg1=$1;ch.arg2=$3;ch.res="t"+to_string(cnt);sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}
              
	;

ExclusiveOrExpression: ExclusiveOrExpressions   {$$=$1;}
;
ExclusiveOrExpressions:
        AndExpression     {$$=$1;}
	|   ExclusiveOrExpression POWER AndExpression    {quad ch;ch.idx=to_string(line++);ch.op=$2;ch.arg1=$1;ch.arg2=$3;ch.res="t"+to_string(cnt);sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}   
	;

InclusiveOrExpression: InclusiveOrExpressions   {$$=$1;}
;

InclusiveOrExpressions:
        ExclusiveOrExpression {$$=$1;}
	|   InclusiveOrExpression ORBIT ExclusiveOrExpression   {quad ch;ch.idx=to_string(line++);ch.op=$2;ch.arg1=$1;ch.arg2=$3;ch.res="t"+to_string(cnt);sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}   
	;

ConditionalAndExpression: ConditionalAndExpressions     {$$=$1;}
;

ConditionalAndExpressions:
        InclusiveOrExpression     {$$=$1;}
	|   ConditionalAndExpression AND InclusiveOrExpression     {quad ch;ch.idx=to_string(line++);ch.op=$2;ch.arg1=$1;ch.arg2=$3;ch.res="t"+to_string(cnt);sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}
	;

ConditionalOrExpression: ConditionalOrExpressions       {$$=$1;}
;

ConditionalOrExpressions:
        ConditionalAndExpression     {$$=$1;}
	|   ConditionalOrExpression OR ConditionalAndExpression     {quad ch;ch.idx=to_string(line++);ch.op=$2;ch.arg1=$1;ch.arg2=$3;ch.res="t"+to_string(cnt);sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}
	;

ConditionalExpression : ConditionalExpressions {$$=$1;}
;

ConditionalExpressions: ConditionalOrExpression   {$$=$1;}   
	|   ConditionalOrExpression QUESTION Expression COLON ConditionalExpression      
	;

AssignmentExpression:
        ConditionalExpression    {$$=$1;}
	|   Assignment      {$$=$1;}
	;

Assignment:
        LeftHandSide AssignmentOperator AssignmentExpression {quad ch;ch.idx=to_string(line++);ch.op=$2;ch.res=$1;ch.arg1=$3; ir.push_back(ch);  }  
	;

LeftHandSide:
        Name          {$$=$1;}
	|   FieldAccess         
	|   ArrayAccesses   {string ch = word+"[" + array3ac($1) + "]";$$=strdup(ch.c_str());}       
	;

 AssignmentOperator:
        EQUAL        {$$=$1;}  
    |   MULEQUAL
    |   DIVEQUAL         
    |   SUBEQUAL          
    |   ANDEQUAL         
    |   ADDEQUAL        
    ;

Expression:
        AssignmentExpression      {$$=$1;}
	;

%%

int main(void) {
        SymTab();
    yyparse();
    cout<<"op,arg1,arg2,res,idx"<<endl;
    for(auto i : ir){
        if(i.op=="GOTO"){
                i.idx="Label" + i.idx;
                for(auto j:ir){
                        if(j.arg1==i.idx){i.idx=j.idx;}
                }
        }
        cout<<i.op<<","<<i.arg1<<","<<i.arg2<<","<<i.res<<","<<i.idx<<endl;
    }
    code_file.open("gen.asm");
    genCode();
    return 0;
}

char * strConcat(char *s1, char*s2){
        char * str3 = (char *) malloc(1 + strlen(s1)+ strlen(s2) );
      strcpy(str3, s1);
      strcat(str3, s2);
      return str3;
}
vector<string> getDim(string s){
     string dim;
     for(int i=0;i<SymbolTable.size();i++)
	{     
		if(SymbolTable[i][0]==s){
                       dim = SymbolTable[i][7];
                }
	}
        if(dim.size()==0)
        {cout<<"Array not declared:"<<endl;
        exit(15);}
        vector<string> t;
        string temp;
        for(int i = dim.size()-1; i>=0 ;i-- ){
                if(dim[i]>='0'&&dim[i]<='9') {temp.insert(temp.begin(),dim[i]) ;}
                if(temp.size()>0&&!(dim[i]>='0'&&dim[i]<='9')){ t.push_back(temp); temp.clear();} 
        }
        return t;
}
string array3ac(string s){
        stringstream str(s);
        getline(str, word, '[');
        vector<string> dim = getDim(word);
        vector<string> t;
        string q = "";
        char ts[10];
        string temp;
        for(int i = s.size()-1; i>=0 ;i-- ){
                if(s[i]!=']'&&s[i]!='[') temp.insert(temp.begin(),s[i]);
                if(s[i]==']'){
                        temp.clear();
                }
                if(s[i]=='['){
                        t.push_back(temp);
                }
        }
        reverse(t.begin(),t.end());
        for(int i = dim.size()-1;i>0;i--){
                dim[i-1] = to_string(stoi(dim[i-1])*stoi(dim[i])*4);
        }
        if(dim.size()==t.size()){
                quad ch;
                ch.res ="t"+to_string(cnt++);
                ch.arg1 = dim[0];
                ch.arg2 = t[0];//dataSize;
                ch.idx = to_string(line++);
                ir.push_back(ch);
                vector<string> ts;
                ts.push_back(ch.res);
                for(int i = 1 ; i<t.size(); i++){
                        ch.arg2 = t[i];
                        ch.res ="t"+to_string(cnt++);
                        ch.arg1 = dim[i];
                        ch.idx = to_string(line++);
                        ir.push_back(ch);
                        ts.push_back(ch.res);
                }
                if(ts.size()>=2){
                        ch.res ="t"+to_string(cnt++);
                        ch.arg1 = ts[0];
                        ch.arg2 = ts[1];
                        ch.idx = to_string(line++);
                        ir.push_back(ch);
                        for(int i = 2; i<ts.size();i++){
                                ch.arg1 ="t" +to_string(cnt-1);
                                ch.res = "t"+to_string(cnt++);
                                ch.arg2 = ts[i];
                                ch.idx = to_string(line++);
                                ir.push_back(ch);     
                        }
                       
                }
                 return ch.res;
        }
        else{
                cout<<"Dimensions doesn't match"<<endl;
                exit(8);
        }
        
        return "";
        
}

void SymTab(){
	vector<string> row;
	string line, word;
        
	fstream file ("symbolTable.csv", ios::in);
	if(file.is_open())
	{       
		while(getline(file, line))
		{
			row.clear();
 
			stringstream str(line);
 
			while(getline(str, word, ',')||getline(str, word, '\n')){
				row.push_back(word);
                        }
			SymbolTable.push_back(row);
		}
	}
	else
		{cout<<"Create symbol Table first\n"; exit(5);}
}
vector<string> find(string _lexeme){
        SymTab();
    for(auto i:SymbolTable){
        if(i[0]==_lexeme&&i[4]==_lexeme) return i;
    }
        cout<<"function call before declaration";
        exit(100);
}

int typesize(string s){
        if(s=="int"){
                return 4;
        }
        else if(s=="float"){
                return 8;
        }
        else if(s=="double"){
                return 10;
        }
        else if(s=="char"){
                return 2;
        }
        return 0;

}
string getType(string s){
    for(auto i:SymbolTable){
        if(i[0]==s){return i[2];}
    }
        exit(101);
}
void ch_clear(quad &ch){
    ch.arg1="";
    ch.arg2="";
    ch.idx="";
    ch.op="";
    ch.res="";
}
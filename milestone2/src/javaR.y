%{
#include<bits/stdc++.h>

using namespace std;

extern char yytext[];
extern int column;
extern int yylineno;
int cnt,temp;
int label=0;
vector<string> ir;
int yylex(void);
void yyerror(const char *s);
int check=0;
char buff[100];
char * strConcat(char *s1, char*s2);
vector<vector<string>> SymbolTable;
void SymTab();
string array3ac(string s);
string word;
int breakContinue=0;
int param = 0 ;
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
        ModifiersOpt CLASS IDENTIFIER ClassBody
    |   ModifiersOpt CLASS IDENTIFIER EXTENDS IDENTIFIER ClassBody
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
        VarDeclaratorId
    |   VarDeclaratorId EQUAL  VarInitializer    {
        char ch[100];sprintf(ch,"%s %s %s",$1,$2,$3);ir.push_back(ch);}
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
        MethodHeader MethodBody
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
        IDENTIFIER LEFTBRACKET FormalParameterListOpt RIGHTBRACKET
    |   MethodDeclarator LEFTSQUAREBRACKET RIGHTSQUAREBRACKET
    ;
FormalParameterList:
        FormalParameter
	|   FormalParameterList COMMA FormalParameter
	;
FormalParameter:
        Type VarDeclaratorId
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
	|   BreakStatement   {char ch[100]; sprintf(ch,"GOTO Label:%d",breakContinue+1);ir.push_back(ch);}
	|   ContinueStatement   {char ch[100]; sprintf(ch,"GOTO Label:%d",breakContinue);ir.push_back(ch);}
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
        IF {temp=check;check=3;} LEFTBRACKET Expression RIGHTBRACKET {check = temp;} Statement   {char ch[1000];sprintf(ch,"Label%d",label);ir.push_back(ch);}
    ;
IfThenElseStatement:
        IF {temp=check;check=3;} LEFTBRACKET Expression RIGHTBRACKET {check = temp;} StatementNoShortIf ELSE {char ch[1000];sprintf(ch,"Label%d",label);ir.push_back(ch);} Statement {char ch[1000];sprintf(ch,"next");ir.push_back(ch);}
    ;
IfThenElseStatementNoShortIf:
        IF {temp=check;check=3;} LEFTBRACKET Expression RIGHTBRACKET {check = temp;} StatementNoShortIf ELSE {char ch[1000];sprintf(ch,"Label%d",label);ir.push_back(ch);} StatementNoShortIf {char ch[1000];sprintf(ch,"next");ir.push_back(ch);}
    ;
WhileStatement:
        WHILE {temp=check;check=2;char ch[100]; sprintf(ch,"Label%d",++label); breakContinue=label;$1.num=label;ir.push_back(ch);} LEFTBRACKET Expression RIGHTBRACKET {check = temp;} Statement
            {char ch[1000];sprintf(ch,"JUMP Label%d\nLabel%d",$1.num,$1.num+1);breakContinue--;ir.push_back(ch);}
	;

WhileStatementNoShortIf:
        WHILE {temp=check;check=2;char ch[100]; sprintf(ch,"Label%d",++label); breakContinue=label;$1.num=label;ir.push_back(ch); } LEFTBRACKET Expression RIGHTBRACKET {check = temp;} StatementNoShortIf
            {char ch[1000];sprintf(ch,"JUMP Label%d\nLabel%d",$1.num,$1.num+1);breakContinue--;ir.push_back(ch);}
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
        FOR LEFTBRACKET ForInitOpt SEMICOLON {temp=check;check=1;char ch[100]; sprintf(ch,"Label%d",++label);breakContinue=label;$1.num=label;ir.push_back(ch);} ExpressionOpt {check = temp;} SEMICOLON ForUpdateOpt RIGHTBRACKET Statement    {
                char ch[100];sprintf(ch,"JUMP Label%d\nLabel%d",$1.num,$1.num+1);
                breakContinue--;
                ir.push_back(ch);
                }
	;

ForStatementNoShortIf:
        FOR LEFTBRACKET ForInitOpt SEMICOLON {temp=check;check=1;char ch[100]; sprintf(ch,"Label%d",++label);breakContinue=label;$1.num=label;ir.push_back(ch);} ExpressionOpt {check = temp;} SEMICOLON ForUpdateOpt RIGHTBRACKET StatementNoShortIf   {
                char ch[100];sprintf(ch,"JUMP Label%d\nLabel%d",$1.num,$1.num+1);
                breakContinue--;
                ir.push_back(ch);
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
    |   LEFTBRACKET Expression RIGHTBRACKET    {$$ = strConcat($1,strConcat($2,$3));}
    |   FieldAccess
    |   MethodInvocation
    |   ArrayAccesses   {string ch = word+"[" + array3ac($1) + "]";$$=strdup(ch.c_str());}
    ;
ArrayAccesses:
    ArrayAccess {$$=$1;}
    ;
ArgumentList:
        Expression    {char ch[100];param++;sprintf(ch,"param %s",$1);ir.push_back(ch);}
	|   ArgumentList COMMA Expression       {char ch[100];param++;sprintf(ch,"param %s",$3);ir.push_back(ch);}
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
        Name {param=0;} LEFTBRACKET ArgumentListOpt RIGHTBRACKET      {char ch[100];sprintf(ch,"t%d = Call %s,%d",cnt,$1,param);ir.push_back(ch);sprintf($$,"t%d",cnt++);}
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
	|   Name     {$$=$1;}
	|   PostIncrementExpression {$$=$1;}
	|   PostDecrementExpression      {$$=$1;}
	;

PostIncrementExpression:
        PostFixExpression ADDADD    {char ch[100];sprintf(ch,"t%d=%s+1\n%s=t%d",cnt,$1,$1,cnt); sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}
	;

PostDecrementExpression:
        PostFixExpression SUBSUB     {char ch[100];sprintf(ch,"t%d=%s-1\n%s=t%d",cnt,$1,$1,cnt); sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}
	;

UnaryExpression:
        PreIncrementExpression      {$$=$1;}
        |   PreDecrementExpression      {$$=$1;}
	|   ADD UnaryExpression      {char ch[100];sprintf(ch,"t%d=%s%s\n%s=t%d",cnt,$1,$2,$2,cnt); sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}
	|   SUB UnaryExpression     {char ch[100];sprintf(ch,"t%d=%s%s\n%s=t%d",cnt,$1,$2,$2,cnt); sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}
	|   UnaryExpressionNotPlusMinus   {$$=$1;}
	;

PreIncrementExpression:
        ADDADD UnaryExpression       {char ch[100];sprintf(ch,"t%d=1+%s\n%s=t%d",cnt,$2,$2,cnt); sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}
	;

PreDecrementExpression:
        SUBSUB UnaryExpression       {char ch[100];sprintf(ch,"t%d=%s-1\n%s=t%d",cnt,$2,$2,cnt); sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}
	;

UnaryExpressionNotPlusMinus:
        PostFixExpression      {$$=$1;}
	|   TILDE UnaryExpression    {char ch[100];sprintf(ch,"t%d=%s%s",cnt,$1,$2); sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}
	|   NOT UnaryExpression      {char ch[100];sprintf(ch,"t%d=%s%s",cnt,$1,$2); sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}
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
	|   MultiplicativeExpression MUL UnaryExpression       {char ch[100];sprintf(ch,"t%d=%s%s%s",cnt,$1,$2,$3); sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}
	|   MultiplicativeExpression DIV UnaryExpression       {char ch[100];sprintf(ch,"t%d=%s%s%s",cnt,$1,$2,$3); sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}
	|   MultiplicativeExpression MOD UnaryExpression       {char ch[100];sprintf(ch,"t%d=%s%s%s",cnt,$1,$2,$3); sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}
	;

AdditiveExpression: AdditiveExpressions {$$=$1;}
;

AdditiveExpressions:
        MultiplicativeExpression     {$$=$1;}
	|   AdditiveExpression ADD MultiplicativeExpression      {char ch[100];sprintf(ch,"t%d=%s%s%s",cnt,$1,$2,$3); sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}
	|   AdditiveExpression SUB  MultiplicativeExpression     {char ch[100];sprintf(ch,"t%d=%s%s%s",cnt,$1,$2,$3); sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}
	;

ShiftExpression: ShiftExpressions {$$=$1;}
;

ShiftExpressions:
        AdditiveExpression     {$$=$1;}
	|   ShiftExpression DOUBLELESS AdditiveExpression     {char ch[100];sprintf(ch,"t%d=%s%s%s",cnt,$1,$2,$3); sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}
	|   ShiftExpression DOUBLEGREATER AdditiveExpression   {char ch[100];sprintf(ch,"t%d=%s%s%s",cnt,$1,$2,$3); sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}
	|   ShiftExpression TRIPLEGREATER AdditiveExpression     {char ch[100];sprintf(ch,"t%d=%s%s%s",cnt,$1,$2,$3); sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}
	;

RelationalExpression: RelationalExpressions     {$$=$1;}
;

RelationalExpressions:
        ShiftExpression     {$$=$1;}
	|   RelationalExpression LESS ShiftExpression      {if(check==1){char ch[100];sprintf(ch,"if NOT (%s %s %s) GOTO Label%d",$1,$2,$3,label+1);label++;ir.push_back(ch);}
        else if(check==2){char ch[100];sprintf(ch,"if NOT (%s %s %s) GOTO Label%d",$1,$2,$3,label+1);label++;ir.push_back(ch);strcpy(ch,buff);}
        else if(check==3){char ch[100];sprintf(ch,"if NOT (%s %s %s) GOTO Label%d",$1,$2,$3,label+1);label++;ir.push_back(ch);strcpy(ch,buff);}
        else{{char ch[100];sprintf(ch,"t%d=%s%s%s",cnt,$1,$2,$3); sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}}
        }
	|   RelationalExpression GREATER ShiftExpression     {if(check==1){char ch[100];sprintf(ch,"if NOT (%s %s %s) GOTO Label%d",$1,$2,$3,label)+1;ir.push_back(ch);}
        else if(check==2){char ch[100];sprintf(ch,"if NOT (%s %s %s) GOTO Label%d",$1,$2,$3,label+1);label++;ir.push_back(ch);strcpy(ch,buff);}
        else if(check==3){char ch[100];sprintf(ch,"if NOT (%s %s %s) GOTO Label%d",$1,$2,$3,label+1);label++;ir.push_back(ch);strcpy(ch,buff);}
        else{{char ch[100];sprintf(ch,"t%d=%s%s%s",cnt,$1,$2,$3); sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}}
        }
	|   RelationalExpression LESSEQUAL ShiftExpression      {if(check==1){char ch[100];sprintf(ch,"if NOT (%s %s %s) GOTO Label%d",$1,$2,$3,label+1);label++;ir.push_back(ch);}
        else if(check==2){char ch[100];sprintf(ch,"if NOT (%s %s %s) GOTO Label%d",$1,$2,$3,label+1);label++;ir.push_back(ch);strcpy(ch,buff);}
        else if(check==3){char ch[100];sprintf(ch,"if NOT (%s %s %s) GOTO Label%d",$1,$2,$3,label+1);label++;ir.push_back(ch);strcpy(ch,buff);}
        else{{char ch[100];sprintf(ch,"t%d=%s%s%s",cnt,$1,$2,$3); sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}}
        }
	|   RelationalExpression GREATEREQUAL ShiftExpression      {if(check==1){char ch[100];sprintf(ch,"if NOT (%s %s %s) GOTO Label%d",$1,$2,$3,label+1);label++;ir.push_back(ch);}
        else if(check==2){char ch[100];sprintf(ch,"if NOT (%s %s %s) GOTO Label%d",$1,$2,$3,label+1);label++;ir.push_back(ch);strcpy(ch,buff);}
        else if(check==3){char ch[100];sprintf(ch,"if NOT (%s %s %s) GOTO Label%d",$1,$2,$3,label+1);label++;ir.push_back(ch);strcpy(ch,buff);}
        else{{char ch[100];sprintf(ch,"t%d=%s%s%s",cnt,$1,$2,$3); sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}}
        }
;

EqualityExpression: EqualityExpressions {$$=$1;}
;

EqualityExpressions:
        RelationalExpression      {$$=$1;}
	|   EqualityExpression EQUALEQUAL RelationalExpression       {if(check==1){char ch[100];sprintf(ch,"if NOT (%s %s %s) GOTO Label%d",$1,$2,$3,label+1);label++;ir.push_back(ch);}
        else if(check==2){char ch[100];sprintf(ch,"if NOT (%s %s %s) GOTO Label%d",$1,$2,$3,label+1);label++;ir.push_back(ch);strcpy(ch,buff);}
        else if(check==3){char ch[100];sprintf(ch,"if NOT (%s %s %s) GOTO Label%d",$1,$2,$3,label);ir.push_back(ch);strcpy(ch,buff);}
        else{{char ch[100];sprintf(ch,"t%d=%s%s%s",cnt,$1,$2,$3); sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}}
        }
	|   EqualityExpression NOTEQUAL RelationalExpression         {if(check==1){char ch[100];sprintf(ch,"if NOT (%s %s %s) GOTO Label%d",$1,$2,$3,label+1);label++;ir.push_back(ch);}
        else if(check==2){char ch[100];sprintf(ch,"if NOT (%s %s %s) GOTO Label%d",$1,$2,$3,label+1);label++;ir.push_back(ch);strcpy(ch,buff);}
        else if(check==3){char ch[100];sprintf(ch,"if NOT (%s %s %s) GOTO Label%d",$1,$2,$3,label);ir.push_back(ch);strcpy(ch,buff);}
        else{{char ch[100];sprintf(ch,"t%d=%s%s%s",cnt,$1,$2,$3); sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}}
        }
	;
AndExpression: AndExpressions   {$$=$1;}
;
AndExpressions:
        EqualityExpression      {$$=$1;}
	|   AndExpression ANDBIT EqualityExpression {char ch[100];sprintf(ch,"t%d=%s%s%s",cnt,$1,$2,$3); sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}

	;

ExclusiveOrExpression: ExclusiveOrExpressions   {$$=$1;}
;
ExclusiveOrExpressions:
        AndExpression     {$$=$1;}
	|   ExclusiveOrExpression POWER AndExpression     {char ch[100];sprintf(ch,"t%d=%s%s%s",cnt,$1,$2,$3); sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}
	;

InclusiveOrExpression: InclusiveOrExpressions   {$$=$1;}
;

InclusiveOrExpressions:
        ExclusiveOrExpression {$$=$1;}
	|   InclusiveOrExpression ORBIT ExclusiveOrExpression    {char ch[100];sprintf(ch,"t%d=%s%s%s",cnt,$1,$2,$3); sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}
	;

ConditionalAndExpression: ConditionalAndExpressions     {$$=$1;}
;

ConditionalAndExpressions:
        InclusiveOrExpression     {$$=$1;}
	|   ConditionalAndExpression AND InclusiveOrExpression      {char ch[100];sprintf(ch,"t%d=%s%s%s",cnt,$1,$2,$3); sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}
	;

ConditionalOrExpression: ConditionalOrExpressions       {$$=$1;}
;

ConditionalOrExpressions:
        ConditionalAndExpression     {$$=$1;}
	|   ConditionalOrExpression OR ConditionalAndExpression      {char ch[100];sprintf(ch,"t%d=%s%s%s",cnt,$1,$2,$3); sprintf($$,"t%d",cnt);cnt++;ir.push_back(ch);}
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
        LeftHandSide AssignmentOperator AssignmentExpression {char ch[100];sprintf(ch,"%s%s%s",$1,$2,$3); ir.push_back(ch);  }
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
    yyparse();
    for(auto i : ir){
        cout<<i<<endl;
    }
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
                if(dim[i]>='0'&&dim[i]<='9') temp.insert(temp.begin(),dim[i]);
                if(temp.size()>0&&!(dim[i]>='0'&&dim[i]<='9')){ t.push_back(temp); temp.clear();}
        }
        return t;
}
string array3ac(string s){
        stringstream str(s);
        getline(str, word, '[');
        SymTab();
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
        if(dim.size()==t.size()){
                for(int i = 0 ; i<t.size(); i++){
                        string pro = t[i];
                        for(int j = i+1;j<dim.size();j++){
                              pro+="*"+dim[j];
                        }
                        char ch2[100];
                        sprintf(ts,"t%d",cnt);
                        q+="+";
                        q+=ts;
                        sprintf(ch2,"t%d=",cnt++);
                        char *ch=strConcat(ch2,strdup(pro.c_str()));

                        ir.push_back(ch);
                }
        }
        else{
                cout<<"Dimensions doesn't match"<<endl;
                exit(8);
        }
        char ch[100];
        sprintf(ch,"t%d",cnt++);
        q.erase(q.begin(),q.begin()+1);
        string p = "";
        p+=ch;
        p += "="+q;
        ir.push_back(p);
        return ch;

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

	// for(int i=0;i<SymbolTable.size();i++)
	// {
	// 	for(int j=0;j<SymbolTable[i].size();j++)
	// 	{
	// 		cout<<SymbolTable[i][j]<<" ";
	// 	}
	// 	cout<<"\n";
	// }
}

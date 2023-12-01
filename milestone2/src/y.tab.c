/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 1 "java.y"

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

#line 109 "y.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

/* Use api.header.include to #include this header
   instead of duplicating it here.  */
#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    SYSTEMOUTPRINTLN = 258,        /* SYSTEMOUTPRINTLN  */
    LEFTBRACKET = 259,             /* LEFTBRACKET  */
    RIGHTBRACKET = 260,            /* RIGHTBRACKET  */
    LEFTCURLYBRACKET = 261,        /* LEFTCURLYBRACKET  */
    RIGHTCURLYBRACKET = 262,       /* RIGHTCURLYBRACKET  */
    LEFTSQUAREBRACKET = 263,       /* LEFTSQUAREBRACKET  */
    RIGHTSQUAREBRACKET = 264,      /* RIGHTSQUAREBRACKET  */
    THREEDOT = 265,                /* THREEDOT  */
    SEMICOLON = 266,               /* SEMICOLON  */
    COMMA = 267,                   /* COMMA  */
    DOT = 268,                     /* DOT  */
    ATTHERATE = 269,               /* ATTHERATE  */
    DOUBLECOLON = 270,             /* DOUBLECOLON  */
    TRIPLEGREATEREQUAL = 271,      /* TRIPLEGREATEREQUAL  */
    TRIPLEGREATER = 272,           /* TRIPLEGREATER  */
    DOUBLELESSEQUAL = 273,         /* DOUBLELESSEQUAL  */
    DOUBLEGREATEREQUAL = 274,      /* DOUBLEGREATEREQUAL  */
    DOUBLELESS = 275,              /* DOUBLELESS  */
    DOUBLEGREATER = 276,           /* DOUBLEGREATER  */
    ADDEQUAL = 277,                /* ADDEQUAL  */
    SUBEQUAL = 278,                /* SUBEQUAL  */
    MULEQUAL = 279,                /* MULEQUAL  */
    DIVEQUAL = 280,                /* DIVEQUAL  */
    ANDEQUAL = 281,                /* ANDEQUAL  */
    OREQUAL = 282,                 /* OREQUAL  */
    POWEREQUAL = 283,              /* POWEREQUAL  */
    MODEQUAL = 284,                /* MODEQUAL  */
    ARROW = 285,                   /* ARROW  */
    EQUALEQUAL = 286,              /* EQUALEQUAL  */
    GREATEREQUAL = 287,            /* GREATEREQUAL  */
    LESSEQUAL = 288,               /* LESSEQUAL  */
    NOTEQUAL = 289,                /* NOTEQUAL  */
    AND = 290,                     /* AND  */
    OR = 291,                      /* OR  */
    ADDADD = 292,                  /* ADDADD  */
    SUBSUB = 293,                  /* SUBSUB  */
    EQUAL = 294,                   /* EQUAL  */
    LESS = 295,                    /* LESS  */
    GREATER = 296,                 /* GREATER  */
    NOT = 297,                     /* NOT  */
    TILDE = 298,                   /* TILDE  */
    QUESTION = 299,                /* QUESTION  */
    COLON = 300,                   /* COLON  */
    ADD = 301,                     /* ADD  */
    SUB = 302,                     /* SUB  */
    MUL = 303,                     /* MUL  */
    DIV = 304,                     /* DIV  */
    ANDBIT = 305,                  /* ANDBIT  */
    ORBIT = 306,                   /* ORBIT  */
    POWER = 307,                   /* POWER  */
    MOD = 308,                     /* MOD  */
    ABSTRACT = 309,                /* ABSTRACT  */
    CONTINUE = 310,                /* CONTINUE  */
    FOR = 311,                     /* FOR  */
    STRING = 312,                  /* STRING  */
    NEW = 313,                     /* NEW  */
    SWITCH = 314,                  /* SWITCH  */
    ASSERT = 315,                  /* ASSERT  */
    DEFAULT = 316,                 /* DEFAULT  */
    IF = 317,                      /* IF  */
    PACKAGE = 318,                 /* PACKAGE  */
    SYNCHRONIZED = 319,            /* SYNCHRONIZED  */
    BOOLEAN = 320,                 /* BOOLEAN  */
    DO = 321,                      /* DO  */
    GOTO = 322,                    /* GOTO  */
    PRIVATE = 323,                 /* PRIVATE  */
    THIS = 324,                    /* THIS  */
    BREAK = 325,                   /* BREAK  */
    DOUBLE = 326,                  /* DOUBLE  */
    IMPLEMENTS = 327,              /* IMPLEMENTS  */
    PROTECTED = 328,               /* PROTECTED  */
    THROWS = 329,                  /* THROWS  */
    BYTE = 330,                    /* BYTE  */
    ELSE = 331,                    /* ELSE  */
    IMPORT = 332,                  /* IMPORT  */
    PUBLIC = 333,                  /* PUBLIC  */
    THROW = 334,                   /* THROW  */
    CASE = 335,                    /* CASE  */
    ENUM = 336,                    /* ENUM  */
    INSTANCE = 337,                /* INSTANCE  */
    RETURN = 338,                  /* RETURN  */
    TRANSIENT = 339,               /* TRANSIENT  */
    CATCH = 340,                   /* CATCH  */
    EXTENDS = 341,                 /* EXTENDS  */
    INT = 342,                     /* INT  */
    SHORT = 343,                   /* SHORT  */
    TRY = 344,                     /* TRY  */
    CHAR = 345,                    /* CHAR  */
    FINAL = 346,                   /* FINAL  */
    INTERFACE = 347,               /* INTERFACE  */
    STATIC = 348,                  /* STATIC  */
    VOID = 349,                    /* VOID  */
    CLASS = 350,                   /* CLASS  */
    FINALLY = 351,                 /* FINALLY  */
    LONG = 352,                    /* LONG  */
    STRICTFP = 353,                /* STRICTFP  */
    VOLATILE = 354,                /* VOLATILE  */
    CONST = 355,                   /* CONST  */
    FLOAT = 356,                   /* FLOAT  */
    NATIVE = 357,                  /* NATIVE  */
    SUPER = 358,                   /* SUPER  */
    WHILE = 359,                   /* WHILE  */
    UNDERSEMICOLONORE = 360,       /* UNDERSEMICOLONORE  */
    EXPORTS = 361,                 /* EXPORTS  */
    OPENS = 362,                   /* OPENS  */
    REQUIRES = 363,                /* REQUIRES  */
    USES = 364,                    /* USES  */
    MODULE = 365,                  /* MODULE  */
    PERMITS = 366,                 /* PERMITS  */
    SEALED = 367,                  /* SEALED  */
    VAR = 368,                     /* VAR  */
    NONSEALED = 369,               /* NONSEALED  */
    PROVIDES = 370,                /* PROVIDES  */
    TO = 371,                      /* TO  */
    WITH = 372,                    /* WITH  */
    OPEN = 373,                    /* OPEN  */
    RECORD = 374,                  /* RECORD  */
    TRANSITIVE = 375,              /* TRANSITIVE  */
    YIELD = 376,                   /* YIELD  */
    NULLLITERAL = 377,             /* NULLLITERAL  */
    SUSPEND = 378,                 /* SUSPEND  */
    INTLITERAL = 379,              /* INTLITERAL  */
    FLOATLITERAL = 380,            /* FLOATLITERAL  */
    BINARYLITERAL = 381,           /* BINARYLITERAL  */
    HEXLITERAL = 382,              /* HEXLITERAL  */
    IDENTIFIER = 383,              /* IDENTIFIER  */
    STRINGLITERAL = 384,           /* STRINGLITERAL  */
    CHARLITERAL = 385,             /* CHARLITERAL  */
    BOOLEANLITERAL = 386           /* BOOLEANLITERAL  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif
/* Token kinds.  */
#define YYEMPTY -2
#define YYEOF 0
#define YYerror 256
#define YYUNDEF 257
#define SYSTEMOUTPRINTLN 258
#define LEFTBRACKET 259
#define RIGHTBRACKET 260
#define LEFTCURLYBRACKET 261
#define RIGHTCURLYBRACKET 262
#define LEFTSQUAREBRACKET 263
#define RIGHTSQUAREBRACKET 264
#define THREEDOT 265
#define SEMICOLON 266
#define COMMA 267
#define DOT 268
#define ATTHERATE 269
#define DOUBLECOLON 270
#define TRIPLEGREATEREQUAL 271
#define TRIPLEGREATER 272
#define DOUBLELESSEQUAL 273
#define DOUBLEGREATEREQUAL 274
#define DOUBLELESS 275
#define DOUBLEGREATER 276
#define ADDEQUAL 277
#define SUBEQUAL 278
#define MULEQUAL 279
#define DIVEQUAL 280
#define ANDEQUAL 281
#define OREQUAL 282
#define POWEREQUAL 283
#define MODEQUAL 284
#define ARROW 285
#define EQUALEQUAL 286
#define GREATEREQUAL 287
#define LESSEQUAL 288
#define NOTEQUAL 289
#define AND 290
#define OR 291
#define ADDADD 292
#define SUBSUB 293
#define EQUAL 294
#define LESS 295
#define GREATER 296
#define NOT 297
#define TILDE 298
#define QUESTION 299
#define COLON 300
#define ADD 301
#define SUB 302
#define MUL 303
#define DIV 304
#define ANDBIT 305
#define ORBIT 306
#define POWER 307
#define MOD 308
#define ABSTRACT 309
#define CONTINUE 310
#define FOR 311
#define STRING 312
#define NEW 313
#define SWITCH 314
#define ASSERT 315
#define DEFAULT 316
#define IF 317
#define PACKAGE 318
#define SYNCHRONIZED 319
#define BOOLEAN 320
#define DO 321
#define GOTO 322
#define PRIVATE 323
#define THIS 324
#define BREAK 325
#define DOUBLE 326
#define IMPLEMENTS 327
#define PROTECTED 328
#define THROWS 329
#define BYTE 330
#define ELSE 331
#define IMPORT 332
#define PUBLIC 333
#define THROW 334
#define CASE 335
#define ENUM 336
#define INSTANCE 337
#define RETURN 338
#define TRANSIENT 339
#define CATCH 340
#define EXTENDS 341
#define INT 342
#define SHORT 343
#define TRY 344
#define CHAR 345
#define FINAL 346
#define INTERFACE 347
#define STATIC 348
#define VOID 349
#define CLASS 350
#define FINALLY 351
#define LONG 352
#define STRICTFP 353
#define VOLATILE 354
#define CONST 355
#define FLOAT 356
#define NATIVE 357
#define SUPER 358
#define WHILE 359
#define UNDERSEMICOLONORE 360
#define EXPORTS 361
#define OPENS 362
#define REQUIRES 363
#define USES 364
#define MODULE 365
#define PERMITS 366
#define SEALED 367
#define VAR 368
#define NONSEALED 369
#define PROVIDES 370
#define TO 371
#define WITH 372
#define OPEN 373
#define RECORD 374
#define TRANSITIVE 375
#define YIELD 376
#define NULLLITERAL 377
#define SUSPEND 378
#define INTLITERAL 379
#define FLOATLITERAL 380
#define BINARYLITERAL 381
#define HEXLITERAL 382
#define IDENTIFIER 383
#define STRINGLITERAL 384
#define CHARLITERAL 385
#define BOOLEANLITERAL 386

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 42 "java.y"

        double num;
    char *nd;

    char *id;

#line 431 "y.tab.c"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_SYSTEMOUTPRINTLN = 3,           /* SYSTEMOUTPRINTLN  */
  YYSYMBOL_LEFTBRACKET = 4,                /* LEFTBRACKET  */
  YYSYMBOL_RIGHTBRACKET = 5,               /* RIGHTBRACKET  */
  YYSYMBOL_LEFTCURLYBRACKET = 6,           /* LEFTCURLYBRACKET  */
  YYSYMBOL_RIGHTCURLYBRACKET = 7,          /* RIGHTCURLYBRACKET  */
  YYSYMBOL_LEFTSQUAREBRACKET = 8,          /* LEFTSQUAREBRACKET  */
  YYSYMBOL_RIGHTSQUAREBRACKET = 9,         /* RIGHTSQUAREBRACKET  */
  YYSYMBOL_THREEDOT = 10,                  /* THREEDOT  */
  YYSYMBOL_SEMICOLON = 11,                 /* SEMICOLON  */
  YYSYMBOL_COMMA = 12,                     /* COMMA  */
  YYSYMBOL_DOT = 13,                       /* DOT  */
  YYSYMBOL_ATTHERATE = 14,                 /* ATTHERATE  */
  YYSYMBOL_DOUBLECOLON = 15,               /* DOUBLECOLON  */
  YYSYMBOL_TRIPLEGREATEREQUAL = 16,        /* TRIPLEGREATEREQUAL  */
  YYSYMBOL_TRIPLEGREATER = 17,             /* TRIPLEGREATER  */
  YYSYMBOL_DOUBLELESSEQUAL = 18,           /* DOUBLELESSEQUAL  */
  YYSYMBOL_DOUBLEGREATEREQUAL = 19,        /* DOUBLEGREATEREQUAL  */
  YYSYMBOL_DOUBLELESS = 20,                /* DOUBLELESS  */
  YYSYMBOL_DOUBLEGREATER = 21,             /* DOUBLEGREATER  */
  YYSYMBOL_ADDEQUAL = 22,                  /* ADDEQUAL  */
  YYSYMBOL_SUBEQUAL = 23,                  /* SUBEQUAL  */
  YYSYMBOL_MULEQUAL = 24,                  /* MULEQUAL  */
  YYSYMBOL_DIVEQUAL = 25,                  /* DIVEQUAL  */
  YYSYMBOL_ANDEQUAL = 26,                  /* ANDEQUAL  */
  YYSYMBOL_OREQUAL = 27,                   /* OREQUAL  */
  YYSYMBOL_POWEREQUAL = 28,                /* POWEREQUAL  */
  YYSYMBOL_MODEQUAL = 29,                  /* MODEQUAL  */
  YYSYMBOL_ARROW = 30,                     /* ARROW  */
  YYSYMBOL_EQUALEQUAL = 31,                /* EQUALEQUAL  */
  YYSYMBOL_GREATEREQUAL = 32,              /* GREATEREQUAL  */
  YYSYMBOL_LESSEQUAL = 33,                 /* LESSEQUAL  */
  YYSYMBOL_NOTEQUAL = 34,                  /* NOTEQUAL  */
  YYSYMBOL_AND = 35,                       /* AND  */
  YYSYMBOL_OR = 36,                        /* OR  */
  YYSYMBOL_ADDADD = 37,                    /* ADDADD  */
  YYSYMBOL_SUBSUB = 38,                    /* SUBSUB  */
  YYSYMBOL_EQUAL = 39,                     /* EQUAL  */
  YYSYMBOL_LESS = 40,                      /* LESS  */
  YYSYMBOL_GREATER = 41,                   /* GREATER  */
  YYSYMBOL_NOT = 42,                       /* NOT  */
  YYSYMBOL_TILDE = 43,                     /* TILDE  */
  YYSYMBOL_QUESTION = 44,                  /* QUESTION  */
  YYSYMBOL_COLON = 45,                     /* COLON  */
  YYSYMBOL_ADD = 46,                       /* ADD  */
  YYSYMBOL_SUB = 47,                       /* SUB  */
  YYSYMBOL_MUL = 48,                       /* MUL  */
  YYSYMBOL_DIV = 49,                       /* DIV  */
  YYSYMBOL_ANDBIT = 50,                    /* ANDBIT  */
  YYSYMBOL_ORBIT = 51,                     /* ORBIT  */
  YYSYMBOL_POWER = 52,                     /* POWER  */
  YYSYMBOL_MOD = 53,                       /* MOD  */
  YYSYMBOL_ABSTRACT = 54,                  /* ABSTRACT  */
  YYSYMBOL_CONTINUE = 55,                  /* CONTINUE  */
  YYSYMBOL_FOR = 56,                       /* FOR  */
  YYSYMBOL_STRING = 57,                    /* STRING  */
  YYSYMBOL_NEW = 58,                       /* NEW  */
  YYSYMBOL_SWITCH = 59,                    /* SWITCH  */
  YYSYMBOL_ASSERT = 60,                    /* ASSERT  */
  YYSYMBOL_DEFAULT = 61,                   /* DEFAULT  */
  YYSYMBOL_IF = 62,                        /* IF  */
  YYSYMBOL_PACKAGE = 63,                   /* PACKAGE  */
  YYSYMBOL_SYNCHRONIZED = 64,              /* SYNCHRONIZED  */
  YYSYMBOL_BOOLEAN = 65,                   /* BOOLEAN  */
  YYSYMBOL_DO = 66,                        /* DO  */
  YYSYMBOL_GOTO = 67,                      /* GOTO  */
  YYSYMBOL_PRIVATE = 68,                   /* PRIVATE  */
  YYSYMBOL_THIS = 69,                      /* THIS  */
  YYSYMBOL_BREAK = 70,                     /* BREAK  */
  YYSYMBOL_DOUBLE = 71,                    /* DOUBLE  */
  YYSYMBOL_IMPLEMENTS = 72,                /* IMPLEMENTS  */
  YYSYMBOL_PROTECTED = 73,                 /* PROTECTED  */
  YYSYMBOL_THROWS = 74,                    /* THROWS  */
  YYSYMBOL_BYTE = 75,                      /* BYTE  */
  YYSYMBOL_ELSE = 76,                      /* ELSE  */
  YYSYMBOL_IMPORT = 77,                    /* IMPORT  */
  YYSYMBOL_PUBLIC = 78,                    /* PUBLIC  */
  YYSYMBOL_THROW = 79,                     /* THROW  */
  YYSYMBOL_CASE = 80,                      /* CASE  */
  YYSYMBOL_ENUM = 81,                      /* ENUM  */
  YYSYMBOL_INSTANCE = 82,                  /* INSTANCE  */
  YYSYMBOL_RETURN = 83,                    /* RETURN  */
  YYSYMBOL_TRANSIENT = 84,                 /* TRANSIENT  */
  YYSYMBOL_CATCH = 85,                     /* CATCH  */
  YYSYMBOL_EXTENDS = 86,                   /* EXTENDS  */
  YYSYMBOL_INT = 87,                       /* INT  */
  YYSYMBOL_SHORT = 88,                     /* SHORT  */
  YYSYMBOL_TRY = 89,                       /* TRY  */
  YYSYMBOL_CHAR = 90,                      /* CHAR  */
  YYSYMBOL_FINAL = 91,                     /* FINAL  */
  YYSYMBOL_INTERFACE = 92,                 /* INTERFACE  */
  YYSYMBOL_STATIC = 93,                    /* STATIC  */
  YYSYMBOL_VOID = 94,                      /* VOID  */
  YYSYMBOL_CLASS = 95,                     /* CLASS  */
  YYSYMBOL_FINALLY = 96,                   /* FINALLY  */
  YYSYMBOL_LONG = 97,                      /* LONG  */
  YYSYMBOL_STRICTFP = 98,                  /* STRICTFP  */
  YYSYMBOL_VOLATILE = 99,                  /* VOLATILE  */
  YYSYMBOL_CONST = 100,                    /* CONST  */
  YYSYMBOL_FLOAT = 101,                    /* FLOAT  */
  YYSYMBOL_NATIVE = 102,                   /* NATIVE  */
  YYSYMBOL_SUPER = 103,                    /* SUPER  */
  YYSYMBOL_WHILE = 104,                    /* WHILE  */
  YYSYMBOL_UNDERSEMICOLONORE = 105,        /* UNDERSEMICOLONORE  */
  YYSYMBOL_EXPORTS = 106,                  /* EXPORTS  */
  YYSYMBOL_OPENS = 107,                    /* OPENS  */
  YYSYMBOL_REQUIRES = 108,                 /* REQUIRES  */
  YYSYMBOL_USES = 109,                     /* USES  */
  YYSYMBOL_MODULE = 110,                   /* MODULE  */
  YYSYMBOL_PERMITS = 111,                  /* PERMITS  */
  YYSYMBOL_SEALED = 112,                   /* SEALED  */
  YYSYMBOL_VAR = 113,                      /* VAR  */
  YYSYMBOL_NONSEALED = 114,                /* NONSEALED  */
  YYSYMBOL_PROVIDES = 115,                 /* PROVIDES  */
  YYSYMBOL_TO = 116,                       /* TO  */
  YYSYMBOL_WITH = 117,                     /* WITH  */
  YYSYMBOL_OPEN = 118,                     /* OPEN  */
  YYSYMBOL_RECORD = 119,                   /* RECORD  */
  YYSYMBOL_TRANSITIVE = 120,               /* TRANSITIVE  */
  YYSYMBOL_YIELD = 121,                    /* YIELD  */
  YYSYMBOL_NULLLITERAL = 122,              /* NULLLITERAL  */
  YYSYMBOL_SUSPEND = 123,                  /* SUSPEND  */
  YYSYMBOL_INTLITERAL = 124,               /* INTLITERAL  */
  YYSYMBOL_FLOATLITERAL = 125,             /* FLOATLITERAL  */
  YYSYMBOL_BINARYLITERAL = 126,            /* BINARYLITERAL  */
  YYSYMBOL_HEXLITERAL = 127,               /* HEXLITERAL  */
  YYSYMBOL_IDENTIFIER = 128,               /* IDENTIFIER  */
  YYSYMBOL_STRINGLITERAL = 129,            /* STRINGLITERAL  */
  YYSYMBOL_CHARLITERAL = 130,              /* CHARLITERAL  */
  YYSYMBOL_BOOLEANLITERAL = 131,           /* BOOLEANLITERAL  */
  YYSYMBOL_YYACCEPT = 132,                 /* $accept  */
  YYSYMBOL_Start = 133,                    /* Start  */
  YYSYMBOL_Literal = 134,                  /* Literal  */
  YYSYMBOL_Type = 135,                     /* Type  */
  YYSYMBOL_PrimitiveType = 136,            /* PrimitiveType  */
  YYSYMBOL_NumericType = 137,              /* NumericType  */
  YYSYMBOL_IntegralType = 138,             /* IntegralType  */
  YYSYMBOL_FloatingPointType = 139,        /* FloatingPointType  */
  YYSYMBOL_NonPrimitiveType = 140,         /* NonPrimitiveType  */
  YYSYMBOL_ArrayType = 141,                /* ArrayType  */
  YYSYMBOL_Name = 142,                     /* Name  */
  YYSYMBOL_SimpleName = 143,               /* SimpleName  */
  YYSYMBOL_QualifiedName = 144,            /* QualifiedName  */
  YYSYMBOL_CompilationUnit = 145,          /* CompilationUnit  */
  YYSYMBOL_ImportDeclarationOpt = 146,     /* ImportDeclarationOpt  */
  YYSYMBOL_ImportDeclarationOpts = 147,    /* ImportDeclarationOpts  */
  YYSYMBOL_TypeDeclarationOpt = 148,       /* TypeDeclarationOpt  */
  YYSYMBOL_TypeDeclarationOpts = 149,      /* TypeDeclarationOpts  */
  YYSYMBOL_ImportDeclarations = 150,       /* ImportDeclarations  */
  YYSYMBOL_TypeDeclarations = 151,         /* TypeDeclarations  */
  YYSYMBOL_ImportDeclaration = 152,        /* ImportDeclaration  */
  YYSYMBOL_SingleTypeImportDeclaration = 153, /* SingleTypeImportDeclaration  */
  YYSYMBOL_TypeImportOnDemandDeclaration = 154, /* TypeImportOnDemandDeclaration  */
  YYSYMBOL_TypeDeclaration = 155,          /* TypeDeclaration  */
  YYSYMBOL_Modifiers = 156,                /* Modifiers  */
  YYSYMBOL_Modifier = 157,                 /* Modifier  */
  YYSYMBOL_ClassDeclaration = 158,         /* ClassDeclaration  */
  YYSYMBOL_159_1 = 159,                    /* $@1  */
  YYSYMBOL_160_2 = 160,                    /* $@2  */
  YYSYMBOL_ModifiersOpt = 161,             /* ModifiersOpt  */
  YYSYMBOL_ModifiersOpts = 162,            /* ModifiersOpts  */
  YYSYMBOL_ClassBody = 163,                /* ClassBody  */
  YYSYMBOL_ClassBodyDeclarationsOpt = 164, /* ClassBodyDeclarationsOpt  */
  YYSYMBOL_ClassBodyDeclarationsOpts = 165, /* ClassBodyDeclarationsOpts  */
  YYSYMBOL_ClassBodyDeclarations = 166,    /* ClassBodyDeclarations  */
  YYSYMBOL_ClassBodyDeclaration = 167,     /* ClassBodyDeclaration  */
  YYSYMBOL_ClassMemberDeclaration = 168,   /* ClassMemberDeclaration  */
  YYSYMBOL_FieldDeclaration = 169,         /* FieldDeclaration  */
  YYSYMBOL_VarDeclarators = 170,           /* VarDeclarators  */
  YYSYMBOL_VarDeclarator = 171,            /* VarDeclarator  */
  YYSYMBOL_VarDeclaratorId = 172,          /* VarDeclaratorId  */
  YYSYMBOL_VarDeclaratorIds = 173,         /* VarDeclaratorIds  */
  YYSYMBOL_VarInitializer = 174,           /* VarInitializer  */
  YYSYMBOL_MethodDeclaration = 175,        /* MethodDeclaration  */
  YYSYMBOL_MethodHeader = 176,             /* MethodHeader  */
  YYSYMBOL_FormalParameterListOpt = 177,   /* FormalParameterListOpt  */
  YYSYMBOL_178_3 = 178,                    /* $@3  */
  YYSYMBOL_FormalParameterListOpts = 179,  /* FormalParameterListOpts  */
  YYSYMBOL_MethodDeclarator = 180,         /* MethodDeclarator  */
  YYSYMBOL_MethodDeclarators = 181,        /* MethodDeclarators  */
  YYSYMBOL_182_4 = 182,                    /* $@4  */
  YYSYMBOL_FormalParameterList = 183,      /* FormalParameterList  */
  YYSYMBOL_FormalParameter = 184,          /* FormalParameter  */
  YYSYMBOL_MethodBody = 185,               /* MethodBody  */
  YYSYMBOL_StaticInitializer = 186,        /* StaticInitializer  */
  YYSYMBOL_ConstructorDeclaration = 187,   /* ConstructorDeclaration  */
  YYSYMBOL_ConstructorDeclarator = 188,    /* ConstructorDeclarator  */
  YYSYMBOL_ExplicitConstructorInvocationOpt = 189, /* ExplicitConstructorInvocationOpt  */
  YYSYMBOL_ExplicitConstructorInvocationOpts = 190, /* ExplicitConstructorInvocationOpts  */
  YYSYMBOL_BlockStatementOpt = 191,        /* BlockStatementOpt  */
  YYSYMBOL_BlockStatementOpts = 192,       /* BlockStatementOpts  */
  YYSYMBOL_ArgumentListOpt = 193,          /* ArgumentListOpt  */
  YYSYMBOL_ArgumentListOpts = 194,         /* ArgumentListOpts  */
  YYSYMBOL_ConstructorBody = 195,          /* ConstructorBody  */
  YYSYMBOL_ExplicitConstructorInvocation = 196, /* ExplicitConstructorInvocation  */
  YYSYMBOL_VarInitializersOpt = 197,       /* VarInitializersOpt  */
  YYSYMBOL_VarInitializersOpts = 198,      /* VarInitializersOpts  */
  YYSYMBOL_CommaOpt = 199,                 /* CommaOpt  */
  YYSYMBOL_CommaOpts = 200,                /* CommaOpts  */
  YYSYMBOL_ArrayInitializer = 201,         /* ArrayInitializer  */
  YYSYMBOL_VarInitializers = 202,          /* VarInitializers  */
  YYSYMBOL_Block = 203,                    /* Block  */
  YYSYMBOL_204_5 = 204,                    /* $@5  */
  YYSYMBOL_BlockStatements = 205,          /* BlockStatements  */
  YYSYMBOL_BlockStatement = 206,           /* BlockStatement  */
  YYSYMBOL_LocalVariableDeclarationStatement = 207, /* LocalVariableDeclarationStatement  */
  YYSYMBOL_LocalVariableDeclaration = 208, /* LocalVariableDeclaration  */
  YYSYMBOL_VariableDeclarators = 209,      /* VariableDeclarators  */
  YYSYMBOL_VariableDeclarator = 210,       /* VariableDeclarator  */
  YYSYMBOL_VariableInitializer = 211,      /* VariableInitializer  */
  YYSYMBOL_VariableDeclaratorId = 212,     /* VariableDeclaratorId  */
  YYSYMBOL_Statement = 213,                /* Statement  */
  YYSYMBOL_PrintlnStatement = 214,         /* PrintlnStatement  */
  YYSYMBOL_StatementNoShortIf = 215,       /* StatementNoShortIf  */
  YYSYMBOL_StatementWithoutTrailingSubstatement = 216, /* StatementWithoutTrailingSubstatement  */
  YYSYMBOL_EmptyStatement = 217,           /* EmptyStatement  */
  YYSYMBOL_LabeledStatement = 218,         /* LabeledStatement  */
  YYSYMBOL_LabeledStatementNoShortIf = 219, /* LabeledStatementNoShortIf  */
  YYSYMBOL_ExpressionStatement = 220,      /* ExpressionStatement  */
  YYSYMBOL_StatementExpression = 221,      /* StatementExpression  */
  YYSYMBOL_ClassInstanceCreationExpression = 222, /* ClassInstanceCreationExpression  */
  YYSYMBOL_ClassType = 223,                /* ClassType  */
  YYSYMBOL_ClassOrInterfaceType = 224,     /* ClassOrInterfaceType  */
  YYSYMBOL_IfThenStatement = 225,          /* IfThenStatement  */
  YYSYMBOL_IfThenElseStatement = 226,      /* IfThenElseStatement  */
  YYSYMBOL_IfThenElseStatementNoShortIf = 227, /* IfThenElseStatementNoShortIf  */
  YYSYMBOL_WhileStatement = 228,           /* WhileStatement  */
  YYSYMBOL_WhileStatementNoShortIf = 229,  /* WhileStatementNoShortIf  */
  YYSYMBOL_ForInitOpt = 230,               /* ForInitOpt  */
  YYSYMBOL_ForInitOpts = 231,              /* ForInitOpts  */
  YYSYMBOL_ExpressionOpt = 232,            /* ExpressionOpt  */
  YYSYMBOL_ExpressionOpts = 233,           /* ExpressionOpts  */
  YYSYMBOL_ForUpdateOpt = 234,             /* ForUpdateOpt  */
  YYSYMBOL_ForUpdateOpts = 235,            /* ForUpdateOpts  */
  YYSYMBOL_ForStatement = 236,             /* ForStatement  */
  YYSYMBOL_ForStatementNoShortIf = 237,    /* ForStatementNoShortIf  */
  YYSYMBOL_ForInit = 238,                  /* ForInit  */
  YYSYMBOL_ForUpdate = 239,                /* ForUpdate  */
  YYSYMBOL_StatementExpressionList = 240,  /* StatementExpressionList  */
  YYSYMBOL_IDENTOpt = 241,                 /* IDENTOpt  */
  YYSYMBOL_IDENTOpts = 242,                /* IDENTOpts  */
  YYSYMBOL_BreakStatement = 243,           /* BreakStatement  */
  YYSYMBOL_ContinueStatement = 244,        /* ContinueStatement  */
  YYSYMBOL_ReturnStatement = 245,          /* ReturnStatement  */
  YYSYMBOL_Primary = 246,                  /* Primary  */
  YYSYMBOL_PrimaryNoNewArray = 247,        /* PrimaryNoNewArray  */
  YYSYMBOL_ArgumentList = 248,             /* ArgumentList  */
  YYSYMBOL_DimsOpt = 249,                  /* DimsOpt  */
  YYSYMBOL_DimsOpts = 250,                 /* DimsOpts  */
  YYSYMBOL_ArrayCreationExpression = 251,  /* ArrayCreationExpression  */
  YYSYMBOL_DimExprs = 252,                 /* DimExprs  */
  YYSYMBOL_DimExpr = 253,                  /* DimExpr  */
  YYSYMBOL_Dims = 254,                     /* Dims  */
  YYSYMBOL_FieldAccess = 255,              /* FieldAccess  */
  YYSYMBOL_MethodInvocation = 256,         /* MethodInvocation  */
  YYSYMBOL_ArrayAccess = 257,              /* ArrayAccess  */
  YYSYMBOL_PostFixExpression = 258,        /* PostFixExpression  */
  YYSYMBOL_PostIncrementExpression = 259,  /* PostIncrementExpression  */
  YYSYMBOL_PostDecrementExpression = 260,  /* PostDecrementExpression  */
  YYSYMBOL_UnaryExpression = 261,          /* UnaryExpression  */
  YYSYMBOL_PreIncrementExpression = 262,   /* PreIncrementExpression  */
  YYSYMBOL_PreDecrementExpression = 263,   /* PreDecrementExpression  */
  YYSYMBOL_UnaryExpressionNotPlusMinus = 264, /* UnaryExpressionNotPlusMinus  */
  YYSYMBOL_CastExpression = 265,           /* CastExpression  */
  YYSYMBOL_MultiplicativeExpression = 266, /* MultiplicativeExpression  */
  YYSYMBOL_MultiplicativeExpressions = 267, /* MultiplicativeExpressions  */
  YYSYMBOL_AdditiveExpression = 268,       /* AdditiveExpression  */
  YYSYMBOL_AdditiveExpressions = 269,      /* AdditiveExpressions  */
  YYSYMBOL_ShiftExpression = 270,          /* ShiftExpression  */
  YYSYMBOL_ShiftExpressions = 271,         /* ShiftExpressions  */
  YYSYMBOL_RelationalExpression = 272,     /* RelationalExpression  */
  YYSYMBOL_RelationalExpressions = 273,    /* RelationalExpressions  */
  YYSYMBOL_EqualityExpression = 274,       /* EqualityExpression  */
  YYSYMBOL_EqualityExpressions = 275,      /* EqualityExpressions  */
  YYSYMBOL_AndExpression = 276,            /* AndExpression  */
  YYSYMBOL_AndExpressions = 277,           /* AndExpressions  */
  YYSYMBOL_ExclusiveOrExpression = 278,    /* ExclusiveOrExpression  */
  YYSYMBOL_ExclusiveOrExpressions = 279,   /* ExclusiveOrExpressions  */
  YYSYMBOL_InclusiveOrExpression = 280,    /* InclusiveOrExpression  */
  YYSYMBOL_InclusiveOrExpressions = 281,   /* InclusiveOrExpressions  */
  YYSYMBOL_ConditionalAndExpression = 282, /* ConditionalAndExpression  */
  YYSYMBOL_ConditionalAndExpressions = 283, /* ConditionalAndExpressions  */
  YYSYMBOL_ConditionalOrExpression = 284,  /* ConditionalOrExpression  */
  YYSYMBOL_ConditionalOrExpressions = 285, /* ConditionalOrExpressions  */
  YYSYMBOL_ConditionalExpression = 286,    /* ConditionalExpression  */
  YYSYMBOL_ConditionalExpressions = 287,   /* ConditionalExpressions  */
  YYSYMBOL_AssignmentExpression = 288,     /* AssignmentExpression  */
  YYSYMBOL_Assignment = 289,               /* Assignment  */
  YYSYMBOL_290_6 = 290,                    /* $@6  */
  YYSYMBOL_LeftHandSide = 291,             /* LeftHandSide  */
  YYSYMBOL_AssignmentOperator = 292,       /* AssignmentOperator  */
  YYSYMBOL_Expression = 293                /* Expression  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  14
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1262

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  132
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  162
/* YYNRULES -- Number of rules.  */
#define YYNRULES  299
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  476

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   386


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,    67,    67,    70,    71,    72,    73,    74,    75,    78,
      79,    82,    83,    84,    87,    88,    91,    92,    93,    96,
      97,   100,   103,   104,   105,   108,   109,   112,   115,   118,
     120,   124,   125,   129,   133,   134,   138,   139,   142,   143,
     146,   147,   150,   153,   156,   159,   160,   163,   164,   165,
     168,   168,   169,   169,   171,   174,   175,   180,   183,   187,
     188,   192,   193,   196,   197,   198,   201,   202,   205,   208,
     209,   212,   213,   215,   219,   220,   223,   224,   227,   230,
     231,   234,   234,   238,   239,   242,   245,   245,   246,   249,
     250,   253,   256,   259,   262,   265,   268,   271,   272,   276,
     279,   280,   284,   287,   288,   292,   295,   296,   299,   303,
     304,   308,   311,   312,   316,   319,   320,   323,   323,   326,
     327,   330,   331,   334,   336,   338,   339,   342,   343,   346,
     347,   350,   351,   355,   356,   357,   358,   359,   360,   361,
     364,   367,   368,   369,   370,   371,   375,   376,   377,   378,
     379,   380,   383,   386,   389,   393,   396,   397,   398,   399,
     400,   401,   402,   405,   408,   411,   415,   418,   421,   424,
     428,   430,   434,   435,   439,   442,   443,   447,   450,   451,
     456,   460,   466,   467,   471,   475,   476,   479,   481,   482,
     487,   491,   495,   496,   499,   500,   503,   504,   505,   506,
     507,   508,   511,   512,   515,   518,   519,   523,   524,   527,
     528,   531,   534,   535,   538,   539,   542,   543,   544,   545,
     546,   547,   550,   551,   555,   556,   557,   558,   562,   566,
     570,   571,   572,   573,   574,   578,   582,   586,   587,   588,
     589,   593,   594,   595,   598,   602,   603,   604,   605,   608,
     612,   613,   614,   617,   621,   622,   623,   624,   627,   631,
     632,   633,   634,   635,   638,   642,   643,   644,   646,   649,
     650,   653,   656,   657,   660,   664,   665,   668,   672,   673,
     676,   680,   681,   684,   687,   688,   692,   693,   697,   697,
     701,   702,   703,   707,   708,   709,   710,   711,   712,   716
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "SYSTEMOUTPRINTLN",
  "LEFTBRACKET", "RIGHTBRACKET", "LEFTCURLYBRACKET", "RIGHTCURLYBRACKET",
  "LEFTSQUAREBRACKET", "RIGHTSQUAREBRACKET", "THREEDOT", "SEMICOLON",
  "COMMA", "DOT", "ATTHERATE", "DOUBLECOLON", "TRIPLEGREATEREQUAL",
  "TRIPLEGREATER", "DOUBLELESSEQUAL", "DOUBLEGREATEREQUAL", "DOUBLELESS",
  "DOUBLEGREATER", "ADDEQUAL", "SUBEQUAL", "MULEQUAL", "DIVEQUAL",
  "ANDEQUAL", "OREQUAL", "POWEREQUAL", "MODEQUAL", "ARROW", "EQUALEQUAL",
  "GREATEREQUAL", "LESSEQUAL", "NOTEQUAL", "AND", "OR", "ADDADD", "SUBSUB",
  "EQUAL", "LESS", "GREATER", "NOT", "TILDE", "QUESTION", "COLON", "ADD",
  "SUB", "MUL", "DIV", "ANDBIT", "ORBIT", "POWER", "MOD", "ABSTRACT",
  "CONTINUE", "FOR", "STRING", "NEW", "SWITCH", "ASSERT", "DEFAULT", "IF",
  "PACKAGE", "SYNCHRONIZED", "BOOLEAN", "DO", "GOTO", "PRIVATE", "THIS",
  "BREAK", "DOUBLE", "IMPLEMENTS", "PROTECTED", "THROWS", "BYTE", "ELSE",
  "IMPORT", "PUBLIC", "THROW", "CASE", "ENUM", "INSTANCE", "RETURN",
  "TRANSIENT", "CATCH", "EXTENDS", "INT", "SHORT", "TRY", "CHAR", "FINAL",
  "INTERFACE", "STATIC", "VOID", "CLASS", "FINALLY", "LONG", "STRICTFP",
  "VOLATILE", "CONST", "FLOAT", "NATIVE", "SUPER", "WHILE",
  "UNDERSEMICOLONORE", "EXPORTS", "OPENS", "REQUIRES", "USES", "MODULE",
  "PERMITS", "SEALED", "VAR", "NONSEALED", "PROVIDES", "TO", "WITH",
  "OPEN", "RECORD", "TRANSITIVE", "YIELD", "NULLLITERAL", "SUSPEND",
  "INTLITERAL", "FLOATLITERAL", "BINARYLITERAL", "HEXLITERAL",
  "IDENTIFIER", "STRINGLITERAL", "CHARLITERAL", "BOOLEANLITERAL",
  "$accept", "Start", "Literal", "Type", "PrimitiveType", "NumericType",
  "IntegralType", "FloatingPointType", "NonPrimitiveType", "ArrayType",
  "Name", "SimpleName", "QualifiedName", "CompilationUnit",
  "ImportDeclarationOpt", "ImportDeclarationOpts", "TypeDeclarationOpt",
  "TypeDeclarationOpts", "ImportDeclarations", "TypeDeclarations",
  "ImportDeclaration", "SingleTypeImportDeclaration",
  "TypeImportOnDemandDeclaration", "TypeDeclaration", "Modifiers",
  "Modifier", "ClassDeclaration", "$@1", "$@2", "ModifiersOpt",
  "ModifiersOpts", "ClassBody", "ClassBodyDeclarationsOpt",
  "ClassBodyDeclarationsOpts", "ClassBodyDeclarations",
  "ClassBodyDeclaration", "ClassMemberDeclaration", "FieldDeclaration",
  "VarDeclarators", "VarDeclarator", "VarDeclaratorId", "VarDeclaratorIds",
  "VarInitializer", "MethodDeclaration", "MethodHeader",
  "FormalParameterListOpt", "$@3", "FormalParameterListOpts",
  "MethodDeclarator", "MethodDeclarators", "$@4", "FormalParameterList",
  "FormalParameter", "MethodBody", "StaticInitializer",
  "ConstructorDeclaration", "ConstructorDeclarator",
  "ExplicitConstructorInvocationOpt", "ExplicitConstructorInvocationOpts",
  "BlockStatementOpt", "BlockStatementOpts", "ArgumentListOpt",
  "ArgumentListOpts", "ConstructorBody", "ExplicitConstructorInvocation",
  "VarInitializersOpt", "VarInitializersOpts", "CommaOpt", "CommaOpts",
  "ArrayInitializer", "VarInitializers", "Block", "$@5", "BlockStatements",
  "BlockStatement", "LocalVariableDeclarationStatement",
  "LocalVariableDeclaration", "VariableDeclarators", "VariableDeclarator",
  "VariableInitializer", "VariableDeclaratorId", "Statement",
  "PrintlnStatement", "StatementNoShortIf",
  "StatementWithoutTrailingSubstatement", "EmptyStatement",
  "LabeledStatement", "LabeledStatementNoShortIf", "ExpressionStatement",
  "StatementExpression", "ClassInstanceCreationExpression", "ClassType",
  "ClassOrInterfaceType", "IfThenStatement", "IfThenElseStatement",
  "IfThenElseStatementNoShortIf", "WhileStatement",
  "WhileStatementNoShortIf", "ForInitOpt", "ForInitOpts", "ExpressionOpt",
  "ExpressionOpts", "ForUpdateOpt", "ForUpdateOpts", "ForStatement",
  "ForStatementNoShortIf", "ForInit", "ForUpdate",
  "StatementExpressionList", "IDENTOpt", "IDENTOpts", "BreakStatement",
  "ContinueStatement", "ReturnStatement", "Primary", "PrimaryNoNewArray",
  "ArgumentList", "DimsOpt", "DimsOpts", "ArrayCreationExpression",
  "DimExprs", "DimExpr", "Dims", "FieldAccess", "MethodInvocation",
  "ArrayAccess", "PostFixExpression", "PostIncrementExpression",
  "PostDecrementExpression", "UnaryExpression", "PreIncrementExpression",
  "PreDecrementExpression", "UnaryExpressionNotPlusMinus",
  "CastExpression", "MultiplicativeExpression",
  "MultiplicativeExpressions", "AdditiveExpression", "AdditiveExpressions",
  "ShiftExpression", "ShiftExpressions", "RelationalExpression",
  "RelationalExpressions", "EqualityExpression", "EqualityExpressions",
  "AndExpression", "AndExpressions", "ExclusiveOrExpression",
  "ExclusiveOrExpressions", "InclusiveOrExpression",
  "InclusiveOrExpressions", "ConditionalAndExpression",
  "ConditionalAndExpressions", "ConditionalOrExpression",
  "ConditionalOrExpressions", "ConditionalExpression",
  "ConditionalExpressions", "AssignmentExpression", "Assignment", "$@6",
  "LeftHandSide", "AssignmentOperator", "Expression", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-427)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-293)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     -37,   -79,    61,  -427,    47,  -427,   -37,  -427,  -427,  -427,
    -427,    51,  -427,  -427,  -427,  -427,  -427,  -427,  -427,  -427,
      47,  -427,   123,  -427,  -427,   -27,  -427,  -427,   -38,  -427,
    -427,   -50,    62,    91,     0,  -427,  -427,   117,    41,    17,
    -427,   -14,   141,   -32,   145,  -427,    19,  -427,  -427,  -427,
    -427,   141,  -427,  -427,   117,  -427,  -427,  -427,  -427,  -427,
    -427,  -427,    26,  -427,  -427,    29,   163,  -427,  -427,  -427,
    -427,   176,    30,   189,   198,  -427,  -427,  -427,  -427,  -427,
     771,  -427,   200,  -427,   202,   154,  -427,    20,  -427,   200,
     208,   211,   213,    82,  -427,   -55,  -427,   225,  1131,  -427,
    1131,  1131,    84,   230,    93,   233,  -427,    84,  1131,   219,
     245,  -427,  1131,  -427,  -427,   210,  -427,  -427,  -427,  -427,
     132,   201,   258,  -427,  -427,   771,  -427,  -427,   259,  -427,
    -427,  -427,  -427,  -427,  -427,   267,  -427,  -427,  -427,  -427,
    -427,  -427,  -427,  -427,   287,   293,  -427,    83,   108,   228,
     203,   206,   221,  -427,  -427,  -427,  -427,   301,   300,  -427,
     183,   304,  1087,  -427,  -427,  -427,  -427,   317,    93,   319,
     325,   771,  -427,  -427,  1131,   972,  1131,  1131,  1131,  1131,
     -34,   302,  -427,   203,  -427,  -427,  -427,  -427,  -427,  -427,
    -427,   150,  -427,   122,  -427,    55,  -427,   156,  -427,    23,
    -427,   280,  -427,   281,  -427,   284,  -427,   297,  -427,   -17,
    -427,  -427,  -427,  -427,  -427,   331,    28,  -427,  -427,  -427,
    -427,  -427,   327,  -427,  1019,   335,   326,   344,  -427,  1131,
     338,   339,  -427,  -427,   223,  1131,   343,   867,  -427,   345,
    -427,    21,  1131,  1131,   432,  -427,  -427,  -427,  -427,   231,
    1131,  -427,  -427,   295,  -427,  -427,  -427,  -427,  -427,  1087,
    -427,  -427,  -427,  -427,   183,  -427,   346,  -427,  1131,  1131,
     349,   355,   353,   607,   358,  -427,  -427,  -427,  -427,  1131,
    1131,  1131,  1131,  1131,  1131,  1131,  1131,  1131,  1131,  1131,
    1131,  1131,  1131,  1131,  1131,  1131,  1131,  1131,  1131,  1131,
    -427,  -427,  -427,  -427,   360,  -427,  -427,   352,  1131,   364,
    -427,  1131,   369,  -427,  -427,   125,   370,  -427,  -427,   132,
     371,  1087,   372,  -427,   366,  -427,   374,   373,   139,   376,
    -427,  -427,  -427,  -427,  -427,  -427,  1131,   378,  -427,   367,
    -427,   377,   380,    93,   385,   386,  -427,   383,   388,   390,
    -427,   393,  1117,   113,     8,  -427,  -427,  -427,   150,   150,
     122,   122,   122,    55,    55,    55,    55,   156,   156,    23,
     280,   281,   284,   297,   359,  1131,   144,   396,  1117,  -427,
     393,   402,   903,  1131,  1131,   867,  -427,  -427,  -427,  -427,
    -427,  -427,  1131,  -427,  -427,  1131,  1131,  -427,  -427,  -427,
    -427,   404,  -427,  1087,  -427,   403,   405,  -427,  -427,  1131,
     409,     8,  -427,  1131,   411,  -427,  -427,  -427,   417,   419,
     420,   382,  -427,   337,   354,  -427,  -427,  -427,  -427,   426,
     425,  -427,  -427,   428,   430,  -427,  -427,  -427,  -427,  -427,
    -427,  -427,  -427,   144,  1019,  1131,  1131,   903,   867,  -427,
    -427,  -427,  -427,   435,  -427,  -427,   352,   431,   439,   440,
    -427,  -427,   867,  1131,   903,   903,  -427,   436,   387,  -427,
     144,   903,   441,  -427,   903,  -427
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
      32,     0,     0,     2,    35,    30,    31,    36,    40,    41,
      27,     0,    25,    26,     1,    48,    47,    49,    29,    33,
      34,    38,    55,    45,    44,     0,    54,    37,     0,    39,
      46,     0,     0,    28,    50,    43,    42,     0,     0,    56,
      51,     0,    49,     0,     0,    58,    56,    61,    63,    66,
      67,     0,    64,    65,     0,   117,    93,    13,    12,    20,
      16,    18,     0,    17,    19,     0,     9,    11,    14,    15,
      10,    21,     0,    25,     0,    57,    62,    78,    92,    53,
     101,    86,    80,    85,    74,     0,    69,    71,    73,    79,
       0,     0,     0,     0,    81,    98,    94,     0,     0,   152,
       0,     0,   189,     0,     0,     0,   197,   189,   176,     0,
       0,     8,   176,     3,     4,    27,     6,     7,     5,   196,
       0,   290,     0,    99,   146,   100,   119,   121,     0,   122,
     139,   133,   147,   134,   148,     0,   162,   135,   136,   137,
     138,   149,   150,   151,   224,   194,   195,   199,   200,   201,
       0,   159,   160,   157,   158,   156,   288,     0,     0,    68,
       0,     0,     0,    22,    24,    23,    28,     0,    84,     0,
       0,   101,    96,    97,     0,     0,     0,     0,     0,     0,
       0,   225,   200,   237,   226,   227,   245,   230,   231,   234,
     240,   250,   244,   254,   249,   259,   253,   265,   258,   269,
     264,   272,   268,   275,   271,   278,   274,   281,   277,   284,
     280,   286,   283,   299,   287,     0,   225,   199,   201,   235,
     236,   188,     0,   187,   173,     0,   165,     0,   164,     0,
       0,     0,   174,   175,     0,     0,     0,     0,   131,   124,
     125,   127,   104,   104,     0,   118,   120,   123,   155,     0,
       0,   228,   229,     0,    81,    88,    74,    70,    75,   110,
      72,    77,    76,    95,     0,    82,    83,    89,   104,   104,
       0,     0,   206,   225,     0,   239,   238,   232,   233,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     198,   191,   183,   185,     0,   171,   172,   182,     0,   207,
     209,   104,     0,   190,   192,   215,     0,   193,   153,     0,
       0,     0,     0,   102,   103,   202,     0,     0,   214,     0,
     298,   296,   294,   295,   297,   293,     0,     0,   115,   113,
     108,   109,    91,     0,     0,     0,   105,     0,     0,     0,
     204,   205,     0,     0,   198,   246,   247,   248,   251,   252,
     257,   255,   256,   263,   262,   260,   261,   266,   267,   270,
     273,   276,   279,   282,     0,   176,     0,     0,     0,   210,
     208,     0,     0,   104,   104,     0,   126,   132,   130,   128,
     129,   216,     0,   219,   222,   104,   104,   223,   289,    87,
     112,     0,   111,     0,    90,     0,     0,   140,   212,     0,
       0,     0,   242,     0,     0,   186,   211,   163,     0,     0,
       0,    27,   166,     0,   133,   142,   143,   144,   145,     0,
       0,   169,   203,     0,     0,   114,   116,   106,   107,   241,
     213,   243,   285,   179,   173,     0,     0,     0,     0,   218,
     221,   217,   220,     0,   177,   178,   184,     0,     0,     0,
     154,   167,     0,   176,     0,     0,   180,     0,     0,   170,
     179,     0,     0,   168,     0,   181
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -427,  -427,  -427,   -40,   -86,  -427,  -427,  -427,  -427,  -427,
      -1,   406,  -427,  -427,  -427,  -427,  -427,  -427,  -427,  -427,
     442,  -427,  -427,   434,  -427,   437,  -427,  -427,  -427,   -24,
    -427,   398,  -427,  -427,  -427,   412,  -427,  -427,  -427,   290,
     209,  -427,  -252,  -427,  -427,   227,  -427,  -427,   399,  -427,
    -427,  -427,   134,  -427,  -427,  -427,  -427,  -427,  -427,   311,
    -427,  -223,  -427,  -427,  -427,  -427,  -427,  -427,  -427,   162,
    -427,   -21,  -427,  -427,   362,  -427,  -220,  -427,   165,  -427,
    -427,  -229,  -427,  -330,  -373,  -427,  -427,  -427,  -427,  -213,
    -427,  -427,  -427,  -427,  -427,  -427,  -427,  -427,    48,  -427,
    -111,  -427,    15,  -427,  -427,  -427,  -427,  -427,  -426,   384,
    -427,  -427,  -427,  -427,  -427,  -427,  -427,  -427,  -427,  -427,
    -427,   184,  -257,   410,   -78,   506,   -45,    -9,    24,   -95,
      75,   131,  -341,  -427,    -7,  -427,   -60,  -427,    56,  -427,
     -30,  -427,   205,  -427,   207,  -427,   204,  -427,   199,  -427,
     214,  -427,  -427,  -427,    90,  -427,   161,   191,  -427,  -427,
    -427,   368
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     2,   119,   120,    66,    67,    68,    69,    70,    71,
     181,    12,    13,     3,     4,     5,    18,    19,     6,    20,
       7,     8,     9,    21,    22,    23,    24,    37,    38,    25,
      26,    40,    44,    45,    46,    47,    48,    49,    85,    86,
      87,    88,   260,    50,    51,   167,   168,   265,    82,    83,
     157,   266,   267,    77,    52,    53,    74,   171,   172,   122,
     123,   322,   323,    96,   173,   339,   340,   401,   402,   261,
     341,   124,    80,   125,   126,   127,   128,   239,   240,   389,
     241,   129,   130,   423,   131,   132,   133,   425,   134,   135,
     136,   227,   228,   137,   138,   426,   139,   427,   304,   305,
     231,   232,   453,   454,   140,   428,   306,   455,   307,   222,
     223,   141,   142,   143,   144,   145,   324,   349,   350,   146,
     309,   310,   351,   147,   182,   149,   183,   184,   185,   186,
     187,   188,   189,   190,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   200,   201,   202,   203,   204,   205,   206,
     207,   208,   209,   210,   211,   212,   213,   214,   253,   156,
     336,   325
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      11,   236,   148,    65,   302,   219,   220,   338,   318,   424,
      32,   303,   175,   412,   169,    43,   353,   456,   225,   298,
     326,    56,    43,    57,   -60,    57,   -59,   299,   161,   320,
      78,    58,   242,    58,   243,   150,   279,    59,    92,    59,
       1,    93,    72,    93,   456,   344,   345,   148,   170,    10,
     176,   177,   380,    60,   292,    60,    61,   293,    61,   162,
     321,    14,    62,    63,    28,    63,   180,    64,    31,    64,
     441,   151,   285,    35,   424,   286,   287,   106,    34,   121,
     150,   275,   276,   277,   278,    15,   -52,    15,   381,   272,
      33,   424,   424,   148,   225,    16,    10,    16,   424,   216,
     216,   424,    36,   226,   152,  -291,  -291,  -291,  -291,  -291,
      42,   109,    42,  -161,    54,    15,   151,   460,   411,  -161,
    -161,   410,  -291,    39,   121,    16,   150,    41,   264,   383,
     111,   384,   113,   114,   468,   469,    10,   116,   117,   118,
      17,   473,   -56,   395,   475,   396,   148,    55,    98,   152,
      57,   436,    75,   422,    81,   153,   431,    84,    58,   148,
     429,   430,   151,   415,    59,   159,   160,    72,   283,   284,
     121,    90,   433,   434,   273,   216,   216,   216,   216,   150,
      60,   100,   101,    61,    91,   355,   356,   357,   288,   289,
      63,    15,   150,    94,    64,   152,   290,   291,   280,   281,
     153,    16,   104,   282,    95,   242,   -86,   243,   158,   244,
     166,   154,   221,   106,    93,   151,    17,   163,   318,   461,
     164,    10,   165,   121,   302,   360,   361,   362,   151,   174,
     303,   303,   234,   466,   224,   422,   431,   229,  -225,  -225,
     251,   252,   461,  -226,  -226,   466,   153,   109,   152,   235,
    -292,  -292,  -292,  -292,  -292,   237,   154,   303,  -227,  -227,
     238,   152,   367,   368,   414,   245,   111,  -292,   113,   114,
     247,   155,    10,   116,   117,   118,   358,   359,   248,   216,
     216,   216,   216,   216,   216,   216,   216,   216,   216,   216,
     216,   216,   216,   216,   216,   216,   216,   216,   148,   153,
     249,   250,   154,   264,   148,   254,   242,   148,   243,   255,
     279,   256,   153,   258,   439,    93,   155,   330,   331,   332,
     333,   334,   263,   268,  -290,  -290,  -290,  -290,  -290,   269,
     294,   150,   297,   295,   335,   296,   300,   150,   301,    93,
     150,  -290,    72,   308,   363,   364,   365,   366,   311,   313,
     314,   315,   467,   216,   317,   154,   346,   319,   343,   328,
     347,   348,   155,   354,   376,   148,   148,   151,   154,   148,
     148,   375,   378,   151,   382,   385,   151,   391,   392,   400,
     387,   393,   394,   399,   148,   397,   148,   148,   161,   403,
     405,   406,   148,   148,   407,   409,   148,   408,   150,   150,
     152,   410,   150,   150,   413,   416,   152,   417,   216,   152,
     216,   435,   216,   448,   437,   155,   438,   150,   440,   150,
     150,   444,   443,   445,   446,   150,   150,   447,   155,   150,
    -141,   449,   450,   451,   151,   151,   175,   452,   151,   151,
     462,   165,   463,   121,   464,   465,   474,   470,    27,    73,
     257,   153,    79,   151,    29,   151,   151,   153,    76,    30,
     153,   151,   151,   471,    89,   151,   215,   152,   152,   100,
     101,   152,   152,   342,   176,   177,   233,   404,   178,   179,
     233,   337,   270,   388,   386,   472,   152,   246,   152,   152,
     180,   230,   457,   379,   152,   152,   372,   398,   152,   369,
     371,   106,   370,   442,     0,     0,     0,   154,     0,     0,
     217,   217,   373,   154,     0,     0,   154,     0,   153,   153,
       0,     0,   153,   153,     0,     0,     0,     0,     0,     0,
     262,     0,     0,     0,     0,   109,     0,   153,     0,   153,
     153,     0,   271,   274,     0,   153,   153,     0,     0,   153,
       0,     0,     0,     0,   111,     0,   113,   114,     0,     0,
      10,   116,   117,   118,     0,     0,     0,   155,     0,     0,
       0,     0,     0,   155,   154,   154,   155,     0,   154,   154,
       0,     0,     0,     0,     0,     0,   217,   217,   217,   217,
       0,     0,     0,   154,     0,   154,   154,   312,     0,     0,
       0,   154,   154,   316,     0,   154,   218,   218,     0,     0,
       0,   242,   327,   243,     0,   352,     0,     0,   329,     0,
      93,     0,     0,     0,     0,     0,     0,   262,     0,  -290,
    -290,  -290,  -290,  -290,   155,   155,     0,     0,   155,   155,
       0,     0,     0,     0,     0,     0,  -290,   327,     0,     0,
       0,     0,     0,   155,     0,   155,   155,     0,     0,     0,
       0,   155,   155,     0,     0,   155,     0,   374,     0,     0,
       0,     0,     0,     0,     0,     0,   377,     0,     0,     0,
       0,     0,   218,   218,   218,   218,     0,     0,     0,   390,
     217,   217,   217,   217,   217,   217,   217,   217,   217,   217,
     217,   217,   217,   217,   217,   217,   217,   217,   217,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     327,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   233,     0,     0,   377,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     432,     0,     0,     0,   217,     0,     0,     0,     0,     0,
       0,   262,     0,     0,    97,    98,     0,    55,     0,     0,
       0,     0,    99,     0,     0,     0,   218,   218,   218,   218,
     218,   218,   218,   218,   218,   218,   218,   218,   218,   218,
     218,   218,   218,   218,   218,     0,     0,     0,   100,   101,
       0,     0,     0,   458,   459,     0,     0,     0,     0,   217,
       0,   217,     0,   217,     0,     0,   102,   103,    57,   104,
       0,   233,     0,   105,     0,     0,    58,     0,     0,     0,
     106,   107,    59,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   108,     0,     0,     0,    60,     0,
     218,    61,     0,     0,     0,     0,     0,     0,    63,     0,
      97,    98,    64,    55,   109,   110,     0,     0,    99,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   111,   112,   113,   114,     0,     0,   115,
     116,   117,   118,     0,   100,   101,    97,    98,     0,    55,
       0,     0,     0,     0,    99,   218,     0,   218,     0,   218,
       0,     0,   102,   103,     0,   104,     0,     0,     0,   105,
       0,     0,     0,     0,     0,     0,   106,   107,     0,     0,
     100,   101,     0,     0,     0,     0,     0,     0,     0,     0,
     108,     0,     0,     0,     0,     0,     0,     0,   102,   418,
       0,   104,     0,     0,     0,   419,     0,     0,     0,     0,
     109,   110,   106,   107,     0,     0,   175,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   108,     0,     0,   111,
     112,   113,   114,     0,     0,   115,   116,   117,   118,     0,
       0,     0,     0,     0,     0,     0,   109,   420,     0,   100,
     101,     0,     0,     0,   176,   177,     0,     0,   178,   179,
       0,     0,     0,    98,     0,   111,   112,   113,   114,    57,
     180,   421,   116,   117,   118,     0,     0,    58,     0,     0,
       0,   106,     0,    59,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   100,   101,     0,    60,
       0,     0,    61,     0,     0,     0,     0,     0,     0,    63,
       0,     0,     0,    64,     0,   109,    57,   104,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,   106,     0,
      59,   175,     0,   259,   111,     0,   113,   114,     0,     0,
      10,   116,   117,   118,     0,     0,    60,     0,     0,    61,
       0,     0,     0,     0,     0,     0,    63,     0,     0,     0,
      64,   175,   109,     0,   100,   101,   408,     0,     0,   176,
     177,     0,     0,   178,   179,   175,     0,     0,     0,     0,
       0,   111,     0,   113,   114,   180,     0,    10,   116,   117,
     118,     0,     0,     0,   100,   101,   106,     0,     0,   176,
     177,     0,     0,   178,   179,     0,     0,     0,   100,   101,
       0,     0,     0,   176,   177,   180,     0,   178,   179,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,   180,
     109,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,   111,
       0,   113,   114,     0,     0,    10,   116,   117,   118,     0,
     109,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   109,     0,     0,     0,     0,   111,
       0,   113,   114,     0,     0,    10,   116,   117,   118,     0,
       0,     0,     0,   111,     0,   113,   114,     0,     0,    10,
     116,   117,   118
};

static const yytype_int16 yycheck[] =
{
       1,   112,    80,    43,   224,   100,   101,   259,   237,   382,
      48,   224,     4,   354,    69,    39,   273,   443,   104,    36,
     243,    42,    46,    57,     7,    57,     7,    44,     8,     8,
      51,    65,     4,    65,     6,    80,     8,    71,     8,    71,
      77,    13,    43,    13,   470,   268,   269,   125,   103,   128,
      42,    43,   309,    87,    31,    87,    90,    34,    90,    39,
      39,     0,    94,    97,    13,    97,    58,   101,    95,   101,
     411,    80,    17,    11,   447,    20,    21,    69,   128,    80,
     125,   176,   177,   178,   179,    68,    86,    68,   311,   175,
     128,   464,   465,   171,   180,    78,   128,    78,   471,   100,
     101,   474,    11,   104,    80,    22,    23,    24,    25,    26,
      93,   103,    93,     5,   128,    68,   125,   447,     5,    11,
      12,     8,    39,     6,   125,    78,   171,    86,   168,     4,
     122,     6,   124,   125,   464,   465,   128,   129,   130,   131,
      93,   471,    95,     4,   474,     6,   224,     6,     4,   125,
      57,   403,     7,   382,   128,    80,   385,   128,    65,   237,
     383,   384,   171,   376,    71,    11,    12,   168,    46,    47,
     171,     8,   395,   396,   175,   176,   177,   178,   179,   224,
      87,    37,    38,    90,     8,   280,   281,   282,    32,    33,
      97,    68,   237,     4,   101,   171,    40,    41,    48,    49,
     125,    78,    58,    53,     6,     4,     4,     6,     8,     8,
     128,    80,   128,    69,    13,   224,    93,     9,   447,   448,
       9,   128,     9,   224,   444,   285,   286,   287,   237,     4,
     443,   444,    13,   462,     4,   464,   465,     4,    37,    38,
      37,    38,   471,    37,    38,   474,   171,   103,   224,     4,
      22,    23,    24,    25,    26,    45,   125,   470,    37,    38,
     128,   237,   292,   293,   375,     7,   122,    39,   124,   125,
      11,    80,   128,   129,   130,   131,   283,   284,    11,   280,
     281,   282,   283,   284,   285,   286,   287,   288,   289,   290,
     291,   292,   293,   294,   295,   296,   297,   298,   376,   224,
      13,     8,   171,   343,   382,     4,     4,   385,     6,     9,
       8,   128,   237,     9,   409,    13,   125,    22,    23,    24,
      25,    26,     5,     4,    22,    23,    24,    25,    26,     4,
      50,   376,    35,    52,    39,    51,     5,   382,    11,    13,
     385,    39,   343,     8,   288,   289,   290,   291,     4,    11,
      11,   128,   463,   354,    11,   224,     7,    12,    12,   128,
       5,     8,   171,     5,    12,   443,   444,   376,   237,   447,
     448,    11,     8,   382,     5,     5,   385,     5,    12,    12,
       9,     7,     9,     5,   462,     9,   464,   465,     8,    12,
       5,     5,   470,   471,    11,     5,   474,     9,   443,   444,
     376,     8,   447,   448,    45,     9,   382,     5,   409,   385,
     411,     7,   413,    76,    11,   224,    11,   462,     9,   464,
     465,     4,    11,     4,     4,   470,   471,    45,   237,   474,
      76,     5,     7,     5,   443,   444,     4,     7,   447,   448,
       5,     9,    11,   444,     5,     5,     5,    11,     6,    43,
     160,   376,    54,   462,    20,   464,   465,   382,    46,    22,
     385,   470,   471,    76,    65,   474,    98,   443,   444,    37,
      38,   447,   448,   264,    42,    43,   108,   343,    46,    47,
     112,   254,   171,   321,   319,   470,   462,   125,   464,   465,
      58,   107,   444,   309,   470,   471,   297,   336,   474,   294,
     296,    69,   295,   413,    -1,    -1,    -1,   376,    -1,    -1,
     100,   101,   298,   382,    -1,    -1,   385,    -1,   443,   444,
      -1,    -1,   447,   448,    -1,    -1,    -1,    -1,    -1,    -1,
     162,    -1,    -1,    -1,    -1,   103,    -1,   462,    -1,   464,
     465,    -1,   174,   175,    -1,   470,   471,    -1,    -1,   474,
      -1,    -1,    -1,    -1,   122,    -1,   124,   125,    -1,    -1,
     128,   129,   130,   131,    -1,    -1,    -1,   376,    -1,    -1,
      -1,    -1,    -1,   382,   443,   444,   385,    -1,   447,   448,
      -1,    -1,    -1,    -1,    -1,    -1,   176,   177,   178,   179,
      -1,    -1,    -1,   462,    -1,   464,   465,   229,    -1,    -1,
      -1,   470,   471,   235,    -1,   474,   100,   101,    -1,    -1,
      -1,     4,   244,     6,    -1,     8,    -1,    -1,   250,    -1,
      13,    -1,    -1,    -1,    -1,    -1,    -1,   259,    -1,    22,
      23,    24,    25,    26,   443,   444,    -1,    -1,   447,   448,
      -1,    -1,    -1,    -1,    -1,    -1,    39,   279,    -1,    -1,
      -1,    -1,    -1,   462,    -1,   464,   465,    -1,    -1,    -1,
      -1,   470,   471,    -1,    -1,   474,    -1,   299,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   308,    -1,    -1,    -1,
      -1,    -1,   176,   177,   178,   179,    -1,    -1,    -1,   321,
     280,   281,   282,   283,   284,   285,   286,   287,   288,   289,
     290,   291,   292,   293,   294,   295,   296,   297,   298,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     352,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   375,    -1,    -1,   378,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     392,    -1,    -1,    -1,   354,    -1,    -1,    -1,    -1,    -1,
      -1,   403,    -1,    -1,     3,     4,    -1,     6,    -1,    -1,
      -1,    -1,    11,    -1,    -1,    -1,   280,   281,   282,   283,
     284,   285,   286,   287,   288,   289,   290,   291,   292,   293,
     294,   295,   296,   297,   298,    -1,    -1,    -1,    37,    38,
      -1,    -1,    -1,   445,   446,    -1,    -1,    -1,    -1,   409,
      -1,   411,    -1,   413,    -1,    -1,    55,    56,    57,    58,
      -1,   463,    -1,    62,    -1,    -1,    65,    -1,    -1,    -1,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,    87,    -1,
     354,    90,    -1,    -1,    -1,    -1,    -1,    -1,    97,    -1,
       3,     4,   101,     6,   103,   104,    -1,    -1,    11,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   122,   123,   124,   125,    -1,    -1,   128,
     129,   130,   131,    -1,    37,    38,     3,     4,    -1,     6,
      -1,    -1,    -1,    -1,    11,   409,    -1,   411,    -1,   413,
      -1,    -1,    55,    56,    -1,    58,    -1,    -1,    -1,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    69,    70,    -1,    -1,
      37,    38,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      83,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,    56,
      -1,    58,    -1,    -1,    -1,    62,    -1,    -1,    -1,    -1,
     103,   104,    69,    70,    -1,    -1,     4,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,   122,
     123,   124,   125,    -1,    -1,   128,   129,   130,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,    37,
      38,    -1,    -1,    -1,    42,    43,    -1,    -1,    46,    47,
      -1,    -1,    -1,     4,    -1,   122,   123,   124,   125,    57,
      58,   128,   129,   130,   131,    -1,    -1,    65,    -1,    -1,
      -1,    69,    -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    87,
      -1,    -1,    90,    -1,    -1,    -1,    -1,    -1,    -1,    97,
      -1,    -1,    -1,   101,    -1,   103,    57,    58,    -1,    -1,
      -1,    -1,    -1,    -1,    65,    -1,    -1,    -1,    69,    -1,
      71,     4,    -1,     6,   122,    -1,   124,   125,    -1,    -1,
     128,   129,   130,   131,    -1,    -1,    87,    -1,    -1,    90,
      -1,    -1,    -1,    -1,    -1,    -1,    97,    -1,    -1,    -1,
     101,     4,   103,    -1,    37,    38,     9,    -1,    -1,    42,
      43,    -1,    -1,    46,    47,     4,    -1,    -1,    -1,    -1,
      -1,   122,    -1,   124,   125,    58,    -1,   128,   129,   130,
     131,    -1,    -1,    -1,    37,    38,    69,    -1,    -1,    42,
      43,    -1,    -1,    46,    47,    -1,    -1,    -1,    37,    38,
      -1,    -1,    -1,    42,    43,    58,    -1,    46,    47,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    69,    -1,    -1,    58,
     103,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,
      -1,   124,   125,    -1,    -1,   128,   129,   130,   131,    -1,
     103,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,    -1,   122,
      -1,   124,   125,    -1,    -1,   128,   129,   130,   131,    -1,
      -1,    -1,    -1,   122,    -1,   124,   125,    -1,    -1,   128,
     129,   130,   131
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,    77,   133,   145,   146,   147,   150,   152,   153,   154,
     128,   142,   143,   144,     0,    68,    78,    93,   148,   149,
     151,   155,   156,   157,   158,   161,   162,   152,    13,   155,
     157,    95,    48,   128,   128,    11,    11,   159,   160,     6,
     163,    86,    93,   161,   164,   165,   166,   167,   168,   169,
     175,   176,   186,   187,   128,     6,   203,    57,    65,    71,
      87,    90,    94,    97,   101,   135,   136,   137,   138,   139,
     140,   141,   142,   143,   188,     7,   167,   185,   203,   163,
     204,   128,   180,   181,   128,   170,   171,   172,   173,   180,
       8,     8,     8,    13,     4,     6,   195,     3,     4,    11,
      37,    38,    55,    56,    58,    62,    69,    70,    83,   103,
     104,   122,   123,   124,   125,   128,   129,   130,   131,   134,
     135,   142,   191,   192,   203,   205,   206,   207,   208,   213,
     214,   216,   217,   218,   220,   221,   222,   225,   226,   228,
     236,   243,   244,   245,   246,   247,   251,   255,   256,   257,
     258,   259,   260,   262,   263,   289,   291,   182,     8,    11,
      12,     8,    39,     9,     9,     9,   128,   177,   178,    69,
     103,   189,   190,   196,     4,     4,    42,    43,    46,    47,
      58,   142,   256,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   293,   142,   255,   257,   261,
     261,   128,   241,   242,     4,   136,   142,   223,   224,     4,
     241,   232,   233,   293,    13,     4,   232,    45,   128,   209,
     210,   212,     4,     6,     8,     7,   206,    11,    11,    13,
       8,    37,    38,   290,     4,     9,   128,   171,     9,     6,
     174,   201,   293,     5,   135,   179,   183,   184,     4,     4,
     191,   293,   136,   142,   293,   261,   261,   261,   261,     8,
      48,    49,    53,    46,    47,    17,    20,    21,    32,    33,
      40,    41,    31,    34,    50,    52,    51,    35,    36,    44,
       5,    11,   208,   221,   230,   231,   238,   240,     8,   252,
     253,     4,   293,    11,    11,   128,   293,    11,   213,    12,
       8,    39,   193,   194,   248,   293,   193,   293,   128,   293,
      22,    23,    24,    25,    26,    39,   292,   177,   174,   197,
     198,   202,   172,    12,   193,   193,     7,     5,     8,   249,
     250,   254,     8,   254,     5,   261,   261,   261,   266,   266,
     268,   268,   268,   270,   270,   270,   270,   272,   272,   274,
     276,   278,   280,   282,   293,    11,    12,   293,     8,   253,
     254,   193,     5,     4,     6,     5,   210,     9,   201,   211,
     293,     5,    12,     7,     9,     4,     6,     9,   288,     5,
      12,   199,   200,    12,   184,     5,     5,    11,     9,     5,
       8,     5,   264,    45,   232,   221,     9,     5,    56,    62,
     104,   128,   213,   215,   216,   219,   227,   229,   237,   193,
     193,   213,   293,   193,   193,     7,   174,    11,    11,   261,
       9,   264,   286,    11,     4,     4,     4,    45,    76,     5,
       7,     5,     7,   234,   235,   239,   240,   230,   293,   293,
     215,   213,     5,    11,     5,     5,   213,   232,   215,   215,
      11,    76,   234,   215,     5,   215
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int16 yyr1[] =
{
       0,   132,   133,   134,   134,   134,   134,   134,   134,   135,
     135,   136,   136,   136,   137,   137,   138,   138,   138,   139,
     139,   140,   141,   141,   141,   142,   142,   143,   144,   145,
     146,   147,   147,   148,   149,   149,   150,   150,   151,   151,
     152,   152,   153,   154,   155,   156,   156,   157,   157,   157,
     159,   158,   160,   158,   161,   162,   162,   163,   164,   165,
     165,   166,   166,   167,   167,   167,   168,   168,   169,   170,
     170,   171,   171,   172,   173,   173,   174,   174,   175,   176,
     176,   178,   177,   179,   179,   180,   182,   181,   181,   183,
     183,   184,   185,   186,   187,   188,   189,   190,   190,   191,
     192,   192,   193,   194,   194,   195,   196,   196,   197,   198,
     198,   199,   200,   200,   201,   202,   202,   204,   203,   205,
     205,   206,   206,   207,   208,   209,   209,   210,   210,   211,
     211,   212,   212,   213,   213,   213,   213,   213,   213,   213,
     214,   215,   215,   215,   215,   215,   216,   216,   216,   216,
     216,   216,   217,   218,   219,   220,   221,   221,   221,   221,
     221,   221,   221,   222,   223,   224,   225,   226,   227,   228,
     229,   230,   231,   231,   232,   233,   233,   234,   235,   235,
     236,   237,   238,   238,   239,   240,   240,   241,   242,   242,
     243,   244,   245,   245,   246,   246,   247,   247,   247,   247,
     247,   247,   248,   248,   249,   250,   250,   251,   251,   252,
     252,   253,   254,   254,   255,   255,   256,   256,   256,   256,
     256,   256,   257,   257,   258,   258,   258,   258,   259,   260,
     261,   261,   261,   261,   261,   262,   263,   264,   264,   264,
     264,   265,   265,   265,   266,   267,   267,   267,   267,   268,
     269,   269,   269,   270,   271,   271,   271,   271,   272,   273,
     273,   273,   273,   273,   274,   275,   275,   275,   276,   277,
     277,   278,   279,   279,   280,   281,   281,   282,   283,   283,
     284,   285,   285,   286,   287,   287,   288,   288,   290,   289,
     291,   291,   291,   292,   292,   292,   292,   292,   292,   293
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     3,     3,     3,     1,     1,     1,     3,     2,
       1,     1,     0,     1,     1,     0,     1,     2,     1,     2,
       1,     1,     5,     5,     1,     1,     2,     1,     1,     1,
       0,     5,     0,     7,     1,     1,     0,     3,     1,     1,
       0,     1,     2,     1,     1,     1,     1,     1,     4,     1,
       3,     1,     3,     1,     1,     3,     1,     1,     2,     3,
       3,     0,     2,     1,     0,     1,     0,     5,     3,     1,
       3,     2,     1,     2,     3,     4,     1,     1,     0,     1,
       1,     0,     1,     1,     0,     4,     5,     5,     1,     1,
       0,     1,     1,     0,     4,     1,     3,     0,     4,     1,
       2,     1,     1,     2,     2,     1,     3,     1,     3,     1,
       1,     1,     3,     1,     1,     1,     1,     1,     1,     1,
       5,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     3,     3,     2,     1,     1,     1,     1,
       1,     1,     1,     5,     1,     1,     5,     7,     7,     5,
       5,     1,     1,     0,     1,     1,     0,     1,     1,     0,
       9,     9,     1,     1,     1,     1,     3,     1,     1,     0,
       3,     3,     3,     3,     1,     1,     1,     1,     3,     1,
       1,     1,     1,     3,     1,     1,     0,     3,     4,     1,
       2,     3,     2,     3,     3,     3,     4,     6,     6,     4,
       6,     6,     4,     4,     1,     1,     1,     1,     2,     2,
       1,     1,     2,     2,     1,     2,     2,     1,     2,     2,
       1,     5,     4,     5,     1,     1,     3,     3,     3,     1,
       1,     3,     3,     1,     1,     3,     3,     3,     1,     1,
       3,     3,     3,     3,     1,     1,     3,     3,     1,     1,
       3,     1,     1,     3,     1,     1,     3,     1,     1,     3,
       1,     1,     3,     1,     1,     5,     1,     1,     0,     4,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 3: /* Literal: INTLITERAL  */
#line 70 "java.y"
                              {(yyval.nd)="int";}
#line 2286 "y.tab.c"
    break;

  case 4: /* Literal: FLOATLITERAL  */
#line 71 "java.y"
                                {(yyval.nd) = "float";}
#line 2292 "y.tab.c"
    break;

  case 5: /* Literal: BOOLEANLITERAL  */
#line 72 "java.y"
                                 {(yyval.nd)="boolean";}
#line 2298 "y.tab.c"
    break;

  case 6: /* Literal: STRINGLITERAL  */
#line 73 "java.y"
                                {(yyval.nd)="string";}
#line 2304 "y.tab.c"
    break;

  case 7: /* Literal: CHARLITERAL  */
#line 74 "java.y"
                                {(yyval.nd)="char";}
#line 2310 "y.tab.c"
    break;

  case 8: /* Literal: NULLLITERAL  */
#line 75 "java.y"
                                {(yyval.nd)="Null";}
#line 2316 "y.tab.c"
    break;

  case 12: /* PrimitiveType: BOOLEAN  */
#line 83 "java.y"
                               {(yyval.nd) = (yyvsp[0].id);type = (yyvsp[0].id);}
#line 2322 "y.tab.c"
    break;

  case 13: /* PrimitiveType: STRING  */
#line 84 "java.y"
                                {(yyval.nd) = (yyvsp[0].id);type = (yyvsp[0].id);}
#line 2328 "y.tab.c"
    break;

  case 16: /* IntegralType: INT  */
#line 91 "java.y"
                                {(yyval.nd) = (yyvsp[0].id);type = (yyvsp[0].id);}
#line 2334 "y.tab.c"
    break;

  case 17: /* IntegralType: LONG  */
#line 92 "java.y"
                                {(yyval.nd) = (yyvsp[0].id);type = (yyvsp[0].id);}
#line 2340 "y.tab.c"
    break;

  case 18: /* IntegralType: CHAR  */
#line 93 "java.y"
                                {(yyval.nd) = (yyvsp[0].id);type = (yyvsp[0].id);}
#line 2346 "y.tab.c"
    break;

  case 19: /* FloatingPointType: FLOAT  */
#line 96 "java.y"
                                {(yyval.nd) = (yyvsp[0].id);type = (yyvsp[0].id);}
#line 2352 "y.tab.c"
    break;

  case 20: /* FloatingPointType: DOUBLE  */
#line 97 "java.y"
                                {(yyval.nd) = (yyvsp[0].id);type = (yyvsp[0].id);}
#line 2358 "y.tab.c"
    break;

  case 22: /* ArrayType: PrimitiveType LEFTSQUAREBRACKET RIGHTSQUAREBRACKET  */
#line 103 "java.y"
                                                            {type =(yyvsp[-2].nd);}
#line 2364 "y.tab.c"
    break;

  case 23: /* ArrayType: Name LEFTSQUAREBRACKET RIGHTSQUAREBRACKET  */
#line 104 "java.y"
                                                     {insert_entry((yyvsp[-2].nd),"array",type,localClassId, localFuncId ,yylineno,scope);}
#line 2370 "y.tab.c"
    break;

  case 25: /* Name: SimpleName  */
#line 108 "java.y"
                      {(yyval.nd)=(yyvsp[0].nd);}
#line 2376 "y.tab.c"
    break;

  case 26: /* Name: QualifiedName  */
#line 109 "java.y"
                        {(yyval.nd)=(yyvsp[0].nd);}
#line 2382 "y.tab.c"
    break;

  case 27: /* SimpleName: IDENTIFIER  */
#line 112 "java.y"
                     {(yyval.nd)=(yyvsp[0].id);}
#line 2388 "y.tab.c"
    break;

  case 28: /* QualifiedName: Name DOT IDENTIFIER  */
#line 115 "java.y"
                            { (yyval.nd)= strConcat((yyvsp[-2].nd),strConcat((yyvsp[-1]. id),(yyvsp[0].id)));}
#line 2394 "y.tab.c"
    break;

  case 42: /* SingleTypeImportDeclaration: IMPORT Name DOT IDENTIFIER SEMICOLON  */
#line 150 "java.y"
                                               {char * temp= strConcat((yyvsp[-4].id),strConcat((yyvsp[-3].nd),strConcat((yyvsp[-2]. id),(yyvsp[-1].id))));insert_entry(temp,"headerfile","N/A","N/A","N/A",yylineno,scope);}
#line 2400 "y.tab.c"
    break;

  case 43: /* TypeImportOnDemandDeclaration: IMPORT Name DOT MUL SEMICOLON  */
#line 153 "java.y"
                                         {char * temp= strConcat((yyvsp[-4].id),strConcat((yyvsp[-3].nd),strConcat((yyvsp[-2]. id),(yyvsp[-1]. id))));insert_entry(temp,"headerfile","N/A","N/A","N/A",yylineno,scope);}
#line 2406 "y.tab.c"
    break;

  case 50: /* $@1: %empty  */
#line 168 "java.y"
                                      {localClassId=(yyvsp[0].id);insert_entry((yyvsp[0].id),"class","N/A",localClassId,localFuncId,yylineno,scope);scope++;}
#line 2412 "y.tab.c"
    break;

  case 51: /* ClassDeclaration: ModifiersOpt CLASS IDENTIFIER $@1 ClassBody  */
#line 168 "java.y"
                                                                                                                                                  { scope--;}
#line 2418 "y.tab.c"
    break;

  case 52: /* $@2: %empty  */
#line 169 "java.y"
                                      {localClassId=(yyvsp[0].id);insert_entry((yyvsp[0].id),"class","N/A",localClassId,localFuncId,yylineno,scope);scope++;}
#line 2424 "y.tab.c"
    break;

  case 53: /* ClassDeclaration: ModifiersOpt CLASS IDENTIFIER $@2 EXTENDS IDENTIFIER ClassBody  */
#line 169 "java.y"
                                                                                                                                                                        {scope--;}
#line 2430 "y.tab.c"
    break;

  case 68: /* FieldDeclaration: ModifiersOpt Type VarDeclarators SEMICOLON  */
#line 205 "java.y"
                                                      {type = (yyvsp[-2].nd);}
#line 2436 "y.tab.c"
    break;

  case 74: /* VarDeclaratorIds: IDENTIFIER  */
#line 219 "java.y"
                        {insert_entry((yyvsp[0].id),"variable",type,localClassId,localFuncId,yylineno,scope);}
#line 2442 "y.tab.c"
    break;

  case 79: /* MethodHeader: ModifiersOpt Type MethodDeclarator  */
#line 230 "java.y"
                                              {type=(yyvsp[-1].nd);}
#line 2448 "y.tab.c"
    break;

  case 80: /* MethodHeader: ModifiersOpt VOID MethodDeclarator  */
#line 231 "java.y"
                                                   {type="void";}
#line 2454 "y.tab.c"
    break;

  case 81: /* $@3: %empty  */
#line 234 "java.y"
                        {scope++;}
#line 2460 "y.tab.c"
    break;

  case 82: /* FormalParameterListOpt: $@3 FormalParameterListOpts  */
#line 234 "java.y"
                                                           {scope--;}
#line 2466 "y.tab.c"
    break;

  case 86: /* $@4: %empty  */
#line 245 "java.y"
                   {localFuncId=(yyvsp[0].id);}
#line 2472 "y.tab.c"
    break;

  case 87: /* MethodDeclarators: IDENTIFIER $@4 LEFTBRACKET FormalParameterListOpt RIGHTBRACKET  */
#line 245 "java.y"
                                                                                            { insert_entry((yyvsp[-4].id),"function",type,localClassId,localFuncId,yylineno,scope);}
#line 2478 "y.tab.c"
    break;

  case 91: /* FormalParameter: Type VarDeclaratorId  */
#line 253 "java.y"
                               {type=(yyvsp[-1].nd);}
#line 2484 "y.tab.c"
    break;

  case 95: /* ConstructorDeclarator: SimpleName LEFTBRACKET FormalParameterListOpt RIGHTBRACKET  */
#line 265 "java.y"
                                                                       {localFuncId = (yyvsp[-3].nd) ;insert_entry((yyvsp[-3].nd),"function","N/A",localClassId,localFuncId,yylineno,scope);}
#line 2490 "y.tab.c"
    break;

  case 117: /* $@5: %empty  */
#line 323 "java.y"
                         {scope++;}
#line 2496 "y.tab.c"
    break;

  case 118: /* Block: LEFTCURLYBRACKET $@5 BlockStatementOpt RIGHTCURLYBRACKET  */
#line 323 "java.y"
                                                                           {scope--;}
#line 2502 "y.tab.c"
    break;

  case 124: /* LocalVariableDeclaration: Type VariableDeclarators  */
#line 336 "java.y"
                                                   {type = (yyvsp[-1].nd);}
#line 2508 "y.tab.c"
    break;

  case 128: /* VariableDeclarator: VariableDeclaratorId EQUAL VariableInitializer  */
#line 343 "java.y"
                                                                 {if(!typeCheck((yyvsp[-2].nd),(yyvsp[0].nd))){cout<<"Type Mismatch:"<<yylineno<<endl; exit(2);}}
#line 2514 "y.tab.c"
    break;

  case 129: /* VariableInitializer: Expression  */
#line 346 "java.y"
                                {(yyval.nd)=(yyvsp[0].nd);}
#line 2520 "y.tab.c"
    break;

  case 131: /* VariableDeclaratorId: IDENTIFIER  */
#line 350 "java.y"
                                 {insert_entry((yyvsp[0].id),"variable",type,localClassId,localFuncId,yylineno,scope);(yyval.nd)=type;}
#line 2526 "y.tab.c"
    break;

  case 132: /* VariableDeclaratorId: VariableDeclaratorId LEFTSQUAREBRACKET RIGHTSQUAREBRACKET  */
#line 351 "java.y"
                                                                            {(yyval.nd)=type;}
#line 2532 "y.tab.c"
    break;

  case 151: /* StatementWithoutTrailingSubstatement: ReturnStatement  */
#line 380 "java.y"
                               {char * ch = getReturnType(localClassId,localFuncId);if(!typeCheck((yyvsp[0].nd),ch)){cout<<"Return type mismatch"<<yylineno<<endl;exit(56);}}
#line 2538 "y.tab.c"
    break;

  case 174: /* ExpressionOpt: ExpressionOpts  */
#line 439 "java.y"
                              {(yyval.nd)=(yyvsp[0].nd);}
#line 2544 "y.tab.c"
    break;

  case 175: /* ExpressionOpts: Expression  */
#line 442 "java.y"
                      {(yyval.nd)=(yyvsp[0].nd);}
#line 2550 "y.tab.c"
    break;

  case 188: /* IDENTOpts: IDENTIFIER  */
#line 481 "java.y"
                        {insert_entry((yyvsp[0].id),"variable",type,localClassId,"N/A",yylineno,scope);}
#line 2556 "y.tab.c"
    break;

  case 192: /* ReturnStatement: RETURN ExpressionOpt SEMICOLON  */
#line 495 "java.y"
                                          {(yyval.nd)=(yyvsp[-1].nd);}
#line 2562 "y.tab.c"
    break;

  case 194: /* Primary: PrimaryNoNewArray  */
#line 499 "java.y"
                             {(yyval.nd)=(yyvsp[0].nd);}
#line 2568 "y.tab.c"
    break;

  case 196: /* PrimaryNoNewArray: Literal  */
#line 503 "java.y"
                  {(yyval.nd)=(yyvsp[0].nd);}
#line 2574 "y.tab.c"
    break;

  case 199: /* PrimaryNoNewArray: FieldAccess  */
#line 506 "java.y"
                       {(yyval.nd)=(yyvsp[0].nd);}
#line 2580 "y.tab.c"
    break;

  case 200: /* PrimaryNoNewArray: MethodInvocation  */
#line 507 "java.y"
                           {(yyval.nd)=(yyvsp[0].nd);}
#line 2586 "y.tab.c"
    break;

  case 201: /* PrimaryNoNewArray: ArrayAccess  */
#line 508 "java.y"
                       {(yyval.nd)=(yyvsp[0].nd);}
#line 2592 "y.tab.c"
    break;

  case 215: /* FieldAccess: SUPER DOT IDENTIFIER  */
#line 539 "java.y"
                                   {char *temp = strConcat((yyvsp[-2].id),strConcat((yyvsp[-1]. id),(yyvsp[0].id)));insert_entry(temp,"fieldaccess","N/A",localClassId,"N/A",yylineno,scope); (yyval.nd) = temp;}
#line 2598 "y.tab.c"
    break;

  case 216: /* MethodInvocation: Name LEFTBRACKET ArgumentListOpt RIGHTBRACKET  */
#line 542 "java.y"
                                                           {(yyval.nd) = getType((yyvsp[-3].nd),localClassId,localFuncId,scope);}
#line 2604 "y.tab.c"
    break;

  case 217: /* MethodInvocation: Primary DOT IDENTIFIER LEFTBRACKET ArgumentListOpt RIGHTBRACKET  */
#line 543 "java.y"
                                                                                 {(yyval.nd) = getType((yyvsp[-3].id),localClassId,localFuncId,scope);}
#line 2610 "y.tab.c"
    break;

  case 218: /* MethodInvocation: SUPER DOT IDENTIFIER LEFTBRACKET ArgumentListOpt RIGHTBRACKET  */
#line 544 "java.y"
                                                                              {(yyval.nd) = getType((yyvsp[-3].id),localClassId,localFuncId,scope);}
#line 2616 "y.tab.c"
    break;

  case 219: /* MethodInvocation: Name LEFTCURLYBRACKET ArgumentListOpt RIGHTCURLYBRACKET  */
#line 545 "java.y"
                                                                         {(yyval.nd) = getType((yyvsp[-3].nd),localClassId,localFuncId,scope);}
#line 2622 "y.tab.c"
    break;

  case 220: /* MethodInvocation: Primary DOT IDENTIFIER LEFTCURLYBRACKET ArgumentListOpt RIGHTCURLYBRACKET  */
#line 546 "java.y"
                                                                                        {(yyval.nd) = getType((yyvsp[-3].id),localClassId,localFuncId,scope);}
#line 2628 "y.tab.c"
    break;

  case 221: /* MethodInvocation: SUPER DOT IDENTIFIER LEFTCURLYBRACKET ArgumentListOpt RIGHTCURLYBRACKET  */
#line 547 "java.y"
                                                                                        {(yyval.nd) = getType((yyvsp[-3].id),localClassId,localFuncId,scope);}
#line 2634 "y.tab.c"
    break;

  case 222: /* ArrayAccess: Name LEFTSQUAREBRACKET Expression RIGHTSQUAREBRACKET  */
#line 550 "java.y"
                                                                {(yyval.nd) = getType((yyvsp[-3].nd),localClassId,localFuncId,scope);}
#line 2640 "y.tab.c"
    break;

  case 224: /* PostFixExpression: Primary  */
#line 555 "java.y"
                     {(yyval.nd)=(yyvsp[0].nd);}
#line 2646 "y.tab.c"
    break;

  case 225: /* PostFixExpression: Name  */
#line 556 "java.y"
                      {(yyval.nd) = getType((yyvsp[0].nd),localClassId,localFuncId,scope);}
#line 2652 "y.tab.c"
    break;

  case 226: /* PostFixExpression: PostIncrementExpression  */
#line 557 "java.y"
                                      {(yyval.nd)=(yyvsp[0].nd);}
#line 2658 "y.tab.c"
    break;

  case 227: /* PostFixExpression: PostDecrementExpression  */
#line 558 "java.y"
                                        {(yyval.nd)=(yyvsp[0].nd);}
#line 2664 "y.tab.c"
    break;

  case 228: /* PostIncrementExpression: PostFixExpression ADDADD  */
#line 562 "java.y"
                                    {if(typeCheck((yyvsp[-1].nd),"int")){(yyval.nd)=(yyvsp[-1].nd);} else {cout<<"Type Mismatch"<<yylineno<<endl;exit(3);}}
#line 2670 "y.tab.c"
    break;

  case 229: /* PostDecrementExpression: PostFixExpression SUBSUB  */
#line 566 "java.y"
                                     {if(typeCheck((yyvsp[-1].nd),"int"))(yyval.nd)=(yyvsp[-1].nd); else {cout<<"Type Mismatch"<<yylineno<<endl;exit(4);}}
#line 2676 "y.tab.c"
    break;

  case 230: /* UnaryExpression: PreIncrementExpression  */
#line 570 "java.y"
                                  {(yyval.nd)=(yyvsp[0].nd);}
#line 2682 "y.tab.c"
    break;

  case 231: /* UnaryExpression: PreDecrementExpression  */
#line 571 "java.y"
                                     {(yyval.nd)=(yyvsp[0].nd);}
#line 2688 "y.tab.c"
    break;

  case 232: /* UnaryExpression: ADD UnaryExpression  */
#line 572 "java.y"
                                    {if(typeCheck((yyvsp[-1]. id),"int"))(yyval.nd)=(yyvsp[0].nd); else {cout<<"Type Mismatch"<<yylineno<<endl;exit(5);}}
#line 2694 "y.tab.c"
    break;

  case 233: /* UnaryExpression: SUB UnaryExpression  */
#line 573 "java.y"
                                    {if(typeCheck((yyvsp[-1]. id),"int"))(yyval.nd)=(yyvsp[0].nd); else {cout<<"Type Mismatch"<<yylineno<<endl;exit(6);}}
#line 2700 "y.tab.c"
    break;

  case 234: /* UnaryExpression: UnaryExpressionNotPlusMinus  */
#line 574 "java.y"
                                            {(yyval.nd)=(yyvsp[0].nd);}
#line 2706 "y.tab.c"
    break;

  case 235: /* PreIncrementExpression: ADDADD UnaryExpression  */
#line 578 "java.y"
                                  {if(typeCheck((yyvsp[-1]. id),"int"))(yyval.nd)=(yyvsp[0].nd); else {cout<<"Type Mismatch"<<yylineno<<endl;exit(7);}}
#line 2712 "y.tab.c"
    break;

  case 236: /* PreDecrementExpression: SUBSUB UnaryExpression  */
#line 582 "java.y"
                                  {if(typeCheck((yyvsp[-1]. id),"int"))(yyval.nd)=(yyvsp[0].nd); else {cout<<"Type Mismatch"<<yylineno<<endl;exit(8);}}
#line 2718 "y.tab.c"
    break;

  case 237: /* UnaryExpressionNotPlusMinus: PostFixExpression  */
#line 586 "java.y"
                               {(yyval.nd)=(yyvsp[0].nd);}
#line 2724 "y.tab.c"
    break;

  case 238: /* UnaryExpressionNotPlusMinus: TILDE UnaryExpression  */
#line 587 "java.y"
                                      {(yyval.nd)=(yyvsp[0].nd);}
#line 2730 "y.tab.c"
    break;

  case 239: /* UnaryExpressionNotPlusMinus: NOT UnaryExpression  */
#line 588 "java.y"
                                     {if(typeCheck((yyvsp[-1]. id),"boolean"))(yyval.nd)="boolean"; else {cout<<"Type Mismatch"<<yylineno<<endl;exit(9);}}
#line 2736 "y.tab.c"
    break;

  case 240: /* UnaryExpressionNotPlusMinus: CastExpression  */
#line 589 "java.y"
                                {(yyval.nd)="int";}
#line 2742 "y.tab.c"
    break;

  case 244: /* MultiplicativeExpression: MultiplicativeExpressions  */
#line 598 "java.y"
                                                     {(yyval.nd)=(yyvsp[0].nd);}
#line 2748 "y.tab.c"
    break;

  case 245: /* MultiplicativeExpressions: UnaryExpression  */
#line 602 "java.y"
                            {(yyval.nd)=(yyvsp[0].nd);}
#line 2754 "y.tab.c"
    break;

  case 246: /* MultiplicativeExpressions: MultiplicativeExpression MUL UnaryExpression  */
#line 603 "java.y"
                                                             {if(typeCheck((yyvsp[-2].nd),(yyvsp[0].nd))){(yyval.nd)=(yyvsp[-2].nd);}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(10);}}
#line 2760 "y.tab.c"
    break;

  case 247: /* MultiplicativeExpressions: MultiplicativeExpression DIV UnaryExpression  */
#line 604 "java.y"
                                                             {if(typeCheck((yyvsp[-2].nd),(yyvsp[0].nd))){(yyval.nd)=(yyvsp[-2].nd);}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(11);}}
#line 2766 "y.tab.c"
    break;

  case 248: /* MultiplicativeExpressions: MultiplicativeExpression MOD UnaryExpression  */
#line 605 "java.y"
                                                              {if(typeCheck((yyvsp[-2].nd),(yyvsp[0].nd))){(yyval.nd)=(yyvsp[-2].nd);}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(12);}}
#line 2772 "y.tab.c"
    break;

  case 249: /* AdditiveExpression: AdditiveExpressions  */
#line 608 "java.y"
                                       {(yyval.nd)=(yyvsp[0].nd);}
#line 2778 "y.tab.c"
    break;

  case 250: /* AdditiveExpressions: MultiplicativeExpression  */
#line 612 "java.y"
                                  {(yyval.nd)=(yyvsp[0].nd);}
#line 2784 "y.tab.c"
    break;

  case 251: /* AdditiveExpressions: AdditiveExpression ADD MultiplicativeExpression  */
#line 613 "java.y"
                                                               {if(typeCheck((yyvsp[-2].nd),(yyvsp[0].nd))){(yyval.nd)=(yyvsp[-2].nd);}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(13);}}
#line 2790 "y.tab.c"
    break;

  case 252: /* AdditiveExpressions: AdditiveExpression SUB MultiplicativeExpression  */
#line 614 "java.y"
                                                               {if(typeCheck((yyvsp[-2].nd),(yyvsp[0].nd))){(yyval.nd)=(yyvsp[-2].nd);}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(14);}}
#line 2796 "y.tab.c"
    break;

  case 253: /* ShiftExpression: ShiftExpressions  */
#line 617 "java.y"
                                 {(yyval.nd)=(yyvsp[0].nd);}
#line 2802 "y.tab.c"
    break;

  case 254: /* ShiftExpressions: AdditiveExpression  */
#line 621 "java.y"
                              {(yyval.nd)=(yyvsp[0].nd);}
#line 2808 "y.tab.c"
    break;

  case 255: /* ShiftExpressions: ShiftExpression DOUBLELESS AdditiveExpression  */
#line 622 "java.y"
                                                              {if(typeCheck((yyvsp[-2].nd),(yyvsp[0].nd))){(yyval.nd)=(yyvsp[-2].nd);}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(15);}}
#line 2814 "y.tab.c"
    break;

  case 256: /* ShiftExpressions: ShiftExpression DOUBLEGREATER AdditiveExpression  */
#line 623 "java.y"
                                                                {if(typeCheck((yyvsp[-2].nd),(yyvsp[0].nd))){(yyval.nd)=(yyvsp[-2].nd);}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(16);}}
#line 2820 "y.tab.c"
    break;

  case 257: /* ShiftExpressions: ShiftExpression TRIPLEGREATER AdditiveExpression  */
#line 624 "java.y"
                                                                 {if(typeCheck((yyvsp[-2].nd),(yyvsp[0].nd))){(yyval.nd)=(yyvsp[-2].nd);}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(17);}}
#line 2826 "y.tab.c"
    break;

  case 258: /* RelationalExpression: RelationalExpressions  */
#line 627 "java.y"
                                           {(yyval.nd)=(yyvsp[0].nd);}
#line 2832 "y.tab.c"
    break;

  case 259: /* RelationalExpressions: ShiftExpression  */
#line 631 "java.y"
                            {(yyval.nd)=(yyvsp[0].nd);}
#line 2838 "y.tab.c"
    break;

  case 260: /* RelationalExpressions: RelationalExpression LESS ShiftExpression  */
#line 632 "java.y"
                                                             {if(typeCheck((yyvsp[-2].nd),(yyvsp[0].nd))){(yyval.nd)="boolean";}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(18);}}
#line 2844 "y.tab.c"
    break;

  case 261: /* RelationalExpressions: RelationalExpression GREATER ShiftExpression  */
#line 633 "java.y"
                                                              {if(typeCheck((yyvsp[-2].nd),(yyvsp[0].nd))){(yyval.nd)="boolean";}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(19);}}
#line 2850 "y.tab.c"
    break;

  case 262: /* RelationalExpressions: RelationalExpression LESSEQUAL ShiftExpression  */
#line 634 "java.y"
                                                               {if(typeCheck((yyvsp[-2].nd),(yyvsp[0].nd))){(yyval.nd)="boolean";}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(20);}}
#line 2856 "y.tab.c"
    break;

  case 263: /* RelationalExpressions: RelationalExpression GREATEREQUAL ShiftExpression  */
#line 635 "java.y"
                                                                 {if(typeCheck((yyvsp[-2].nd),(yyvsp[0].nd))){(yyval.nd)="boolean";}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(21);}}
#line 2862 "y.tab.c"
    break;

  case 264: /* EqualityExpression: EqualityExpressions  */
#line 638 "java.y"
                                       {(yyval.nd)=(yyvsp[0].nd);}
#line 2868 "y.tab.c"
    break;

  case 265: /* EqualityExpressions: RelationalExpression  */
#line 642 "java.y"
                               {(yyval.nd)=(yyvsp[0].nd);}
#line 2874 "y.tab.c"
    break;

  case 266: /* EqualityExpressions: EqualityExpression EQUALEQUAL RelationalExpression  */
#line 643 "java.y"
                                                                   {if(typeCheck((yyvsp[-2].nd),(yyvsp[0].nd))){(yyval.nd)="boolean";}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(22);}}
#line 2880 "y.tab.c"
    break;

  case 267: /* EqualityExpressions: EqualityExpression NOTEQUAL RelationalExpression  */
#line 644 "java.y"
                                                                   {if(typeCheck((yyvsp[-2].nd),(yyvsp[0].nd))){(yyval.nd)="boolean";}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(23);}}
#line 2886 "y.tab.c"
    break;

  case 268: /* AndExpression: AndExpressions  */
#line 646 "java.y"
                                {(yyval.nd)=(yyvsp[0].nd);}
#line 2892 "y.tab.c"
    break;

  case 269: /* AndExpressions: EqualityExpression  */
#line 649 "java.y"
                               {(yyval.nd)=(yyvsp[0].nd);}
#line 2898 "y.tab.c"
    break;

  case 270: /* AndExpressions: AndExpression ANDBIT EqualityExpression  */
#line 650 "java.y"
                                                           {if(typeCheck((yyvsp[-2].nd),(yyvsp[0].nd))){(yyval.nd)="boolean";}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(24);}}
#line 2904 "y.tab.c"
    break;

  case 271: /* ExclusiveOrExpression: ExclusiveOrExpressions  */
#line 653 "java.y"
                                             {(yyval.nd)=(yyvsp[0].nd);}
#line 2910 "y.tab.c"
    break;

  case 272: /* ExclusiveOrExpressions: AndExpression  */
#line 656 "java.y"
                          {(yyval.nd)=(yyvsp[0].nd);}
#line 2916 "y.tab.c"
    break;

  case 273: /* ExclusiveOrExpressions: ExclusiveOrExpression POWER AndExpression  */
#line 657 "java.y"
                                                             {if(typeCheck((yyvsp[-2].nd),(yyvsp[0].nd))){(yyval.nd)=(yyvsp[-2].nd);}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(25);}}
#line 2922 "y.tab.c"
    break;

  case 274: /* InclusiveOrExpression: InclusiveOrExpressions  */
#line 660 "java.y"
                                              {(yyval.nd)=(yyvsp[0].nd);}
#line 2928 "y.tab.c"
    break;

  case 275: /* InclusiveOrExpressions: ExclusiveOrExpression  */
#line 664 "java.y"
                              {(yyval.nd)=(yyvsp[0].nd);}
#line 2934 "y.tab.c"
    break;

  case 276: /* InclusiveOrExpressions: InclusiveOrExpression ORBIT ExclusiveOrExpression  */
#line 665 "java.y"
                                                                 {if(typeCheck((yyvsp[-2].nd),(yyvsp[0].nd))){(yyval.nd)=(yyvsp[-2].nd);}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(26);}}
#line 2940 "y.tab.c"
    break;

  case 277: /* ConditionalAndExpression: ConditionalAndExpressions  */
#line 668 "java.y"
                                                    {(yyval.nd)=(yyvsp[0].nd);}
#line 2946 "y.tab.c"
    break;

  case 278: /* ConditionalAndExpressions: InclusiveOrExpression  */
#line 672 "java.y"
                                  {(yyval.nd)=(yyvsp[0].nd);}
#line 2952 "y.tab.c"
    break;

  case 279: /* ConditionalAndExpressions: ConditionalAndExpression AND InclusiveOrExpression  */
#line 673 "java.y"
                                                                   {if(typeCheck((yyvsp[-2].nd),(yyvsp[0].nd))){(yyval.nd)="boolean";}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(27);}}
#line 2958 "y.tab.c"
    break;

  case 280: /* ConditionalOrExpression: ConditionalOrExpressions  */
#line 676 "java.y"
                                                  {(yyval.nd)=(yyvsp[0].nd);}
#line 2964 "y.tab.c"
    break;

  case 281: /* ConditionalOrExpressions: ConditionalAndExpression  */
#line 680 "java.y"
                                     {(yyval.nd)=(yyvsp[0].nd);}
#line 2970 "y.tab.c"
    break;

  case 282: /* ConditionalOrExpressions: ConditionalOrExpression OR ConditionalAndExpression  */
#line 681 "java.y"
                                                                    {if(typeCheck((yyvsp[-2].nd),(yyvsp[0].nd))){(yyval.nd)="boolean";}else{cout<<"type Mismatch:"<<yylineno<<endl;exit(28);}}
#line 2976 "y.tab.c"
    break;

  case 283: /* ConditionalExpression: ConditionalExpressions  */
#line 684 "java.y"
                                               {(yyval.nd)=(yyvsp[0].nd);}
#line 2982 "y.tab.c"
    break;

  case 284: /* ConditionalExpressions: ConditionalOrExpression  */
#line 687 "java.y"
                                                     {(yyval.nd)=(yyvsp[0].nd);}
#line 2988 "y.tab.c"
    break;

  case 285: /* ConditionalExpressions: ConditionalOrExpression QUESTION Expression COLON ConditionalExpression  */
#line 688 "java.y"
                                                                                         {if(typeCheck((yyvsp[-4].nd),"boolean")){(yyval.nd)=(yyvsp[-2].nd);}else{(yyval.nd)=(yyvsp[0].nd);}}
#line 2994 "y.tab.c"
    break;

  case 286: /* AssignmentExpression: ConditionalExpression  */
#line 692 "java.y"
                                 {(yyval.nd)=(yyvsp[0].nd);}
#line 3000 "y.tab.c"
    break;

  case 287: /* AssignmentExpression: Assignment  */
#line 693 "java.y"
                            {(yyval.nd)=(yyvsp[0].nd);}
#line 3006 "y.tab.c"
    break;

  case 288: /* $@6: %empty  */
#line 697 "java.y"
                     {t1 = (yyvsp[0].nd);}
#line 3012 "y.tab.c"
    break;

  case 289: /* Assignment: LeftHandSide $@6 AssignmentOperator AssignmentExpression  */
#line 697 "java.y"
                                                                         {t2 = (yyvsp[-1].nd);if(typeCheck(t1,t2)){(yyval.nd)=t1;} else{cout<<"type Mismatch:"<<yylineno<<endl;exit(29);}}
#line 3018 "y.tab.c"
    break;

  case 290: /* LeftHandSide: Name  */
#line 701 "java.y"
                      {char *t = getType((yyvsp[0].nd),localClassId,localFuncId,scope);(yyval.nd)=t;}
#line 3024 "y.tab.c"
    break;

  case 291: /* LeftHandSide: FieldAccess  */
#line 702 "java.y"
                            {(yyval.nd) = (yyvsp[0].nd);}
#line 3030 "y.tab.c"
    break;

  case 292: /* LeftHandSide: ArrayAccess  */
#line 703 "java.y"
                              {(yyval.nd)=(yyvsp[0].nd);}
#line 3036 "y.tab.c"
    break;

  case 299: /* Expression: AssignmentExpression  */
#line 716 "java.y"
                                     {(yyval.nd)=(yyvsp[0].nd);}
#line 3042 "y.tab.c"
    break;


#line 3046 "y.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 719 "java.y"


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

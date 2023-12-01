/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

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

#line 336 "y.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_Y_TAB_H_INCLUDED  */

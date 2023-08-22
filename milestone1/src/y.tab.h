/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
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
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

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

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    SYSTEMOUTPRINTLN = 258,
    LEFTBRACKET = 259,
    RIGHTBRACKET = 260,
    LEFTCURLYBRACKET = 261,
    RIGHTCURLYBRACKET = 262,
    LEFTSQUAREBRACKET = 263,
    RIGHTSQUAREBRACKET = 264,
    THREEDOT = 265,
    SEMICOLON = 266,
    COMMA = 267,
    DOT = 268,
    ATTHERATE = 269,
    DOUBLECOLON = 270,
    TRIPLEGREATEREQUAL = 271,
    TRIPLEGREATER = 272,
    DOUBLELESSEQUAL = 273,
    DOUBLEGREATEREQUAL = 274,
    DOUBLELESS = 275,
    DOUBLEGREATER = 276,
    ADDEQUAL = 277,
    SUBEQUAL = 278,
    MULEQUAL = 279,
    DIVEQUAL = 280,
    ANDEQUAL = 281,
    OREQUAL = 282,
    POWEREQUAL = 283,
    MODEQUAL = 284,
    ARROW = 285,
    EQUALEQUAL = 286,
    GREATEREQUAL = 287,
    LESSEQUAL = 288,
    NOTEQUAL = 289,
    AND = 290,
    OR = 291,
    ADDADD = 292,
    SUBSUB = 293,
    EQUAL = 294,
    LESS = 295,
    GREATER = 296,
    NOT = 297,
    TILDE = 298,
    QUESTION = 299,
    COLON = 300,
    ADD = 301,
    SUB = 302,
    MUL = 303,
    DIV = 304,
    ANDBIT = 305,
    ORBIT = 306,
    POWER = 307,
    MOD = 308,
    ABSTRACT = 309,
    CONTINUE = 310,
    FOR = 311,
    NEW = 312,
    SWITCH = 313,
    ASSERT = 314,
    DEFAULT = 315,
    IF = 316,
    PACKAGE = 317,
    SYNCHRONIZED = 318,
    BOOLEAN = 319,
    DO = 320,
    GOTO = 321,
    PRIVATE = 322,
    THIS = 323,
    BREAK = 324,
    DOUBLE = 325,
    IMPLEMENTS = 326,
    PROTECTED = 327,
    THROWS = 328,
    BYTE = 329,
    ELSE = 330,
    IMPORT = 331,
    PUBLIC = 332,
    THROW = 333,
    CASE = 334,
    ENUM = 335,
    INSTANCE = 336,
    RETURN = 337,
    TRANSIENT = 338,
    CATCH = 339,
    EXTENDS = 340,
    INT = 341,
    SHORT = 342,
    TRY = 343,
    CHAR = 344,
    FINAL = 345,
    INTERFACE = 346,
    STATIC = 347,
    VOID = 348,
    CLASS = 349,
    FINALLY = 350,
    LONG = 351,
    STRICTFP = 352,
    VOLATILE = 353,
    CONST = 354,
    FLOAT = 355,
    NATIVE = 356,
    SUPER = 357,
    WHILE = 358,
    UNDERSEMICOLONORE = 359,
    EXPORTS = 360,
    OPENS = 361,
    REQUIRES = 362,
    USES = 363,
    MODULE = 364,
    PERMITS = 365,
    SEALED = 366,
    VAR = 367,
    NONSEALED = 368,
    PROVIDES = 369,
    TO = 370,
    WITH = 371,
    OPEN = 372,
    RECORD = 373,
    TRANSITIVE = 374,
    YIELD = 375,
    NULLLITERAL = 376,
    SUSPEND = 377,
    INTLITERAL = 378,
    FLOATLITERAL = 379,
    BINARYLITERAL = 380,
    HEXLITERAL = 381,
    IDENTIFIER = 382,
    STRINGLITERAL = 383,
    CHARLITERAL = 384,
    BOOLEANLITERAL = 385
  };
#endif
/* Tokens.  */
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
#define NEW 312
#define SWITCH 313
#define ASSERT 314
#define DEFAULT 315
#define IF 316
#define PACKAGE 317
#define SYNCHRONIZED 318
#define BOOLEAN 319
#define DO 320
#define GOTO 321
#define PRIVATE 322
#define THIS 323
#define BREAK 324
#define DOUBLE 325
#define IMPLEMENTS 326
#define PROTECTED 327
#define THROWS 328
#define BYTE 329
#define ELSE 330
#define IMPORT 331
#define PUBLIC 332
#define THROW 333
#define CASE 334
#define ENUM 335
#define INSTANCE 336
#define RETURN 337
#define TRANSIENT 338
#define CATCH 339
#define EXTENDS 340
#define INT 341
#define SHORT 342
#define TRY 343
#define CHAR 344
#define FINAL 345
#define INTERFACE 346
#define STATIC 347
#define VOID 348
#define CLASS 349
#define FINALLY 350
#define LONG 351
#define STRICTFP 352
#define VOLATILE 353
#define CONST 354
#define FLOAT 355
#define NATIVE 356
#define SUPER 357
#define WHILE 358
#define UNDERSEMICOLONORE 359
#define EXPORTS 360
#define OPENS 361
#define REQUIRES 362
#define USES 363
#define MODULE 364
#define PERMITS 365
#define SEALED 366
#define VAR 367
#define NONSEALED 368
#define PROVIDES 369
#define TO 370
#define WITH 371
#define OPEN 372
#define RECORD 373
#define TRANSITIVE 374
#define YIELD 375
#define NULLLITERAL 376
#define SUSPEND 377
#define INTLITERAL 378
#define FLOATLITERAL 379
#define BINARYLITERAL 380
#define HEXLITERAL 381
#define IDENTIFIER 382
#define STRINGLITERAL 383
#define CHARLITERAL 384
#define BOOLEANLITERAL 385

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 25 "java.y"

        double num;
    node *nd;
    nodes *ndl;
    char *id;

#line 324 "y.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */

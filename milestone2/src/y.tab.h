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
    STRING = 312,
    NEW = 313,
    SWITCH = 314,
    ASSERT = 315,
    DEFAULT = 316,
    IF = 317,
    PACKAGE = 318,
    SYNCHRONIZED = 319,
    BOOLEAN = 320,
    DO = 321,
    GOTO = 322,
    PRIVATE = 323,
    THIS = 324,
    BREAK = 325,
    DOUBLE = 326,
    IMPLEMENTS = 327,
    PROTECTED = 328,
    THROWS = 329,
    BYTE = 330,
    ELSE = 331,
    IMPORT = 332,
    PUBLIC = 333,
    THROW = 334,
    CASE = 335,
    ENUM = 336,
    INSTANCE = 337,
    RETURN = 338,
    TRANSIENT = 339,
    CATCH = 340,
    EXTENDS = 341,
    INT = 342,
    SHORT = 343,
    TRY = 344,
    CHAR = 345,
    FINAL = 346,
    INTERFACE = 347,
    STATIC = 348,
    VOID = 349,
    CLASS = 350,
    FINALLY = 351,
    LONG = 352,
    STRICTFP = 353,
    VOLATILE = 354,
    CONST = 355,
    FLOAT = 356,
    NATIVE = 357,
    SUPER = 358,
    WHILE = 359,
    UNDERSEMICOLONORE = 360,
    EXPORTS = 361,
    OPENS = 362,
    REQUIRES = 363,
    USES = 364,
    MODULE = 365,
    PERMITS = 366,
    SEALED = 367,
    VAR = 368,
    NONSEALED = 369,
    PROVIDES = 370,
    TO = 371,
    WITH = 372,
    OPEN = 373,
    RECORD = 374,
    TRANSITIVE = 375,
    YIELD = 376,
    NULLLITERAL = 377,
    SUSPEND = 378,
    INTLITERAL = 379,
    FLOATLITERAL = 380,
    BINARYLITERAL = 381,
    HEXLITERAL = 382,
    IDENTIFIER = 383,
    STRINGLITERAL = 384,
    CHARLITERAL = 385,
    BOOLEANLITERAL = 386
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

#line 326 "y.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */

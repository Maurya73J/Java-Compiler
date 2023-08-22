/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison implementation for Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.5.1"

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

#line 92 "y.tab.c"

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

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

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

#line 411 "y.tab.c"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */



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
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                            \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
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

#if ! defined yyoverflow || YYERROR_VERBOSE

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
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


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
#define YYLAST   1142

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  131
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  154
/* YYNRULES -- Number of rules.  */
#define YYNRULES  287
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  460

#define YYUNDEFTOK  2
#define YYMAXUTOK   385


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

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
     125,   126,   127,   128,   129,   130
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,    50,    50,    53,    54,    55,    56,    57,    58,    61,
      62,    65,    66,    69,    70,    73,    74,    75,    78,    79,
      82,    93,    99,   106,   110,   116,   122,   130,   136,   143,
     147,   151,   158,   162,   166,   170,   176,   180,   186,   190,
     196,   206,   216,   222,   226,   232,   236,   240,   246,   253,
     263,   269,   273,   277,   285,   292,   296,   300,   304,   310,
     314,   318,   324,   328,   334,   343,   347,   357,   361,   368,
     375,   381,   392,   396,   402,   409,   415,   423,   430,   434,
     439,   445,   457,   467,   471,   479,   486,   492,   499,   507,
     516,   522,   526,   530,   536,   540,   544,   550,   554,   558,
     567,   575,   585,   592,   596,   600,   606,   610,   614,   623,
     627,   633,   641,   645,   651,   655,   661,   667,   673,   674,
     677,   678,   681,   682,   685,   686,   690,   694,   698,   702,
     706,   710,   714,   720,   730,   734,   738,   742,   746,   753,
     757,   761,   765,   769,   773,   779,   785,   793,   801,   808,
     812,   816,   820,   824,   828,   832,   838,   848,   851,   855,
     865,   877,   889,   900,   909,   916,   920,   924,   930,   934,
     938,   944,   948,   953,   968,   984,   988,   995,  1002,  1006,
    1012,  1017,  1021,  1026,  1035,  1044,  1050,  1058,  1062,  1068,
    1072,  1076,  1082,  1086,  1090,  1096,  1100,  1120,  1127,  1135,
    1139,  1145,  1153,  1160,  1170,  1176,  1184,  1191,  1200,  1209,
    1216,  1225,  1236,  1243,  1253,  1257,  1261,  1265,  1272,  1280,
    1288,  1292,  1296,  1301,  1306,  1313,  1321,  1329,  1333,  1338,
    1343,  1358,  1365,  1375,  1382,  1386,  1393,  1401,  1409,  1416,
    1420,  1430,  1442,  1449,  1453,  1463,  1473,  1485,  1492,  1498,
    1508,  1518,  1528,  1540,  1547,  1553,  1563,  1574,  1580,  1586,
    1598,  1604,  1610,  1622,  1629,  1635,  1647,  1654,  1660,  1672,
    1679,  1685,  1697,  1703,  1709,  1727,  1731,  1738,  1747,  1751,
    1755,  1762,  1766,  1770,  1774,  1778,  1782,  1789
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "SYSTEMOUTPRINTLN", "LEFTBRACKET",
  "RIGHTBRACKET", "LEFTCURLYBRACKET", "RIGHTCURLYBRACKET",
  "LEFTSQUAREBRACKET", "RIGHTSQUAREBRACKET", "THREEDOT", "SEMICOLON",
  "COMMA", "DOT", "ATTHERATE", "DOUBLECOLON", "TRIPLEGREATEREQUAL",
  "TRIPLEGREATER", "DOUBLELESSEQUAL", "DOUBLEGREATEREQUAL", "DOUBLELESS",
  "DOUBLEGREATER", "ADDEQUAL", "SUBEQUAL", "MULEQUAL", "DIVEQUAL",
  "ANDEQUAL", "OREQUAL", "POWEREQUAL", "MODEQUAL", "ARROW", "EQUALEQUAL",
  "GREATEREQUAL", "LESSEQUAL", "NOTEQUAL", "AND", "OR", "ADDADD", "SUBSUB",
  "EQUAL", "LESS", "GREATER", "NOT", "TILDE", "QUESTION", "COLON", "ADD",
  "SUB", "MUL", "DIV", "ANDBIT", "ORBIT", "POWER", "MOD", "ABSTRACT",
  "CONTINUE", "FOR", "NEW", "SWITCH", "ASSERT", "DEFAULT", "IF", "PACKAGE",
  "SYNCHRONIZED", "BOOLEAN", "DO", "GOTO", "PRIVATE", "THIS", "BREAK",
  "DOUBLE", "IMPLEMENTS", "PROTECTED", "THROWS", "BYTE", "ELSE", "IMPORT",
  "PUBLIC", "THROW", "CASE", "ENUM", "INSTANCE", "RETURN", "TRANSIENT",
  "CATCH", "EXTENDS", "INT", "SHORT", "TRY", "CHAR", "FINAL", "INTERFACE",
  "STATIC", "VOID", "CLASS", "FINALLY", "LONG", "STRICTFP", "VOLATILE",
  "CONST", "FLOAT", "NATIVE", "SUPER", "WHILE", "UNDERSEMICOLONORE",
  "EXPORTS", "OPENS", "REQUIRES", "USES", "MODULE", "PERMITS", "SEALED",
  "VAR", "NONSEALED", "PROVIDES", "TO", "WITH", "OPEN", "RECORD",
  "TRANSITIVE", "YIELD", "NULLLITERAL", "SUSPEND", "INTLITERAL",
  "FLOATLITERAL", "BINARYLITERAL", "HEXLITERAL", "IDENTIFIER",
  "STRINGLITERAL", "CHARLITERAL", "BOOLEANLITERAL", "$accept", "Start",
  "Literal", "Type", "PrimitiveType", "NumericType", "IntegralType",
  "FloatingPointType", "NonPrimitiveType", "ArrayType", "Name",
  "SimpleName", "QualifiedName", "CompilationUnit", "ImportDeclarationOpt",
  "ImportDeclarationOpts", "TypeDeclarationOpt", "TypeDeclarationOpts",
  "ImportDeclarations", "TypeDeclarations", "ImportDeclaration",
  "SingleTypeImportDeclaration", "TypeImportOnDemandDeclaration",
  "TypeDeclaration", "Modifiers", "Modifier", "ClassDeclaration",
  "ModifiersOpt", "ModifiersOpts", "ClassBody", "ClassBodyDeclarationsOpt",
  "ClassBodyDeclarationsOpts", "ClassBodyDeclarations",
  "ClassBodyDeclaration", "ClassMemberDeclaration", "FieldDeclaration",
  "VarDeclarators", "VarDeclarator", "VarDeclaratorId", "VarDeclaratorIds",
  "VarInitializer", "MethodDeclaration", "MethodHeader",
  "FormalParameterListOpt", "FormalParameterListOpts", "MethodDeclarator",
  "MethodDeclarators", "FormalParameterList", "FormalParameter",
  "MethodBody", "StaticInitializer", "ConstructorDeclaration",
  "ConstructorDeclarator", "ExplicitConstructorInvocationOpt",
  "ExplicitConstructorInvocationOpts", "BlockStatementOpt",
  "BlockStatementOpts", "ArgumentListOpt", "ArgumentListOpts",
  "ConstructorBody", "ExplicitConstructorInvocation", "VarInitializersOpt",
  "VarInitializersOpts", "CommaOpt", "CommaOpts", "ArrayInitializer",
  "VarInitializers", "Block", "BlockStatements", "BlockStatement",
  "LocalVariableDeclarationStatement", "LocalVariableDeclaration",
  "VariableDeclarators", "VariableDeclarator", "VariableInitializer",
  "VariableDeclaratorId", "Statement", "PrintlnStatement",
  "StatementNoShortIf", "StatementWithoutTrailingSubstatement",
  "EmptyStatement", "LabeledStatement", "LabeledStatementNoShortIf",
  "ExpressionStatement", "StatementExpression",
  "ClassInstanceCreationExpression", "ClassType", "ClassOrInterfaceType",
  "IfThenStatement", "IfThenElseStatement", "IfThenElseStatementNoShortIf",
  "WhileStatement", "WhileStatementNoShortIf", "ForInitOpt", "ForInitOpts",
  "ExpressionOpt", "ExpressionOpts", "ForUpdateOpt", "ForUpdateOpts",
  "ForStatement", "ForStatementNoShortIf", "ForInit", "ForUpdate",
  "StatementExpressionList", "IDENTOpt", "IDENTOpts", "BreakStatement",
  "ContinueStatement", "ReturnStatement", "Primary", "PrimaryNoNewArray",
  "ArgumentList", "ArrayCreationExpression", "DimExprs", "DimExpr", "Dims",
  "FieldAccess", "MethodInvocation", "ArrayAccess", "PostFixExpression",
  "PostIncrementExpression", "PostDecrementExpression", "UnaryExpression",
  "PreIncrementExpression", "PreDecrementExpression",
  "UnaryExpressionNotPlusMinus", "CastExpression",
  "MultiplicativeExpression", "MultiplicativeExpressions",
  "AdditiveExpression", "AdditiveExpressions", "ShiftExpression",
  "ShiftExpressions", "RelationalExpression", "RelationalExpressions",
  "EqualityExpression", "EqualityExpressions", "AndExpression",
  "AndExpressions", "ExclusiveOrExpression", "ExclusiveOrExpressions",
  "InclusiveOrExpression", "InclusiveOrExpressions",
  "ConditionalAndExpression", "ConditionalAndExpressions",
  "ConditionalOrExpression", "ConditionalOrExpressions",
  "ConditionalExpression", "ConditionalExpressions",
  "AssignmentExpression", "Assignment", "LeftHandSide",
  "AssignmentOperator", "Expression", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_int16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,   376,   377,   378,   379,   380,   381,   382,   383,   384,
     385
};
# endif

#define YYPACT_NINF (-400)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-281)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     -46,  -116,    34,  -400,    53,  -400,   -46,  -400,  -400,  -400,
    -400,    26,  -400,  -400,  -400,  -400,  -400,  -400,  -400,  -400,
      53,  -400,    49,  -400,  -400,   -37,  -400,  -400,   -42,  -400,
    -400,   -64,    62,    90,     1,  -400,  -400,     7,   -13,  -400,
     113,    96,   122,  -400,    11,  -400,  -400,  -400,  -400,   113,
    -400,  -400,   145,   754,  -400,  -400,  -400,  -400,  -400,    35,
    -400,  -400,    42,  -400,  -400,  -400,  -400,  -400,   176,    23,
     190,   191,  -400,  -400,  -400,  -400,  -400,   222,  1012,  -400,
    1012,  1012,    73,   224,   -38,   232,  -400,    73,  1012,   227,
     241,  -400,  1012,  -400,  -400,   201,  -400,  -400,  -400,  -400,
     120,    29,   242,  -400,  -400,   754,  -400,  -400,   247,  -400,
    -400,  -400,  -400,  -400,  -400,   248,  -400,  -400,  -400,  -400,
    -400,  -400,  -400,  -400,   239,   253,  -400,   196,    97,   277,
      44,   178,   194,  -400,  -400,  -400,   306,   258,   255,  -400,
     258,    33,  -400,    15,  -400,   255,   274,   278,   159,   -38,
     -59,  -400,   -53,  1012,  1012,  1012,  1012,  1012,    79,   476,
    -400,    44,  -400,  -400,  -400,  -400,  -400,  -400,  -400,    89,
    -400,   187,  -400,   119,  -400,   140,  -400,   152,  -400,   240,
    -400,   237,  -400,   243,  -400,   256,  -400,   -17,  -400,  -400,
    -400,  -400,  -400,   287,   109,  -400,  -400,  -400,  -400,  -400,
     285,  -400,   518,   290,   292,   302,  -400,  1012,   296,   298,
    -400,  -400,   184,  1012,   301,   764,  -400,   305,  -400,    21,
    1012,  1012,   905,  -400,  -400,  -400,  -400,   192,  1012,  -400,
    -400,  -400,  -400,  -400,  -400,  -400,  -400,  1012,   -38,   309,
    -400,   193,   313,   962,  -400,  -400,  -400,   193,   318,  -400,
     315,  -400,   310,   321,   754,  -400,  -400,   329,   572,   330,
    -400,  -400,  -400,  -400,  1012,  1012,  1012,  1012,  1012,  1012,
    1012,  1012,  1012,  1012,  1012,  1012,  1012,  1012,  1012,  1012,
    1012,  1012,  1012,  1012,  1012,  -400,  -400,  -400,  -400,   326,
    -400,  -400,   327,  1012,   332,  -400,  1012,   333,  -400,  -400,
     189,   336,  -400,  -400,   120,   334,   962,   337,  -400,   339,
    -400,   341,   340,   208,   343,  -400,   349,  -400,  -400,  -400,
    -400,   962,  -400,  -400,  -400,   347,  -400,   -38,  1012,  1012,
     350,   348,   918,   183,     4,  -400,  -400,  -400,    89,    89,
     187,   187,   187,   119,   119,   119,   119,   140,   140,   152,
     240,   237,   243,   256,   316,  1012,   575,   353,   918,  -400,
     355,   360,   868,  1012,  1012,   764,  -400,  -400,  -400,  -400,
    -400,  -400,  1012,  -400,  -400,  1012,  1012,  -400,  -400,  -400,
     354,  -400,   356,  -400,   364,   365,  -400,  -400,  -400,     4,
     369,  -400,  1012,   362,  -400,  -400,  -400,   370,   375,   376,
     344,  -400,   317,   323,  -400,  -400,  -400,  -400,   379,   374,
    -400,  -400,   380,   383,  -400,   386,  -400,   962,   388,   398,
    -400,  -400,  -400,   575,   518,  1012,  1012,   868,   764,  -400,
    -400,  -400,  -400,  -400,  -400,  -400,  -400,   395,  -400,  -400,
     327,   399,   413,   414,  -400,  -400,   764,  1012,   868,   868,
    -400,   409,   352,  -400,   575,   868,   417,  -400,   868,  -400
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
      30,     0,     0,     2,    33,    28,    29,    34,    38,    39,
      25,     0,    23,    24,     1,    46,    45,    47,    27,    31,
      32,    36,    51,    43,    42,     0,    50,    35,     0,    37,
      44,     0,     0,    26,     0,    41,    40,    52,     0,    48,
      47,     0,     0,    54,    52,    57,    59,    62,    63,     0,
      60,    61,     0,    95,    87,    12,    19,    15,    17,     0,
      16,    18,     0,     9,    11,    13,    14,    10,    20,     0,
      23,     0,    53,    58,    74,    86,    49,     0,     0,   145,
       0,     0,   182,     0,     0,     0,   190,   182,   169,     0,
       0,     8,   169,     3,     4,    25,     6,     7,     5,   189,
       0,   278,     0,    93,   139,    94,   112,   114,     0,   115,
     132,   126,   140,   127,   141,     0,   155,   128,   129,   130,
     131,   142,   143,   144,   214,   187,   188,   192,   193,   194,
       0,   152,   153,   150,   151,   149,     0,     0,    76,    80,
      70,     0,    65,    67,    69,    75,     0,     0,     0,    79,
      92,    88,     0,     0,     0,     0,     0,     0,     0,   215,
     193,   227,   216,   217,   234,   220,   221,   224,   230,   239,
     233,   243,   238,   248,   242,   254,   247,   258,   253,   261,
     257,   264,   260,   267,   263,   270,   266,   273,   269,   275,
     272,   287,   276,     0,   215,   192,   194,   225,   226,   181,
       0,   180,   166,     0,   158,     0,   157,     0,     0,     0,
     167,   168,     0,     0,     0,     0,   124,   117,   118,   120,
      98,    98,     0,   111,   113,   116,   148,     0,     0,   218,
     219,   286,   284,   282,   283,   285,   281,     0,    79,     0,
      64,     0,     0,     0,    22,    21,    26,     0,     0,    77,
      78,    83,     0,     0,    95,    90,    91,     0,   215,     0,
     229,   228,   222,   223,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   191,   184,   176,   178,     0,
     164,   165,   175,     0,   198,   199,    98,     0,   183,   185,
     205,     0,   186,   146,     0,     0,     0,     0,    96,    97,
     195,     0,     0,   204,     0,   277,     0,    82,    70,    66,
      71,   104,    68,    73,    72,    85,    89,     0,    98,    98,
       0,     0,     0,     0,   191,   235,   236,   237,   240,   241,
     246,   244,   245,   252,   251,   249,   250,   255,   256,   259,
     262,   265,   268,   271,     0,   169,     0,     0,     0,   200,
     197,     0,     0,    98,    98,     0,   119,   125,   123,   121,
     122,   206,     0,   209,   212,    98,    98,   213,    81,   109,
     107,   102,   103,    84,     0,     0,    99,   133,   202,     0,
       0,   231,     0,     0,   179,   201,   156,     0,     0,     0,
      25,   159,     0,   126,   135,   136,   137,   138,     0,     0,
     162,   196,     0,     0,   106,     0,   105,     0,     0,     0,
     232,   203,   274,   172,   166,     0,     0,     0,     0,   208,
     211,   207,   210,   108,   110,   100,   101,     0,   170,   171,
     177,     0,     0,     0,   147,   160,     0,   169,     0,     0,
     173,     0,     0,   163,   172,     0,     0,   161,     0,   174
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -400,  -400,   272,   -39,   -71,  -400,  -400,  -400,  -400,  -400,
      -1,   387,  -400,  -400,  -400,  -400,  -400,  -400,  -400,  -400,
     425,  -400,  -400,   412,  -400,   411,  -400,    61,  -400,   393,
    -400,  -400,  -400,   391,  -400,  -400,  -400,   198,   205,  -400,
    -317,  -400,  -400,   202,  -400,   392,  -400,  -400,   114,  -400,
    -400,  -400,  -400,  -400,  -400,   199,  -400,  -205,  -400,  -400,
    -400,  -400,  -400,  -400,  -400,   153,  -400,   -25,  -400,   359,
    -400,  -197,  -400,   157,  -400,  -400,  -198,  -400,  -399,  -337,
    -400,  -400,  -400,  -400,  -199,  -400,  -400,  -400,  -400,  -400,
    -400,  -400,  -400,    41,  -400,   -70,  -400,    14,  -400,  -400,
    -400,  -400,  -400,  -385,   384,  -400,  -400,  -400,  -400,  -400,
    -400,  -400,  -400,  -400,   179,   180,   389,   -52,   473,   -41,
     -12,     2,   -60,    39,    82,  -324,  -400,   -30,  -400,   -94,
    -400,   -65,  -400,   -34,  -400,   197,  -400,   203,  -400,   210,
    -400,   195,  -400,   209,  -400,  -400,  -400,    80,  -400,   249,
      93,  -400,  -400,   268
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     2,    99,   100,    63,    64,    65,    66,    67,    68,
     159,    12,    13,     3,     4,     5,    18,    19,     6,    20,
       7,     8,     9,    21,    22,    23,    24,    25,    26,    39,
      42,    43,    44,    45,    46,    47,   141,   142,   143,   144,
     322,    48,    49,   248,   249,   138,   139,   250,   251,    74,
      50,    51,    71,   254,   255,   102,   103,   307,   308,   151,
     256,   380,   381,   415,   416,   323,   382,   104,   105,   106,
     107,   108,   217,   218,   369,   219,   109,   110,   402,   111,
     112,   113,   404,   114,   115,   116,   205,   206,   117,   118,
     405,   119,   406,   289,   290,   209,   210,   437,   438,   120,
     407,   291,   439,   292,   200,   201,   121,   122,   123,   124,
     125,   309,   126,   294,   295,   333,   127,   160,   129,   161,
     162,   163,   164,   165,   166,   167,   168,   169,   170,   171,
     172,   173,   174,   175,   176,   177,   178,   179,   180,   181,
     182,   183,   184,   185,   186,   187,   188,   189,   190,   191,
     192,   136,   237,   310
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      11,   128,    62,   288,   379,   287,    32,    37,   153,   252,
     391,    10,   130,   203,   -56,    54,   311,   303,   -55,   283,
     197,   198,   214,   242,    75,   403,    55,   284,   444,   305,
       1,   147,    56,   220,    14,   221,   148,   222,   440,    28,
      69,   131,   148,   253,   240,   241,   154,   155,    57,   452,
     453,    58,   101,   128,   243,   132,   457,    31,    60,   459,
     306,   158,    61,    34,   130,   420,  -215,  -215,    91,   440,
      93,    94,    86,    35,    15,    96,    97,    98,    15,   194,
     194,   229,   230,   204,    16,    33,    38,   203,    16,    10,
     403,   361,   133,   131,   260,   261,   262,   263,    41,    40,
     434,    36,  -154,    40,   101,    41,    89,   132,  -154,  -154,
     247,   403,   403,   220,    52,   221,    15,   264,   403,    53,
      15,   403,   148,   384,   385,    91,    16,    93,    94,    72,
      16,    10,    96,    97,    98,   134,   270,   265,   266,   271,
     272,    17,   267,    55,   133,    17,   135,   -52,    69,    56,
     128,    37,   258,   194,   194,   194,   194,   394,   408,   409,
      55,   130,   137,   128,   401,    57,    56,   410,    58,   140,
     412,   413,   273,   274,   130,    60,   340,   341,   342,    61,
     275,   276,    57,   277,   146,    58,   278,   134,   389,    59,
     131,   390,    60,   363,   149,   364,    61,   150,   135,   247,
     199,   101,   128,   131,   132,   335,   336,   337,   343,   344,
     345,   346,   375,   130,   376,  -216,  -216,   132,  -279,  -279,
    -279,  -279,  -279,    10,   288,   288,   152,   287,   202,   303,
     445,  -217,  -217,   268,   269,  -279,   207,    69,   338,   339,
     212,   133,   131,   347,   348,   213,   215,   216,   450,   223,
     401,   410,   227,   101,   133,   288,   132,   445,   225,   226,
     450,   228,   238,   239,   194,   194,   194,   194,   194,   194,
     194,   194,   194,   194,   194,   194,   194,   194,   194,   194,
     194,   194,   194,   244,   134,   393,   246,   245,   247,   280,
     279,   282,   285,   133,   281,   135,   286,   134,   293,  -280,
    -280,  -280,  -280,  -280,   128,   148,   296,   298,   135,   299,
     128,   300,   302,   128,   328,   130,  -280,   304,   317,   313,
     318,   130,   320,   326,   130,   329,    69,   327,   231,   232,
     233,   234,   235,   194,   331,   334,   134,   355,   362,   356,
     358,   365,   371,   367,   131,   236,   193,   135,   373,   374,
     131,   372,   377,   131,   378,   242,   211,   386,   132,   387,
     211,   392,   395,   390,   132,   396,   414,   132,   417,   418,
     419,   128,   128,   423,   424,   128,   128,   451,   421,   425,
     426,   430,   130,   130,   429,   431,   130,   130,   194,   427,
     432,   194,   428,   433,   128,   133,   128,   128,  -134,   435,
     446,   133,   128,   128,   133,   130,   128,   130,   130,   436,
     447,   131,   131,   130,   130,   131,   131,   130,   448,   449,
     454,   259,   458,   101,   257,   132,   132,   455,    70,   132,
     132,    27,    29,    30,   131,    73,   131,   131,   134,   319,
     316,   383,   131,   131,   134,    76,   131,   134,   132,   135,
     132,   132,   325,   330,   145,   135,   132,   132,   135,   368,
     132,   366,   133,   133,   224,   441,   133,   133,   456,   195,
     195,   208,   422,   359,   360,   297,   349,   352,     0,     0,
     220,   301,   221,   350,   264,   133,   315,   133,   133,   148,
     312,   351,   353,   133,   133,     0,   314,   133,  -278,  -278,
    -278,  -278,  -278,     0,     0,   134,   134,     0,     0,   134,
     134,   324,     0,     0,     0,  -278,   135,   135,     0,     0,
     135,   135,    78,     0,     0,     0,     0,     0,   134,     0,
     134,   134,   312,     0,     0,     0,   134,   134,     0,   135,
     134,   135,   135,   195,   195,   195,   195,   135,   135,     0,
       0,   135,   354,   196,   196,    80,    81,     0,     0,     0,
       0,   357,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   370,    84,   220,     0,   221,    78,
     332,     0,    55,     0,     0,   148,    86,     0,    56,   324,
       0,     0,     0,     0,  -278,  -278,  -278,  -278,  -278,     0,
     312,     0,     0,     0,    57,     0,     0,    58,     0,     0,
       0,  -278,    80,    81,    60,     0,     0,     0,    61,     0,
      89,     0,     0,   211,     0,     0,   357,   196,   196,   196,
     196,     0,    84,     0,     0,     0,     0,     0,     0,    91,
     411,    93,    94,    86,     0,    10,    96,    97,    98,     0,
       0,     0,     0,     0,   195,   195,   195,   195,   195,   195,
     195,   195,   195,   195,   195,   195,   195,   195,   195,   195,
     195,   195,   195,     0,     0,     0,     0,    89,     0,     0,
       0,     0,     0,     0,     0,   324,     0,     0,     0,     0,
       0,     0,     0,   442,   443,     0,    91,     0,    93,    94,
       0,     0,    10,    96,    97,    98,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   211,     0,     0,     0,     0,
       0,     0,     0,   195,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   196,   196,
     196,   196,   196,   196,   196,   196,   196,   196,   196,   196,
     196,   196,   196,   196,   196,   196,   196,    77,    78,     0,
      53,     0,     0,     0,     0,    79,     0,    77,    78,     0,
      53,     0,     0,     0,     0,    79,     0,     0,   195,     0,
       0,   195,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,    81,     0,     0,     0,     0,     0,     0,     0,
       0,    80,    81,     0,     0,     0,     0,   196,     0,    82,
      83,    84,     0,     0,     0,    85,     0,     0,    55,    82,
      83,    84,    86,    87,    56,    85,     0,     0,     0,     0,
       0,     0,    86,    87,     0,     0,    88,     0,     0,     0,
      57,     0,     0,    58,     0,     0,    88,     0,     0,     0,
      60,     0,     0,     0,    61,     0,    89,    90,     0,     0,
       0,     0,   196,     0,     0,   196,    89,    90,     0,     0,
       0,    77,    78,     0,    53,    91,    92,    93,    94,    79,
       0,    95,    96,    97,    98,    91,    92,    93,    94,     0,
       0,    95,    96,    97,    98,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,    81,     0,     0,   153,
       0,     0,     0,     0,   245,     0,     0,     0,     0,     0,
       0,     0,   153,    82,   397,    84,     0,   388,     0,   398,
       0,     0,     0,     0,     0,     0,    86,    87,     0,     0,
       0,     0,    80,    81,     0,     0,     0,   154,   155,     0,
      88,   156,   157,     0,     0,    80,    81,     0,     0,     0,
     154,   155,   158,     0,   156,   157,   153,     0,   321,     0,
      89,   399,     0,    86,     0,   158,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    86,     0,     0,    91,
      92,    93,    94,     0,     0,   400,    96,    97,    98,    80,
      81,     0,     0,     0,   154,   155,     0,    89,   156,   157,
       0,     0,     0,     0,     0,     0,   153,     0,     0,   158,
      89,     0,     0,     0,     0,     0,    91,     0,    93,    94,
      86,     0,    10,    96,    97,    98,     0,     0,     0,    91,
       0,    93,    94,     0,     0,    10,    96,    97,    98,    80,
      81,     0,     0,     0,   154,   155,     0,     0,   156,   157,
       0,     0,     0,     0,    89,     0,     0,     0,     0,   158,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      86,     0,     0,    91,     0,    93,    94,     0,     0,    10,
      96,    97,    98,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    89,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    91,     0,    93,    94,     0,     0,    10,
      96,    97,    98
};

static const yytype_int16 yycheck[] =
{
       1,    53,    41,   202,   321,   202,    48,     6,     4,    68,
     334,   127,    53,    84,     7,    40,   221,   215,     7,    36,
      80,    81,    92,     8,    49,   362,    64,    44,   427,     8,
      76,     8,    70,     4,     0,     6,    13,     8,   423,    13,
      41,    53,    13,   102,    11,    12,    42,    43,    86,   448,
     449,    89,    53,   105,    39,    53,   455,    94,    96,   458,
      39,    57,   100,   127,   105,   389,    37,    38,   121,   454,
     123,   124,    68,    11,    67,   128,   129,   130,    67,    80,
      81,    37,    38,    84,    77,   127,    85,   158,    77,   127,
     427,   296,    53,   105,   154,   155,   156,   157,    37,    92,
     417,    11,     5,    92,   105,    44,   102,   105,    11,    12,
     149,   448,   449,     4,   127,     6,    67,     8,   455,     6,
      67,   458,    13,   328,   329,   121,    77,   123,   124,     7,
      77,   127,   128,   129,   130,    53,    17,    48,    49,    20,
      21,    92,    53,    64,   105,    92,    53,    94,   149,    70,
     202,     6,   153,   154,   155,   156,   157,   356,   363,   364,
      64,   202,   127,   215,   362,    86,    70,   365,    89,   127,
     375,   376,    32,    33,   215,    96,   270,   271,   272,   100,
      40,    41,    86,    31,     8,    89,    34,   105,     5,    93,
     202,     8,    96,     4,     4,     6,   100,     6,   105,   238,
     127,   202,   254,   215,   202,   265,   266,   267,   273,   274,
     275,   276,     4,   254,     6,    37,    38,   215,    22,    23,
      24,    25,    26,   127,   423,   424,     4,   424,     4,   427,
     428,    37,    38,    46,    47,    39,     4,   238,   268,   269,
      13,   202,   254,   277,   278,     4,    45,   127,   446,     7,
     448,   449,    13,   254,   215,   454,   254,   455,    11,    11,
     458,     8,     4,     8,   265,   266,   267,   268,   269,   270,
     271,   272,   273,   274,   275,   276,   277,   278,   279,   280,
     281,   282,   283,     9,   202,   355,   127,     9,   327,    52,
      50,    35,     5,   254,    51,   202,    11,   215,     8,    22,
      23,    24,    25,    26,   356,    13,     4,    11,   215,    11,
     362,   127,    11,   365,     4,   356,    39,    12,     9,   127,
     127,   362,     9,     5,   365,     4,   327,    12,    22,    23,
      24,    25,    26,   334,     5,     5,   254,    11,     5,    12,
       8,     5,     5,     9,   356,    39,    78,   254,     7,     9,
     362,    12,     9,   365,     5,     8,    88,     7,   356,    11,
      92,    45,     9,     8,   362,     5,    12,   365,    12,     5,
       5,   423,   424,    11,     4,   427,   428,   447,     9,     4,
       4,     7,   423,   424,     5,     5,   427,   428,   389,    45,
       7,   392,    75,     7,   446,   356,   448,   449,    75,    11,
       5,   362,   454,   455,   365,   446,   458,   448,   449,    11,
      11,   423,   424,   454,   455,   427,   428,   458,     5,     5,
      11,   153,     5,   424,   152,   423,   424,    75,    41,   427,
     428,     6,    20,    22,   446,    44,   448,   449,   356,   241,
     238,   327,   454,   455,   362,    52,   458,   365,   446,   356,
     448,   449,   247,   254,    62,   362,   454,   455,   365,   306,
     458,   304,   423,   424,   105,   424,   427,   428,   454,    80,
      81,    87,   392,   294,   294,   207,   279,   282,    -1,    -1,
       4,   213,     6,   280,     8,   446,   237,   448,   449,    13,
     222,   281,   283,   454,   455,    -1,   228,   458,    22,    23,
      24,    25,    26,    -1,    -1,   423,   424,    -1,    -1,   427,
     428,   243,    -1,    -1,    -1,    39,   423,   424,    -1,    -1,
     427,   428,     4,    -1,    -1,    -1,    -1,    -1,   446,    -1,
     448,   449,   264,    -1,    -1,    -1,   454,   455,    -1,   446,
     458,   448,   449,   154,   155,   156,   157,   454,   455,    -1,
      -1,   458,   284,    80,    81,    37,    38,    -1,    -1,    -1,
      -1,   293,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   306,    57,     4,    -1,     6,     4,
       8,    -1,    64,    -1,    -1,    13,    68,    -1,    70,   321,
      -1,    -1,    -1,    -1,    22,    23,    24,    25,    26,    -1,
     332,    -1,    -1,    -1,    86,    -1,    -1,    89,    -1,    -1,
      -1,    39,    37,    38,    96,    -1,    -1,    -1,   100,    -1,
     102,    -1,    -1,   355,    -1,    -1,   358,   154,   155,   156,
     157,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,   121,
     372,   123,   124,    68,    -1,   127,   128,   129,   130,    -1,
      -1,    -1,    -1,    -1,   265,   266,   267,   268,   269,   270,
     271,   272,   273,   274,   275,   276,   277,   278,   279,   280,
     281,   282,   283,    -1,    -1,    -1,    -1,   102,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   417,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   425,   426,    -1,   121,    -1,   123,   124,
      -1,    -1,   127,   128,   129,   130,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   447,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   334,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   265,   266,
     267,   268,   269,   270,   271,   272,   273,   274,   275,   276,
     277,   278,   279,   280,   281,   282,   283,     3,     4,    -1,
       6,    -1,    -1,    -1,    -1,    11,    -1,     3,     4,    -1,
       6,    -1,    -1,    -1,    -1,    11,    -1,    -1,   389,    -1,
      -1,   392,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    37,    38,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    37,    38,    -1,    -1,    -1,    -1,   334,    -1,    55,
      56,    57,    -1,    -1,    -1,    61,    -1,    -1,    64,    55,
      56,    57,    68,    69,    70,    61,    -1,    -1,    -1,    -1,
      -1,    -1,    68,    69,    -1,    -1,    82,    -1,    -1,    -1,
      86,    -1,    -1,    89,    -1,    -1,    82,    -1,    -1,    -1,
      96,    -1,    -1,    -1,   100,    -1,   102,   103,    -1,    -1,
      -1,    -1,   389,    -1,    -1,   392,   102,   103,    -1,    -1,
      -1,     3,     4,    -1,     6,   121,   122,   123,   124,    11,
      -1,   127,   128,   129,   130,   121,   122,   123,   124,    -1,
      -1,   127,   128,   129,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    -1,     4,
      -1,    -1,    -1,    -1,     9,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     4,    55,    56,    57,    -1,     9,    -1,    61,
      -1,    -1,    -1,    -1,    -1,    -1,    68,    69,    -1,    -1,
      -1,    -1,    37,    38,    -1,    -1,    -1,    42,    43,    -1,
      82,    46,    47,    -1,    -1,    37,    38,    -1,    -1,    -1,
      42,    43,    57,    -1,    46,    47,     4,    -1,     6,    -1,
     102,   103,    -1,    68,    -1,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    68,    -1,    -1,   121,
     122,   123,   124,    -1,    -1,   127,   128,   129,   130,    37,
      38,    -1,    -1,    -1,    42,    43,    -1,   102,    46,    47,
      -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,    57,
     102,    -1,    -1,    -1,    -1,    -1,   121,    -1,   123,   124,
      68,    -1,   127,   128,   129,   130,    -1,    -1,    -1,   121,
      -1,   123,   124,    -1,    -1,   127,   128,   129,   130,    37,
      38,    -1,    -1,    -1,    42,    43,    -1,    -1,    46,    47,
      -1,    -1,    -1,    -1,   102,    -1,    -1,    -1,    -1,    57,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      68,    -1,    -1,   121,    -1,   123,   124,    -1,    -1,   127,
     128,   129,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   102,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   121,    -1,   123,   124,    -1,    -1,   127,
     128,   129,   130
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,    76,   132,   144,   145,   146,   149,   151,   152,   153,
     127,   141,   142,   143,     0,    67,    77,    92,   147,   148,
     150,   154,   155,   156,   157,   158,   159,   151,    13,   154,
     156,    94,    48,   127,   127,    11,    11,     6,    85,   160,
      92,   158,   161,   162,   163,   164,   165,   166,   172,   173,
     181,   182,   127,     6,   198,    64,    70,    86,    89,    93,
      96,   100,   134,   135,   136,   137,   138,   139,   140,   141,
     142,   183,     7,   164,   180,   198,   160,     3,     4,    11,
      37,    38,    55,    56,    57,    61,    68,    69,    82,   102,
     103,   121,   122,   123,   124,   127,   128,   129,   130,   133,
     134,   141,   186,   187,   198,   199,   200,   201,   202,   207,
     208,   210,   211,   212,   214,   215,   216,   219,   220,   222,
     230,   237,   238,   239,   240,   241,   243,   247,   248,   249,
     250,   251,   252,   254,   255,   281,   282,   127,   176,   177,
     127,   167,   168,   169,   170,   176,     8,     8,    13,     4,
       6,   190,     4,     4,    42,    43,    46,    47,    57,   141,
     248,   250,   251,   252,   253,   254,   255,   256,   257,   258,
     259,   260,   261,   262,   263,   264,   265,   266,   267,   268,
     269,   270,   271,   272,   273,   274,   275,   276,   277,   278,
     279,   280,   281,   284,   141,   247,   249,   253,   253,   127,
     235,   236,     4,   135,   141,   217,   218,     4,   235,   226,
     227,   284,    13,     4,   226,    45,   127,   203,   204,   206,
       4,     6,     8,     7,   200,    11,    11,    13,     8,    37,
      38,    22,    23,    24,    25,    26,    39,   283,     4,     8,
      11,    12,     8,    39,     9,     9,   127,   134,   174,   175,
     178,   179,    68,   102,   184,   185,   191,   133,   141,   284,
     253,   253,   253,   253,     8,    48,    49,    53,    46,    47,
      17,    20,    21,    32,    33,    40,    41,    31,    34,    50,
      52,    51,    35,    36,    44,     5,    11,   202,   215,   224,
     225,   232,   234,     8,   244,   245,     4,   284,    11,    11,
     127,   284,    11,   207,    12,     8,    39,   188,   189,   242,
     284,   188,   284,   127,   284,   280,   174,     9,   127,   168,
       9,     6,   171,   196,   284,   169,     5,    12,     4,     4,
     186,     5,     8,   246,     5,   253,   253,   253,   258,   258,
     260,   260,   260,   262,   262,   262,   262,   264,   264,   266,
     268,   270,   272,   274,   284,    11,    12,   284,     8,   245,
     246,   188,     5,     4,     6,     5,   204,     9,   196,   205,
     284,     5,    12,     7,     9,     4,     6,     9,     5,   171,
     192,   193,   197,   179,   188,   188,     7,    11,     9,     5,
       8,   256,    45,   226,   215,     9,     5,    56,    61,   103,
     127,   207,   209,   210,   213,   221,   223,   231,   188,   188,
     207,   284,   188,   188,    12,   194,   195,    12,     5,     5,
     256,     9,   278,    11,     4,     4,     4,    45,    75,     5,
       7,     5,     7,     7,   171,    11,    11,   228,   229,   233,
     234,   224,   284,   284,   209,   207,     5,    11,     5,     5,
     207,   226,   209,   209,    11,    75,   228,   209,     5,   209
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   131,   132,   133,   133,   133,   133,   133,   133,   134,
     134,   135,   135,   136,   136,   137,   137,   137,   138,   138,
     139,   140,   140,   141,   141,   142,   143,   144,   145,   146,
     146,   147,   148,   148,   149,   149,   150,   150,   151,   151,
     152,   153,   154,   155,   155,   156,   156,   156,   157,   157,
     158,   159,   159,   160,   161,   162,   162,   163,   163,   164,
     164,   164,   165,   165,   166,   167,   167,   168,   168,   169,
     170,   170,   171,   171,   172,   173,   173,   174,   175,   175,
     176,   177,   177,   178,   178,   179,   180,   181,   182,   183,
     184,   185,   185,   186,   187,   187,   188,   189,   189,   190,
     191,   191,   192,   193,   193,   194,   195,   195,   196,   197,
     197,   198,   199,   199,   200,   200,   201,   202,   203,   203,
     204,   204,   205,   205,   206,   206,   207,   207,   207,   207,
     207,   207,   207,   208,   209,   209,   209,   209,   209,   210,
     210,   210,   210,   210,   210,   211,   212,   213,   214,   215,
     215,   215,   215,   215,   215,   215,   216,   217,   218,   219,
     220,   221,   222,   223,   224,   225,   225,   226,   227,   227,
     228,   229,   229,   230,   231,   232,   232,   233,   234,   234,
     235,   236,   236,   237,   238,   239,   239,   240,   240,   241,
     241,   241,   241,   241,   241,   242,   242,   243,   243,   244,
     244,   245,   246,   246,   247,   247,   248,   248,   248,   248,
     248,   248,   249,   249,   250,   250,   250,   250,   251,   252,
     253,   253,   253,   253,   253,   254,   255,   256,   256,   256,
     256,   257,   257,   258,   259,   259,   259,   259,   260,   261,
     261,   261,   262,   263,   263,   263,   263,   264,   265,   265,
     265,   265,   265,   266,   267,   267,   267,   268,   269,   269,
     270,   271,   271,   272,   273,   273,   274,   275,   275,   276,
     277,   277,   278,   279,   279,   280,   280,   281,   282,   282,
     282,   283,   283,   283,   283,   283,   283,   284
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     3,     1,     1,     1,     3,     2,     1,     1,
       0,     1,     1,     0,     1,     2,     1,     2,     1,     1,
       5,     5,     1,     1,     2,     1,     1,     1,     4,     6,
       1,     1,     0,     3,     1,     1,     0,     1,     2,     1,
       1,     1,     1,     1,     4,     1,     3,     1,     3,     1,
       1,     3,     1,     1,     2,     3,     3,     1,     1,     0,
       1,     4,     3,     1,     3,     2,     1,     2,     3,     4,
       1,     1,     0,     1,     1,     0,     1,     1,     0,     4,
       5,     5,     1,     1,     0,     1,     1,     0,     4,     1,
       3,     3,     1,     2,     1,     1,     2,     2,     1,     3,
       1,     3,     1,     1,     1,     3,     1,     1,     1,     1,
       1,     1,     1,     5,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     3,     2,     1,
       1,     1,     1,     1,     1,     1,     5,     1,     1,     5,
       7,     7,     5,     5,     1,     1,     0,     1,     1,     0,
       1,     1,     0,     9,     9,     1,     1,     1,     1,     3,
       1,     1,     0,     3,     3,     3,     3,     1,     1,     1,
       1,     3,     1,     1,     1,     1,     3,     4,     3,     1,
       2,     3,     2,     3,     3,     3,     4,     6,     6,     4,
       6,     6,     4,     4,     1,     1,     1,     1,     2,     2,
       1,     1,     2,     2,     1,     2,     2,     1,     2,     2,
       1,     4,     5,     1,     1,     3,     3,     3,     1,     1,
       3,     3,     1,     1,     3,     3,     3,     1,     1,     3,
       3,     3,     3,     1,     1,     3,     3,     1,     1,     3,
       1,     1,     3,     1,     1,     3,     1,     1,     3,     1,
       1,     3,     1,     1,     5,     1,     1,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


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

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



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

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YYUSE (yyoutput);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yytype], *yyvaluep);
# endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyo, yytype, yyvaluep);
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
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, int yyrule)
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
                       yystos[+yyssp[yyi + 1 - yynrhs]],
                       &yyvsp[(yyi + 1) - (yynrhs)]
                                              );
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
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
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


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
#  else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            else
              goto append;

          append:
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                yy_state_t *yyssp, int yytoken)
{
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Actual size of YYARG. */
  int yycount = 0;
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[+*yyssp];
      YYPTRDIFF_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
      yysize = yysize0;
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYPTRDIFF_T yysize1
                    = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
                    yysize = yysize1;
                  else
                    return 2;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    /* Don't count the "%s"s in the final size, but reserve room for
       the terminator.  */
    YYPTRDIFF_T yysize1 = yysize + (yystrlen (yyformat) - 2 * yycount) + 1;
    if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
      yysize = yysize1;
    else
      return 2;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
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
    yy_state_fast_t yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss;
    yy_state_t *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYPTRDIFF_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
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

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    goto yyexhaustedlab;
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
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
# undef YYSTACK_RELOCATE
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

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
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
  case 3:
#line 53 "java.y"
                                {(yyval.nd) = new node(++cnt,"literals"); char tmpstr[20]; sprintf(tmpstr, "%g", (yyvsp[0].num)); nodtostr((yyval.nd), string(tmpstr),"INTLITERAL");}
#line 2146 "y.tab.c"
    break;

  case 4:
#line 54 "java.y"
                                {(yyval.nd) = new node(++cnt,"literals"); char tmpstr[20]; sprintf(tmpstr, "%g", (yyvsp[0].num)); nodtostr((yyval.nd), string(tmpstr),"FLOATLITERAL");}
#line 2152 "y.tab.c"
    break;

  case 5:
#line 55 "java.y"
                                    {(yyval.nd) = new node(++cnt,"literals"); nodtostr((yyval.nd), (yyvsp[0].id),"BOOLEANLITERAL");}
#line 2158 "y.tab.c"
    break;

  case 6:
#line 56 "java.y"
                                {(yyval.nd) = new node(++cnt,"literals"); nodtostr((yyval.nd), (yyvsp[0].id),"STRINGLITERAL");}
#line 2164 "y.tab.c"
    break;

  case 7:
#line 57 "java.y"
                                {(yyval.nd) = new node(++cnt,"literals"); nodtostr((yyval.nd), (yyvsp[0].id),"CHARLITERAL");}
#line 2170 "y.tab.c"
    break;

  case 8:
#line 58 "java.y"
                                {(yyval.nd) = new node(++cnt,"literals"); nodtostr((yyval.nd), (yyvsp[0].id),"NULLLITERAL");}
#line 2176 "y.tab.c"
    break;

  case 9:
#line 61 "java.y"
                                {(yyval.nd) = new node(++cnt,"type"); nodjoin((yyval.nd),(yyvsp[0].nd));}
#line 2182 "y.tab.c"
    break;

  case 10:
#line 62 "java.y"
                                 {(yyval.nd) = new node(++cnt,"type"); nodjoin((yyval.nd),(yyvsp[0].nd));}
#line 2188 "y.tab.c"
    break;

  case 11:
#line 65 "java.y"
                                {(yyval.nd) = new node(++cnt,"PrimitiveType"); nodjoin((yyval.nd),(yyvsp[0].nd));}
#line 2194 "y.tab.c"
    break;

  case 12:
#line 66 "java.y"
                                 {(yyval.nd) = new node(++cnt,"PrimitiveType"); nodtostr((yyval.nd),(yyvsp[0].id),"BOOLEAN");}
#line 2200 "y.tab.c"
    break;

  case 13:
#line 69 "java.y"
                                {(yyval.nd) = new node(++cnt,"NumericType"); nodjoin((yyval.nd),(yyvsp[0].nd));}
#line 2206 "y.tab.c"
    break;

  case 14:
#line 70 "java.y"
                                {(yyval.nd) = new node(++cnt,"NumericType"); nodjoin((yyval.nd),(yyvsp[0].nd));}
#line 2212 "y.tab.c"
    break;

  case 15:
#line 73 "java.y"
                                {(yyval.nd) = new node(++cnt,"IntegralType"); nodtostr((yyval.nd),(yyvsp[0].id),"INT");}
#line 2218 "y.tab.c"
    break;

  case 16:
#line 74 "java.y"
                                {(yyval.nd) = new node(++cnt,"IntegralType"); nodtostr((yyval.nd),(yyvsp[0].id),"LONG");}
#line 2224 "y.tab.c"
    break;

  case 17:
#line 75 "java.y"
                                {(yyval.nd) = new node(++cnt,"IntegralType"); nodtostr((yyval.nd),(yyvsp[0].id),"CHAR");}
#line 2230 "y.tab.c"
    break;

  case 18:
#line 78 "java.y"
                                {(yyval.nd) = new node(++cnt,"FloatingPointType"); nodtostr((yyval.nd),(yyvsp[0].id),"FLOAT");}
#line 2236 "y.tab.c"
    break;

  case 19:
#line 79 "java.y"
                                {(yyval.nd) = new node(++cnt,"FloatingPointType"); nodtostr((yyval.nd),(yyvsp[0].id),"DOUBLE");}
#line 2242 "y.tab.c"
    break;

  case 20:
#line 82 "java.y"
                                {(yyval.nd) = new node(++cnt,"NonPrimitiveType"); nodjoin((yyval.nd),(yyvsp[0].nd));}
#line 2248 "y.tab.c"
    break;

  case 21:
#line 93 "java.y"
                                                    {
                (yyval.nd) = new node(++cnt,"ArrayType");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtostr((yyval.nd),(yyvsp[-1]. id),"LEFTSQUAREBRACKET");
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTSQUAREBRACKET");
        }
#line 2259 "y.tab.c"
    break;

  case 22:
#line 99 "java.y"
                                                             {
                (yyval.nd) = (yyvsp[-2].nd);
                nodtostr((yyval.nd),(yyvsp[-1]. id),"LEFTSQUAREBRACKET");
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTSQUAREBRACKET");
    }
#line 2269 "y.tab.c"
    break;

  case 23:
#line 106 "java.y"
                        {
                (yyval.nd) = new node(++cnt,"Name");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2278 "y.tab.c"
    break;

  case 24:
#line 110 "java.y"
                        {
                (yyval.nd) = new node(++cnt,"Name");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2287 "y.tab.c"
    break;

  case 25:
#line 116 "java.y"
                        {
                (yyval.nd) = new node(++cnt,"SimpleName");
                nodtostr((yyval.nd),(yyvsp[0].id),"IDENTIFIER");
        }
#line 2296 "y.tab.c"
    break;

  case 26:
#line 122 "java.y"
                            {
                (yyval.nd) = new node(++cnt,"QualifiedName");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtostr((yyval.nd),(yyvsp[-1]. id),"DOT");
                nodtostr((yyval.nd),(yyvsp[0].id),"IDENTIFIER");
        }
#line 2307 "y.tab.c"
    break;

  case 27:
#line 130 "java.y"
                                                {
                (yyval.nd) = new node(++cnt,"CompilationUnit");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2317 "y.tab.c"
    break;

  case 28:
#line 136 "java.y"
                                           {
        (yyval.nd) = new node(++cnt,"ImportDeclarationOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 2326 "y.tab.c"
    break;

  case 29:
#line 143 "java.y"
                                {
                (yyval.ndl) = new nodes();
                listpushback((yyval.ndl),(yyvsp[0].ndl));
        }
#line 2335 "y.tab.c"
    break;

  case 30:
#line 147 "java.y"
          { (yyval.ndl) = new nodes();}
#line 2341 "y.tab.c"
    break;

  case 31:
#line 151 "java.y"
                                       {
        (yyval.nd) = new node(++cnt,"TypeDeclarationOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 2350 "y.tab.c"
    break;

  case 32:
#line 158 "java.y"
                                {
                (yyval.ndl) = new nodes();
                listpushback((yyval.ndl),(yyvsp[0].ndl));
        }
#line 2359 "y.tab.c"
    break;

  case 33:
#line 162 "java.y"
          { (yyval.ndl) = new nodes();}
#line 2365 "y.tab.c"
    break;

  case 34:
#line 166 "java.y"
                                {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 2374 "y.tab.c"
    break;

  case 35:
#line 170 "java.y"
                                                {
                (yyval.ndl) = (yyvsp[-1].ndl) ;
                (yyval.ndl)->push_back((yyvsp[0].nd));
    }
#line 2383 "y.tab.c"
    break;

  case 36:
#line 176 "java.y"
                                {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 2392 "y.tab.c"
    break;

  case 37:
#line 180 "java.y"
                                            {
                (yyval.ndl) = (yyvsp[-1].ndl) ;
                (yyval.ndl)->push_back((yyvsp[0].nd));
    }
#line 2401 "y.tab.c"
    break;

  case 38:
#line 186 "java.y"
                                        {
                (yyval.nd) = new node(++cnt,"ImportDeclaration");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2410 "y.tab.c"
    break;

  case 39:
#line 190 "java.y"
                                          {
                (yyval.nd) = new node(++cnt,"ImportDeclaration");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2419 "y.tab.c"
    break;

  case 40:
#line 196 "java.y"
                                               {
                (yyval.nd) = new node(++cnt,"SingleTypeImportDeclaration");
                nodtostr((yyval.nd),(yyvsp[-4].id),"IMPORT");
                nodjoin((yyval.nd),(yyvsp[-3].nd));
                nodtostr((yyval.nd),(yyvsp[-2]. id),"DOT");
                nodtostr((yyval.nd),(yyvsp[-1].id),"IDENTIFIER");
                nodtostr((yyval.nd),(yyvsp[0]. id),"SEMICOLON");
        }
#line 2432 "y.tab.c"
    break;

  case 41:
#line 206 "java.y"
                                        {
                (yyval.nd) = new node(++cnt,"TypeImportOnDemandDeclaration");
                nodtostr((yyval.nd),(yyvsp[-4].id),"IMPORT");
                nodjoin((yyval.nd),(yyvsp[-3].nd));
                nodtostr((yyval.nd),(yyvsp[-2]. id),"DOT");
                nodtostr((yyval.nd),(yyvsp[-1]. id),"MUL");
                nodtostr((yyval.nd),(yyvsp[0]. id),"SEMICOLON");
        }
#line 2445 "y.tab.c"
    break;

  case 42:
#line 216 "java.y"
                                {
                (yyval.nd) = new node(++cnt,"TypeDeclaration");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2454 "y.tab.c"
    break;

  case 43:
#line 222 "java.y"
                        {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 2463 "y.tab.c"
    break;

  case 44:
#line 226 "java.y"
                                {
                (yyval.ndl) = (yyvsp[-1].ndl);
                (yyvsp[-1].ndl)->push_back((yyvsp[0].nd));
    }
#line 2472 "y.tab.c"
    break;

  case 45:
#line 232 "java.y"
                        {
                (yyval.nd) = new node(++cnt,"Modifier");
                nodtostr((yyval.nd),(yyvsp[0].id),"PUBLIC");
        }
#line 2481 "y.tab.c"
    break;

  case 46:
#line 236 "java.y"
                        {
                (yyval.nd) = new node(++cnt,"Modifier");
                nodtostr((yyval.nd),(yyvsp[0].id),"PRIVATE");
        }
#line 2490 "y.tab.c"
    break;

  case 47:
#line 240 "java.y"
                       {
                (yyval.nd) = new node(++cnt,"Modifier");
                nodtostr((yyval.nd),(yyvsp[0].id),"STATIC");
        }
#line 2499 "y.tab.c"
    break;

  case 48:
#line 246 "java.y"
                                                {
                (yyval.nd) = new node(++cnt,"ClassDeclaration");
                nodjoin((yyval.nd),(yyvsp[-3].nd));
                nodtostr((yyval.nd),(yyvsp[-2].id),"CLASS");
                nodtostr((yyval.nd),(yyvsp[-1].id),"IDENTIFIER");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2511 "y.tab.c"
    break;

  case 49:
#line 253 "java.y"
                                                                        {
                (yyval.nd) = new node(++cnt,"ClassDeclaration");
                nodjoin((yyval.nd),(yyvsp[-5].nd));
                nodtostr((yyval.nd),(yyvsp[-4].id),"CLASS");
                nodtostr((yyval.nd),(yyvsp[-3].id),"IDENTIFIER");
                nodtostr((yyval.nd),(yyvsp[-2].id),"EXTENDS");
                nodtostr((yyval.nd),(yyvsp[-1].id),"IDENTIFIER");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2525 "y.tab.c"
    break;

  case 50:
#line 263 "java.y"
                            {
        (yyval.nd) = new node(++cnt,"ModifiersOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 2534 "y.tab.c"
    break;

  case 51:
#line 269 "java.y"
                        {
                (yyval.ndl) = new nodes();
                listpushback((yyval.ndl),(yyvsp[0].ndl));
        }
#line 2543 "y.tab.c"
    break;

  case 52:
#line 273 "java.y"
          { (yyval.ndl) = new nodes() ;}
#line 2549 "y.tab.c"
    break;

  case 53:
#line 277 "java.y"
                                                                           {
                (yyval.nd) = new node(++cnt,"ClassBody");
                nodtostr((yyval.nd),(yyvsp[-2]. id),"LEFTCURLYBRACKET");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTCURLYBRACKET");
        }
#line 2560 "y.tab.c"
    break;

  case 54:
#line 285 "java.y"
                                                   {
(yyval.nd) = new node(++cnt,"ClassBodyDeclarationsOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 2569 "y.tab.c"
    break;

  case 55:
#line 292 "java.y"
                                {
                (yyval.ndl) = new nodes();
                listpushback((yyval.ndl),(yyvsp[0].ndl));
        }
#line 2578 "y.tab.c"
    break;

  case 56:
#line 296 "java.y"
           { (yyval.ndl) = new nodes(); }
#line 2584 "y.tab.c"
    break;

  case 57:
#line 300 "java.y"
                                {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 2593 "y.tab.c"
    break;

  case 58:
#line 304 "java.y"
                                                        {
        (yyval.ndl) = (yyvsp[-1].ndl);
        (yyvsp[-1].ndl)->push_back((yyvsp[0].nd));
    }
#line 2602 "y.tab.c"
    break;

  case 59:
#line 310 "java.y"
                                        {
                (yyval.nd) = new node(++cnt,"ClassBodyDeclaration");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2611 "y.tab.c"
    break;

  case 60:
#line 314 "java.y"
                                {
                (yyval.nd) = new node(++cnt,"ClassBodyDeclaration");
                nodjoin((yyval.nd),(yyvsp[0].nd));
    }
#line 2620 "y.tab.c"
    break;

  case 61:
#line 318 "java.y"
                                     {
                (yyval.nd) = new node(++cnt,"ClassBodyDeclaration");
                nodjoin((yyval.nd),(yyvsp[0].nd));
    }
#line 2629 "y.tab.c"
    break;

  case 62:
#line 324 "java.y"
                                  {
                (yyval.nd) = new node(++cnt,"ClassMemberDeclaration");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2638 "y.tab.c"
    break;

  case 63:
#line 328 "java.y"
                                   {
                (yyval.nd) = new node(++cnt,"ClassMemberDeclaration");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2647 "y.tab.c"
    break;

  case 64:
#line 334 "java.y"
                                                       {
                (yyval.nd) = new node(++cnt,"FieldDeclaration");
                nodjoin((yyval.nd),(yyvsp[-3].nd));
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtolis((yyval.nd),(yyvsp[-1].ndl));
                nodtostr((yyval.nd),(yyvsp[0]. id),"SEMICOLON");
        }
#line 2659 "y.tab.c"
    break;

  case 65:
#line 343 "java.y"
                        {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 2668 "y.tab.c"
    break;

  case 66:
#line 347 "java.y"
                                                {
                (yyval.ndl) = (yyvsp[-2].ndl);
                // $1 = new nodes();                            //here
                // node* new1 = new node(++cnt,$2);
                // nodes* new2 = new nodes(new1);
                (yyvsp[-2].ndl)->push_back(new node(++cnt,(yyvsp[-1]. id)));
                (yyvsp[-2].ndl)->push_back((yyvsp[0].nd));
    }
#line 2681 "y.tab.c"
    break;

  case 67:
#line 357 "java.y"
                               {
                (yyval.nd)  = new node(++cnt,"VarDeclarator");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2690 "y.tab.c"
    break;

  case 68:
#line 361 "java.y"
                                                  {
                (yyval.nd)  = new node(++cnt,"VarDeclarator");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtostr((yyval.nd),(yyvsp[-1]. id),"EQUAL");
                nodjoin((yyval.nd),(yyvsp[0].nd));
    }
#line 2701 "y.tab.c"
    break;

  case 69:
#line 368 "java.y"
                                  {
         (yyval.nd) = new node(++cnt,"VarDeclaratorId");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 2710 "y.tab.c"
    break;

  case 70:
#line 375 "java.y"
                        {
                // $$ = new node(++cnt,"VarDeclaratorId");
                // nodtostr($$,$1,"IDENTIFIER");
                 (yyval.ndl) = new nodes();
                 (yyval.ndl)->push_back(new node(++cnt,(yyvsp[0].id)));
        }
#line 2721 "y.tab.c"
    break;

  case 71:
#line 381 "java.y"
                                                             {
                (yyval.ndl) = new nodes();
                // $$->push_back($1);

                // node* new1 = ;
                // node* new2 = ;     //here
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-1]. id)));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[0]. id)));
        }
#line 2735 "y.tab.c"
    break;

  case 72:
#line 392 "java.y"
                        {
                (yyval.nd) = new node(++cnt,"VarInitializer");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2744 "y.tab.c"
    break;

  case 73:
#line 396 "java.y"
                                {
                (yyval.nd) = new node(++cnt,"VarInitializer");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2753 "y.tab.c"
    break;

  case 74:
#line 402 "java.y"
                                {
                (yyval.nd) = new node(++cnt,"MethodDeclaration");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2763 "y.tab.c"
    break;

  case 75:
#line 409 "java.y"
                                                {
                (yyval.nd) = new node(++cnt,"MethodHeader");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2774 "y.tab.c"
    break;

  case 76:
#line 415 "java.y"
                                                    {
                (yyval.nd) = new node(++cnt,"MethodHeader");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtostr((yyval.nd),(yyvsp[-1].id),"VOID");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2785 "y.tab.c"
    break;

  case 77:
#line 423 "java.y"
                                               {
 (yyval.nd) = new node(++cnt,"FormalParameterListOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 2794 "y.tab.c"
    break;

  case 78:
#line 430 "java.y"
                                {
                (yyval.ndl) = new nodes();
                listpushback((yyval.ndl),(yyvsp[0].ndl));
        }
#line 2803 "y.tab.c"
    break;

  case 79:
#line 434 "java.y"
          {(yyval.ndl) = new nodes();}
#line 2809 "y.tab.c"
    break;

  case 80:
#line 439 "java.y"
                                    {
        (yyval.nd) = new node(++cnt,"MethodDeclarator");
        nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 2818 "y.tab.c"
    break;

  case 81:
#line 445 "java.y"
                                                                           {
                // $$ = new node(++cnt,"MethodDeclarator");
                // nodtostr($$,$1,"IDENTIFIER");
                // nodtostr($$,$2,"LEFTBRACKET");
                // nodjoin($$,$3);
                // nodtostr($$,$4,"RIGHTBRACKET");
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-3].id)));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-2]. id)));
                (yyval.ndl)->push_back((yyvsp[-1].nd));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[0]. id)));
        }
#line 2835 "y.tab.c"
    break;

  case 82:
#line 457 "java.y"
                                                                     {
                (yyval.ndl) = new nodes();
                // node* new1 = ;      //here
                // node* new2 = ;
                (yyval.ndl)->push_back((yyvsp[-2].nd));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-1]. id)));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[0]. id)));
    }
#line 2848 "y.tab.c"
    break;

  case 83:
#line 467 "java.y"
                                {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 2857 "y.tab.c"
    break;

  case 84:
#line 471 "java.y"
                                                        {
                (yyval.ndl) = (yyvsp[-2].ndl);
                node* new1 = new node(++cnt,(yyvsp[-1]. id));          // here
                (yyvsp[-2].ndl)->push_back(new1);
                (yyvsp[-2].ndl)->push_back((yyvsp[0].nd));
        }
#line 2868 "y.tab.c"
    break;

  case 85:
#line 479 "java.y"
                                {
                (yyval.nd) = new node(++cnt,"FormalParameter");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2878 "y.tab.c"
    break;

  case 86:
#line 486 "java.y"
                {
                (yyval.nd) = new node(++cnt,"MethodBody");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2887 "y.tab.c"
    break;

  case 87:
#line 492 "java.y"
                        {
                (yyval.nd) = new node(++cnt,"StaticInitializer");
                nodtostr((yyval.nd),(yyvsp[-1].id),"STATIC");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2897 "y.tab.c"
    break;

  case 88:
#line 499 "java.y"
                                                                {
                (yyval.nd) = new node(++cnt,"ConstructorDeclaration");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2908 "y.tab.c"
    break;

  case 89:
#line 507 "java.y"
                                                                           {
                (yyval.nd) = new node(++cnt,"ConstructorDeclarator");
                nodjoin((yyval.nd),(yyvsp[-3].nd));
                nodtostr((yyval.nd),(yyvsp[-2]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTBRACKET");
        }
#line 2920 "y.tab.c"
    break;

  case 90:
#line 516 "java.y"
                                                                   {
        (yyval.nd) = new node(++cnt,"ExplicitConstructorInvocationOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 2929 "y.tab.c"
    break;

  case 91:
#line 522 "java.y"
                                                {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 2938 "y.tab.c"
    break;

  case 92:
#line 526 "java.y"
           { (yyval.ndl) = new nodes(); }
#line 2944 "y.tab.c"
    break;

  case 93:
#line 530 "java.y"
                                      {
        (yyval.nd) = new node(++cnt,"BlockStatementOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 2953 "y.tab.c"
    break;

  case 94:
#line 536 "java.y"
                        {
                (yyval.ndl) = new nodes();
                listpushback((yyval.ndl),(yyvsp[0].ndl));
        }
#line 2962 "y.tab.c"
    break;

  case 95:
#line 540 "java.y"
          { (yyval.ndl) = new nodes();}
#line 2968 "y.tab.c"
    break;

  case 96:
#line 544 "java.y"
                                  {
        (yyval.nd) = new node(++cnt,"ArgumentListOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 2977 "y.tab.c"
    break;

  case 97:
#line 550 "java.y"
                        {
                (yyval.ndl) = new nodes();
                listpushback((yyval.ndl),(yyvsp[0].ndl));
        }
#line 2986 "y.tab.c"
    break;

  case 98:
#line 554 "java.y"
          { (yyval.ndl) = new nodes(); }
#line 2992 "y.tab.c"
    break;

  case 99:
#line 558 "java.y"
                                                                                              {
                (yyval.nd) = new node(++cnt,"ContructorBody");
                nodtostr((yyval.nd),(yyvsp[-3]. id),"LEFTCURLYBRACKET");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTCURLYBRACKET");
        }
#line 3004 "y.tab.c"
    break;

  case 100:
#line 567 "java.y"
                                                                  {
                (yyval.nd) = new node(++cnt,"ExplicitConstructorInvocation");
                nodtostr((yyval.nd),(yyvsp[-4].id),"THIS");
                nodtostr((yyval.nd),(yyvsp[-3]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtostr((yyval.nd),(yyvsp[-1]. id),"RIGHTBRACKET");
                nodtostr((yyval.nd),(yyvsp[0]. id),"SEMICOLON");
        }
#line 3017 "y.tab.c"
    break;

  case 101:
#line 575 "java.y"
                                                                  {
                (yyval.nd) = new node(++cnt,"ExplicitConstructorInvocation");
                nodtostr((yyval.nd),(yyvsp[-4].id),"SUPER");
                nodtostr((yyval.nd),(yyvsp[-3]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtostr((yyval.nd),(yyvsp[-1]. id),"RIGHTBRACKET");
                nodtostr((yyval.nd),(yyvsp[0]. id),"SEMICOLON");
    }
#line 3030 "y.tab.c"
    break;

  case 102:
#line 585 "java.y"
                                       {
        (yyval.nd) = new node(++cnt,"VarInitializersOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 3039 "y.tab.c"
    break;

  case 103:
#line 592 "java.y"
                               {
                (yyval.ndl) = new nodes();
                listpushback((yyval.ndl),(yyvsp[0].ndl));
        }
#line 3048 "y.tab.c"
    break;

  case 104:
#line 596 "java.y"
          { (yyval.ndl) = new nodes(); }
#line 3054 "y.tab.c"
    break;

  case 105:
#line 600 "java.y"
                    {
        (yyval.nd) = new node(++cnt,"CommaOpt");
        nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 3063 "y.tab.c"
    break;

  case 106:
#line 606 "java.y"
                        {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[0]. id)));
        }
#line 3072 "y.tab.c"
    break;

  case 107:
#line 610 "java.y"
          { (yyval.ndl) = new nodes() ; }
#line 3078 "y.tab.c"
    break;

  case 108:
#line 614 "java.y"
                                                                           {
                (yyval.nd) = new node(++cnt,"ArrayInitializer");
                nodtostr((yyval.nd),(yyvsp[-3]. id),"LEFTCURLYBRACKET");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTCURLYBRACKET");
        }
#line 3090 "y.tab.c"
    break;

  case 109:
#line 623 "java.y"
                        {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 3099 "y.tab.c"
    break;

  case 110:
#line 627 "java.y"
                                                  {
                (yyval.ndl) = (yyvsp[-2].ndl);
                (yyvsp[-2].ndl)->push_back((yyvsp[0].nd));
        }
#line 3108 "y.tab.c"
    break;

  case 111:
#line 633 "java.y"
                                                                   {
                (yyval.nd) = new node(++cnt,"Block");
                nodtostr((yyval.nd),(yyvsp[-2]. id),"LEFTCURLYBRACKET");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTCURLYBRACKET");
        }
#line 3119 "y.tab.c"
    break;

  case 112:
#line 641 "java.y"
                                {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 3128 "y.tab.c"
    break;

  case 113:
#line 645 "java.y"
                                        {
                (yyval.ndl) = (yyvsp[-1].ndl);
                (yyvsp[-1].ndl)->push_back((yyvsp[0].nd));
    }
#line 3137 "y.tab.c"
    break;

  case 114:
#line 651 "java.y"
                                                {
                (yyval.nd) = new node(++cnt,"BlockStatement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3146 "y.tab.c"
    break;

  case 115:
#line 655 "java.y"
                        {
                (yyval.nd) = new node(++cnt,"BlockStatement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
    }
#line 3155 "y.tab.c"
    break;

  case 116:
#line 661 "java.y"
                                                  {
                (yyval.nd) = new node(++cnt,"LocalVariableDeclarationStatement");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"SEMICOLON");
        }
#line 3165 "y.tab.c"
    break;

  case 117:
#line 667 "java.y"
                                                   {
                (yyval.nd) = new node(++cnt,"LocalVariableDeclaration");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtolis((yyval.nd),(yyvsp[0].ndl));
                }
#line 3175 "y.tab.c"
    break;

  case 118:
#line 673 "java.y"
                                        {(yyval.ndl) = new nodes(); (yyval.ndl)->push_back((yyvsp[0].nd));}
#line 3181 "y.tab.c"
    break;

  case 119:
#line 674 "java.y"
                                                               {(yyval.ndl)=(yyvsp[-2].ndl); (yyvsp[-2].ndl)->push_back((yyvsp[0].nd));}
#line 3187 "y.tab.c"
    break;

  case 120:
#line 677 "java.y"
                                         { (yyval.nd) = new node(++cnt,"VariableDeclarator"); nodjoin((yyval.nd),(yyvsp[0].nd));}
#line 3193 "y.tab.c"
    break;

  case 121:
#line 678 "java.y"
                                                                 { (yyval.nd) = new node(++cnt,"VariableDeclarator"); nodjoin((yyval.nd),(yyvsp[-2].nd)); nodtostr((yyval.nd),(yyvsp[-1]. id),"EQUAL"); nodjoin((yyval.nd),(yyvsp[0].nd));}
#line 3199 "y.tab.c"
    break;

  case 122:
#line 681 "java.y"
                                { (yyval.nd) = new node(++cnt,"VariableInitializer"); nodjoin((yyval.nd),(yyvsp[0].nd));}
#line 3205 "y.tab.c"
    break;

  case 123:
#line 682 "java.y"
                                   { (yyval.nd) = new node(++cnt,"VariableInitializer"); nodjoin((yyval.nd),(yyvsp[0].nd));}
#line 3211 "y.tab.c"
    break;

  case 124:
#line 685 "java.y"
                                 {(yyval.nd) = new node(++cnt,"VariableDeclaratorId"); nodtostr((yyval.nd),(yyvsp[0].id),"IDENTIFIER");}
#line 3217 "y.tab.c"
    break;

  case 125:
#line 686 "java.y"
                                                                            {(yyval.nd) = (yyvsp[-2].nd); node* n1 = new node(++cnt,(yyvsp[-1]. id)); node* n2 = new node(++cnt,(yyvsp[0]. id)); nodjoin((yyval.nd),n1); nodjoin((yyval.nd),n2);}
#line 3223 "y.tab.c"
    break;

  case 126:
#line 690 "java.y"
                                                {
                (yyval.nd) = new node(++cnt,"Statement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3232 "y.tab.c"
    break;

  case 127:
#line 694 "java.y"
                                {
                (yyval.nd) = new node(++cnt,"Statement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3241 "y.tab.c"
    break;

  case 128:
#line 698 "java.y"
                               {
                (yyval.nd) = new node(++cnt,"Statement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3250 "y.tab.c"
    break;

  case 129:
#line 702 "java.y"
                                   {
                (yyval.nd) = new node(++cnt,"Statement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3259 "y.tab.c"
    break;

  case 130:
#line 706 "java.y"
                              {
                (yyval.nd) = new node(++cnt,"Statement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3268 "y.tab.c"
    break;

  case 131:
#line 710 "java.y"
                            {
                (yyval.nd) = new node(++cnt,"Statement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3277 "y.tab.c"
    break;

  case 132:
#line 714 "java.y"
                                {
                (yyval.nd) = new node(++cnt,"Statement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3286 "y.tab.c"
    break;

  case 133:
#line 720 "java.y"
                                                                               {
                        (yyval.nd) = new node(++cnt,"PrintlnStatement");
                        nodtostr((yyval.nd),(yyvsp[-4]. id),"SYSTEMOUTPRINTLN");
                        nodtostr((yyval.nd),(yyvsp[-3]. id),"LEFTBRACKET");
                        nodjoin((yyval.nd),(yyvsp[-2].nd));
                        nodtostr((yyval.nd),(yyvsp[-1]. id),"RIGHTBRACKET");
                        nodtostr((yyval.nd),(yyvsp[0]. id),"SEMICOLON");
                }
#line 3299 "y.tab.c"
    break;

  case 134:
#line 730 "java.y"
                                                {
                (yyval.nd) = new node(++cnt,"StatementNoShortIf");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3308 "y.tab.c"
    break;

  case 135:
#line 734 "java.y"
                                         {
                (yyval.nd) = new node(++cnt,"StatementNoShortIf");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3317 "y.tab.c"
    break;

  case 136:
#line 738 "java.y"
                                            {
                (yyval.nd) = new node(++cnt,"StatementNoShortIf");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3326 "y.tab.c"
    break;

  case 137:
#line 742 "java.y"
                                       {
                (yyval.nd) = new node(++cnt,"StatementNoShortIf");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3335 "y.tab.c"
    break;

  case 138:
#line 746 "java.y"
                                     {
                (yyval.nd) = new node(++cnt,"StatementNoShortIf");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3344 "y.tab.c"
    break;

  case 139:
#line 753 "java.y"
                 {
                (yyval.nd) = new node(++cnt,"StatementWithoutTrailingSubstatement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3353 "y.tab.c"
    break;

  case 140:
#line 757 "java.y"
                              {
                (yyval.nd) = new node(++cnt,"StatementWithoutTrailingSubstatement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3362 "y.tab.c"
    break;

  case 141:
#line 761 "java.y"
                                   {
                (yyval.nd) = new node(++cnt,"StatementWithoutTrailingSubstatement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3371 "y.tab.c"
    break;

  case 142:
#line 765 "java.y"
                              {
                (yyval.nd) = new node(++cnt,"StatementWithoutTrailingSubstatement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3380 "y.tab.c"
    break;

  case 143:
#line 769 "java.y"
                                 {
                (yyval.nd) = new node(++cnt,"StatementWithoutTrailingSubstatement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3389 "y.tab.c"
    break;

  case 144:
#line 773 "java.y"
                               {
                (yyval.nd) = new node(++cnt,"StatementWithoutTrailingSubstatement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3398 "y.tab.c"
    break;

  case 145:
#line 779 "java.y"
                     {
                (yyval.nd) = new node(++cnt,"EmptyStatement");
                nodtostr((yyval.nd),(yyvsp[0]. id),"SEMICOLON");
        }
#line 3407 "y.tab.c"
    break;

  case 146:
#line 785 "java.y"
                                      {
                (yyval.nd) = new node(++cnt,"LabeledStatement");
                nodtostr((yyval.nd),(yyvsp[-2].id),"IDENTIFIER");
                nodtostr((yyval.nd),(yyvsp[-1]. id),"COLON");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3418 "y.tab.c"
    break;

  case 147:
#line 793 "java.y"
                                               {
                (yyval.nd) = new node(++cnt,"LabeledStatementNoShortIf");
                nodtostr((yyval.nd),(yyvsp[-2].id),"IDENTIFIER");
                nodtostr((yyval.nd),(yyvsp[-1]. id),"COLON");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3429 "y.tab.c"
    break;

  case 148:
#line 801 "java.y"
                                         {
                (yyval.nd) = new node(++cnt,"ExpressionStatement");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"SEMICOLON");
        }
#line 3439 "y.tab.c"
    break;

  case 149:
#line 808 "java.y"
                      {
                (yyval.nd) = new node(++cnt,"StatementExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3448 "y.tab.c"
    break;

  case 150:
#line 812 "java.y"
                                  {
                (yyval.nd) = new node(++cnt,"StatementExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3457 "y.tab.c"
    break;

  case 151:
#line 816 "java.y"
                                  {
                (yyval.nd) = new node(++cnt,"StatementExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3466 "y.tab.c"
    break;

  case 152:
#line 820 "java.y"
                                   {
                (yyval.nd) = new node(++cnt,"StatementExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3475 "y.tab.c"
    break;

  case 153:
#line 824 "java.y"
                                   {
                (yyval.nd) = new node(++cnt,"StatementExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3484 "y.tab.c"
    break;

  case 154:
#line 828 "java.y"
                            {
                (yyval.nd) = new node(++cnt,"StatementExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3493 "y.tab.c"
    break;

  case 155:
#line 832 "java.y"
                                           {
                (yyval.nd) = new node(++cnt,"StatementExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3502 "y.tab.c"
    break;

  case 156:
#line 838 "java.y"
                                                                                        {
        (yyval.nd)=new node(++cnt,"ClassInstanceCreationExpression");
        nodtostr((yyval.nd),(yyvsp[-4].id),"NEW");
        nodjoin((yyval.nd),(yyvsp[-3].nd));
        nodtostr((yyval.nd),(yyvsp[-2]. id),"LEFTBRACKET");
        nodjoin((yyval.nd),(yyvsp[-1].nd));
        nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTBRACKET");
}
#line 3515 "y.tab.c"
    break;

  case 157:
#line 848 "java.y"
                                       {(yyval.nd)=new node(++cnt,"ClassType"); nodjoin((yyval.nd),(yyvsp[0].nd));}
#line 3521 "y.tab.c"
    break;

  case 158:
#line 851 "java.y"
                           {(yyval.nd)=new node(++cnt,"ClassOrInterfaceType"); nodjoin((yyval.nd),(yyvsp[0].nd));}
#line 3527 "y.tab.c"
    break;

  case 159:
#line 855 "java.y"
                                                            {
                (yyval.nd) = new node(++cnt,"IfThenStatement");
                nodtostr((yyval.nd),(yyvsp[-4].id),"IF");
                nodtostr((yyval.nd),(yyvsp[-3]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtostr((yyval.nd),(yyvsp[-1]. id),"RIGHTBRACKET");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3540 "y.tab.c"
    break;

  case 160:
#line 865 "java.y"
                                                                                     {
                (yyval.nd) = new node(++cnt,"IfThenElseStatement");
                nodtostr((yyval.nd),(yyvsp[-6].id),"IF");
                nodtostr((yyval.nd),(yyvsp[-5]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-4].nd));
                nodtostr((yyval.nd),(yyvsp[-3]. id),"RIGHTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtostr((yyval.nd),(yyvsp[-1].id),"ELSE");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3555 "y.tab.c"
    break;

  case 161:
#line 877 "java.y"
                                                                                             {
                (yyval.nd) = new node(++cnt,"IfThenElseStatementNoShortIf");
                nodtostr((yyval.nd),(yyvsp[-6].id),"IF");
                nodtostr((yyval.nd),(yyvsp[-5]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-4].nd));
                nodtostr((yyval.nd),(yyvsp[-3]. id),"RIGHTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtostr((yyval.nd),(yyvsp[-1].id),"ELSE");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3570 "y.tab.c"
    break;

  case 162:
#line 889 "java.y"
                                                               {
                (yyval.nd) = new node(++cnt,"WhileStatement");
                nodtostr((yyval.nd),(yyvsp[-4].id),"WHILE");
                nodtostr((yyval.nd),(yyvsp[-3]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtostr((yyval.nd),(yyvsp[-1]. id),"RIGHTBRACKET");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3583 "y.tab.c"
    break;

  case 163:
#line 900 "java.y"
                                                                        {
                (yyval.nd) = new node(++cnt,"WhileStatementNoShortIf");
                nodtostr((yyval.nd),(yyvsp[-4].id),"WHILE");
                nodtostr((yyval.nd),(yyvsp[-3]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtostr((yyval.nd),(yyvsp[-1]. id),"RIGHTBRACKET");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3596 "y.tab.c"
    break;

  case 164:
#line 909 "java.y"
                        {
 (yyval.nd) = new node(++cnt,"ForInitOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 3605 "y.tab.c"
    break;

  case 165:
#line 916 "java.y"
                    {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 3614 "y.tab.c"
    break;

  case 166:
#line 920 "java.y"
          { (yyval.ndl) = new nodes(); }
#line 3620 "y.tab.c"
    break;

  case 167:
#line 924 "java.y"
                              {
        (yyval.nd) = new node(++cnt,"ExpressionOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 3629 "y.tab.c"
    break;

  case 168:
#line 930 "java.y"
                       {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 3638 "y.tab.c"
    break;

  case 169:
#line 934 "java.y"
          { (yyval.ndl) = new nodes(); }
#line 3644 "y.tab.c"
    break;

  case 170:
#line 938 "java.y"
                            {
        (yyval.nd) = new node(++cnt,"ForUpdateOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 3653 "y.tab.c"
    break;

  case 171:
#line 944 "java.y"
                      {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 3662 "y.tab.c"
    break;

  case 172:
#line 948 "java.y"
          { (yyval.ndl) = new nodes(); }
#line 3668 "y.tab.c"
    break;

  case 173:
#line 953 "java.y"
                                                                                                             {
                (yyval.nd) = new node(++cnt,"ForStatement");
                nodtostr((yyval.nd),(yyvsp[-8].id),"FOR");
                nodtostr((yyval.nd),(yyvsp[-7]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-6].nd));
                nodtostr((yyval.nd),(yyvsp[-5]. id),"SEMICOLON");
                nodjoin((yyval.nd),(yyvsp[-4].nd));
                nodtostr((yyval.nd),(yyvsp[-3]. id),"SEMICOLON");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtostr((yyval.nd),(yyvsp[-1]. id),"RIGHTBRACKET");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3685 "y.tab.c"
    break;

  case 174:
#line 968 "java.y"
                                                                                                                 {
                (yyval.nd) = new node(++cnt,"ForStatement");
                nodtostr((yyval.nd),(yyvsp[-8].id),"FOR");
                nodtostr((yyval.nd),(yyvsp[-7]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-6].nd));
                nodtostr((yyval.nd),(yyvsp[-5]. id),"SEMICOLON");
                nodjoin((yyval.nd),(yyvsp[-4].nd));
                nodtostr((yyval.nd),(yyvsp[-3]. id),"SEMICOLON");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtostr((yyval.nd),(yyvsp[-1]. id),"RIGHTBRACKET");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3702 "y.tab.c"
    break;

  case 175:
#line 984 "java.y"
                                     {        //dekhna hai
                (yyval.nd) = new node(++cnt,"ForInit");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
        }
#line 3711 "y.tab.c"
    break;

  case 176:
#line 988 "java.y"
                                          {
                (yyval.nd) = new node(++cnt,"ForInit");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3720 "y.tab.c"
    break;

  case 177:
#line 995 "java.y"
                                    {
                (yyval.nd) = new node(++cnt,"ForInitOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
        }
#line 3729 "y.tab.c"
    break;

  case 178:
#line 1002 "java.y"
                                {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 3738 "y.tab.c"
    break;

  case 179:
#line 1006 "java.y"
                                                                {
                (yyval.ndl) = (yyvsp[-2].ndl);
                (yyvsp[-2].ndl)->push_back((yyvsp[0].nd));
        }
#line 3747 "y.tab.c"
    break;

  case 180:
#line 1012 "java.y"
                    {
        (yyval.nd) = new node(++cnt,"IDENTOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 3756 "y.tab.c"
    break;

  case 181:
#line 1017 "java.y"
                         {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[0].id)));
        }
#line 3765 "y.tab.c"
    break;

  case 182:
#line 1021 "java.y"
           { (yyval.ndl) = new nodes(); }
#line 3771 "y.tab.c"
    break;

  case 183:
#line 1026 "java.y"
                                    {
                (yyval.nd) = new node(++cnt,"BreakStatement");
                nodtostr((yyval.nd),(yyvsp[-2].id),"BREAK");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"SEMICOLON");
        }
#line 3782 "y.tab.c"
    break;

  case 184:
#line 1035 "java.y"
                                       {
                (yyval.nd) = new node(++cnt,"ContinueStatement");
                nodtostr((yyval.nd),(yyvsp[-2].id),"CONTINUE");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"SEMICOLON");
        }
#line 3793 "y.tab.c"
    break;

  case 185:
#line 1044 "java.y"
                                          {
                (yyval.nd) = new node(++cnt,"ReturnStatement");
                nodtostr((yyval.nd),(yyvsp[-2].id),"RETURN");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"SEMICOLON");
        }
#line 3804 "y.tab.c"
    break;

  case 186:
#line 1050 "java.y"
                                               {
                (yyval.nd) = new node(++cnt,"ReturnStatement");
                nodtostr((yyval.nd),(yyvsp[-2].id),"SUSPEND");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"SEMICOLON");
        }
#line 3815 "y.tab.c"
    break;

  case 187:
#line 1058 "java.y"
                             {
                (yyval.nd) = new node(++cnt,"Primary");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3824 "y.tab.c"
    break;

  case 188:
#line 1062 "java.y"
                                   {
                (yyval.nd) = new node(++cnt,"Primary");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3833 "y.tab.c"
    break;

  case 189:
#line 1068 "java.y"
                   {
                (yyval.nd) = new node(++cnt,"PrimaryNoNewArray");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3842 "y.tab.c"
    break;

  case 190:
#line 1072 "java.y"
                {
                (yyval.nd) = new node(++cnt,"PrimaryNoNewArray");
                nodtostr((yyval.nd),(yyvsp[0].id),"THIS");
        }
#line 3851 "y.tab.c"
    break;

  case 191:
#line 1076 "java.y"
                                               {
                (yyval.nd) = new node(++cnt,"PrimaryNoNewArray");
                nodtostr((yyval.nd),(yyvsp[-2]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTBRACKET");
        }
#line 3862 "y.tab.c"
    break;

  case 192:
#line 1082 "java.y"
                       {
                (yyval.nd) = new node(++cnt,"PrimaryNoNewArray");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3871 "y.tab.c"
    break;

  case 193:
#line 1086 "java.y"
                            {
                (yyval.nd) = new node(++cnt,"PrimaryNoNewArray");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3880 "y.tab.c"
    break;

  case 194:
#line 1090 "java.y"
                       {
                (yyval.nd) = new node(++cnt,"PrimaryNoNewArray");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3889 "y.tab.c"
    break;

  case 195:
#line 1096 "java.y"
                      {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 3898 "y.tab.c"
    break;

  case 196:
#line 1100 "java.y"
                                                {
                (yyval.ndl) = (yyvsp[-2].ndl);
                (yyvsp[-2].ndl)->push_back((yyvsp[0].nd));
        }
#line 3907 "y.tab.c"
    break;

  case 197:
#line 1120 "java.y"
                                             {
                (yyval.nd) = new node(++cnt,"ArrayCreationExpression");
                nodtostr((yyval.nd),(yyvsp[-3].id),"NEW");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtolis((yyval.nd),(yyvsp[-1].ndl));
                nodtolis((yyval.nd),(yyvsp[0].ndl));
        }
#line 3919 "y.tab.c"
    break;

  case 198:
#line 1127 "java.y"
                                         {
                (yyval.nd) = new node(++cnt,"ArrayCreationExpression");
                nodtostr((yyval.nd),(yyvsp[-2].id),"NEW");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtolis((yyval.nd),(yyvsp[0].ndl));
        }
#line 3930 "y.tab.c"
    break;

  case 199:
#line 1135 "java.y"
                {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 3939 "y.tab.c"
    break;

  case 200:
#line 1139 "java.y"
                                {
                (yyval.ndl) = (yyvsp[-1].ndl);
                (yyvsp[-1].ndl)->push_back((yyvsp[0].nd));
        }
#line 3948 "y.tab.c"
    break;

  case 201:
#line 1145 "java.y"
                                                             {
                (yyval.nd) = new node(++cnt,"DimExpr");
                nodtostr((yyval.nd),(yyvsp[-2]. id),"LEFTSQUAREBRACKET");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTSQUAREBRACKET");
        }
#line 3959 "y.tab.c"
    break;

  case 202:
#line 1153 "java.y"
                                                     {
                (yyval.ndl) = new nodes();
                node* n1 = new node(++cnt,(yyvsp[-1]. id));
                node* n2 = new node(++cnt,(yyvsp[0]. id));                          //here
                (yyval.ndl)->push_back(n1);
                (yyval.ndl)->push_back(n2);
        }
#line 3971 "y.tab.c"
    break;

  case 203:
#line 1160 "java.y"
                                                     {
                (yyval.ndl) = (yyvsp[-2].ndl);
                node* n1 = new node(++cnt,(yyvsp[-1]. id));
                node* n2 = new node(++cnt,(yyvsp[0]. id));
                (yyvsp[-2].ndl)->push_back(n1);
                (yyvsp[-2].ndl)->push_back(n2);

    }
#line 3984 "y.tab.c"
    break;

  case 204:
#line 1170 "java.y"
                                        {
                (yyval.nd) = new node(++cnt,"FieldAccess");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtostr((yyval.nd),(yyvsp[-1]. id),"DOT");
                nodtostr((yyval.nd),(yyvsp[0].id),"IDENTIFIER");
        }
#line 3995 "y.tab.c"
    break;

  case 205:
#line 1176 "java.y"
                                        {
                (yyval.nd) = new node(++cnt,"FieldAccess");
                nodtostr((yyval.nd),(yyvsp[-2].id),"SUPER");
                nodtostr((yyval.nd),(yyvsp[-1]. id),"DOT");
                nodtostr((yyval.nd),(yyvsp[0].id),"IDENTIFIER");
        }
#line 4006 "y.tab.c"
    break;

  case 206:
#line 1184 "java.y"
                                                           {
                (yyval.nd) = new node(++cnt,"MethodInvocation");
                nodjoin((yyval.nd),(yyvsp[-3].nd));
                nodtostr((yyval.nd),(yyvsp[-2]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTBRACKET");
        }
#line 4018 "y.tab.c"
    break;

  case 207:
#line 1191 "java.y"
                                                                                 {
                (yyval.nd) = new node(++cnt,"MethodInvocation");
                nodjoin((yyval.nd),(yyvsp[-5].nd));
                nodtostr((yyval.nd),(yyvsp[-4]. id),"DOT");
                nodtostr((yyval.nd),(yyvsp[-3].id),"IDENTIFIER");
                nodtostr((yyval.nd),(yyvsp[-2]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTBRACKET");
        }
#line 4032 "y.tab.c"
    break;

  case 208:
#line 1200 "java.y"
                                                                               {
                (yyval.nd) = new node(++cnt,"MethodInvocation");
                nodtostr((yyval.nd),(yyvsp[-5].id),"SUPER");
                nodtostr((yyval.nd),(yyvsp[-4]. id),"DOT");
                nodtostr((yyval.nd),(yyvsp[-3].id),"IDENTIFIER");
                nodtostr((yyval.nd),(yyvsp[-2]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTBRACKET");
        }
#line 4046 "y.tab.c"
    break;

  case 209:
#line 1209 "java.y"
                                                                         {
                (yyval.nd) = new node(++cnt,"MethodInvocation");
                nodjoin((yyval.nd),(yyvsp[-3].nd));
                nodtostr((yyval.nd),(yyvsp[-2]. id),"LEFTCURLYBRACKET");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTCURLYBRACKET");
        }
#line 4058 "y.tab.c"
    break;

  case 210:
#line 1216 "java.y"
                                                                                           {
                (yyval.nd) = new node(++cnt,"MethodInvocation");
                nodjoin((yyval.nd),(yyvsp[-5].nd));
                nodtostr((yyval.nd),(yyvsp[-4]. id),"DOT");
                nodtostr((yyval.nd),(yyvsp[-3].id),"IDENTIFIER");
                nodtostr((yyval.nd),(yyvsp[-2]. id),"LEFTCURLYBRACKET");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTCURLYBRACKET");
        }
#line 4072 "y.tab.c"
    break;

  case 211:
#line 1225 "java.y"
                                                                                         {
                (yyval.nd) = new node(++cnt,"MethodInvocation");
                nodtostr((yyval.nd),(yyvsp[-5].id),"SUPER");
                nodtostr((yyval.nd),(yyvsp[-4]. id),"DOT");
                nodtostr((yyval.nd),(yyvsp[-3].id),"IDENTIFIER");
                nodtostr((yyval.nd),(yyvsp[-2]. id),"LEFTCURLYBRACKET");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTCURLYBRACKET");
        }
#line 4086 "y.tab.c"
    break;

  case 212:
#line 1236 "java.y"
                                                                  {
                (yyval.nd) = new node(++cnt,"ArrayAcces");
                nodjoin((yyval.nd),(yyvsp[-3].nd));
                nodtostr((yyval.nd),(yyvsp[-2]. id),"LEFTSQUAREBRACKET");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTSQUAREBRACKET");
        }
#line 4098 "y.tab.c"
    break;

  case 213:
#line 1243 "java.y"
                                                                                   {
                (yyval.nd) = new node(++cnt,"ArrayAcces");
                nodjoin((yyval.nd),(yyvsp[-3].nd));
                nodtostr((yyval.nd),(yyvsp[-2]. id),"LEFTSQUAREBRACKET");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTSQUAREBRACKET");
        }
#line 4110 "y.tab.c"
    break;

  case 214:
#line 1253 "java.y"
                     {
                (yyval.nd) = new node(++cnt,"PostFixExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4119 "y.tab.c"
    break;

  case 215:
#line 1257 "java.y"
                      {
                (yyval.nd) = new node(++cnt,"PostFixExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4128 "y.tab.c"
    break;

  case 216:
#line 1261 "java.y"
                                         {
                (yyval.nd) = new node(++cnt,"PostFixExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4137 "y.tab.c"
    break;

  case 217:
#line 1265 "java.y"
                                         {
                (yyval.nd) = new node(++cnt,"PostFixExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4146 "y.tab.c"
    break;

  case 218:
#line 1272 "java.y"
                                      {
                (yyval.nd) = new node(++cnt,"PostIncrementExpression");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"ADDADD");
        }
#line 4156 "y.tab.c"
    break;

  case 219:
#line 1280 "java.y"
                                      {
                (yyval.nd) = new node(++cnt,"PostDecrementExpression");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"SUBSUB");
        }
#line 4166 "y.tab.c"
    break;

  case 220:
#line 1288 "java.y"
                                    {
                (yyval.nd) = new node(++cnt,"UnaryExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4175 "y.tab.c"
    break;

  case 221:
#line 1292 "java.y"
                                        {
                (yyval.nd) = new node(++cnt,"UnaryExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4184 "y.tab.c"
    break;

  case 222:
#line 1296 "java.y"
                                     {
                (yyval.nd) = new node(++cnt,"UnaryExpression");
                nodtostr((yyval.nd),(yyvsp[-1]. id),"ADD");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4194 "y.tab.c"
    break;

  case 223:
#line 1301 "java.y"
                                     {
                (yyval.nd) = new node(++cnt,"UnaryExpression");
                nodtostr((yyval.nd),(yyvsp[-1]. id),"SUB");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4204 "y.tab.c"
    break;

  case 224:
#line 1306 "java.y"
                                             {
                (yyval.nd) = new node(++cnt,"UnaryExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4213 "y.tab.c"
    break;

  case 225:
#line 1313 "java.y"
                                    {
                (yyval.nd) = new node(++cnt,"PreIncrementExpression");
                nodtostr((yyval.nd),(yyvsp[-1]. id),"ADDADD");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4223 "y.tab.c"
    break;

  case 226:
#line 1321 "java.y"
                                    {
                (yyval.nd) = new node(++cnt,"PreDecrementExpression");
                nodtostr((yyval.nd),(yyvsp[-1]. id),"SUBSUB");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4233 "y.tab.c"
    break;

  case 227:
#line 1329 "java.y"
                               {
                (yyval.nd) = new node(++cnt,"UnaryExpressionNotPlusMinus");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4242 "y.tab.c"
    break;

  case 228:
#line 1333 "java.y"
                                       {
                (yyval.nd) = new node(++cnt,"UnaryExpressionNotPlusMinus");
                nodtostr((yyval.nd),(yyvsp[-1]. id),"TILDE");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4252 "y.tab.c"
    break;

  case 229:
#line 1338 "java.y"
                                     {
                (yyval.nd) = new node(++cnt,"UnaryExpressionNotPlusMinus");
                nodtostr((yyval.nd),(yyvsp[-1]. id),"NOT");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4262 "y.tab.c"
    break;

  case 230:
#line 1343 "java.y"
                                {
                (yyval.nd) = new node(++cnt,"UnaryExpressionNotPlusMinus");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4271 "y.tab.c"
    break;

  case 231:
#line 1358 "java.y"
                                                                                 {
                (yyval.nd) = new node(++cnt,"CastExpression");
                nodtostr((yyval.nd),(yyvsp[-3]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtostr((yyval.nd),(yyvsp[-1]. id),"RIGHTBRACKET");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4283 "y.tab.c"
    break;

  case 232:
#line 1365 "java.y"
                                                                                {
                (yyval.nd) = new node(++cnt,"CastExpression");
                nodtostr((yyval.nd),(yyvsp[-4]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-3].nd));
                nodtolis((yyval.nd),(yyvsp[-2].ndl));
                nodtostr((yyval.nd),(yyvsp[-1]. id),"RIGHTBRACKET");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4296 "y.tab.c"
    break;

  case 233:
#line 1375 "java.y"
                                                     {
        (yyval.nd)=new node(++cnt,"MultiplicativeExpression");
        nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 4305 "y.tab.c"
    break;

  case 234:
#line 1382 "java.y"
                             {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4314 "y.tab.c"
    break;

  case 235:
#line 1386 "java.y"
                                                                {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[-2].nd));
                // node* n1 =;
                (yyval.ndl)->push_back( new node(++cnt,(yyvsp[-1]. id)));
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4326 "y.tab.c"
    break;

  case 236:
#line 1393 "java.y"
                                                                {
                // $$ = $1;
                // node* n1 = ;
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[-2].nd));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-1]. id)));
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4339 "y.tab.c"
    break;

  case 237:
#line 1401 "java.y"
                                                                {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[-2].nd));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-1]. id)));
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4350 "y.tab.c"
    break;

  case 238:
#line 1409 "java.y"
                                       {
        (yyval.nd) = new node(++cnt,"AdditiveExpression");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 4359 "y.tab.c"
    break;

  case 239:
#line 1416 "java.y"
                                      {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4368 "y.tab.c"
    break;

  case 240:
#line 1420 "java.y"
                                                                   {
                // $$ = $1;
                // // node* n1 = ;
                // $1->push_back(new node(++cnt,$2));
                // $1->push_back($3);
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[-2].nd));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-1]. id)));
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4383 "y.tab.c"
    break;

  case 241:
#line 1430 "java.y"
                                                                    {
                // $$ = $1;
                // // node* n1 = ;
                // $1->push_back(new node(++cnt,$2));
                // $1->push_back($3);
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[-2].nd));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-1]. id)));
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4398 "y.tab.c"
    break;

  case 242:
#line 1442 "java.y"
                                 {
        (yyval.nd) = new node(++cnt,"ShiftExpression");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 4407 "y.tab.c"
    break;

  case 243:
#line 1449 "java.y"
                                {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4416 "y.tab.c"
    break;

  case 244:
#line 1453 "java.y"
                                                                 {
                // $$ = $1;
                // // node* n1 = ;
                // $1->push_back(new node(++cnt,$2));
                // $1->push_back($3);
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[-2].nd));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-1]. id)));
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4431 "y.tab.c"
    break;

  case 245:
#line 1463 "java.y"
                                                                    {
                // $$ = $1;
                // // node* n1 = ;
                // $1->push_back(new node(++cnt,$2));
                // $1->push_back($3);
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[-2].nd));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-1]. id)));
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4446 "y.tab.c"
    break;

  case 246:
#line 1473 "java.y"
                                                                    {
                // $$ = $1;
                // // node* n1 = ;
                // $1->push_back(new node(++cnt,$2));
                // $1->push_back($3);
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[-2].nd));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-1]. id)));
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4461 "y.tab.c"
    break;

  case 247:
#line 1485 "java.y"
                                           {
        (yyval.nd) = new node(++cnt,"RelationalExpression");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 4470 "y.tab.c"
    break;

  case 248:
#line 1492 "java.y"
                             {
                // $$ = new node(++cnt,"RelationalExpression");
                // nodjoin($$,$1);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4481 "y.tab.c"
    break;

  case 249:
#line 1498 "java.y"
                                                             {
                // $$ = $1;
                // node* n1 = new node(++cnt,$2);
                // $1->push_back(n1);
                // $1->push_back($3);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[-2].nd));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-1]. id)));
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4496 "y.tab.c"
    break;

  case 250:
#line 1508 "java.y"
                                                                {
                // $$ = $1;
                // node* n1 = new node(++cnt,$2);
                // $1->push_back(n1);
                // $1->push_back($3);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[-2].nd));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-1]. id)));
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4511 "y.tab.c"
    break;

  case 251:
#line 1518 "java.y"
                                                                  {
                // $$ = $1;
                // node* n1 = new node(++cnt,$2);
                // $1->push_back(n1);
                // $1->push_back($3);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[-2].nd));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-1]. id)));
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4526 "y.tab.c"
    break;

  case 252:
#line 1528 "java.y"
                                                                     {
                // $$ = $1;
                // node* n1 = new node(++cnt,$2);
                // $1->push_back(n1);
                // $1->push_back($3);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[-2].nd));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-1]. id)));
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4541 "y.tab.c"
    break;

  case 253:
#line 1540 "java.y"
                                       {
 (yyval.nd) = new node(++cnt,"EqualityExpression");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 4550 "y.tab.c"
    break;

  case 254:
#line 1547 "java.y"
                                  {
                // $$ = new node(++cnt,"EqualityExpression");
                // nodjoin($$,$1);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4561 "y.tab.c"
    break;

  case 255:
#line 1553 "java.y"
                                                                      {
                // $$ = $1;
                // node* n1 = new node(++cnt,$2);
                // $1->push_back(n1);
                // $1->push_back($3);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[-2].nd));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-1]. id)));
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4576 "y.tab.c"
    break;

  case 256:
#line 1563 "java.y"
                                                                    {
                // $$ = $1;
                // node* n1 = new node(++cnt,$2);
                // $1->push_back(n1);
                // $1->push_back($3);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[-2].nd));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-1]. id)));
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4591 "y.tab.c"
    break;

  case 257:
#line 1574 "java.y"
                             {
        (yyval.nd) = new node(++cnt,"AndExpression");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 4600 "y.tab.c"
    break;

  case 258:
#line 1580 "java.y"
                                {
                // $$ = new node(++cnt,"AndExpression");
                // nodjoin($$,$1);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4611 "y.tab.c"
    break;

  case 259:
#line 1586 "java.y"
                                                           {
                // $$ = $1;
                // node* n1 = new node(++cnt,$2);
                // $1->push_back(n1);
                // $1->push_back($3);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[-2].nd));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-1]. id)));
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4626 "y.tab.c"
    break;

  case 260:
#line 1598 "java.y"
                                             {
        (yyval.nd) = new node(++cnt,"ExclusiveOrExpression");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 4635 "y.tab.c"
    break;

  case 261:
#line 1604 "java.y"
                           {
                // $$ = new node(++cnt,"ExclusiveOrExpression");
                // nodjoin($$,$1);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4646 "y.tab.c"
    break;

  case 262:
#line 1610 "java.y"
                                                             {
                // $$ = $1;
                // node* n1 = new node(++cnt,$2);
                // $1->push_back(n1);
                // $1->push_back($3);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[-2].nd));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-1]. id)));
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4661 "y.tab.c"
    break;

  case 263:
#line 1622 "java.y"
                                             {
        (yyval.nd) = new node(++cnt,"InclusiveOrExpression");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 4670 "y.tab.c"
    break;

  case 264:
#line 1629 "java.y"
                                   {
                // $$ = new node(++cnt,"InclusiveOrExpression");
                // nodjoin($$,$1);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4681 "y.tab.c"
    break;

  case 265:
#line 1635 "java.y"
                                                                     {
                // $$ = $1;
                // node* n1 = new node(++cnt,$2);
                // $1->push_back(n1);
                // $1->push_back($3);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[-2].nd));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-1]. id)));
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4696 "y.tab.c"
    break;

  case 266:
#line 1647 "java.y"
                                                   {
        (yyval.nd) = new node(++cnt,"ConditionalAndExpression");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 4705 "y.tab.c"
    break;

  case 267:
#line 1654 "java.y"
                                   {
                // $$ = new node(++cnt,"conditionalAndExpression");
                // nodjoin($$,$1);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4716 "y.tab.c"
    break;

  case 268:
#line 1660 "java.y"
                                                                      {
                // $$ = $1;
                // node* n1 = new node(++cnt,$2);
                // $1->push_back(n1);
                // $1->push_back($3);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[-2].nd));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-1]. id)));
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4731 "y.tab.c"
    break;

  case 269:
#line 1672 "java.y"
                                                 {
        (yyval.nd) = new node(++cnt,"ConditionalOrExpression");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 4740 "y.tab.c"
    break;

  case 270:
#line 1679 "java.y"
                                      {
                // $$ = new node(++cnt,"ConditionalOrExpression");
                // nodjoin($$,$1);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4751 "y.tab.c"
    break;

  case 271:
#line 1685 "java.y"
                                                                       {
                // $$ = $1;
                // node* n1 = new node(++cnt,$2);
                // $1->push_back(n1);
                // $1->push_back($3);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[-2].nd));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-1]. id)));
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4766 "y.tab.c"
    break;

  case 272:
#line 1697 "java.y"
                                               {
        (yyval.nd) = new node(++cnt,"ConditionalExpression");
        nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 4775 "y.tab.c"
    break;

  case 273:
#line 1703 "java.y"
                                                     {
                // $$ = new node(++cnt,"ConditionalExpression");
                // nodjoin($$,$1);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4786 "y.tab.c"
    break;

  case 274:
#line 1709 "java.y"
                                                                                           {
                // $$ = $1;
                // node* n1 = new node(++cnt,$2);
                // $1->push_back(n1);
                // $1->push_back($3);
                // node* n2 = new node(++cnt,$3);
                // $1->push_back(n2);
                // $1->push_back($5);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[-4].nd));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-3]. id)));
                (yyval.ndl)->push_back((yyvsp[-2].nd));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-1]. id)));
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4806 "y.tab.c"
    break;

  case 275:
#line 1727 "java.y"
                                   {
                (yyval.nd) = new node(++cnt,"AssignmentExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4815 "y.tab.c"
    break;

  case 276:
#line 1731 "java.y"
                            {
                (yyval.nd) = new node(++cnt,"AssignmentExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4824 "y.tab.c"
    break;

  case 277:
#line 1738 "java.y"
                                                                  {
                (yyval.nd) = new node(++cnt,"Assignment");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4835 "y.tab.c"
    break;

  case 278:
#line 1747 "java.y"
                      {
                (yyval.nd) = new node(++cnt,"LeftHandSide");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4844 "y.tab.c"
    break;

  case 279:
#line 1751 "java.y"
                                 {
                (yyval.nd) = new node(++cnt,"LeftHandSide");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4853 "y.tab.c"
    break;

  case 280:
#line 1755 "java.y"
                                 {
                (yyval.nd) = new node(++cnt,"LeftHandSide");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4862 "y.tab.c"
    break;

  case 281:
#line 1762 "java.y"
                       {
                (yyval.nd) = new node(++cnt,"AssignmentOperator");
                nodtostr((yyval.nd),(yyvsp[0]. id),"EQUAL");
        }
#line 4871 "y.tab.c"
    break;

  case 282:
#line 1766 "java.y"
                          {
                (yyval.nd) = new node(++cnt,"AssignmentOperator");
                nodtostr((yyval.nd),(yyvsp[0]. id),"MULEQUAL");
        }
#line 4880 "y.tab.c"
    break;

  case 283:
#line 1770 "java.y"
                          {
                (yyval.nd) = new node(++cnt,"AssignmentOperator");
                nodtostr((yyval.nd),(yyvsp[0]. id),"DIVEQUAL");
        }
#line 4889 "y.tab.c"
    break;

  case 284:
#line 1774 "java.y"
                          {
                (yyval.nd) = new node(++cnt,"AssignmentOperator");
                nodtostr((yyval.nd),(yyvsp[0]. id),"SUBEQUAL");
        }
#line 4898 "y.tab.c"
    break;

  case 285:
#line 1778 "java.y"
                          {
                (yyval.nd) = new node(++cnt,"AssignmentOperator");
                nodtostr((yyval.nd),(yyvsp[0]. id),"ANDEQUAL");
        }
#line 4907 "y.tab.c"
    break;

  case 286:
#line 1782 "java.y"
                          {
                (yyval.nd) = new node(++cnt,"AssignmentOperator");
                nodtostr((yyval.nd),(yyvsp[0]. id),"ADDEQUAL");
        }
#line 4916 "y.tab.c"
    break;

  case 287:
#line 1789 "java.y"
                                      {
                (yyval.nd) = new node(++cnt,"Expression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4925 "y.tab.c"
    break;


#line 4929 "y.tab.c"

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
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

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
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *, YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
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

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
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
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;


#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif


/*-----------------------------------------------------.
| yyreturn -- parsing is finished, return the result.  |
`-----------------------------------------------------*/
yyreturn:
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
                  yystos[+*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 1795 "java.y"


int main(void) {
    yyparse();
    return 0;
}

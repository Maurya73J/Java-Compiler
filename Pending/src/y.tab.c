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
#define YYLAST   1089

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  131
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  151
/* YYNRULES -- Number of rules.  */
#define YYNRULES  281
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  449

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
      82,    92,    98,   105,   109,   115,   121,   129,   135,   142,
     146,   150,   157,   161,   165,   169,   175,   179,   185,   189,
     195,   205,   215,   221,   225,   231,   235,   239,   245,   252,
     262,   268,   272,   276,   284,   291,   295,   299,   303,   309,
     313,   317,   323,   327,   333,   342,   346,   356,   360,   367,
     374,   380,   391,   395,   401,   408,   414,   422,   429,   433,
     438,   444,   456,   466,   470,   478,   485,   491,   498,   506,
     515,   521,   525,   529,   535,   539,   543,   549,   553,   557,
     566,   574,   584,   591,   595,   599,   605,   609,   613,   622,
     626,   632,   640,   644,   650,   654,   660,   666,   672,   673,
     676,   677,   680,   681,   684,   685,   689,   693,   697,   701,
     705,   709,   713,   719,   729,   733,   737,   741,   745,   752,
     756,   760,   764,   768,   772,   778,   784,   792,   800,   807,
     811,   815,   819,   823,   827,   831,   837,   847,   850,   854,
     864,   876,   888,   899,   908,   915,   919,   923,   929,   933,
     937,   943,   947,   952,   967,   983,   987,   994,  1001,  1005,
    1011,  1016,  1020,  1025,  1034,  1043,  1049,  1057,  1067,  1071,
    1075,  1081,  1085,  1089,  1095,  1099,  1146,  1153,  1163,  1169,
    1177,  1184,  1193,  1202,  1209,  1218,  1229,  1236,  1246,  1250,
    1254,  1258,  1265,  1273,  1281,  1285,  1289,  1294,  1299,  1306,
    1314,  1322,  1326,  1331,  1336,  1351,  1358,  1368,  1375,  1379,
    1386,  1394,  1402,  1409,  1413,  1423,  1435,  1442,  1446,  1456,
    1466,  1478,  1485,  1491,  1501,  1511,  1521,  1533,  1540,  1546,
    1556,  1567,  1573,  1579,  1591,  1597,  1603,  1615,  1622,  1628,
    1640,  1647,  1653,  1665,  1672,  1678,  1690,  1696,  1702,  1720,
    1724,  1731,  1740,  1744,  1748,  1755,  1759,  1763,  1767,  1771,
    1775,  1782
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
  "ArgumentList", "Dims", "FieldAccess", "MethodInvocation", "ArrayAccess",
  "PostFixExpression", "PostIncrementExpression",
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
  "ConditionalExpressions", "AssignmentExpression", "Assignment",
  "LeftHandSide", "AssignmentOperator", "Expression", YY_NULLPTR
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

#define YYPACT_NINF (-394)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-275)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     -30,   -65,    68,  -394,    96,  -394,   -30,  -394,  -394,  -394,
    -394,    58,  -394,  -394,  -394,  -394,  -394,  -394,  -394,  -394,
      96,  -394,   104,  -394,  -394,    -5,  -394,  -394,   -32,  -394,
    -394,   -34,    91,    94,    11,  -394,  -394,    21,     5,  -394,
     150,   -19,   110,  -394,    24,  -394,  -394,  -394,  -394,   150,
    -394,  -394,   163,   751,  -394,  -394,  -394,  -394,  -394,    60,
    -394,  -394,    71,  -394,  -394,  -394,  -394,  -394,   204,    45,
     212,   213,  -394,  -394,  -394,  -394,  -394,   216,   918,  -394,
     918,   918,    97,   221,   -65,   224,  -394,    97,   918,   225,
     233,  -394,   918,  -394,  -394,   196,  -394,  -394,  -394,  -394,
     115,    19,   242,  -394,  -394,   751,  -394,  -394,   244,  -394,
    -394,  -394,  -394,  -394,  -394,   245,  -394,  -394,  -394,  -394,
    -394,  -394,  -394,  -394,   246,   252,   111,    61,   184,    38,
      82,   103,  -394,  -394,  -394,   207,   276,   273,  -394,   276,
     171,  -394,    22,  -394,   273,   285,   286,   158,    78,   -53,
    -394,    56,   918,   918,   918,   918,   918,   283,  -394,    38,
    -394,  -394,  -394,  -394,  -394,  -394,  -394,    90,  -394,   129,
    -394,   106,  -394,   203,  -394,   -10,  -394,   247,  -394,   240,
    -394,   249,  -394,   263,  -394,   -18,  -394,  -394,  -394,  -394,
    -394,   297,   280,  -394,  -394,  -394,  -394,  -394,   292,  -394,
     902,   298,   310,  -394,   918,   312,   313,  -394,  -394,   194,
     918,   315,   767,  -394,   316,  -394,    25,   918,   918,   530,
    -394,  -394,  -394,  -394,   202,   918,  -394,  -394,  -394,  -394,
    -394,  -394,  -394,  -394,   918,    78,   321,  -394,   205,   322,
       1,  -394,  -394,  -394,   205,   328,  -394,   323,  -394,   332,
     333,   751,  -394,  -394,   335,   586,   337,  -394,  -394,  -394,
    -394,   918,   918,   918,   918,   918,   918,   918,   918,   918,
     918,   918,   918,   918,   918,   918,   918,   918,   918,   918,
     918,   918,  -394,  -394,  -394,  -394,   327,  -394,  -394,   331,
     918,   341,  -394,  -394,   164,   342,  -394,  -394,   115,   339,
       1,   345,  -394,   343,  -394,   344,   347,   209,   349,  -394,
     348,  -394,  -394,  -394,  -394,     1,  -394,  -394,  -394,   352,
    -394,    78,   918,   918,   360,   357,   576,    55,   417,  -394,
    -394,  -394,    90,    90,   129,   129,   129,   106,   106,   106,
     106,   203,   203,   -10,   247,   240,   249,   263,   324,   918,
     959,   365,   855,   918,   918,   767,  -394,  -394,  -394,  -394,
    -394,  -394,   918,  -394,  -394,   918,   918,  -394,  -394,  -394,
     359,  -394,   362,  -394,   370,   374,  -394,  -394,  -394,   417,
     371,  -394,   918,   372,  -394,  -394,   378,   381,   384,   354,
    -394,   325,   334,  -394,  -394,  -394,  -394,   385,   401,  -394,
    -394,   405,   404,  -394,   406,  -394,     1,   409,   411,  -394,
    -394,  -394,   959,   902,   918,   918,   855,   767,  -394,  -394,
    -394,  -394,  -394,  -394,  -394,  -394,   412,  -394,  -394,   331,
     414,   423,   425,  -394,  -394,   767,   918,   855,   855,  -394,
     420,   363,  -394,   959,   855,   429,  -394,   855,  -394
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
       0,     0,   182,     0,     0,     0,   189,   182,   169,     0,
       0,     8,   169,     3,     4,    25,     6,     7,     5,   188,
       0,   272,     0,    93,   139,    94,   112,   114,     0,   115,
     132,   126,   140,   127,   141,     0,   155,   128,   129,   130,
     131,   142,   143,   144,   208,   187,   191,   192,   193,     0,
     152,   153,   150,   151,   149,     0,     0,    76,    80,    70,
       0,    65,    67,    69,    75,     0,     0,     0,    79,    92,
      88,     0,     0,     0,     0,     0,     0,   209,   192,   221,
     210,   211,   228,   214,   215,   218,   224,   233,   227,   237,
     232,   242,   236,   248,   241,   252,   247,   255,   251,   258,
     254,   261,   257,   264,   260,   267,   263,   269,   266,   281,
     270,     0,   209,   191,   193,   219,   220,   181,     0,   180,
     166,   158,     0,   157,     0,     0,     0,   167,   168,     0,
       0,     0,     0,   124,   117,   118,   120,    98,    98,     0,
     111,   113,   116,   148,     0,     0,   212,   213,   280,   278,
     276,   277,   279,   275,     0,    79,     0,    64,     0,     0,
       0,    22,    21,    26,     0,     0,    77,    78,    83,     0,
       0,    95,    90,    91,     0,   209,     0,   223,   222,   216,
     217,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   190,   184,   176,   178,     0,   164,   165,   175,
      98,     0,   183,   185,   199,     0,   186,   146,     0,     0,
       0,     0,    96,    97,   194,     0,     0,   198,     0,   271,
       0,    82,    70,    66,    71,   104,    68,    73,    72,    85,
      89,     0,    98,    98,     0,     0,     0,     0,   190,   229,
     230,   231,   234,   235,   240,   238,   239,   246,   245,   243,
     244,   249,   250,   253,   256,   259,   262,   265,     0,   169,
       0,     0,     0,    98,    98,     0,   119,   125,   123,   121,
     122,   200,     0,   203,   206,    98,    98,   207,    81,   109,
     107,   102,   103,    84,     0,     0,    99,   133,   196,     0,
       0,   225,     0,     0,   179,   156,     0,     0,     0,    25,
     159,     0,   126,   135,   136,   137,   138,     0,     0,   162,
     195,     0,     0,   106,     0,   105,     0,     0,     0,   226,
     197,   268,   172,   166,     0,     0,     0,     0,   202,   205,
     201,   204,   108,   110,   100,   101,     0,   170,   171,   177,
       0,     0,     0,   147,   160,     0,   169,     0,     0,   173,
       0,     0,   163,   172,     0,     0,   161,     0,   174
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -394,  -394,   265,   -38,  -394,  -394,  -394,  -394,  -394,  -394,
      -1,   394,  -394,  -394,  -394,  -394,  -394,  -394,  -394,  -394,
     436,  -394,  -394,   424,  -394,   421,  -394,    -3,  -394,   395,
    -394,  -394,  -394,   410,  -394,  -394,  -394,   210,   206,  -394,
    -309,  -394,  -394,   218,  -394,   396,  -394,  -394,   136,  -394,
    -394,  -394,  -394,  -394,  -394,   211,  -394,  -208,  -394,  -394,
    -394,  -394,  -394,  -394,  -394,   161,  -394,   -20,  -394,   361,
    -394,  -196,  -394,   166,  -394,  -394,  -190,  -394,  -326,  -338,
    -394,  -394,  -394,  -394,  -191,  -394,  -394,  -394,  -394,  -394,
    -394,  -394,  -394,    54,  -394,   -91,  -394,    26,  -394,  -394,
    -394,  -394,  -394,  -393,   383,  -394,  -394,  -394,  -394,  -394,
    -394,  -394,  -394,   447,   -51,   473,   -40,   -11,     2,   -69,
      39,   113,  -320,  -394,   -74,  -394,  -232,  -394,    46,  -394,
     -71,  -394,   195,  -394,   201,  -394,   214,  -394,   193,  -394,
     199,  -394,  -394,  -394,   102,  -394,   239,   229,  -394,  -394,
     271
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     2,    99,   100,    63,    64,    65,    66,    67,    68,
     157,    12,    13,     3,     4,     5,    18,    19,     6,    20,
       7,     8,     9,    21,    22,    23,    24,    25,    26,    39,
      42,    43,    44,    45,    46,    47,   140,   141,   142,   143,
     316,    48,    49,   245,   246,   137,   138,   247,   248,    74,
      50,    51,    71,   251,   252,   102,   103,   301,   302,   150,
     253,   370,   371,   404,   405,   317,   372,   104,   105,   106,
     107,   108,   214,   215,   359,   216,   109,   110,   391,   111,
     112,   113,   393,   114,   115,   116,   202,   203,   117,   118,
     394,   119,   395,   286,   287,   206,   207,   426,   427,   120,
     396,   288,   428,   289,   198,   199,   121,   122,   123,   124,
     125,   303,   327,   126,   158,   128,   159,   160,   161,   162,
     163,   164,   165,   166,   167,   168,   169,   170,   171,   172,
     173,   174,   175,   176,   177,   178,   179,   180,   181,   182,
     183,   184,   185,   186,   187,   188,   189,   190,   135,   234,
     304
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      11,   211,   127,    62,   284,   152,   369,   315,   381,   285,
     305,   195,   196,   129,   392,   249,    32,    37,   280,   429,
      54,   274,   297,   217,   275,   218,   281,   219,   -56,    75,
     239,   -55,   147,   299,    41,   334,   335,   336,    80,    81,
      69,    41,   130,   153,   154,    55,     1,   155,   156,   250,
     429,    56,   101,   146,   127,   131,  -209,  -209,   147,   409,
     379,   240,    10,   380,   300,   129,  -154,    57,    14,    86,
      58,    28,  -154,  -154,    59,   226,   227,    60,   392,   192,
     192,    61,   351,   201,   257,   258,   259,   260,    15,    31,
     433,    15,   132,    34,   130,    33,    38,   423,    16,   392,
     392,    16,    35,    89,   101,    36,   392,   131,    10,   392,
     244,   441,   442,    40,   374,   375,    40,    72,   446,  -210,
    -210,   448,    91,   267,    93,    94,   268,   269,    10,    96,
      97,    98,    52,  -273,  -273,  -273,  -273,  -273,   262,   263,
    -211,  -211,    55,   264,   132,   397,   398,    69,    56,   127,
    -273,   255,   192,   192,   192,   192,    53,   401,   402,   384,
     129,   127,   390,    15,    57,   399,   133,    58,   353,    37,
     354,    15,   129,    16,    60,   265,   266,    91,    61,    93,
      94,    16,   237,   238,    96,    97,    98,   136,    17,   130,
     -52,   332,   333,   329,   330,   331,    17,   244,   139,   101,
     127,   130,   131,   341,   342,    10,  -274,  -274,  -274,  -274,
    -274,   129,   145,   365,   131,   366,   148,   284,   133,   149,
     151,   285,   285,  -274,   197,   200,   297,   434,   204,   228,
     229,   230,   231,   232,    69,   270,   271,   210,   209,   132,
     130,   212,   213,   272,   273,   439,   233,   390,   399,   220,
     101,   132,   285,   131,   434,   222,   223,   439,   383,   224,
     225,   192,   192,   192,   192,   192,   192,   192,   192,   192,
     192,   192,   192,   192,   192,   192,   192,   192,   192,   192,
     235,   236,   134,   244,   217,   243,   218,   217,   261,   218,
     132,   261,   277,   147,   241,   242,   147,   276,   279,   127,
     278,   127,   282,   283,   127,  -272,  -272,  -272,  -272,  -272,
     129,   147,   129,   133,   290,   129,   337,   338,   339,   340,
      69,   294,  -272,   292,   293,   133,   296,   192,   298,   307,
     311,   314,   312,   320,   134,   321,   322,   323,   349,   130,
     325,   130,   328,   350,   130,   440,   352,   355,   357,   191,
     361,   363,   131,   368,   131,   362,   364,   131,   367,   208,
     239,   127,   127,   208,   133,   127,   127,   376,   377,   382,
     385,   403,   129,   129,   406,   407,   129,   129,   192,   408,
     410,   192,   413,   412,   127,   414,   127,   127,   415,   132,
     418,   132,   127,   127,   132,   129,   127,   129,   129,   416,
     417,   130,   130,   129,   129,   130,   130,   129,   419,  -134,
     420,   421,   101,   422,   131,   131,   254,   435,   131,   131,
     424,   152,   425,   256,   130,   436,   130,   130,   437,   134,
     438,   443,   130,   130,   447,    70,   130,   131,   444,   131,
     131,   134,    27,    30,    29,   131,   131,    76,   313,   131,
     319,   132,   132,   310,    73,   132,   132,   373,   144,   153,
     154,   358,   324,   133,   356,   133,   221,   430,   133,   445,
     205,   343,   346,   309,   132,   291,   132,   132,   344,   347,
     134,   295,   132,   132,   411,    86,   132,     0,     0,     0,
     306,     0,   345,     0,     0,     0,   308,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   318,     0,     0,     0,     0,     0,     0,     0,    89,
       0,     0,     0,     0,     0,   133,   133,   193,   193,   133,
     133,     0,   306,     0,   152,     0,     0,     0,    91,   242,
      93,    94,     0,     0,    10,    96,    97,    98,   133,     0,
     133,   133,   348,   194,   194,     0,   133,   133,     0,     0,
     133,     0,     0,     0,     0,     0,     0,    80,    81,     0,
       0,   360,   153,   154,     0,     0,   155,   156,     0,   134,
     152,   134,     0,     0,   134,   378,   318,     0,     0,     0,
     217,     0,   218,     0,   326,     0,     0,   306,    86,   147,
     193,   193,   193,   193,     0,     0,     0,     0,  -272,  -272,
    -272,  -272,  -272,    80,    81,     0,     0,     0,   153,   154,
     208,     0,   155,   156,     0,  -272,   194,   194,   194,   194,
       0,     0,    89,   400,     0,     0,     0,     0,     0,     0,
       0,   134,   134,     0,    86,   134,   134,     0,     0,     0,
       0,    91,     0,    93,    94,     0,     0,    10,    96,    97,
      98,     0,     0,     0,   134,     0,   134,   134,     0,     0,
       0,     0,   134,   134,     0,     0,   134,   318,    89,     0,
       0,     0,     0,     0,     0,   431,   432,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    91,     0,    93,
      94,     0,     0,    10,    96,    97,    98,   208,     0,   193,
     193,   193,   193,   193,   193,   193,   193,   193,   193,   193,
     193,   193,   193,   193,   193,   193,   193,   193,     0,     0,
       0,     0,     0,     0,     0,   194,   194,   194,   194,   194,
     194,   194,   194,   194,   194,   194,   194,   194,   194,   194,
     194,   194,   194,   194,    77,    78,     0,    53,     0,     0,
       0,     0,    79,     0,     0,     0,     0,     0,     0,     0,
      77,    78,     0,    53,     0,   193,     0,     0,    79,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    80,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   194,     0,     0,    80,    81,    82,    83,    84,     0,
       0,     0,    85,     0,     0,    55,     0,     0,     0,    86,
      87,    56,    82,    83,    84,     0,   193,     0,    85,   193,
       0,     0,     0,    88,     0,    86,    87,    57,     0,     0,
      58,     0,     0,     0,     0,     0,     0,    60,     0,    88,
       0,    61,   194,    89,    90,   194,     0,     0,    77,    78,
       0,    53,     0,     0,     0,     0,    79,     0,     0,    89,
      90,     0,    91,    92,    93,    94,     0,     0,    95,    96,
      97,    98,     0,     0,     0,     0,     0,     0,    91,    92,
      93,    94,    80,    81,    95,    96,    97,    98,     0,     0,
       0,     0,     0,     0,     0,     0,    78,     0,     0,     0,
      82,   386,    84,     0,     0,     0,   387,     0,     0,     0,
       0,     0,   152,    86,    87,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    88,     0,    80,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,    81,    89,   388,    84,
     153,   154,     0,    78,   155,   156,    55,     0,     0,     0,
      86,     0,    56,     0,     0,     0,    91,    92,    93,    94,
       0,     0,   389,    96,    97,    98,    86,     0,    57,     0,
       0,    58,     0,     0,     0,     0,    80,    81,    60,     0,
       0,     0,    61,     0,    89,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    84,     0,     0,     0,
      89,     0,     0,    91,     0,    93,    94,    86,     0,    10,
      96,    97,    98,     0,     0,     0,     0,     0,     0,    91,
       0,    93,    94,     0,     0,    10,    96,    97,    98,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    89,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      91,     0,    93,    94,     0,     0,    10,    96,    97,    98
};

static const yytype_int16 yycheck[] =
{
       1,    92,    53,    41,   200,     4,   315,     6,   328,   200,
     218,    80,    81,    53,   352,    68,    48,     6,    36,   412,
      40,    31,   212,     4,    34,     6,    44,     8,     7,    49,
       8,     7,    13,     8,    37,   267,   268,   269,    37,    38,
      41,    44,    53,    42,    43,    64,    76,    46,    47,   102,
     443,    70,    53,     8,   105,    53,    37,    38,    13,   379,
       5,    39,   127,     8,    39,   105,     5,    86,     0,    68,
      89,    13,    11,    12,    93,    37,    38,    96,   416,    80,
      81,   100,   290,    84,   153,   154,   155,   156,    67,    94,
     416,    67,    53,   127,   105,   127,    85,   406,    77,   437,
     438,    77,    11,   102,   105,    11,   444,   105,   127,   447,
     148,   437,   438,    92,   322,   323,    92,     7,   444,    37,
      38,   447,   121,    17,   123,   124,    20,    21,   127,   128,
     129,   130,   127,    22,    23,    24,    25,    26,    48,    49,
      37,    38,    64,    53,   105,   353,   354,   148,    70,   200,
      39,   152,   153,   154,   155,   156,     6,   365,   366,   350,
     200,   212,   352,    67,    86,   355,    53,    89,     4,     6,
       6,    67,   212,    77,    96,    46,    47,   121,   100,   123,
     124,    77,    11,    12,   128,   129,   130,   127,    92,   200,
      94,   265,   266,   262,   263,   264,    92,   235,   127,   200,
     251,   212,   200,   274,   275,   127,    22,    23,    24,    25,
      26,   251,     8,     4,   212,     6,     4,   413,   105,     6,
       4,   412,   413,    39,   127,     4,   416,   417,     4,    22,
      23,    24,    25,    26,   235,    32,    33,     4,    13,   200,
     251,    45,   127,    40,    41,   435,    39,   437,   438,     7,
     251,   212,   443,   251,   444,    11,    11,   447,   349,    13,
       8,   262,   263,   264,   265,   266,   267,   268,   269,   270,
     271,   272,   273,   274,   275,   276,   277,   278,   279,   280,
       4,     8,    53,   321,     4,   127,     6,     4,     8,     6,
     251,     8,    52,    13,     9,     9,    13,    50,    35,   350,
      51,   352,     5,    11,   355,    22,    23,    24,    25,    26,
     350,    13,   352,   200,     4,   355,   270,   271,   272,   273,
     321,   127,    39,    11,    11,   212,    11,   328,    12,   127,
       9,     9,   127,     5,   105,    12,     4,     4,    11,   350,
       5,   352,     5,    12,   355,   436,     5,     5,     9,    78,
       5,     7,   350,     5,   352,    12,     9,   355,     9,    88,
       8,   412,   413,    92,   251,   416,   417,     7,    11,    45,
       5,    12,   412,   413,    12,     5,   416,   417,   379,     5,
       9,   382,     4,    11,   435,     4,   437,   438,     4,   350,
       5,   352,   443,   444,   355,   435,   447,   437,   438,    45,
      75,   412,   413,   443,   444,   416,   417,   447,     7,    75,
       5,     7,   413,     7,   412,   413,   151,     5,   416,   417,
      11,     4,    11,   152,   435,    11,   437,   438,     5,   200,
       5,    11,   443,   444,     5,    41,   447,   435,    75,   437,
     438,   212,     6,    22,    20,   443,   444,    52,   238,   447,
     244,   412,   413,   235,    44,   416,   417,   321,    62,    42,
      43,   300,   251,   350,   298,   352,   105,   413,   355,   443,
      87,   276,   279,   234,   435,   204,   437,   438,   277,   280,
     251,   210,   443,   444,   382,    68,   447,    -1,    -1,    -1,
     219,    -1,   278,    -1,    -1,    -1,   225,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   240,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,
      -1,    -1,    -1,    -1,    -1,   412,   413,    80,    81,   416,
     417,    -1,   261,    -1,     4,    -1,    -1,    -1,   121,     9,
     123,   124,    -1,    -1,   127,   128,   129,   130,   435,    -1,
     437,   438,   281,    80,    81,    -1,   443,   444,    -1,    -1,
     447,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,
      -1,   300,    42,    43,    -1,    -1,    46,    47,    -1,   350,
       4,   352,    -1,    -1,   355,     9,   315,    -1,    -1,    -1,
       4,    -1,     6,    -1,     8,    -1,    -1,   326,    68,    13,
     153,   154,   155,   156,    -1,    -1,    -1,    -1,    22,    23,
      24,    25,    26,    37,    38,    -1,    -1,    -1,    42,    43,
     349,    -1,    46,    47,    -1,    39,   153,   154,   155,   156,
      -1,    -1,   102,   362,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   412,   413,    -1,    68,   416,   417,    -1,    -1,    -1,
      -1,   121,    -1,   123,   124,    -1,    -1,   127,   128,   129,
     130,    -1,    -1,    -1,   435,    -1,   437,   438,    -1,    -1,
      -1,    -1,   443,   444,    -1,    -1,   447,   406,   102,    -1,
      -1,    -1,    -1,    -1,    -1,   414,   415,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   121,    -1,   123,
     124,    -1,    -1,   127,   128,   129,   130,   436,    -1,   262,
     263,   264,   265,   266,   267,   268,   269,   270,   271,   272,
     273,   274,   275,   276,   277,   278,   279,   280,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   262,   263,   264,   265,   266,
     267,   268,   269,   270,   271,   272,   273,   274,   275,   276,
     277,   278,   279,   280,     3,     4,    -1,     6,    -1,    -1,
      -1,    -1,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,    -1,     6,    -1,   328,    -1,    -1,    11,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   328,    -1,    -1,    37,    38,    55,    56,    57,    -1,
      -1,    -1,    61,    -1,    -1,    64,    -1,    -1,    -1,    68,
      69,    70,    55,    56,    57,    -1,   379,    -1,    61,   382,
      -1,    -1,    -1,    82,    -1,    68,    69,    86,    -1,    -1,
      89,    -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,    82,
      -1,   100,   379,   102,   103,   382,    -1,    -1,     3,     4,
      -1,     6,    -1,    -1,    -1,    -1,    11,    -1,    -1,   102,
     103,    -1,   121,   122,   123,   124,    -1,    -1,   127,   128,
     129,   130,    -1,    -1,    -1,    -1,    -1,    -1,   121,   122,
     123,   124,    37,    38,   127,   128,   129,   130,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,
      55,    56,    57,    -1,    -1,    -1,    61,    -1,    -1,    -1,
      -1,    -1,     4,    68,    69,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    37,
      38,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    37,    38,   102,   103,    57,
      42,    43,    -1,     4,    46,    47,    64,    -1,    -1,    -1,
      68,    -1,    70,    -1,    -1,    -1,   121,   122,   123,   124,
      -1,    -1,   127,   128,   129,   130,    68,    -1,    86,    -1,
      -1,    89,    -1,    -1,    -1,    -1,    37,    38,    96,    -1,
      -1,    -1,   100,    -1,   102,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    57,    -1,    -1,    -1,
     102,    -1,    -1,   121,    -1,   123,   124,    68,    -1,   127,
     128,   129,   130,    -1,    -1,    -1,    -1,    -1,    -1,   121,
      -1,   123,   124,    -1,    -1,   127,   128,   129,   130,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     121,    -1,   123,   124,    -1,    -1,   127,   128,   129,   130
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
     230,   237,   238,   239,   240,   241,   244,   245,   246,   247,
     248,   249,   251,   252,   278,   279,   127,   176,   177,   127,
     167,   168,   169,   170,   176,     8,     8,    13,     4,     6,
     190,     4,     4,    42,    43,    46,    47,   141,   245,   247,
     248,   249,   250,   251,   252,   253,   254,   255,   256,   257,
     258,   259,   260,   261,   262,   263,   264,   265,   266,   267,
     268,   269,   270,   271,   272,   273,   274,   275,   276,   277,
     278,   281,   141,   244,   246,   250,   250,   127,   235,   236,
       4,   141,   217,   218,     4,   235,   226,   227,   281,    13,
       4,   226,    45,   127,   203,   204,   206,     4,     6,     8,
       7,   200,    11,    11,    13,     8,    37,    38,    22,    23,
      24,    25,    26,    39,   280,     4,     8,    11,    12,     8,
      39,     9,     9,   127,   134,   174,   175,   178,   179,    68,
     102,   184,   185,   191,   133,   141,   281,   250,   250,   250,
     250,     8,    48,    49,    53,    46,    47,    17,    20,    21,
      32,    33,    40,    41,    31,    34,    50,    52,    51,    35,
      36,    44,     5,    11,   202,   215,   224,   225,   232,   234,
       4,   281,    11,    11,   127,   281,    11,   207,    12,     8,
      39,   188,   189,   242,   281,   188,   281,   127,   281,   277,
     174,     9,   127,   168,     9,     6,   171,   196,   281,   169,
       5,    12,     4,     4,   186,     5,     8,   243,     5,   250,
     250,   250,   255,   255,   257,   257,   257,   259,   259,   259,
     259,   261,   261,   263,   265,   267,   269,   271,   281,    11,
      12,   188,     5,     4,     6,     5,   204,     9,   196,   205,
     281,     5,    12,     7,     9,     4,     6,     9,     5,   171,
     192,   193,   197,   179,   188,   188,     7,    11,     9,     5,
       8,   253,    45,   226,   215,     5,    56,    61,   103,   127,
     207,   209,   210,   213,   221,   223,   231,   188,   188,   207,
     281,   188,   188,    12,   194,   195,    12,     5,     5,   253,
       9,   275,    11,     4,     4,     4,    45,    75,     5,     7,
       5,     7,     7,   171,    11,    11,   228,   229,   233,   234,
     224,   281,   281,   209,   207,     5,    11,     5,     5,   207,
     226,   209,   209,    11,    75,   228,   209,     5,   209
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
     235,   236,   236,   237,   238,   239,   239,   240,   241,   241,
     241,   241,   241,   241,   242,   242,   243,   243,   244,   244,
     245,   245,   245,   245,   245,   245,   246,   246,   247,   247,
     247,   247,   248,   249,   250,   250,   250,   250,   250,   251,
     252,   253,   253,   253,   253,   254,   254,   255,   256,   256,
     256,   256,   257,   258,   258,   258,   259,   260,   260,   260,
     260,   261,   262,   262,   262,   262,   262,   263,   264,   264,
     264,   265,   266,   266,   267,   268,   268,   269,   270,   270,
     271,   272,   272,   273,   274,   274,   275,   276,   276,   277,
     277,   278,   279,   279,   279,   280,   280,   280,   280,   280,
     280,   281
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
       3,     1,     1,     1,     1,     3,     2,     3,     3,     3,
       4,     6,     6,     4,     6,     6,     4,     4,     1,     1,
       1,     1,     2,     2,     1,     1,     2,     2,     1,     2,
       2,     1,     2,     2,     1,     4,     5,     1,     1,     3,
       3,     3,     1,     1,     3,     3,     1,     1,     3,     3,
       3,     1,     1,     3,     3,     3,     3,     1,     1,     3,
       3,     1,     1,     3,     1,     1,     3,     1,     1,     3,
       1,     1,     3,     1,     1,     3,     1,     1,     5,     1,
       1,     3,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1
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
#line 2130 "y.tab.c"
    break;

  case 4:
#line 54 "java.y"
                                {(yyval.nd) = new node(++cnt,"literals"); char tmpstr[20]; sprintf(tmpstr, "%g", (yyvsp[0].num)); nodtostr((yyval.nd), string(tmpstr),"FLOATLITERAL");}
#line 2136 "y.tab.c"
    break;

  case 5:
#line 55 "java.y"
                                    {(yyval.nd) = new node(++cnt,"literals"); nodtostr((yyval.nd), (yyvsp[0].id),"BOOLEANLITERAL");}
#line 2142 "y.tab.c"
    break;

  case 6:
#line 56 "java.y"
                                {(yyval.nd) = new node(++cnt,"literals"); nodtostr((yyval.nd), (yyvsp[0].id),"STRINGLITERAL");}
#line 2148 "y.tab.c"
    break;

  case 7:
#line 57 "java.y"
                                {(yyval.nd) = new node(++cnt,"literals"); nodtostr((yyval.nd), (yyvsp[0].id),"CHARLITERAL");}
#line 2154 "y.tab.c"
    break;

  case 8:
#line 58 "java.y"
                                {(yyval.nd) = new node(++cnt,"literals"); nodtostr((yyval.nd), (yyvsp[0].id),"NULLLITERAL");}
#line 2160 "y.tab.c"
    break;

  case 9:
#line 61 "java.y"
                                {(yyval.nd) = new node(++cnt,"type"); nodjoin((yyval.nd),(yyvsp[0].nd));}
#line 2166 "y.tab.c"
    break;

  case 10:
#line 62 "java.y"
                                 {(yyval.nd) = new node(++cnt,"type"); nodjoin((yyval.nd),(yyvsp[0].nd));}
#line 2172 "y.tab.c"
    break;

  case 11:
#line 65 "java.y"
                                {(yyval.nd) = new node(++cnt,"PrimitiveType"); nodjoin((yyval.nd),(yyvsp[0].nd));}
#line 2178 "y.tab.c"
    break;

  case 12:
#line 66 "java.y"
                                 {(yyval.nd) = new node(++cnt,"PrimitiveType"); nodtostr((yyval.nd),(yyvsp[0].id),"BOOLEAN");}
#line 2184 "y.tab.c"
    break;

  case 13:
#line 69 "java.y"
                                {(yyval.nd) = new node(++cnt,"NumericType"); nodjoin((yyval.nd),(yyvsp[0].nd));}
#line 2190 "y.tab.c"
    break;

  case 14:
#line 70 "java.y"
                                {(yyval.nd) = new node(++cnt,"NumericType"); nodjoin((yyval.nd),(yyvsp[0].nd));}
#line 2196 "y.tab.c"
    break;

  case 15:
#line 73 "java.y"
                                {(yyval.nd) = new node(++cnt,"IntegralType"); nodtostr((yyval.nd),(yyvsp[0].id),"INT");}
#line 2202 "y.tab.c"
    break;

  case 16:
#line 74 "java.y"
                                {(yyval.nd) = new node(++cnt,"IntegralType"); nodtostr((yyval.nd),(yyvsp[0].id),"LONG");}
#line 2208 "y.tab.c"
    break;

  case 17:
#line 75 "java.y"
                                {(yyval.nd) = new node(++cnt,"IntegralType"); nodtostr((yyval.nd),(yyvsp[0].id),"CHAR");}
#line 2214 "y.tab.c"
    break;

  case 18:
#line 78 "java.y"
                                {(yyval.nd) = new node(++cnt,"FloatingPointType"); nodtostr((yyval.nd),(yyvsp[0].id),"FLOAT");}
#line 2220 "y.tab.c"
    break;

  case 19:
#line 79 "java.y"
                                {(yyval.nd) = new node(++cnt,"FloatingPointType"); nodtostr((yyval.nd),(yyvsp[0].id),"DOUBLE");}
#line 2226 "y.tab.c"
    break;

  case 20:
#line 82 "java.y"
                                {(yyval.nd) = new node(++cnt,"NonPrimitiveType"); nodjoin((yyval.nd),(yyvsp[0].nd));}
#line 2232 "y.tab.c"
    break;

  case 21:
#line 92 "java.y"
                                                    {
                (yyval.nd) = new node(++cnt,"ArrayType");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtostr((yyval.nd),(yyvsp[-1]. id),"LEFTSQUAREBRACKET");
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTSQUAREBRACKET");
        }
#line 2243 "y.tab.c"
    break;

  case 22:
#line 98 "java.y"
                                                             {
                (yyval.nd) = (yyvsp[-2].nd);
                nodtostr((yyval.nd),(yyvsp[-1]. id),"LEFTSQUAREBRACKET");
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTSQUAREBRACKET");
    }
#line 2253 "y.tab.c"
    break;

  case 23:
#line 105 "java.y"
                        {
                (yyval.nd) = new node(++cnt,"Name");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2262 "y.tab.c"
    break;

  case 24:
#line 109 "java.y"
                        {
                (yyval.nd) = new node(++cnt,"Name");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2271 "y.tab.c"
    break;

  case 25:
#line 115 "java.y"
                        {
                (yyval.nd) = new node(++cnt,"SimpleName");
                nodtostr((yyval.nd),(yyvsp[0].id),"IDENTIFIER");
        }
#line 2280 "y.tab.c"
    break;

  case 26:
#line 121 "java.y"
                            {
                (yyval.nd) = new node(++cnt,"QualifiedName");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtostr((yyval.nd),(yyvsp[-1]. id),"DOT");
                nodtostr((yyval.nd),(yyvsp[0].id),"IDENTIFIER");
        }
#line 2291 "y.tab.c"
    break;

  case 27:
#line 129 "java.y"
                                                {
                (yyval.nd) = new node(++cnt,"CompilationUnit");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2301 "y.tab.c"
    break;

  case 28:
#line 135 "java.y"
                                           {
        (yyval.nd) = new node(++cnt,"ImportDeclarationOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 2310 "y.tab.c"
    break;

  case 29:
#line 142 "java.y"
                                {
                (yyval.ndl) = new nodes();
                listpushback((yyval.ndl),(yyvsp[0].ndl));
        }
#line 2319 "y.tab.c"
    break;

  case 30:
#line 146 "java.y"
          { (yyval.ndl) = new nodes();}
#line 2325 "y.tab.c"
    break;

  case 31:
#line 150 "java.y"
                                       {
        (yyval.nd) = new node(++cnt,"TypeDeclarationOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 2334 "y.tab.c"
    break;

  case 32:
#line 157 "java.y"
                                {
                (yyval.ndl) = new nodes();
                listpushback((yyval.ndl),(yyvsp[0].ndl));
        }
#line 2343 "y.tab.c"
    break;

  case 33:
#line 161 "java.y"
          { (yyval.ndl) = new nodes();}
#line 2349 "y.tab.c"
    break;

  case 34:
#line 165 "java.y"
                                {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 2358 "y.tab.c"
    break;

  case 35:
#line 169 "java.y"
                                                {
                (yyval.ndl) = (yyvsp[-1].ndl) ;
                (yyval.ndl)->push_back((yyvsp[0].nd));
    }
#line 2367 "y.tab.c"
    break;

  case 36:
#line 175 "java.y"
                                {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 2376 "y.tab.c"
    break;

  case 37:
#line 179 "java.y"
                                            {
                (yyval.ndl) = (yyvsp[-1].ndl) ;
                (yyval.ndl)->push_back((yyvsp[0].nd));
    }
#line 2385 "y.tab.c"
    break;

  case 38:
#line 185 "java.y"
                                        {
                (yyval.nd) = new node(++cnt,"ImportDeclaration");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2394 "y.tab.c"
    break;

  case 39:
#line 189 "java.y"
                                          {
                (yyval.nd) = new node(++cnt,"ImportDeclaration");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2403 "y.tab.c"
    break;

  case 40:
#line 195 "java.y"
                                               {
                (yyval.nd) = new node(++cnt,"SingleTypeImportDeclaration");
                nodtostr((yyval.nd),(yyvsp[-4].id),"IMPORT");
                nodjoin((yyval.nd),(yyvsp[-3].nd));
                nodtostr((yyval.nd),(yyvsp[-2]. id),"DOT");
                nodtostr((yyval.nd),(yyvsp[-1].id),"IDENTIFIER");
                nodtostr((yyval.nd),(yyvsp[0]. id),"SEMICOLON");
        }
#line 2416 "y.tab.c"
    break;

  case 41:
#line 205 "java.y"
                                        {
                (yyval.nd) = new node(++cnt,"TypeImportOnDemandDeclaration");
                nodtostr((yyval.nd),(yyvsp[-4].id),"IMPORT");
                nodjoin((yyval.nd),(yyvsp[-3].nd));
                nodtostr((yyval.nd),(yyvsp[-2]. id),"DOT");
                nodtostr((yyval.nd),(yyvsp[-1]. id),"MUL");
                nodtostr((yyval.nd),(yyvsp[0]. id),"SEMICOLON");
        }
#line 2429 "y.tab.c"
    break;

  case 42:
#line 215 "java.y"
                                {
                (yyval.nd) = new node(++cnt,"TypeDeclaration");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2438 "y.tab.c"
    break;

  case 43:
#line 221 "java.y"
                        {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 2447 "y.tab.c"
    break;

  case 44:
#line 225 "java.y"
                                {
                (yyval.ndl) = (yyvsp[-1].ndl);
                (yyvsp[-1].ndl)->push_back((yyvsp[0].nd));
    }
#line 2456 "y.tab.c"
    break;

  case 45:
#line 231 "java.y"
                        {
                (yyval.nd) = new node(++cnt,"Modifier");
                nodtostr((yyval.nd),(yyvsp[0].id),"PUBLIC");
        }
#line 2465 "y.tab.c"
    break;

  case 46:
#line 235 "java.y"
                        {
                (yyval.nd) = new node(++cnt,"Modifier");
                nodtostr((yyval.nd),(yyvsp[0].id),"PRIVATE");
        }
#line 2474 "y.tab.c"
    break;

  case 47:
#line 239 "java.y"
                       {
                (yyval.nd) = new node(++cnt,"Modifier");
                nodtostr((yyval.nd),(yyvsp[0].id),"STATIC");
        }
#line 2483 "y.tab.c"
    break;

  case 48:
#line 245 "java.y"
                                                {
                (yyval.nd) = new node(++cnt,"ClassDeclaration");
                nodjoin((yyval.nd),(yyvsp[-3].nd));
                nodtostr((yyval.nd),(yyvsp[-2].id),"CLASS");
                nodtostr((yyval.nd),(yyvsp[-1].id),"IDENTIFIER");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2495 "y.tab.c"
    break;

  case 49:
#line 252 "java.y"
                                                                        {
                (yyval.nd) = new node(++cnt,"ClassDeclaration");
                nodjoin((yyval.nd),(yyvsp[-5].nd));
                nodtostr((yyval.nd),(yyvsp[-4].id),"CLASS");
                nodtostr((yyval.nd),(yyvsp[-3].id),"IDENTIFIER");
                nodtostr((yyval.nd),(yyvsp[-2].id),"EXTENDS");
                nodtostr((yyval.nd),(yyvsp[-1].id),"IDENTIFIER");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2509 "y.tab.c"
    break;

  case 50:
#line 262 "java.y"
                            {
        (yyval.nd) = new node(++cnt,"ModifiersOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 2518 "y.tab.c"
    break;

  case 51:
#line 268 "java.y"
                        {
                (yyval.ndl) = new nodes();
                listpushback((yyval.ndl),(yyvsp[0].ndl));
        }
#line 2527 "y.tab.c"
    break;

  case 52:
#line 272 "java.y"
          { (yyval.ndl) = new nodes() ;}
#line 2533 "y.tab.c"
    break;

  case 53:
#line 276 "java.y"
                                                                           {
                (yyval.nd) = new node(++cnt,"ClassBody");
                nodtostr((yyval.nd),(yyvsp[-2]. id),"LEFTCURLYBRACKET");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTCURLYBRACKET");
        }
#line 2544 "y.tab.c"
    break;

  case 54:
#line 284 "java.y"
                                                   {
(yyval.nd) = new node(++cnt,"ClassBodyDeclarationsOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 2553 "y.tab.c"
    break;

  case 55:
#line 291 "java.y"
                                {
                (yyval.ndl) = new nodes();
                listpushback((yyval.ndl),(yyvsp[0].ndl));
        }
#line 2562 "y.tab.c"
    break;

  case 56:
#line 295 "java.y"
           { (yyval.ndl) = new nodes(); }
#line 2568 "y.tab.c"
    break;

  case 57:
#line 299 "java.y"
                                {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 2577 "y.tab.c"
    break;

  case 58:
#line 303 "java.y"
                                                        {
        (yyval.ndl) = (yyvsp[-1].ndl);
        (yyvsp[-1].ndl)->push_back((yyvsp[0].nd));
    }
#line 2586 "y.tab.c"
    break;

  case 59:
#line 309 "java.y"
                                        {
                (yyval.nd) = new node(++cnt,"ClassBodyDeclaration");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2595 "y.tab.c"
    break;

  case 60:
#line 313 "java.y"
                                {
                (yyval.nd) = new node(++cnt,"ClassBodyDeclaration");
                nodjoin((yyval.nd),(yyvsp[0].nd));
    }
#line 2604 "y.tab.c"
    break;

  case 61:
#line 317 "java.y"
                                     {
                (yyval.nd) = new node(++cnt,"ClassBodyDeclaration");
                nodjoin((yyval.nd),(yyvsp[0].nd));
    }
#line 2613 "y.tab.c"
    break;

  case 62:
#line 323 "java.y"
                                  {
                (yyval.nd) = new node(++cnt,"ClassMemberDeclaration");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2622 "y.tab.c"
    break;

  case 63:
#line 327 "java.y"
                                   {
                (yyval.nd) = new node(++cnt,"ClassMemberDeclaration");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2631 "y.tab.c"
    break;

  case 64:
#line 333 "java.y"
                                                       {
                (yyval.nd) = new node(++cnt,"FieldDeclaration");
                nodjoin((yyval.nd),(yyvsp[-3].nd));
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtolis((yyval.nd),(yyvsp[-1].ndl));
                nodtostr((yyval.nd),(yyvsp[0]. id),"SEMICOLON");
        }
#line 2643 "y.tab.c"
    break;

  case 65:
#line 342 "java.y"
                        {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 2652 "y.tab.c"
    break;

  case 66:
#line 346 "java.y"
                                                {
                (yyval.ndl) = (yyvsp[-2].ndl);
                // $1 = new nodes();                            //here
                // node* new1 = new node(++cnt,$2);
                // nodes* new2 = new nodes(new1);
                (yyvsp[-2].ndl)->push_back(new node(++cnt,(yyvsp[-1]. id)));
                (yyvsp[-2].ndl)->push_back((yyvsp[0].nd));
    }
#line 2665 "y.tab.c"
    break;

  case 67:
#line 356 "java.y"
                               {
                (yyval.nd)  = new node(++cnt,"VarDeclarator");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2674 "y.tab.c"
    break;

  case 68:
#line 360 "java.y"
                                                  {
                (yyval.nd)  = new node(++cnt,"VarDeclarator");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtostr((yyval.nd),(yyvsp[-1]. id),"EQUAL");
                nodjoin((yyval.nd),(yyvsp[0].nd));
    }
#line 2685 "y.tab.c"
    break;

  case 69:
#line 367 "java.y"
                                  {
         (yyval.nd) = new node(++cnt,"VarDeclaratorId");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 2694 "y.tab.c"
    break;

  case 70:
#line 374 "java.y"
                        {
                // $$ = new node(++cnt,"VarDeclaratorId");
                // nodtostr($$,$1,"IDENTIFIER");
                 (yyval.ndl) = new nodes();
                 (yyval.ndl)->push_back(new node(++cnt,(yyvsp[0].id)));
        }
#line 2705 "y.tab.c"
    break;

  case 71:
#line 380 "java.y"
                                                             {
                (yyval.ndl) = new nodes();
                // $$->push_back($1);

                // node* new1 = ;
                // node* new2 = ;     //here
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-1]. id)));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[0]. id)));
        }
#line 2719 "y.tab.c"
    break;

  case 72:
#line 391 "java.y"
                        {
                (yyval.nd) = new node(++cnt,"VarInitializer");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2728 "y.tab.c"
    break;

  case 73:
#line 395 "java.y"
                                {
                (yyval.nd) = new node(++cnt,"VarInitializer");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2737 "y.tab.c"
    break;

  case 74:
#line 401 "java.y"
                                {
                (yyval.nd) = new node(++cnt,"MethodDeclaration");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2747 "y.tab.c"
    break;

  case 75:
#line 408 "java.y"
                                                {
                (yyval.nd) = new node(++cnt,"MethodHeader");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2758 "y.tab.c"
    break;

  case 76:
#line 414 "java.y"
                                                    {
                (yyval.nd) = new node(++cnt,"MethodHeader");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtostr((yyval.nd),(yyvsp[-1].id),"VOID");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2769 "y.tab.c"
    break;

  case 77:
#line 422 "java.y"
                                               {
 (yyval.nd) = new node(++cnt,"FormalParameterListOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 2778 "y.tab.c"
    break;

  case 78:
#line 429 "java.y"
                                {
                (yyval.ndl) = new nodes();
                listpushback((yyval.ndl),(yyvsp[0].ndl));
        }
#line 2787 "y.tab.c"
    break;

  case 79:
#line 433 "java.y"
          {(yyval.ndl) = new nodes();}
#line 2793 "y.tab.c"
    break;

  case 80:
#line 438 "java.y"
                                    {
        (yyval.nd) = new node(++cnt,"MethodDeclarator");
        nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 2802 "y.tab.c"
    break;

  case 81:
#line 444 "java.y"
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
#line 2819 "y.tab.c"
    break;

  case 82:
#line 456 "java.y"
                                                                     {
                (yyval.ndl) = new nodes();
                // node* new1 = ;      //here
                // node* new2 = ;
                (yyval.ndl)->push_back((yyvsp[-2].nd));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-1]. id)));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[0]. id)));
    }
#line 2832 "y.tab.c"
    break;

  case 83:
#line 466 "java.y"
                                {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 2841 "y.tab.c"
    break;

  case 84:
#line 470 "java.y"
                                                        {
                (yyval.ndl) = (yyvsp[-2].ndl);
                node* new1 = new node(++cnt,(yyvsp[-1]. id));          // here
                (yyvsp[-2].ndl)->push_back(new1);
                (yyvsp[-2].ndl)->push_back((yyvsp[0].nd));
        }
#line 2852 "y.tab.c"
    break;

  case 85:
#line 478 "java.y"
                                {
                (yyval.nd) = new node(++cnt,"FormalParameter");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2862 "y.tab.c"
    break;

  case 86:
#line 485 "java.y"
                {
                (yyval.nd) = new node(++cnt,"MethodBody");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2871 "y.tab.c"
    break;

  case 87:
#line 491 "java.y"
                        {
                (yyval.nd) = new node(++cnt,"StaticInitializer");
                nodtostr((yyval.nd),(yyvsp[-1].id),"STATIC");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2881 "y.tab.c"
    break;

  case 88:
#line 498 "java.y"
                                                                {
                (yyval.nd) = new node(++cnt,"ConstructorDeclaration");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 2892 "y.tab.c"
    break;

  case 89:
#line 506 "java.y"
                                                                           {
                (yyval.nd) = new node(++cnt,"ConstructorDeclarator");
                nodjoin((yyval.nd),(yyvsp[-3].nd));
                nodtostr((yyval.nd),(yyvsp[-2]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTBRACKET");
        }
#line 2904 "y.tab.c"
    break;

  case 90:
#line 515 "java.y"
                                                                   {
        (yyval.nd) = new node(++cnt,"ExplicitConstructorInvocationOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 2913 "y.tab.c"
    break;

  case 91:
#line 521 "java.y"
                                                {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 2922 "y.tab.c"
    break;

  case 92:
#line 525 "java.y"
           { (yyval.ndl) = new nodes(); }
#line 2928 "y.tab.c"
    break;

  case 93:
#line 529 "java.y"
                                      {
        (yyval.nd) = new node(++cnt,"BlockStatementOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 2937 "y.tab.c"
    break;

  case 94:
#line 535 "java.y"
                        {
                (yyval.ndl) = new nodes();
                listpushback((yyval.ndl),(yyvsp[0].ndl));
        }
#line 2946 "y.tab.c"
    break;

  case 95:
#line 539 "java.y"
          { (yyval.ndl) = new nodes();}
#line 2952 "y.tab.c"
    break;

  case 96:
#line 543 "java.y"
                                  {
        (yyval.nd) = new node(++cnt,"ArgumentListOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 2961 "y.tab.c"
    break;

  case 97:
#line 549 "java.y"
                        {
                (yyval.ndl) = new nodes();
                listpushback((yyval.ndl),(yyvsp[0].ndl));
        }
#line 2970 "y.tab.c"
    break;

  case 98:
#line 553 "java.y"
          { (yyval.ndl) = new nodes(); }
#line 2976 "y.tab.c"
    break;

  case 99:
#line 557 "java.y"
                                                                                              {
                (yyval.nd) = new node(++cnt,"ContructorBody");
                nodtostr((yyval.nd),(yyvsp[-3]. id),"LEFTCURLYBRACKET");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTCURLYBRACKET");
        }
#line 2988 "y.tab.c"
    break;

  case 100:
#line 566 "java.y"
                                                                  {
                (yyval.nd) = new node(++cnt,"ExplicitConstructorInvocation");
                nodtostr((yyval.nd),(yyvsp[-4].id),"THIS");
                nodtostr((yyval.nd),(yyvsp[-3]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtostr((yyval.nd),(yyvsp[-1]. id),"RIGHTBRACKET");
                nodtostr((yyval.nd),(yyvsp[0]. id),"SEMICOLON");
        }
#line 3001 "y.tab.c"
    break;

  case 101:
#line 574 "java.y"
                                                                  {
                (yyval.nd) = new node(++cnt,"ExplicitConstructorInvocation");
                nodtostr((yyval.nd),(yyvsp[-4].id),"SUPER");
                nodtostr((yyval.nd),(yyvsp[-3]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtostr((yyval.nd),(yyvsp[-1]. id),"RIGHTBRACKET");
                nodtostr((yyval.nd),(yyvsp[0]. id),"SEMICOLON");
    }
#line 3014 "y.tab.c"
    break;

  case 102:
#line 584 "java.y"
                                       {
        (yyval.nd) = new node(++cnt,"VarInitializersOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 3023 "y.tab.c"
    break;

  case 103:
#line 591 "java.y"
                               {
                (yyval.ndl) = new nodes();
                listpushback((yyval.ndl),(yyvsp[0].ndl));
        }
#line 3032 "y.tab.c"
    break;

  case 104:
#line 595 "java.y"
          { (yyval.ndl) = new nodes(); }
#line 3038 "y.tab.c"
    break;

  case 105:
#line 599 "java.y"
                    {
        (yyval.nd) = new node(++cnt,"CommaOpt");
        nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 3047 "y.tab.c"
    break;

  case 106:
#line 605 "java.y"
                        {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[0]. id)));
        }
#line 3056 "y.tab.c"
    break;

  case 107:
#line 609 "java.y"
          { (yyval.ndl) = new nodes() ; }
#line 3062 "y.tab.c"
    break;

  case 108:
#line 613 "java.y"
                                                                           {
                (yyval.nd) = new node(++cnt,"ArrayInitializer");
                nodtostr((yyval.nd),(yyvsp[-3]. id),"LEFTCURLYBRACKET");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTCURLYBRACKET");
        }
#line 3074 "y.tab.c"
    break;

  case 109:
#line 622 "java.y"
                        {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 3083 "y.tab.c"
    break;

  case 110:
#line 626 "java.y"
                                                  {
                (yyval.ndl) = (yyvsp[-2].ndl);
                (yyvsp[-2].ndl)->push_back((yyvsp[0].nd));
        }
#line 3092 "y.tab.c"
    break;

  case 111:
#line 632 "java.y"
                                                                   {
                (yyval.nd) = new node(++cnt,"Block");
                nodtostr((yyval.nd),(yyvsp[-2]. id),"LEFTCURLYBRACKET");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTCURLYBRACKET");
        }
#line 3103 "y.tab.c"
    break;

  case 112:
#line 640 "java.y"
                                {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 3112 "y.tab.c"
    break;

  case 113:
#line 644 "java.y"
                                        {
                (yyval.ndl) = (yyvsp[-1].ndl);
                (yyvsp[-1].ndl)->push_back((yyvsp[0].nd));
    }
#line 3121 "y.tab.c"
    break;

  case 114:
#line 650 "java.y"
                                                {
                (yyval.nd) = new node(++cnt,"BlockStatement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3130 "y.tab.c"
    break;

  case 115:
#line 654 "java.y"
                        {
                (yyval.nd) = new node(++cnt,"BlockStatement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
    }
#line 3139 "y.tab.c"
    break;

  case 116:
#line 660 "java.y"
                                                  {
                (yyval.nd) = new node(++cnt,"LocalVariableDeclarationStatement");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"SEMICOLON");
        }
#line 3149 "y.tab.c"
    break;

  case 117:
#line 666 "java.y"
                                                   {
                (yyval.nd) = new node(++cnt,"LocalVariableDeclaration");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtolis((yyval.nd),(yyvsp[0].ndl));
                }
#line 3159 "y.tab.c"
    break;

  case 118:
#line 672 "java.y"
                                        {(yyval.ndl) = new nodes(); (yyval.ndl)->push_back((yyvsp[0].nd));}
#line 3165 "y.tab.c"
    break;

  case 119:
#line 673 "java.y"
                                                               {(yyval.ndl)=(yyvsp[-2].ndl); (yyvsp[-2].ndl)->push_back((yyvsp[0].nd));}
#line 3171 "y.tab.c"
    break;

  case 120:
#line 676 "java.y"
                                         { (yyval.nd) = new node(++cnt,"VariableDeclarator"); nodjoin((yyval.nd),(yyvsp[0].nd));}
#line 3177 "y.tab.c"
    break;

  case 121:
#line 677 "java.y"
                                                                 { (yyval.nd) = new node(++cnt,"VariableDeclarator"); nodjoin((yyval.nd),(yyvsp[-2].nd)); nodtostr((yyval.nd),(yyvsp[-1]. id),"EQUAL"); nodjoin((yyval.nd),(yyvsp[0].nd));}
#line 3183 "y.tab.c"
    break;

  case 122:
#line 680 "java.y"
                                { (yyval.nd) = new node(++cnt,"VariableInitializer"); nodjoin((yyval.nd),(yyvsp[0].nd));}
#line 3189 "y.tab.c"
    break;

  case 123:
#line 681 "java.y"
                                   { (yyval.nd) = new node(++cnt,"VariableInitializer"); nodjoin((yyval.nd),(yyvsp[0].nd));}
#line 3195 "y.tab.c"
    break;

  case 124:
#line 684 "java.y"
                                 {(yyval.nd) = new node(++cnt,"VariableDeclaratorId"); nodtostr((yyval.nd),(yyvsp[0].id),"IDENTIFIER");}
#line 3201 "y.tab.c"
    break;

  case 125:
#line 685 "java.y"
                                                                            {(yyval.nd) = (yyvsp[-2].nd); node* n1 = new node(++cnt,(yyvsp[-1]. id)); node* n2 = new node(++cnt,(yyvsp[0]. id)); nodjoin((yyval.nd),n1); nodjoin((yyval.nd),n2);}
#line 3207 "y.tab.c"
    break;

  case 126:
#line 689 "java.y"
                                                {
                (yyval.nd) = new node(++cnt,"Statement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3216 "y.tab.c"
    break;

  case 127:
#line 693 "java.y"
                                {
                (yyval.nd) = new node(++cnt,"Statement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3225 "y.tab.c"
    break;

  case 128:
#line 697 "java.y"
                               {
                (yyval.nd) = new node(++cnt,"Statement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3234 "y.tab.c"
    break;

  case 129:
#line 701 "java.y"
                                   {
                (yyval.nd) = new node(++cnt,"Statement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3243 "y.tab.c"
    break;

  case 130:
#line 705 "java.y"
                              {
                (yyval.nd) = new node(++cnt,"Statement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3252 "y.tab.c"
    break;

  case 131:
#line 709 "java.y"
                            {
                (yyval.nd) = new node(++cnt,"Statement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3261 "y.tab.c"
    break;

  case 132:
#line 713 "java.y"
                                {
                (yyval.nd) = new node(++cnt,"Statement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3270 "y.tab.c"
    break;

  case 133:
#line 719 "java.y"
                                                                               {
                        (yyval.nd) = new node(++cnt,"PrintlnStatement");
                        nodtostr((yyval.nd),(yyvsp[-4]. id),"SYSTEMOUTPRINTLN");
                        nodtostr((yyval.nd),(yyvsp[-3]. id),"LEFTBRACKET");
                        nodjoin((yyval.nd),(yyvsp[-2].nd));
                        nodtostr((yyval.nd),(yyvsp[-1]. id),"RIGHTBRACKET");
                        nodtostr((yyval.nd),(yyvsp[0]. id),"SEMICOLON");
                }
#line 3283 "y.tab.c"
    break;

  case 134:
#line 729 "java.y"
                                                {
                (yyval.nd) = new node(++cnt,"StatementNoShortIf");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3292 "y.tab.c"
    break;

  case 135:
#line 733 "java.y"
                                         {
                (yyval.nd) = new node(++cnt,"StatementNoShortIf");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3301 "y.tab.c"
    break;

  case 136:
#line 737 "java.y"
                                            {
                (yyval.nd) = new node(++cnt,"StatementNoShortIf");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3310 "y.tab.c"
    break;

  case 137:
#line 741 "java.y"
                                       {
                (yyval.nd) = new node(++cnt,"StatementNoShortIf");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3319 "y.tab.c"
    break;

  case 138:
#line 745 "java.y"
                                     {
                (yyval.nd) = new node(++cnt,"StatementNoShortIf");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3328 "y.tab.c"
    break;

  case 139:
#line 752 "java.y"
                 {
                (yyval.nd) = new node(++cnt,"StatementWithoutTrailingSubstatement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3337 "y.tab.c"
    break;

  case 140:
#line 756 "java.y"
                              {
                (yyval.nd) = new node(++cnt,"StatementWithoutTrailingSubstatement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3346 "y.tab.c"
    break;

  case 141:
#line 760 "java.y"
                                   {
                (yyval.nd) = new node(++cnt,"StatementWithoutTrailingSubstatement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3355 "y.tab.c"
    break;

  case 142:
#line 764 "java.y"
                              {
                (yyval.nd) = new node(++cnt,"StatementWithoutTrailingSubstatement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3364 "y.tab.c"
    break;

  case 143:
#line 768 "java.y"
                                 {
                (yyval.nd) = new node(++cnt,"StatementWithoutTrailingSubstatement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3373 "y.tab.c"
    break;

  case 144:
#line 772 "java.y"
                               {
                (yyval.nd) = new node(++cnt,"StatementWithoutTrailingSubstatement");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3382 "y.tab.c"
    break;

  case 145:
#line 778 "java.y"
                     {
                (yyval.nd) = new node(++cnt,"EmptyStatement");
                nodtostr((yyval.nd),(yyvsp[0]. id),"SEMICOLON");
        }
#line 3391 "y.tab.c"
    break;

  case 146:
#line 784 "java.y"
                                      {
                (yyval.nd) = new node(++cnt,"LabeledStatement");
                nodtostr((yyval.nd),(yyvsp[-2].id),"IDENTIFIER");
                nodtostr((yyval.nd),(yyvsp[-1]. id),"COLON");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3402 "y.tab.c"
    break;

  case 147:
#line 792 "java.y"
                                               {
                (yyval.nd) = new node(++cnt,"LabeledStatementNoShortIf");
                nodtostr((yyval.nd),(yyvsp[-2].id),"IDENTIFIER");
                nodtostr((yyval.nd),(yyvsp[-1]. id),"COLON");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3413 "y.tab.c"
    break;

  case 148:
#line 800 "java.y"
                                         {
                (yyval.nd) = new node(++cnt,"ExpressionStatement");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"SEMICOLON");
        }
#line 3423 "y.tab.c"
    break;

  case 149:
#line 807 "java.y"
                      {
                (yyval.nd) = new node(++cnt,"StatementExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3432 "y.tab.c"
    break;

  case 150:
#line 811 "java.y"
                                  {
                (yyval.nd) = new node(++cnt,"StatementExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3441 "y.tab.c"
    break;

  case 151:
#line 815 "java.y"
                                  {
                (yyval.nd) = new node(++cnt,"StatementExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3450 "y.tab.c"
    break;

  case 152:
#line 819 "java.y"
                                   {
                (yyval.nd) = new node(++cnt,"StatementExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3459 "y.tab.c"
    break;

  case 153:
#line 823 "java.y"
                                   {
                (yyval.nd) = new node(++cnt,"StatementExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3468 "y.tab.c"
    break;

  case 154:
#line 827 "java.y"
                            {
                (yyval.nd) = new node(++cnt,"StatementExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3477 "y.tab.c"
    break;

  case 155:
#line 831 "java.y"
                                           {
                (yyval.nd) = new node(++cnt,"StatementExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3486 "y.tab.c"
    break;

  case 156:
#line 837 "java.y"
                                                                                        {
        (yyval.nd)=new node(++cnt,"ClassInstanceCreationExpression");
        nodtostr((yyval.nd),(yyvsp[-4].id),"NEW");
        nodjoin((yyval.nd),(yyvsp[-3].nd));
        nodtostr((yyval.nd),(yyvsp[-2]. id),"LEFTBRACKET");
        nodjoin((yyval.nd),(yyvsp[-1].nd));
        nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTBRACKET");
}
#line 3499 "y.tab.c"
    break;

  case 157:
#line 847 "java.y"
                                       {(yyval.nd)=new node(++cnt,"ClassType"); nodjoin((yyval.nd),(yyvsp[0].nd));}
#line 3505 "y.tab.c"
    break;

  case 158:
#line 850 "java.y"
                           {(yyval.nd)=new node(++cnt,"ClassOrInterfaceType"); nodjoin((yyval.nd),(yyvsp[0].nd));}
#line 3511 "y.tab.c"
    break;

  case 159:
#line 854 "java.y"
                                                            {
                (yyval.nd) = new node(++cnt,"IfThenStatement");
                nodtostr((yyval.nd),(yyvsp[-4].id),"IF");
                nodtostr((yyval.nd),(yyvsp[-3]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtostr((yyval.nd),(yyvsp[-1]. id),"RIGHTBRACKET");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3524 "y.tab.c"
    break;

  case 160:
#line 864 "java.y"
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
#line 3539 "y.tab.c"
    break;

  case 161:
#line 876 "java.y"
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
#line 3554 "y.tab.c"
    break;

  case 162:
#line 888 "java.y"
                                                               {
                (yyval.nd) = new node(++cnt,"WhileStatement");
                nodtostr((yyval.nd),(yyvsp[-4].id),"WHILE");
                nodtostr((yyval.nd),(yyvsp[-3]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtostr((yyval.nd),(yyvsp[-1]. id),"RIGHTBRACKET");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3567 "y.tab.c"
    break;

  case 163:
#line 899 "java.y"
                                                                        {
                (yyval.nd) = new node(++cnt,"WhileStatementNoShortIf");
                nodtostr((yyval.nd),(yyvsp[-4].id),"WHILE");
                nodtostr((yyval.nd),(yyvsp[-3]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtostr((yyval.nd),(yyvsp[-1]. id),"RIGHTBRACKET");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3580 "y.tab.c"
    break;

  case 164:
#line 908 "java.y"
                        {
 (yyval.nd) = new node(++cnt,"ForInitOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 3589 "y.tab.c"
    break;

  case 165:
#line 915 "java.y"
                    {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 3598 "y.tab.c"
    break;

  case 166:
#line 919 "java.y"
          { (yyval.ndl) = new nodes(); }
#line 3604 "y.tab.c"
    break;

  case 167:
#line 923 "java.y"
                              {
        (yyval.nd) = new node(++cnt,"ExpressionOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 3613 "y.tab.c"
    break;

  case 168:
#line 929 "java.y"
                       {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 3622 "y.tab.c"
    break;

  case 169:
#line 933 "java.y"
          { (yyval.ndl) = new nodes(); }
#line 3628 "y.tab.c"
    break;

  case 170:
#line 937 "java.y"
                            {
        (yyval.nd) = new node(++cnt,"ForUpdateOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 3637 "y.tab.c"
    break;

  case 171:
#line 943 "java.y"
                      {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 3646 "y.tab.c"
    break;

  case 172:
#line 947 "java.y"
          { (yyval.ndl) = new nodes(); }
#line 3652 "y.tab.c"
    break;

  case 173:
#line 952 "java.y"
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
#line 3669 "y.tab.c"
    break;

  case 174:
#line 967 "java.y"
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
#line 3686 "y.tab.c"
    break;

  case 175:
#line 983 "java.y"
                                     {        //dekhna hai
                (yyval.nd) = new node(++cnt,"ForInit");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
        }
#line 3695 "y.tab.c"
    break;

  case 176:
#line 987 "java.y"
                                          {
                (yyval.nd) = new node(++cnt,"ForInit");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3704 "y.tab.c"
    break;

  case 177:
#line 994 "java.y"
                                    {
                (yyval.nd) = new node(++cnt,"ForInitOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
        }
#line 3713 "y.tab.c"
    break;

  case 178:
#line 1001 "java.y"
                                {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 3722 "y.tab.c"
    break;

  case 179:
#line 1005 "java.y"
                                                                {
                (yyval.ndl) = (yyvsp[-2].ndl);
                (yyvsp[-2].ndl)->push_back((yyvsp[0].nd));
        }
#line 3731 "y.tab.c"
    break;

  case 180:
#line 1011 "java.y"
                    {
        (yyval.nd) = new node(++cnt,"IDENTOpt");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 3740 "y.tab.c"
    break;

  case 181:
#line 1016 "java.y"
                         {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[0].id)));
        }
#line 3749 "y.tab.c"
    break;

  case 182:
#line 1020 "java.y"
           { (yyval.ndl) = new nodes(); }
#line 3755 "y.tab.c"
    break;

  case 183:
#line 1025 "java.y"
                                    {
                (yyval.nd) = new node(++cnt,"BreakStatement");
                nodtostr((yyval.nd),(yyvsp[-2].id),"BREAK");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"SEMICOLON");
        }
#line 3766 "y.tab.c"
    break;

  case 184:
#line 1034 "java.y"
                                       {
                (yyval.nd) = new node(++cnt,"ContinueStatement");
                nodtostr((yyval.nd),(yyvsp[-2].id),"CONTINUE");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"SEMICOLON");
        }
#line 3777 "y.tab.c"
    break;

  case 185:
#line 1043 "java.y"
                                          {
                (yyval.nd) = new node(++cnt,"ReturnStatement");
                nodtostr((yyval.nd),(yyvsp[-2].id),"RETURN");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"SEMICOLON");
        }
#line 3788 "y.tab.c"
    break;

  case 186:
#line 1049 "java.y"
                                               {
                (yyval.nd) = new node(++cnt,"ReturnStatement");
                nodtostr((yyval.nd),(yyvsp[-2].id),"SUSPEND");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"SEMICOLON");
        }
#line 3799 "y.tab.c"
    break;

  case 187:
#line 1057 "java.y"
                             {
                (yyval.nd) = new node(++cnt,"Primary");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3808 "y.tab.c"
    break;

  case 188:
#line 1067 "java.y"
                   {
                (yyval.nd) = new node(++cnt,"PrimaryNoNewArray");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3817 "y.tab.c"
    break;

  case 189:
#line 1071 "java.y"
                {
                (yyval.nd) = new node(++cnt,"PrimaryNoNewArray");
                nodtostr((yyval.nd),(yyvsp[0].id),"THIS");
        }
#line 3826 "y.tab.c"
    break;

  case 190:
#line 1075 "java.y"
                                               {
                (yyval.nd) = new node(++cnt,"PrimaryNoNewArray");
                nodtostr((yyval.nd),(yyvsp[-2]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTBRACKET");
        }
#line 3837 "y.tab.c"
    break;

  case 191:
#line 1081 "java.y"
                       {
                (yyval.nd) = new node(++cnt,"PrimaryNoNewArray");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3846 "y.tab.c"
    break;

  case 192:
#line 1085 "java.y"
                            {
                (yyval.nd) = new node(++cnt,"PrimaryNoNewArray");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3855 "y.tab.c"
    break;

  case 193:
#line 1089 "java.y"
                       {
                (yyval.nd) = new node(++cnt,"PrimaryNoNewArray");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 3864 "y.tab.c"
    break;

  case 194:
#line 1095 "java.y"
                      {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 3873 "y.tab.c"
    break;

  case 195:
#line 1099 "java.y"
                                                {
                (yyval.ndl) = (yyvsp[-2].ndl);
                (yyvsp[-2].ndl)->push_back((yyvsp[0].nd));
        }
#line 3882 "y.tab.c"
    break;

  case 196:
#line 1146 "java.y"
                                                     {
                (yyval.ndl) = new nodes();
                node* n1 = new node(++cnt,(yyvsp[-1]. id));
                node* n2 = new node(++cnt,(yyvsp[0]. id));                          //here
                (yyval.ndl)->push_back(n1);
                (yyval.ndl)->push_back(n2);
        }
#line 3894 "y.tab.c"
    break;

  case 197:
#line 1153 "java.y"
                                                     {
                (yyval.ndl) = (yyvsp[-2].ndl);
                node* n1 = new node(++cnt,(yyvsp[-1]. id));
                node* n2 = new node(++cnt,(yyvsp[0]. id));
                (yyvsp[-2].ndl)->push_back(n1);
                (yyvsp[-2].ndl)->push_back(n2);

    }
#line 3907 "y.tab.c"
    break;

  case 198:
#line 1163 "java.y"
                                        {
                (yyval.nd) = new node(++cnt,"FieldAccess");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtostr((yyval.nd),(yyvsp[-1]. id),"DOT");
                nodtostr((yyval.nd),(yyvsp[0].id),"IDENTIFIER");
        }
#line 3918 "y.tab.c"
    break;

  case 199:
#line 1169 "java.y"
                                        {
                (yyval.nd) = new node(++cnt,"FieldAccess");
                nodtostr((yyval.nd),(yyvsp[-2].id),"SUPER");
                nodtostr((yyval.nd),(yyvsp[-1]. id),"DOT");
                nodtostr((yyval.nd),(yyvsp[0].id),"IDENTIFIER");
        }
#line 3929 "y.tab.c"
    break;

  case 200:
#line 1177 "java.y"
                                                           {
                (yyval.nd) = new node(++cnt,"MethodInvocation");
                nodjoin((yyval.nd),(yyvsp[-3].nd));
                nodtostr((yyval.nd),(yyvsp[-2]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTBRACKET");
        }
#line 3941 "y.tab.c"
    break;

  case 201:
#line 1184 "java.y"
                                                                                 {
                (yyval.nd) = new node(++cnt,"MethodInvocation");
                nodjoin((yyval.nd),(yyvsp[-5].nd));
                nodtostr((yyval.nd),(yyvsp[-4]. id),"DOT");
                nodtostr((yyval.nd),(yyvsp[-3].id),"IDENTIFIER");
                nodtostr((yyval.nd),(yyvsp[-2]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTBRACKET");
        }
#line 3955 "y.tab.c"
    break;

  case 202:
#line 1193 "java.y"
                                                                               {
                (yyval.nd) = new node(++cnt,"MethodInvocation");
                nodtostr((yyval.nd),(yyvsp[-5].id),"SUPER");
                nodtostr((yyval.nd),(yyvsp[-4]. id),"DOT");
                nodtostr((yyval.nd),(yyvsp[-3].id),"IDENTIFIER");
                nodtostr((yyval.nd),(yyvsp[-2]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTBRACKET");
        }
#line 3969 "y.tab.c"
    break;

  case 203:
#line 1202 "java.y"
                                                                         {
                (yyval.nd) = new node(++cnt,"MethodInvocation");
                nodjoin((yyval.nd),(yyvsp[-3].nd));
                nodtostr((yyval.nd),(yyvsp[-2]. id),"LEFTCURLYBRACKET");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTCURLYBRACKET");
        }
#line 3981 "y.tab.c"
    break;

  case 204:
#line 1209 "java.y"
                                                                                           {
                (yyval.nd) = new node(++cnt,"MethodInvocation");
                nodjoin((yyval.nd),(yyvsp[-5].nd));
                nodtostr((yyval.nd),(yyvsp[-4]. id),"DOT");
                nodtostr((yyval.nd),(yyvsp[-3].id),"IDENTIFIER");
                nodtostr((yyval.nd),(yyvsp[-2]. id),"LEFTCURLYBRACKET");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTCURLYBRACKET");
        }
#line 3995 "y.tab.c"
    break;

  case 205:
#line 1218 "java.y"
                                                                                         {
                (yyval.nd) = new node(++cnt,"MethodInvocation");
                nodtostr((yyval.nd),(yyvsp[-5].id),"SUPER");
                nodtostr((yyval.nd),(yyvsp[-4]. id),"DOT");
                nodtostr((yyval.nd),(yyvsp[-3].id),"IDENTIFIER");
                nodtostr((yyval.nd),(yyvsp[-2]. id),"LEFTCURLYBRACKET");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTCURLYBRACKET");
        }
#line 4009 "y.tab.c"
    break;

  case 206:
#line 1229 "java.y"
                                                                  {
                (yyval.nd) = new node(++cnt,"ArrayAcces");
                nodjoin((yyval.nd),(yyvsp[-3].nd));
                nodtostr((yyval.nd),(yyvsp[-2]. id),"LEFTSQUAREBRACKET");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTSQUAREBRACKET");
        }
#line 4021 "y.tab.c"
    break;

  case 207:
#line 1236 "java.y"
                                                                                   {
                (yyval.nd) = new node(++cnt,"ArrayAcces");
                nodjoin((yyval.nd),(yyvsp[-3].nd));
                nodtostr((yyval.nd),(yyvsp[-2]. id),"LEFTSQUAREBRACKET");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"RIGHTSQUAREBRACKET");
        }
#line 4033 "y.tab.c"
    break;

  case 208:
#line 1246 "java.y"
                     {
                (yyval.nd) = new node(++cnt,"PostFixExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4042 "y.tab.c"
    break;

  case 209:
#line 1250 "java.y"
                      {
                (yyval.nd) = new node(++cnt,"PostFixExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4051 "y.tab.c"
    break;

  case 210:
#line 1254 "java.y"
                                         {
                (yyval.nd) = new node(++cnt,"PostFixExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4060 "y.tab.c"
    break;

  case 211:
#line 1258 "java.y"
                                         {
                (yyval.nd) = new node(++cnt,"PostFixExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4069 "y.tab.c"
    break;

  case 212:
#line 1265 "java.y"
                                      {
                (yyval.nd) = new node(++cnt,"PostIncrementExpression");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"ADDADD");
        }
#line 4079 "y.tab.c"
    break;

  case 213:
#line 1273 "java.y"
                                      {
                (yyval.nd) = new node(++cnt,"PostDecrementExpression");
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodtostr((yyval.nd),(yyvsp[0]. id),"SUBSUB");
        }
#line 4089 "y.tab.c"
    break;

  case 214:
#line 1281 "java.y"
                                    {
                (yyval.nd) = new node(++cnt,"UnaryExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4098 "y.tab.c"
    break;

  case 215:
#line 1285 "java.y"
                                        {
                (yyval.nd) = new node(++cnt,"UnaryExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4107 "y.tab.c"
    break;

  case 216:
#line 1289 "java.y"
                                     {
                (yyval.nd) = new node(++cnt,"UnaryExpression");
                nodtostr((yyval.nd),(yyvsp[-1]. id),"ADD");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4117 "y.tab.c"
    break;

  case 217:
#line 1294 "java.y"
                                     {
                (yyval.nd) = new node(++cnt,"UnaryExpression");
                nodtostr((yyval.nd),(yyvsp[-1]. id),"SUB");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4127 "y.tab.c"
    break;

  case 218:
#line 1299 "java.y"
                                             {
                (yyval.nd) = new node(++cnt,"UnaryExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4136 "y.tab.c"
    break;

  case 219:
#line 1306 "java.y"
                                    {
                (yyval.nd) = new node(++cnt,"PreIncrementExpression");
                nodtostr((yyval.nd),(yyvsp[-1]. id),"ADDADD");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4146 "y.tab.c"
    break;

  case 220:
#line 1314 "java.y"
                                    {
                (yyval.nd) = new node(++cnt,"PreDecrementExpression");
                nodtostr((yyval.nd),(yyvsp[-1]. id),"SUBSUB");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4156 "y.tab.c"
    break;

  case 221:
#line 1322 "java.y"
                               {
                (yyval.nd) = new node(++cnt,"UnaryExpressionNotPlusMinus");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4165 "y.tab.c"
    break;

  case 222:
#line 1326 "java.y"
                                       {
                (yyval.nd) = new node(++cnt,"UnaryExpressionNotPlusMinus");
                nodtostr((yyval.nd),(yyvsp[-1]. id),"TILDE");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4175 "y.tab.c"
    break;

  case 223:
#line 1331 "java.y"
                                     {
                (yyval.nd) = new node(++cnt,"UnaryExpressionNotPlusMinus");
                nodtostr((yyval.nd),(yyvsp[-1]. id),"NOT");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4185 "y.tab.c"
    break;

  case 224:
#line 1336 "java.y"
                                {
                (yyval.nd) = new node(++cnt,"UnaryExpressionNotPlusMinus");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4194 "y.tab.c"
    break;

  case 225:
#line 1351 "java.y"
                                                                                 {
                (yyval.nd) = new node(++cnt,"CastExpression");
                nodtostr((yyval.nd),(yyvsp[-3]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodtostr((yyval.nd),(yyvsp[-1]. id),"RIGHTBRACKET");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4206 "y.tab.c"
    break;

  case 226:
#line 1358 "java.y"
                                                                                {
                (yyval.nd) = new node(++cnt,"CastExpression");
                nodtostr((yyval.nd),(yyvsp[-4]. id),"LEFTBRACKET");
                nodjoin((yyval.nd),(yyvsp[-3].nd));
                nodtolis((yyval.nd),(yyvsp[-2].ndl));
                nodtostr((yyval.nd),(yyvsp[-1]. id),"RIGHTBRACKET");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4219 "y.tab.c"
    break;

  case 227:
#line 1368 "java.y"
                                                     {
        (yyval.nd)=new node(++cnt,"MultiplicativeExpression");
        nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 4228 "y.tab.c"
    break;

  case 228:
#line 1375 "java.y"
                             {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4237 "y.tab.c"
    break;

  case 229:
#line 1379 "java.y"
                                                                {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[-2].nd));
                // node* n1 =;
                (yyval.ndl)->push_back( new node(++cnt,(yyvsp[-1]. id)));
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4249 "y.tab.c"
    break;

  case 230:
#line 1386 "java.y"
                                                                {
                // $$ = $1;
                // node* n1 = ;
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[-2].nd));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-1]. id)));
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4262 "y.tab.c"
    break;

  case 231:
#line 1394 "java.y"
                                                                {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[-2].nd));
                (yyval.ndl)->push_back(new node(++cnt,(yyvsp[-1]. id)));
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4273 "y.tab.c"
    break;

  case 232:
#line 1402 "java.y"
                                       {
        (yyval.nd) = new node(++cnt,"AdditiveExpression");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 4282 "y.tab.c"
    break;

  case 233:
#line 1409 "java.y"
                                      {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4291 "y.tab.c"
    break;

  case 234:
#line 1413 "java.y"
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
#line 4306 "y.tab.c"
    break;

  case 235:
#line 1423 "java.y"
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
#line 4321 "y.tab.c"
    break;

  case 236:
#line 1435 "java.y"
                                 {
        (yyval.nd) = new node(++cnt,"ShiftExpression");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 4330 "y.tab.c"
    break;

  case 237:
#line 1442 "java.y"
                                {
                (yyval.ndl) = new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4339 "y.tab.c"
    break;

  case 238:
#line 1446 "java.y"
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
#line 4354 "y.tab.c"
    break;

  case 239:
#line 1456 "java.y"
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
#line 4369 "y.tab.c"
    break;

  case 240:
#line 1466 "java.y"
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
#line 4384 "y.tab.c"
    break;

  case 241:
#line 1478 "java.y"
                                           {
        (yyval.nd) = new node(++cnt,"RelationalExpression");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 4393 "y.tab.c"
    break;

  case 242:
#line 1485 "java.y"
                             {
                // $$ = new node(++cnt,"RelationalExpression");
                // nodjoin($$,$1);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4404 "y.tab.c"
    break;

  case 243:
#line 1491 "java.y"
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
#line 4419 "y.tab.c"
    break;

  case 244:
#line 1501 "java.y"
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
#line 4434 "y.tab.c"
    break;

  case 245:
#line 1511 "java.y"
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
#line 4449 "y.tab.c"
    break;

  case 246:
#line 1521 "java.y"
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
#line 4464 "y.tab.c"
    break;

  case 247:
#line 1533 "java.y"
                                       {
 (yyval.nd) = new node(++cnt,"EqualityExpression");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 4473 "y.tab.c"
    break;

  case 248:
#line 1540 "java.y"
                                  {
                // $$ = new node(++cnt,"EqualityExpression");
                // nodjoin($$,$1);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4484 "y.tab.c"
    break;

  case 249:
#line 1546 "java.y"
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
#line 4499 "y.tab.c"
    break;

  case 250:
#line 1556 "java.y"
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
#line 4514 "y.tab.c"
    break;

  case 251:
#line 1567 "java.y"
                             {
        (yyval.nd) = new node(++cnt,"AndExpression");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 4523 "y.tab.c"
    break;

  case 252:
#line 1573 "java.y"
                                {
                // $$ = new node(++cnt,"AndExpression");
                // nodjoin($$,$1);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4534 "y.tab.c"
    break;

  case 253:
#line 1579 "java.y"
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
#line 4549 "y.tab.c"
    break;

  case 254:
#line 1591 "java.y"
                                             {
        (yyval.nd) = new node(++cnt,"ExclusiveOrExpression");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 4558 "y.tab.c"
    break;

  case 255:
#line 1597 "java.y"
                           {
                // $$ = new node(++cnt,"ExclusiveOrExpression");
                // nodjoin($$,$1);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4569 "y.tab.c"
    break;

  case 256:
#line 1603 "java.y"
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
#line 4584 "y.tab.c"
    break;

  case 257:
#line 1615 "java.y"
                                             {
        (yyval.nd) = new node(++cnt,"InclusiveOrExpression");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 4593 "y.tab.c"
    break;

  case 258:
#line 1622 "java.y"
                                   {
                // $$ = new node(++cnt,"InclusiveOrExpression");
                // nodjoin($$,$1);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4604 "y.tab.c"
    break;

  case 259:
#line 1628 "java.y"
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
#line 4619 "y.tab.c"
    break;

  case 260:
#line 1640 "java.y"
                                                   {
        (yyval.nd) = new node(++cnt,"ConditionalAndExpression");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 4628 "y.tab.c"
    break;

  case 261:
#line 1647 "java.y"
                                   {
                // $$ = new node(++cnt,"conditionalAndExpression");
                // nodjoin($$,$1);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4639 "y.tab.c"
    break;

  case 262:
#line 1653 "java.y"
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
#line 4654 "y.tab.c"
    break;

  case 263:
#line 1665 "java.y"
                                                 {
        (yyval.nd) = new node(++cnt,"ConditionalOrExpression");
                nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 4663 "y.tab.c"
    break;

  case 264:
#line 1672 "java.y"
                                      {
                // $$ = new node(++cnt,"ConditionalOrExpression");
                // nodjoin($$,$1);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4674 "y.tab.c"
    break;

  case 265:
#line 1678 "java.y"
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
#line 4689 "y.tab.c"
    break;

  case 266:
#line 1690 "java.y"
                                               {
        (yyval.nd) = new node(++cnt,"ConditionalExpression");
        nodtolis((yyval.nd),(yyvsp[0].ndl));
}
#line 4698 "y.tab.c"
    break;

  case 267:
#line 1696 "java.y"
                                                     {
                // $$ = new node(++cnt,"ConditionalExpression");
                // nodjoin($$,$1);
                (yyval.ndl)=new nodes();
                (yyval.ndl)->push_back((yyvsp[0].nd));
        }
#line 4709 "y.tab.c"
    break;

  case 268:
#line 1702 "java.y"
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
#line 4729 "y.tab.c"
    break;

  case 269:
#line 1720 "java.y"
                                   {
                (yyval.nd) = new node(++cnt,"AssignmentExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4738 "y.tab.c"
    break;

  case 270:
#line 1724 "java.y"
                            {
                (yyval.nd) = new node(++cnt,"AssignmentExpression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4747 "y.tab.c"
    break;

  case 271:
#line 1731 "java.y"
                                                                  {
                (yyval.nd) = new node(++cnt,"Assignment");
                nodjoin((yyval.nd),(yyvsp[-2].nd));
                nodjoin((yyval.nd),(yyvsp[-1].nd));
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4758 "y.tab.c"
    break;

  case 272:
#line 1740 "java.y"
                      {
                (yyval.nd) = new node(++cnt,"LeftHandSide");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4767 "y.tab.c"
    break;

  case 273:
#line 1744 "java.y"
                                 {
                (yyval.nd) = new node(++cnt,"LeftHandSide");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4776 "y.tab.c"
    break;

  case 274:
#line 1748 "java.y"
                                 {
                (yyval.nd) = new node(++cnt,"LeftHandSide");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4785 "y.tab.c"
    break;

  case 275:
#line 1755 "java.y"
                       {
                (yyval.nd) = new node(++cnt,"AssignmentOperator");
                nodtostr((yyval.nd),(yyvsp[0]. id),"EQUAL");
        }
#line 4794 "y.tab.c"
    break;

  case 276:
#line 1759 "java.y"
                          {
                (yyval.nd) = new node(++cnt,"AssignmentOperator");
                nodtostr((yyval.nd),(yyvsp[0]. id),"MULEQUAL");
        }
#line 4803 "y.tab.c"
    break;

  case 277:
#line 1763 "java.y"
                          {
                (yyval.nd) = new node(++cnt,"AssignmentOperator");
                nodtostr((yyval.nd),(yyvsp[0]. id),"DIVEQUAL");
        }
#line 4812 "y.tab.c"
    break;

  case 278:
#line 1767 "java.y"
                          {
                (yyval.nd) = new node(++cnt,"AssignmentOperator");
                nodtostr((yyval.nd),(yyvsp[0]. id),"SUBEQUAL");
        }
#line 4821 "y.tab.c"
    break;

  case 279:
#line 1771 "java.y"
                          {
                (yyval.nd) = new node(++cnt,"AssignmentOperator");
                nodtostr((yyval.nd),(yyvsp[0]. id),"ANDEQUAL");
        }
#line 4830 "y.tab.c"
    break;

  case 280:
#line 1775 "java.y"
                          {
                (yyval.nd) = new node(++cnt,"AssignmentOperator");
                nodtostr((yyval.nd),(yyvsp[0]. id),"ADDEQUAL");
        }
#line 4839 "y.tab.c"
    break;

  case 281:
#line 1782 "java.y"
                                      {
                (yyval.nd) = new node(++cnt,"Expression");
                nodjoin((yyval.nd),(yyvsp[0].nd));
        }
#line 4848 "y.tab.c"
    break;


#line 4852 "y.tab.c"

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
#line 1788 "java.y"


int main(void) {
    yyparse();
    return 0;
}

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
#line 1 "javaR.y"

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

#line 105 "y.tab.c"

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
    STRING = 310,
    CONTINUE = 311,
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
    UNDERSEMICOLONORE = 358,
    EXPORTS = 359,
    OPENS = 360,
    REQUIRES = 361,
    USES = 362,
    MODULE = 363,
    PERMITS = 364,
    SEALED = 365,
    VAR = 366,
    NONSEALED = 367,
    PROVIDES = 368,
    TO = 369,
    WITH = 370,
    OPEN = 371,
    RECORD = 372,
    TRANSITIVE = 373,
    YIELD = 374,
    NULLLITERAL = 375,
    SUSPEND = 376,
    FOR = 377,
    WHILE = 378,
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
#define STRING 310
#define CONTINUE 311
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
#define UNDERSEMICOLONORE 358
#define EXPORTS 359
#define OPENS 360
#define REQUIRES 361
#define USES 362
#define MODULE 363
#define PERMITS 364
#define SEALED 365
#define VAR 366
#define NONSEALED 367
#define PROVIDES 368
#define TO 369
#define WITH 370
#define OPEN 371
#define RECORD 372
#define TRANSITIVE 373
#define YIELD 374
#define NULLLITERAL 375
#define SUSPEND 376
#define FOR 377
#define WHILE 378
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
#line 38 "javaR.y"

        double num;
    char* str;
    struct loop{
        char *st;
        int num;
    }loop_var;

#line 428 "y.tab.c"

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
#define YYLAST   1475

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  132
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  170
/* YYNRULES -- Number of rules.  */
#define YYNRULES  304
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  483

#define YYUNDEFTOK  2
#define YYMAXUTOK   386


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
     125,   126,   127,   128,   129,   130,   131
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,    65,    65,    68,    69,    70,    71,    72,    73,    76,
      77,    80,    81,    82,    85,    86,    89,    90,    91,    94,
      95,    98,   101,   102,   103,   106,   107,   110,   113,   116,
     118,   122,   123,   127,   131,   132,   136,   137,   140,   141,
     144,   145,   148,   151,   154,   157,   158,   161,   162,   163,
     166,   166,   167,   167,   169,   172,   173,   177,   180,   184,
     185,   189,   190,   193,   194,   195,   198,   199,   202,   205,
     206,   209,   217,   226,   230,   231,   234,   235,   238,   241,
     242,   245,   249,   250,   253,   256,   256,   257,   260,   261,
     264,   267,   270,   273,   276,   282,   285,   286,   292,   293,
     297,   298,   301,   302,   308,   309,   313,   316,   317,   321,
     324,   325,   328,   331,   332,   335,   336,   339,   341,   345,
     346,   347,   348,   349,   350,   351,   354,   355,   358,   359,
     360,   361,   362,   366,   367,   368,   369,   370,   371,   374,
     377,   380,   384,   387,   388,   389,   390,   391,   392,   393,
     396,   399,   402,   406,   406,   406,   409,   409,   409,   409,
     412,   412,   412,   412,   415,   415,   415,   420,   420,   420,
     423,   426,   427,   429,   432,   433,   435,   438,   439,   442,
     442,   442,   448,   448,   448,   455,   456,   460,   464,   465,
     468,   470,   471,   476,   480,   484,   485,   488,   489,   492,
     493,   494,   495,   496,   497,   500,   503,   504,   508,   509,
     513,   514,   517,   518,   521,   524,   525,   528,   529,   532,
     532,   532,   532,   538,   539,   540,   541,   542,   545,   546,
     550,   551,   552,   553,   557,   561,   565,   566,   567,   568,
     569,   573,   577,   581,   582,   583,   584,   588,   589,   590,
     593,   597,   598,   599,   600,   603,   607,   608,   609,   612,
     616,   617,   618,   619,   622,   626,   627,   632,   637,   642,
     649,   653,   654,   659,   665,   668,   669,   673,   676,   677,
     680,   684,   685,   688,   692,   693,   696,   700,   701,   704,
     707,   708,   712,   713,   717,   721,   722,   723,   727,   728,
     729,   730,   731,   732,   736
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
  "STRING", "CONTINUE", "NEW", "SWITCH", "ASSERT", "DEFAULT", "IF",
  "PACKAGE", "SYNCHRONIZED", "BOOLEAN", "DO", "GOTO", "PRIVATE", "THIS",
  "BREAK", "DOUBLE", "IMPLEMENTS", "PROTECTED", "THROWS", "BYTE", "ELSE",
  "IMPORT", "PUBLIC", "THROW", "CASE", "ENUM", "INSTANCE", "RETURN",
  "TRANSIENT", "CATCH", "EXTENDS", "INT", "SHORT", "TRY", "CHAR", "FINAL",
  "INTERFACE", "STATIC", "VOID", "CLASS", "FINALLY", "LONG", "STRICTFP",
  "VOLATILE", "CONST", "FLOAT", "NATIVE", "SUPER", "UNDERSEMICOLONORE",
  "EXPORTS", "OPENS", "REQUIRES", "USES", "MODULE", "PERMITS", "SEALED",
  "VAR", "NONSEALED", "PROVIDES", "TO", "WITH", "OPEN", "RECORD",
  "TRANSITIVE", "YIELD", "NULLLITERAL", "SUSPEND", "FOR", "WHILE",
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
  "FormalParameterListOpt", "FormalParameterListOpts", "MethodDeclarator",
  "MethodDeclarators", "$@3", "FormalParameterList", "FormalParameter",
  "MethodBody", "StaticInitializer", "ConstructorDeclaration",
  "ConstructorDeclarator", "BlockStatementOpt", "BlockStatementOpts",
  "ArgumentListOpt", "ConstructorBody", "ExplicitConstructorInvocation",
  "VarInitializersOpt", "CommaOpt", "CommaOpts", "ArrayInitializer",
  "VarInitializers", "Block", "BlockStatements", "BlockStatement",
  "LocalVariableDeclarationStatement", "LocalVariableDeclaration",
  "Statement", "PrintlnStatement", "StatementNoShortIf",
  "StatementWithoutTrailingSubstatement", "EmptyStatement",
  "LabeledStatement", "LabeledStatementNoShortIf", "ExpressionStatement",
  "StatementExpression", "ClassInstanceCreationExpression", "ClassType",
  "ClassOrInterfaceType", "IfThenStatement", "$@4", "$@5",
  "IfThenElseStatement", "$@6", "$@7", "$@8",
  "IfThenElseStatementNoShortIf", "$@9", "$@10", "$@11", "WhileStatement",
  "$@12", "$@13", "WhileStatementNoShortIf", "$@14", "$@15", "ForInitOpt",
  "ForInitOpts", "ExpressionOpt", "ExpressionOpts", "ForUpdateOpt",
  "ForUpdateOpts", "ForStatement", "$@16", "$@17", "ForStatementNoShortIf",
  "$@18", "$@19", "ForInit", "ForUpdate", "StatementExpressionList",
  "IDENTOpt", "IDENTOpts", "BreakStatement", "ContinueStatement",
  "ReturnStatement", "Primary", "PrimaryNoNewArray", "ArrayAccesses",
  "ArgumentList", "DimsOpt", "ArrayCreationExpression", "DimExprs",
  "DimExpr", "Dims", "FieldAccess", "MethodInvocation", "$@20", "$@21",
  "$@22", "ArrayAccess", "PostFixExpression", "PostIncrementExpression",
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
     385,   386
};
# endif

#define YYPACT_NINF (-429)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-298)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     -61,   -92,    40,  -429,   109,  -429,   -61,  -429,  -429,  -429,
    -429,    38,  -429,  -429,  -429,  -429,  -429,  -429,  -429,  -429,
     109,  -429,    56,  -429,  -429,   -26,  -429,  -429,   -36,  -429,
    -429,   -54,    52,    70,    19,  -429,  -429,    90,    25,     9,
    -429,   -12,   118,    79,   159,  -429,    20,  -429,  -429,  -429,
    -429,   118,  -429,  -429,    90,   829,  -429,  -429,  -429,  -429,
    -429,  -429,     2,  -429,  -429,     7,   185,  -429,  -429,  -429,
    -429,   188,    44,   193,   194,  -429,  -429,  -429,  -429,  -429,
     195,  1344,  -429,  1344,  1344,    78,   128,   201,  -429,    78,
    1344,   198,  -429,  1344,   208,  -429,  -429,  -429,   170,  -429,
    -429,  -429,  -429,    88,   113,   219,  -429,  -429,   829,  -429,
    -429,   220,  -429,  -429,  -429,  -429,  -429,  -429,   225,  -429,
    -429,  -429,  -429,  -429,  -429,  -429,  -429,   217,  -429,   227,
    -429,   307,    71,  -429,   102,   126,   133,  -429,  -429,  -429,
     319,  -429,   230,  -429,   235,   162,  -429,    11,  -429,   230,
     231,   233,   237,   116,   128,   924,  -429,  1198,  1019,  1344,
    1344,  1344,  1344,   -31,    22,  -429,   102,  -429,  -429,  -429,
    -429,  -429,  -429,  -429,    93,  -429,   131,  -429,   135,  -429,
     104,  -429,     3,  -429,   197,  -429,   203,  -429,   206,  -429,
     226,  -429,   -19,  -429,  -429,  -429,  -429,  -429,   257,   221,
     256,  -429,  -429,  -429,  -429,   254,  -429,   260,   276,   265,
    -429,  -429,   287,   281,   282,  -429,  -429,   166,   286,  1118,
     297,   636,  -429,   290,  1344,  1215,   300,  -429,  -429,  -429,
    -429,   178,  1344,  -429,  -429,  -429,  -429,  -429,  -429,  -429,
    -429,  1344,   304,   302,  -429,    88,   305,  1264,  -429,  -429,
    -429,  -429,    88,   310,  -429,   314,  -429,   312,    16,   320,
     829,   306,   329,   311,   299,   331,  -429,  -429,  -429,  -429,
    1344,  1344,  1344,  1344,  1344,  1344,  1344,  1344,  1344,  1344,
    1344,  1344,  1344,  1344,  1344,  1344,  1344,  1344,  1344,  1344,
    1344,  -429,  -429,  1344,   339,  -429,  1344,  1344,  1344,  -429,
    -429,    96,  -429,  -429,  -429,   328,  -429,  -429,   336,  1344,
    -429,   342,   338,  -429,   344,  -429,   123,   345,  -429,   128,
    -429,  -429,  -429,  1264,  -429,  -429,  -429,   343,  -429,   128,
    1344,  1344,  -429,   349,  -429,   346,   351,   357,   355,  1247,
      67,  1294,  -429,  -429,  -429,    93,    93,   131,   131,   131,
     135,   135,   135,   135,   104,   104,     3,   197,   203,   206,
     226,   321,   356,  1247,  -429,   355,   359,   364,   365,  1344,
    1344,  -429,   348,   366,  -429,  1344,  -429,  1344,  1344,  1344,
    -429,   368,  -429,   363,  -429,  -429,   372,   374,  -429,  -429,
    -429,  1344,   371,  1294,  -429,  1344,  -429,  -429,  -429,  -429,
     376,   377,  1344,  -429,  -429,  -429,  -429,   383,   382,  -429,
    1264,   384,  -429,   385,   387,  -429,  -429,  -429,  -429,  1069,
     636,  -429,  -429,  -429,   636,   388,  -429,  -429,  -429,  -429,
    -429,  -429,  -429,   397,  -429,   378,   333,  -429,  -429,  -429,
    -429,  -429,  -429,   395,  -429,  -429,   405,  1118,   413,  1069,
    -429,   348,  1344,   410,  1344,  -429,   636,   417,  -429,  -429,
     336,   419,  -429,   422,  -429,   636,  -429,  1344,  -429,  -429,
    1069,  -429,  1069,   362,   420,  -429,  -429,   348,  1069,   431,
    -429,  1069,  -429
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
      67,     0,    64,    65,     0,    97,    92,    13,    12,    20,
      16,    18,     0,    17,    19,     0,     9,    11,    14,    15,
      10,    21,     0,    25,     0,    57,    62,    78,    91,    53,
       0,     0,   139,     0,     0,   192,     0,     0,   200,   192,
     175,     0,     8,   175,     0,   164,     3,     4,    27,     6,
       7,     5,   199,     0,   295,     0,    95,   133,    96,   113,
     115,     0,   116,   125,   119,   134,   120,   135,     0,   149,
     121,   122,   123,   124,   136,   137,   138,   230,   197,   204,
     198,   202,   148,   205,     0,   146,   147,   144,   145,   143,
       0,    85,    80,    84,    74,     0,    69,    71,    73,    79,
       0,     0,     0,     0,    83,    97,    93,     0,     0,     0,
       0,     0,     0,     0,   231,   203,   243,   232,   233,   251,
     236,   237,   240,   246,   256,   250,   260,   255,   265,   259,
     271,   264,   275,   270,   278,   274,   281,   277,   284,   280,
     287,   283,   290,   286,   292,   289,   304,   293,     0,   231,
     204,   202,   241,   242,   191,     0,   190,     0,   152,     0,
     151,   156,     0,     0,     0,   173,   174,     0,     0,   172,
       0,     0,    74,   118,    99,     0,     0,   112,   114,   117,
     142,     0,     0,   234,   235,   303,   301,   299,   300,   302,
     298,     0,     0,     0,    68,     0,     0,     0,    22,    24,
      23,    28,     0,     0,    81,    82,    88,   200,     0,     0,
      97,     0,     0,   209,   231,     0,   245,   244,   238,   239,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   201,   194,     0,   210,   212,    99,     0,     0,   193,
     195,   218,   196,   186,   188,     0,   170,   171,   185,     0,
     140,     0,    98,   206,     0,   220,   217,     0,   294,    83,
      87,    70,    75,   105,    72,    77,    76,    90,    94,     0,
      99,    99,   101,     0,   127,     0,     0,     0,   208,     0,
       0,   201,   252,   253,   254,   257,   258,   263,   261,   262,
     269,   268,   266,   267,   272,   273,   276,   279,   282,   285,
     288,     0,     0,     0,   213,   211,     0,     0,     0,    99,
      99,   179,     0,     0,   225,     0,   228,    99,    99,    99,
     229,     0,   110,   108,   104,    89,     0,     0,   100,   126,
     215,     0,     0,     0,   248,     0,   214,   150,   157,   154,
       0,     0,   175,   189,   165,   207,   221,     0,     0,    86,
     107,     0,   106,     0,     0,   247,   216,   249,   291,     0,
       0,   224,   227,   180,     0,     0,   223,   226,   111,   109,
     102,   103,   160,     0,   167,    27,     0,   128,   129,   130,
     131,   132,   155,     0,   166,   222,     0,   172,     0,     0,
     158,   178,     0,     0,     0,   141,     0,     0,   176,   177,
     187,     0,   182,     0,   159,     0,   161,   175,   168,   181,
       0,   183,     0,     0,     0,   169,   162,   178,     0,     0,
     163,     0,   184
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -429,  -429,  -429,   -29,   -65,  -429,  -429,  -429,  -429,  -429,
      -1,   400,  -429,  -429,  -429,  -429,  -429,  -429,  -429,  -429,
     432,  -429,  -429,   425,  -429,   430,  -429,  -429,  -429,    14,
    -429,   393,  -429,  -429,  -429,   411,  -429,  -429,   353,   222,
     210,  -429,  -315,  -429,  -429,   146,  -429,   401,  -429,  -429,
    -429,   141,  -429,  -429,  -429,  -429,  -149,  -429,  -264,  -429,
    -429,  -429,  -429,  -429,  -429,  -429,   -20,  -429,   367,  -429,
    -214,  -211,  -429,  -350,  -408,  -429,  -429,  -429,  -429,  -210,
    -429,  -429,  -429,  -429,  -429,  -429,  -429,  -429,  -429,  -429,
    -429,  -429,  -429,  -429,  -429,  -429,  -429,  -429,  -429,  -429,
      24,  -429,   -89,  -429,    -8,  -429,  -429,  -429,  -429,  -429,
    -429,  -429,  -429,  -429,  -428,   391,  -429,  -429,  -429,  -429,
    -429,  -429,   501,  -429,  -429,  -429,  -429,   180,  -251,   572,
     -52,  -429,  -429,  -429,  -429,   -37,   -17,    39,   -82,    77,
     140,  -334,  -429,   -94,  -429,  -188,  -429,   -60,  -429,   -96,
    -429,   196,  -429,   199,  -429,   200,  -429,   204,  -429,   202,
    -429,  -429,  -429,    87,  -429,   242,   155,  -429,  -429,   361
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     2,   102,   103,    66,    67,    68,    69,    70,    71,
     164,    12,    13,     3,     4,     5,    18,    19,     6,    20,
       7,     8,     9,    21,    22,    23,    24,    37,    38,    25,
      26,    40,    44,    45,    46,    47,    48,    49,   145,   146,
     147,   148,   324,    50,    51,   253,   254,   142,   143,   242,
     255,   256,    77,    52,    53,    74,   105,   106,   311,   156,
     260,   383,   411,   412,   325,   384,   107,   108,   109,   110,
     111,   112,   113,   436,   114,   115,   116,   438,   117,   118,
     119,   209,   210,   120,   212,   420,   121,   297,   419,   456,
     439,   446,   470,   478,   122,   220,   424,   440,   448,   472,
     305,   306,   214,   215,   457,   458,   123,   402,   443,   441,
     467,   474,   307,   459,   308,   205,   206,   124,   125,   126,
     127,   128,   129,   312,   337,   130,   294,   295,   338,   131,
     165,   226,   377,   425,   133,   166,   167,   168,   169,   170,
     171,   172,   173,   174,   175,   176,   177,   178,   179,   180,
     181,   182,   183,   184,   185,   186,   187,   188,   189,   190,
     191,   192,   193,   194,   195,   196,   197,   140,   241,   313
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      11,   202,   203,   132,   218,   303,   259,   394,   382,   304,
     310,   437,    32,   340,    65,     1,   -60,   289,   134,   246,
     331,   207,    56,   460,    57,   290,  -219,   -59,   224,   217,
     270,    78,   366,    58,   283,   153,    10,   284,   135,    59,
      14,   437,    72,   365,  -295,  -295,  -295,  -295,  -295,   460,
     247,    28,   152,    43,   104,    60,   132,   153,    61,   417,
      43,  -295,   437,    35,   437,    63,   386,   387,    31,    64,
     437,   134,   393,   437,    34,   392,    15,   266,   267,   268,
     269,    36,   199,   199,  -203,   208,    16,    15,   347,   348,
     349,   135,    33,   263,   136,   428,    39,    16,   207,   455,
     369,    42,   370,   132,   -52,   400,   401,   104,  -203,  -203,
      41,   333,    42,   406,   407,   408,    54,  -219,   134,   224,
     473,   225,   475,    15,    55,   252,   153,   378,   480,   379,
     141,   482,   137,    16,    57,   144,   279,   280,   135,   233,
     234,   271,   272,    58,   281,   282,   273,   136,    17,    59,
    -231,  -231,   276,    72,   104,   277,   278,   264,   199,   199,
     199,   199,   403,  -232,  -232,    60,    75,   132,    61,   132,
    -233,  -233,    62,   244,   245,    63,    15,   274,   275,    64,
     345,   346,   134,    57,   134,   137,    16,   354,   355,   342,
     343,   344,    58,   150,   136,   138,   151,   154,    59,   157,
     155,    17,   135,   -56,   135,   211,   204,    10,   132,   442,
     139,   217,   219,   444,    60,   221,   222,    61,   104,   350,
     351,   352,   353,   134,    63,  -219,   227,   224,    64,   270,
     231,   229,   137,   303,   153,   232,   230,   304,   243,   -85,
     248,   304,   249,   135,   251,   464,   250,   285,   138,  -297,
    -297,  -297,  -297,  -297,   469,   286,    10,   287,   136,   104,
     136,   288,   291,   139,   232,   292,  -297,   304,   293,   296,
     199,   199,   199,   199,   199,   199,   199,   199,   199,   199,
     199,   199,   199,   199,   199,   199,   199,   199,   199,   153,
     252,   298,   299,   300,   301,   138,   137,   302,   137,   136,
     252,   309,   245,  -219,   315,   224,   316,   339,   319,   415,
     139,   320,   153,   423,   322,   328,   330,   334,    72,   336,
     132,  -295,  -295,  -295,  -295,  -295,   329,   332,    72,  -296,
    -296,  -296,  -296,  -296,   335,   134,   341,   137,  -295,   371,
     199,   235,   236,   237,   238,   239,  -296,   363,   372,   374,
     375,   246,    81,   376,   380,   135,   388,   389,   240,   138,
     390,   138,   391,   392,   397,   396,   395,   132,   132,   398,
     399,   404,   132,   409,   139,   410,   139,   413,   471,   414,
     416,   421,   134,   134,   422,    83,    84,   134,   426,   427,
     199,   429,   199,   445,   199,   132,   430,   132,   431,   132,
     138,   447,   135,   135,   132,    86,   451,   135,   450,   452,
     134,   136,   134,   132,   134,   139,    88,   454,   132,   134,
     132,   462,   465,   449,   466,   132,   132,   468,   134,   132,
     135,   477,   135,   134,   135,   134,   481,   476,    27,   135,
     134,   134,   198,    73,   134,    29,   104,    79,   135,   137,
      91,   216,    30,   135,   216,   135,   223,    76,   136,   136,
     135,   135,   327,   136,   135,   381,   149,   321,    92,   479,
     385,   453,    96,    97,   364,   228,    10,    99,   100,   101,
     213,   356,   418,   318,     0,   357,   136,   358,   136,     0,
     136,   360,   359,     0,     0,   136,   137,   137,     0,     0,
       0,   137,     0,     0,   136,     0,     0,     0,     0,   136,
       0,   136,   138,     0,     0,     0,   136,   136,   262,   265,
     136,     0,     0,     0,   137,     0,   137,   139,   137,     0,
       0,     0,     0,   137,     0,     0,     0,     0,     0,     0,
       0,     0,   137,     0,     0,     0,     0,   137,     0,   137,
       0,     0,     0,     0,   137,   137,     0,     0,   137,   138,
     138,     0,     0,     0,   138,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   139,   139,     0,     0,     0,   139,
       0,     0,     0,     0,   200,   200,   314,   138,     0,   138,
       0,   138,     0,   317,     0,     0,   138,     0,     0,     0,
       0,     0,   139,     0,   139,   138,   139,     0,   326,     0,
     138,   139,   138,     0,     0,     0,     0,   138,   138,     0,
     139,   138,     0,     0,     0,   139,     0,   139,     0,     0,
       0,   314,   139,   139,     0,     0,   139,     0,     0,    80,
      81,     0,    55,     0,     0,     0,     0,    82,     0,     0,
       0,   361,     0,     0,   362,   201,   201,     0,   367,   368,
     200,   200,   200,   200,     0,     0,     0,     0,     0,     0,
     373,     0,     0,    83,    84,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   326,     0,     0,     0,     0,     0,
       0,     0,    85,    86,     0,     0,     0,    87,     0,     0,
     314,     0,     0,     0,    88,    89,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    90,     0,
       0,     0,     0,     0,   362,     0,     0,     0,     0,     0,
       0,   201,   201,   201,   201,     0,   405,     0,    91,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    92,    93,    94,    95,
      96,    97,     0,   216,    98,    99,   100,   101,     0,     0,
       0,   326,   200,   200,   200,   200,   200,   200,   200,   200,
     200,   200,   200,   200,   200,   200,   200,   200,   200,   200,
     200,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   461,     0,   463,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   216,     0,
       0,     0,    80,    81,     0,    55,     0,     0,     0,     0,
      82,     0,   200,   201,   201,   201,   201,   201,   201,   201,
     201,   201,   201,   201,   201,   201,   201,   201,   201,   201,
     201,   201,     0,     0,     0,     0,    83,    84,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    57,    85,    86,     0,     0,     0,
      87,     0,   200,    58,   200,     0,   200,    88,    89,    59,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    90,     0,   201,     0,    60,     0,     0,    61,     0,
       0,     0,     0,     0,     0,    63,     0,    80,    81,    64,
      55,    91,     0,     0,     0,    82,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    92,
      93,    94,    95,    96,    97,     0,     0,    98,    99,   100,
     101,    83,    84,   201,     0,   201,     0,   201,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    57,
      85,    86,     0,     0,     0,    87,     0,     0,    58,     0,
       0,     0,   257,    89,    59,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    90,     0,     0,     0,
      60,     0,     0,    61,     0,     0,     0,     0,     0,     0,
      63,     0,     0,   158,    64,     0,   258,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    92,    93,    94,    95,    96,    97,
       0,     0,    98,    99,   100,   101,    83,    84,     0,     0,
       0,   159,   160,     0,     0,   161,   162,     0,     0,     0,
       0,     0,     0,    81,    57,    55,   163,     0,     0,     0,
      82,     0,     0,    58,     0,     0,     0,    88,     0,    59,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    60,    83,    84,    61,     0,
       0,     0,     0,     0,     0,    63,     0,     0,     0,    64,
       0,    91,    81,     0,     0,    85,    86,     0,     0,     0,
     432,     0,     0,     0,     0,     0,     0,    88,    89,    92,
       0,     0,     0,    96,    97,     0,     0,    10,    99,   100,
     101,    90,     0,     0,     0,    83,    84,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    91,     0,    57,     0,    86,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,    88,     0,    59,    92,
      93,   433,   434,    96,    97,     0,     0,   435,    99,   100,
     101,     0,   158,   261,    60,     0,     0,    61,     0,     0,
       0,     0,     0,     0,    63,     0,     0,     0,    64,   158,
      91,     0,     0,     0,   250,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    83,    84,     0,    92,     0,
     159,   160,    96,    97,   161,   162,    10,    99,   100,   101,
       0,   158,    83,    84,     0,   163,   390,   159,   160,     0,
       0,   161,   162,     0,     0,     0,    88,     0,   158,     0,
     323,     0,   163,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    88,    83,    84,     0,     0,     0,   159,
     160,     0,     0,   161,   162,     0,     0,     0,   158,     0,
      91,    83,    84,     0,   163,     0,   159,   160,     0,     0,
     161,   162,     0,     0,     0,    88,     0,    91,    92,     0,
       0,   163,    96,    97,     0,     0,    10,    99,   100,   101,
       0,     0,    88,     0,     0,    92,   159,   160,     0,    96,
      97,     0,     0,    10,    99,   100,   101,     0,   158,    91,
       0,   163,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    88,     0,     0,     0,    91,    92,     0,     0,
       0,    96,    97,     0,     0,    10,    99,   100,   101,     0,
       0,    83,    84,     0,    92,     0,   159,   160,    96,    97,
     161,   162,    10,    99,   100,   101,    91,     0,     0,     0,
       0,   163,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    88,     0,    92,     0,     0,     0,    96,    97,
       0,     0,    10,    99,   100,   101,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    91,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    92,     0,     0,     0,    96,    97,
       0,     0,    10,    99,   100,   101
};

static const yytype_int16 yycheck[] =
{
       1,    83,    84,    55,    93,   219,   155,   341,   323,   219,
     221,   419,    48,   264,    43,    76,     7,    36,    55,     8,
       4,    86,    42,   451,    55,    44,     4,     7,     6,    13,
       8,    51,   296,    64,    31,    13,   128,    34,    55,    70,
       0,   449,    43,   294,    22,    23,    24,    25,    26,   477,
      39,    13,     8,    39,    55,    86,   108,    13,    89,   393,
      46,    39,   470,    11,   472,    96,   330,   331,    94,   100,
     478,   108,     5,   481,   128,     8,    67,   159,   160,   161,
     162,    11,    83,    84,    13,    86,    77,    67,   276,   277,
     278,   108,   128,   158,    55,   410,     6,    77,   163,   449,
       4,    92,     6,   155,    85,   369,   370,   108,    37,    38,
      85,   260,    92,   377,   378,   379,   128,     4,   155,     6,
     470,     8,   472,    67,     6,   154,    13,     4,   478,     6,
     128,   481,    55,    77,    55,   128,    32,    33,   155,    37,
      38,    48,    49,    64,    40,    41,    53,   108,    92,    70,
      37,    38,    17,   154,   155,    20,    21,   158,   159,   160,
     161,   162,   372,    37,    38,    86,     7,   219,    89,   221,
      37,    38,    93,    11,    12,    96,    67,    46,    47,   100,
     274,   275,   219,    55,   221,   108,    77,   283,   284,   271,
     272,   273,    64,     8,   155,    55,     8,     4,    70,     4,
       6,    92,   219,    94,   221,     4,   128,   128,   260,   420,
      55,    13,     4,   424,    86,    45,   128,    89,   219,   279,
     280,   281,   282,   260,    96,     4,     7,     6,   100,     8,
      13,    11,   155,   447,    13,     8,    11,   447,     8,     4,
       9,   451,     9,   260,   128,   456,     9,    50,   108,    22,
      23,    24,    25,    26,   465,    52,   128,    51,   219,   260,
     221,    35,     5,   108,     8,    11,    39,   477,     8,     4,
     271,   272,   273,   274,   275,   276,   277,   278,   279,   280,
     281,   282,   283,   284,   285,   286,   287,   288,   289,    13,
     319,     4,    11,    11,   128,   155,   219,    11,   221,   260,
     329,     4,    12,     4,     4,     6,   128,     8,     4,   391,
     155,     9,    13,   402,     9,     5,     4,    11,   319,     8,
     372,    22,    23,    24,    25,    26,    12,     7,   329,    22,
      23,    24,    25,    26,     5,   372,     5,   260,    39,    11,
     341,    22,    23,    24,    25,    26,    39,     8,    12,     7,
      12,     8,     4,     9,     9,   372,     7,    11,    39,   219,
       9,   221,     5,     8,     5,     9,    45,   419,   420,     5,
       5,     5,   424,     5,   219,    12,   221,     5,   467,     5,
       9,     5,   419,   420,     7,    37,    38,   424,     5,     7,
     391,     7,   393,     5,   395,   447,    11,   449,    11,   451,
     260,     4,   419,   420,   456,    57,    11,   424,    75,     4,
     447,   372,   449,   465,   451,   260,    68,     4,   470,   456,
     472,    11,     5,    45,     5,   477,   478,     5,   465,   481,
     447,    11,   449,   470,   451,   472,     5,    75,     6,   456,
     477,   478,    81,    43,   481,    20,   447,    54,   465,   372,
     102,    90,    22,   470,    93,   472,   103,    46,   419,   420,
     477,   478,   252,   424,   481,   319,    65,   245,   120,   477,
     329,   447,   124,   125,   294,   108,   128,   129,   130,   131,
      89,   285,   395,   241,    -1,   286,   447,   287,   449,    -1,
     451,   289,   288,    -1,    -1,   456,   419,   420,    -1,    -1,
      -1,   424,    -1,    -1,   465,    -1,    -1,    -1,    -1,   470,
      -1,   472,   372,    -1,    -1,    -1,   477,   478,   157,   158,
     481,    -1,    -1,    -1,   447,    -1,   449,   372,   451,    -1,
      -1,    -1,    -1,   456,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   465,    -1,    -1,    -1,    -1,   470,    -1,   472,
      -1,    -1,    -1,    -1,   477,   478,    -1,    -1,   481,   419,
     420,    -1,    -1,    -1,   424,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   419,   420,    -1,    -1,    -1,   424,
      -1,    -1,    -1,    -1,    83,    84,   225,   447,    -1,   449,
      -1,   451,    -1,   232,    -1,    -1,   456,    -1,    -1,    -1,
      -1,    -1,   447,    -1,   449,   465,   451,    -1,   247,    -1,
     470,   456,   472,    -1,    -1,    -1,    -1,   477,   478,    -1,
     465,   481,    -1,    -1,    -1,   470,    -1,   472,    -1,    -1,
      -1,   270,   477,   478,    -1,    -1,   481,    -1,    -1,     3,
       4,    -1,     6,    -1,    -1,    -1,    -1,    11,    -1,    -1,
      -1,   290,    -1,    -1,   293,    83,    84,    -1,   297,   298,
     159,   160,   161,   162,    -1,    -1,    -1,    -1,    -1,    -1,
     309,    -1,    -1,    37,    38,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   323,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    56,    57,    -1,    -1,    -1,    61,    -1,    -1,
     339,    -1,    -1,    -1,    68,    69,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,   363,    -1,    -1,    -1,    -1,    -1,
      -1,   159,   160,   161,   162,    -1,   375,    -1,   102,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   120,   121,   122,   123,
     124,   125,    -1,   402,   128,   129,   130,   131,    -1,    -1,
      -1,   410,   271,   272,   273,   274,   275,   276,   277,   278,
     279,   280,   281,   282,   283,   284,   285,   286,   287,   288,
     289,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   452,    -1,   454,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   467,    -1,
      -1,    -1,     3,     4,    -1,     6,    -1,    -1,    -1,    -1,
      11,    -1,   341,   271,   272,   273,   274,   275,   276,   277,
     278,   279,   280,   281,   282,   283,   284,   285,   286,   287,
     288,   289,    -1,    -1,    -1,    -1,    37,    38,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    55,    56,    57,    -1,    -1,    -1,
      61,    -1,   391,    64,   393,    -1,   395,    68,    69,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    82,    -1,   341,    -1,    86,    -1,    -1,    89,    -1,
      -1,    -1,    -1,    -1,    -1,    96,    -1,     3,     4,   100,
       6,   102,    -1,    -1,    -1,    11,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   120,
     121,   122,   123,   124,   125,    -1,    -1,   128,   129,   130,
     131,    37,    38,   391,    -1,   393,    -1,   395,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,
      56,    57,    -1,    -1,    -1,    61,    -1,    -1,    64,    -1,
      -1,    -1,    68,    69,    70,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      86,    -1,    -1,    89,    -1,    -1,    -1,    -1,    -1,    -1,
      96,    -1,    -1,     4,   100,    -1,   102,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   120,   121,   122,   123,   124,   125,
      -1,    -1,   128,   129,   130,   131,    37,    38,    -1,    -1,
      -1,    42,    43,    -1,    -1,    46,    47,    -1,    -1,    -1,
      -1,    -1,    -1,     4,    55,     6,    57,    -1,    -1,    -1,
      11,    -1,    -1,    64,    -1,    -1,    -1,    68,    -1,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    86,    37,    38,    89,    -1,
      -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,    -1,   100,
      -1,   102,     4,    -1,    -1,    56,    57,    -1,    -1,    -1,
      61,    -1,    -1,    -1,    -1,    -1,    -1,    68,    69,   120,
      -1,    -1,    -1,   124,   125,    -1,    -1,   128,   129,   130,
     131,    82,    -1,    -1,    -1,    37,    38,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   102,    -1,    55,    -1,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    64,    -1,    -1,    -1,    68,    -1,    70,   120,
     121,   122,   123,   124,   125,    -1,    -1,   128,   129,   130,
     131,    -1,     4,     5,    86,    -1,    -1,    89,    -1,    -1,
      -1,    -1,    -1,    -1,    96,    -1,    -1,    -1,   100,     4,
     102,    -1,    -1,    -1,     9,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    37,    38,    -1,   120,    -1,
      42,    43,   124,   125,    46,    47,   128,   129,   130,   131,
      -1,     4,    37,    38,    -1,    57,     9,    42,    43,    -1,
      -1,    46,    47,    -1,    -1,    -1,    68,    -1,     4,    -1,
       6,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    68,    37,    38,    -1,    -1,    -1,    42,
      43,    -1,    -1,    46,    47,    -1,    -1,    -1,     4,    -1,
     102,    37,    38,    -1,    57,    -1,    42,    43,    -1,    -1,
      46,    47,    -1,    -1,    -1,    68,    -1,   102,   120,    -1,
      -1,    57,   124,   125,    -1,    -1,   128,   129,   130,   131,
      -1,    -1,    68,    -1,    -1,   120,    42,    43,    -1,   124,
     125,    -1,    -1,   128,   129,   130,   131,    -1,     4,   102,
      -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    68,    -1,    -1,    -1,   102,   120,    -1,    -1,
      -1,   124,   125,    -1,    -1,   128,   129,   130,   131,    -1,
      -1,    37,    38,    -1,   120,    -1,    42,    43,   124,   125,
      46,    47,   128,   129,   130,   131,   102,    -1,    -1,    -1,
      -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    68,    -1,   120,    -1,    -1,    -1,   124,   125,
      -1,    -1,   128,   129,   130,   131,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   120,    -1,    -1,    -1,   124,   125,
      -1,    -1,   128,   129,   130,   131
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,    76,   133,   145,   146,   147,   150,   152,   153,   154,
     128,   142,   143,   144,     0,    67,    77,    92,   148,   149,
     151,   155,   156,   157,   158,   161,   162,   152,    13,   155,
     157,    94,    48,   128,   128,    11,    11,   159,   160,     6,
     163,    85,    92,   161,   164,   165,   166,   167,   168,   169,
     175,   176,   185,   186,   128,     6,   198,    55,    64,    70,
      86,    89,    93,    96,   100,   135,   136,   137,   138,   139,
     140,   141,   142,   143,   187,     7,   167,   184,   198,   163,
       3,     4,    11,    37,    38,    56,    57,    61,    68,    69,
      82,   102,   120,   121,   122,   123,   124,   125,   128,   129,
     130,   131,   134,   135,   142,   188,   189,   198,   199,   200,
     201,   202,   203,   204,   206,   207,   208,   210,   211,   212,
     215,   218,   226,   238,   249,   250,   251,   252,   253,   254,
     257,   261,   262,   266,   267,   268,   269,   271,   272,   298,
     299,   128,   179,   180,   128,   170,   171,   172,   173,   179,
       8,     8,     8,    13,     4,     6,   191,     4,     4,    42,
      43,    46,    47,    57,   142,   262,   267,   268,   269,   270,
     271,   272,   273,   274,   275,   276,   277,   278,   279,   280,
     281,   282,   283,   284,   285,   286,   287,   288,   289,   290,
     291,   292,   293,   294,   295,   296,   297,   298,   301,   142,
     254,   261,   270,   270,   128,   247,   248,   136,   142,   213,
     214,     4,   216,   247,   234,   235,   301,    13,   234,     4,
     227,    45,   128,   170,     6,     8,   263,     7,   200,    11,
      11,    13,     8,    37,    38,    22,    23,    24,    25,    26,
      39,   300,   181,     8,    11,    12,     8,    39,     9,     9,
       9,   128,   135,   177,   178,   182,   183,    68,   102,   188,
     192,     5,   301,   136,   142,   301,   270,   270,   270,   270,
       8,    48,    49,    53,    46,    47,    17,    20,    21,    32,
      33,    40,    41,    31,    34,    50,    52,    51,    35,    36,
      44,     5,    11,     8,   258,   259,     4,   219,     4,    11,
      11,   128,    11,   202,   211,   232,   233,   244,   246,     4,
     203,   190,   255,   301,   301,     4,   128,   301,   297,     4,
       9,   171,     9,     6,   174,   196,   301,   172,     5,    12,
       4,     4,     7,   188,    11,     5,     8,   256,   260,     8,
     260,     5,   270,   270,   270,   275,   275,   277,   277,   277,
     279,   279,   279,   279,   281,   281,   283,   285,   287,   289,
     291,   301,   301,     8,   259,   260,   190,   301,   301,     4,
       6,    11,    12,   301,     7,    12,     9,   264,     4,     6,
       9,   177,   174,   193,   197,   183,   190,   190,     7,    11,
       9,     5,     8,     5,   273,    45,     9,     5,     5,     5,
     190,   190,   239,   211,     5,   301,   190,   190,   190,     5,
      12,   194,   195,     5,     5,   270,     9,   273,   295,   220,
     217,     5,     7,   234,   228,   265,     5,     7,   174,     7,
      11,    11,    61,   122,   123,   128,   205,   206,   209,   222,
     229,   241,   203,   240,   203,     5,   223,     4,   230,    45,
      75,    11,     4,   232,     4,   205,   221,   236,   237,   245,
     246,   301,    11,   301,   203,     5,     5,   242,     5,   203,
     224,   234,   231,   205,   243,   205,    75,    11,   225,   236,
     205,     5,   205
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
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
     176,   177,   178,   178,   179,   181,   180,   180,   182,   182,
     183,   184,   185,   186,   187,   188,   189,   189,   190,   190,
     191,   191,   192,   192,   193,   193,   194,   195,   195,   196,
     197,   197,   198,   199,   199,   200,   200,   201,   202,   203,
     203,   203,   203,   203,   203,   203,   204,   204,   205,   205,
     205,   205,   205,   206,   206,   206,   206,   206,   206,   207,
     208,   209,   210,   211,   211,   211,   211,   211,   211,   211,
     212,   213,   214,   216,   217,   215,   219,   220,   221,   218,
     223,   224,   225,   222,   227,   228,   226,   230,   231,   229,
     232,   233,   233,   234,   235,   235,   236,   237,   237,   239,
     240,   238,   242,   243,   241,   244,   244,   245,   246,   246,
     247,   248,   248,   249,   250,   251,   251,   252,   252,   253,
     253,   253,   253,   253,   253,   254,   255,   255,   256,   256,
     257,   257,   258,   258,   259,   260,   260,   261,   261,   263,
     264,   265,   262,   262,   262,   262,   262,   262,   266,   266,
     267,   267,   267,   267,   268,   269,   270,   270,   270,   270,
     270,   271,   272,   273,   273,   273,   273,   274,   274,   274,
     275,   276,   276,   276,   276,   277,   278,   278,   278,   279,
     280,   280,   280,   280,   281,   282,   282,   282,   282,   282,
     283,   284,   284,   284,   285,   286,   286,   287,   288,   288,
     289,   290,   290,   291,   292,   292,   293,   294,   294,   295,
     296,   296,   297,   297,   298,   299,   299,   299,   300,   300,
     300,   300,   300,   300,   301
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
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
       3,     1,     1,     0,     1,     0,     5,     3,     1,     3,
       2,     1,     2,     3,     4,     1,     1,     0,     1,     0,
       4,     3,     5,     5,     1,     0,     1,     1,     0,     4,
       1,     3,     3,     1,     2,     1,     1,     2,     2,     1,
       1,     1,     1,     1,     1,     1,     5,     4,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       3,     3,     2,     1,     1,     1,     1,     1,     1,     1,
       5,     1,     1,     0,     0,     7,     0,     0,     0,    10,
       0,     0,     0,    10,     0,     0,     7,     0,     0,     7,
       1,     1,     0,     1,     1,     0,     1,     1,     0,     0,
       0,    11,     0,     0,    11,     1,     1,     1,     1,     3,
       1,     1,     0,     3,     3,     3,     3,     1,     1,     1,
       1,     3,     1,     1,     1,     1,     1,     3,     1,     0,
       3,     4,     1,     2,     3,     2,     3,     3,     3,     0,
       0,     0,     7,     6,     6,     4,     6,     6,     4,     4,
       1,     1,     1,     1,     2,     2,     1,     1,     2,     2,
       1,     2,     2,     1,     2,     2,     1,     5,     4,     5,
       1,     1,     3,     3,     3,     1,     1,     3,     3,     1,
       1,     3,     3,     3,     1,     1,     3,     3,     3,     3,
       1,     1,     3,     3,     1,     1,     3,     1,     1,     3,
       1,     1,     3,     1,     1,     3,     1,     1,     3,     1,
       1,     5,     1,     1,     3,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1
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
  case 25:
#line 106 "javaR.y"
                      {(yyval.str)=(yyvsp[0].str);}
#line 2246 "y.tab.c"
    break;

  case 26:
#line 107 "javaR.y"
                        {(yyval.str)=(yyvsp[0].str);}
#line 2252 "y.tab.c"
    break;

  case 27:
#line 110 "javaR.y"
                     {(yyval.str)=(yyvsp[0].str);}
#line 2258 "y.tab.c"
    break;

  case 28:
#line 113 "javaR.y"
                            { (yyval.str)= strConcat((yyvsp[-2].str),strConcat((yyvsp[-1]. str),(yyvsp[0].str)));}
#line 2264 "y.tab.c"
    break;

  case 50:
#line 166 "javaR.y"
                                      {Klass obj; mp[(yyvsp[0].str)] = obj;currClass=(yyvsp[0].str);quad ch;ch.idx=to_string(line++);ch.arg1=(yyvsp[0].str); ir.push_back(ch);}
#line 2270 "y.tab.c"
    break;

  case 51:
#line 166 "javaR.y"
                                                                                                                                                              {currClass="";quad ch;ch.idx=to_string(line++);ch.arg1 = "End Class";ir.push_back(ch);}
#line 2276 "y.tab.c"
    break;

  case 52:
#line 167 "javaR.y"
                                      {Klass obj; mp[(yyvsp[0].str)] = obj;currClass=(yyvsp[0].str);}
#line 2282 "y.tab.c"
    break;

  case 53:
#line 167 "javaR.y"
                                                                                                               {currClass="";}
#line 2288 "y.tab.c"
    break;

  case 71:
#line 209 "javaR.y"
                         {if(currClass!=""&&currMethod==""){
                        mp[currClass].local.push((yyvsp[0].str));
                }
                else if(currClass!=""&&currMethod!=""){
                        mp[currClass]._vtable.methods[currMethod].local.push((yyvsp[0].str));
                        mp[currClass]._vtable.methods[currMethod].storage+=typesize(getType((yyvsp[0].str)));
                }
        }
#line 2301 "y.tab.c"
    break;

  case 72:
#line 217 "javaR.y"
                                                 { if(currClass!=""&&currMethod==""){
                        mp[currClass].local.push((yyvsp[-2].str));
                }
                else if(currClass!=""&&currMethod!=""){
                        mp[currClass]._vtable.methods[currMethod].local.push((yyvsp[-2].str));
                        mp[currClass]._vtable.methods[currMethod].storage+=typesize(getType((yyvsp[-2].str)));
                }
        quad ch;ch.idx=to_string(line++);ch.arg1=(yyvsp[-2].str);ch.res=(yyvsp[0].str); ch.op = (yyvsp[-1]. str);ir.push_back(ch);}
#line 2314 "y.tab.c"
    break;

  case 78:
#line 238 "javaR.y"
                                {currMethod="";quad ch;ch.idx=to_string(line++); ch.op="Func_"+currClass+ " end";ir.push_back(ch);}
#line 2320 "y.tab.c"
    break;

  case 85:
#line 256 "javaR.y"
                   {act_rec obj; mp[currClass]._vtable.methods[currClass+"."+(yyvsp[0].str)] = obj;currMethod = currClass+"."+(yyvsp[0].str); quad ch;ch.idx=to_string(line++);string temp=(yyvsp[0].str); ch.op="Func_"+temp+" start :";ir.push_back(ch);}
#line 2326 "y.tab.c"
    break;

  case 90:
#line 264 "javaR.y"
                                {mp[currClass]._vtable.methods[currMethod].param.push((yyvsp[0].str)); mp[currClass]._vtable.methods[currMethod].storage+=typesize((yyvsp[-1].str));}
#line 2332 "y.tab.c"
    break;

  case 136:
#line 369 "javaR.y"
                             {quad ch;ch.idx=to_string(line++);ch.arg1="GOTO" ;ch.idx="label"+to_string(breakContinue+1);ir.push_back(ch);}
#line 2338 "y.tab.c"
    break;

  case 137:
#line 370 "javaR.y"
                                {quad ch;ch.idx=to_string(line++); ch.arg1="GOTO" ;ch.idx="label"+to_string(breakContinue);ir.push_back(ch);}
#line 2344 "y.tab.c"
    break;

  case 153:
#line 406 "javaR.y"
           {temp=check;check=3;}
#line 2350 "y.tab.c"
    break;

  case 154:
#line 406 "javaR.y"
                                                                     {check = temp;}
#line 2356 "y.tab.c"
    break;

  case 155:
#line 406 "javaR.y"
                                                                                                 {quad ch;ch.idx=to_string(line++);ch.arg1="Label"+to_string(label);ir.push_back(ch);}
#line 2362 "y.tab.c"
    break;

  case 156:
#line 409 "javaR.y"
                       {temp=check;check=3;}
#line 2368 "y.tab.c"
    break;

  case 157:
#line 409 "javaR.y"
                                                                     {check = temp;}
#line 2374 "y.tab.c"
    break;

  case 158:
#line 409 "javaR.y"
                                                                                                             {quad ch;ch.idx=to_string(line++);ch.arg1="Label"+to_string(label);ir.push_back(ch);}
#line 2380 "y.tab.c"
    break;

  case 159:
#line 409 "javaR.y"
                                                                                                                                                                                                             {quad ch;ch.idx=to_string(line++);ch.arg1="next";ir.push_back(ch);}
#line 2386 "y.tab.c"
    break;

  case 160:
#line 412 "javaR.y"
           {temp=check;check=3;}
#line 2392 "y.tab.c"
    break;

  case 161:
#line 412 "javaR.y"
                                                                     {check = temp;}
#line 2398 "y.tab.c"
    break;

  case 162:
#line 412 "javaR.y"
                                                                                                             {quad ch;ch.idx=to_string(line++);ch.arg1="Label"+to_string(label);ir.push_back(ch);}
#line 2404 "y.tab.c"
    break;

  case 163:
#line 412 "javaR.y"
                                                                                                                                                                                                                      {quad ch;ch.idx=to_string(line++);ch.arg1="next";ir.push_back(ch);}
#line 2410 "y.tab.c"
    break;

  case 164:
#line 415 "javaR.y"
              {temp=check;check=2;quad ch;ch.idx=to_string(line++); ch.arg1="Label"+to_string(++label); breakContinue=label;(yyvsp[0].loop_var).num=label;ir.push_back(ch);}
#line 2416 "y.tab.c"
    break;

  case 165:
#line 415 "javaR.y"
                                                                                                                                                                                                {check = temp;}
#line 2422 "y.tab.c"
    break;

  case 166:
#line 416 "javaR.y"
            {quad ch;ch.idx=to_string((yyvsp[-6].loop_var).num);ch.op="GOTO";quad ch1;ch1.idx=to_string(line++);ch1.arg1="Label"+to_string((yyvsp[-6].loop_var).num+1);breakContinue--;ir.push_back(ch);ir.push_back(ch1);}
#line 2428 "y.tab.c"
    break;

  case 167:
#line 420 "javaR.y"
              {temp=check;check=2;quad ch;ch.idx=to_string(line++); ch.arg1="Label"+to_string(++label); breakContinue=label;(yyvsp[0].loop_var).num=label;ir.push_back(ch);}
#line 2434 "y.tab.c"
    break;

  case 168:
#line 420 "javaR.y"
                                                                                                                                                                                                {check = temp;}
#line 2440 "y.tab.c"
    break;

  case 169:
#line 421 "javaR.y"
            {quad ch;ch.idx=to_string((yyvsp[-6].loop_var).num);ch.op="GOTO";quad ch1;ch1.idx=to_string(line++);ch1.arg1="Label"+to_string((yyvsp[-6].loop_var).num+1);breakContinue--;ir.push_back(ch);ir.push_back(ch1);}
#line 2446 "y.tab.c"
    break;

  case 179:
#line 442 "javaR.y"
                                             {temp=check;check=1;quad ch;ch.idx=to_string(line++); ch.arg1="Label"+to_string(++label); breakContinue=label;(yyvsp[-3].loop_var).num=label;ir.push_back(ch);}
#line 2452 "y.tab.c"
    break;

  case 180:
#line 442 "javaR.y"
                                                                                                                                                                                                         {check = temp;}
#line 2458 "y.tab.c"
    break;

  case 181:
#line 442 "javaR.y"
                                                                                                                                                                                                                                                                          {
                quad ch;ch.idx=to_string((yyvsp[-10].loop_var).num);ch.op="GOTO";quad ch1;ch1.idx=to_string(line++);ch1.arg1="Label"+to_string((yyvsp[-10].loop_var).num+1);breakContinue--;ir.push_back(ch);ir.push_back(ch1);
                }
#line 2466 "y.tab.c"
    break;

  case 182:
#line 448 "javaR.y"
                                             {temp=check;check=1;quad ch;ch.idx=to_string(line++); ch.arg1="Label"+to_string(++label); breakContinue=label;(yyvsp[-3].loop_var).num=label;ir.push_back(ch);}
#line 2472 "y.tab.c"
    break;

  case 183:
#line 448 "javaR.y"
                                                                                                                                                                                                         {check = temp;}
#line 2478 "y.tab.c"
    break;

  case 184:
#line 448 "javaR.y"
                                                                                                                                                                                                                                                                                  {
                quad ch;ch.idx=to_string((yyvsp[-10].loop_var).num);ch.op="GOTO";quad ch1;ch1.idx=to_string(line++);ch1.arg1="Label"+to_string((yyvsp[-10].loop_var).num+1);breakContinue--;ir.push_back(ch);ir.push_back(ch1);
                }
#line 2486 "y.tab.c"
    break;

  case 195:
#line 484 "javaR.y"
                                          {mp[currClass]._vtable.methods[currMethod].returnVal=(yyvsp[-1].str);}
#line 2492 "y.tab.c"
    break;

  case 199:
#line 492 "javaR.y"
                  {if(flag){mp[currClass]._vtable.methods[currMethod].param.push((yyvsp[0].str));}}
#line 2498 "y.tab.c"
    break;

  case 201:
#line 494 "javaR.y"
                                               {(yyval.str) = strConcat((yyvsp[-2]. str),strConcat((yyvsp[-1].str),(yyvsp[0]. str)));}
#line 2504 "y.tab.c"
    break;

  case 204:
#line 497 "javaR.y"
                        {if(flag){mp[currClass]._vtable.methods[currMethod].param.push((yyvsp[0].str));}string ch = word+"[" + array3ac((yyvsp[0].str)) + "]";(yyval.str)=strdup(ch.c_str());}
#line 2510 "y.tab.c"
    break;

  case 205:
#line 500 "javaR.y"
                {(yyval.str)=(yyvsp[0].str);}
#line 2516 "y.tab.c"
    break;

  case 206:
#line 503 "javaR.y"
                      {quad ch;ch.idx=to_string(line++);param++;ch.op="param";ch.arg1= (yyvsp[0].str);ir.push_back(ch);}
#line 2522 "y.tab.c"
    break;

  case 207:
#line 504 "javaR.y"
                                                {quad ch;ch.idx=to_string(line++);param++;ch.op="param";ch.arg1= (yyvsp[-2].str);ir.push_back(ch);}
#line 2528 "y.tab.c"
    break;

  case 219:
#line 532 "javaR.y"
             {param=0;}
#line 2534 "y.tab.c"
    break;

  case 220:
#line 532 "javaR.y"
                                    {flag=1;}
#line 2540 "y.tab.c"
    break;

  case 221:
#line 532 "javaR.y"
                                                              {flag=0;}
#line 2546 "y.tab.c"
    break;

  case 222:
#line 532 "javaR.y"
                                                                                          {
                vector<string> temp = find((yyvsp[-6].str));
                mp[temp[3]]._vtable.methods[temp[3]+"."+temp[4]].controlLink = &mp[currClass]._vtable.methods[currMethod]; 
                // cout<<mp[temp[3]]._vtable.methods[temp[3]+"."+temp[4]].controlLink->param.top();
                quad ch;ch.idx=to_string(line++);ch.op= "pushparam"; ch.arg1=to_string(mp[temp[3]]._vtable.methods[temp[3]+"."+temp[4]].storage);ir.push_back(ch);ch.op="call";ch.res="t"+cnt; ch.arg1=(yyvsp[-6].str);ch.arg2=to_string(param); ir.push_back(ch); ch.op = "popparam" ; ch.arg1 =to_string(mp[temp[3]]._vtable.methods[temp[3]+"."+temp[4]].storage);ir.push_back(ch);sprintf((yyval.str),"t%d",cnt++);                
        }
#line 2557 "y.tab.c"
    break;

  case 228:
#line 545 "javaR.y"
                                                                  {sprintf((yyval.str),strConcat((yyvsp[-3].str),strConcat("[",strConcat((yyvsp[-1].str),"]"))));}
#line 2563 "y.tab.c"
    break;

  case 229:
#line 546 "javaR.y"
                                                                           {sprintf((yyval.str),strConcat((yyvsp[-3].str),strConcat("[",strConcat((yyvsp[-1].str),"]"))));}
#line 2569 "y.tab.c"
    break;

  case 231:
#line 551 "javaR.y"
                     {(yyval.str)=(yyvsp[0].str);if(flag){mp[currClass]._vtable.methods[currMethod].param.push((yyvsp[0].str));}}
#line 2575 "y.tab.c"
    break;

  case 232:
#line 552 "javaR.y"
                                    {(yyval.str)=(yyvsp[0].str);}
#line 2581 "y.tab.c"
    break;

  case 233:
#line 553 "javaR.y"
                                         {(yyval.str)=(yyvsp[0].str);}
#line 2587 "y.tab.c"
    break;

  case 234:
#line 557 "javaR.y"
                                    {quad ch;ch.idx=to_string(line++);ch.op="+";ch.res="t"+to_string(cnt);ch.arg1=(yyvsp[-1].str);ch.arg2="1";ir.push_back(ch);ch_clear(ch); ch.op="=";ch.idx=to_string(line++);ch.res=(yyvsp[-1].str);ch.arg1="t"+to_string(cnt);ir.push_back(ch); sprintf((yyval.str),"t%d",cnt);cnt++;}
#line 2593 "y.tab.c"
    break;

  case 235:
#line 561 "javaR.y"
                                      {quad ch;ch.idx=to_string(line++);ch.op="-";ch.res="t"+to_string(cnt);ch.arg1=(yyvsp[-1].str);ch.arg2="1";ir.push_back(ch); (ch); ch.op="=";ch.idx=to_string(line++);ch.res=(yyvsp[-1].str);ch.arg1="t"+to_string(cnt);ir.push_back(ch); sprintf((yyval.str),"t%d",cnt);cnt++;}
#line 2599 "y.tab.c"
    break;

  case 236:
#line 565 "javaR.y"
                                    {(yyval.str)=(yyvsp[0].str);}
#line 2605 "y.tab.c"
    break;

  case 237:
#line 566 "javaR.y"
                                        {(yyval.str)=(yyvsp[0].str);}
#line 2611 "y.tab.c"
    break;

  case 238:
#line 567 "javaR.y"
                                     {quad ch;ch.idx=to_string(line++);ch.op="unary+";ch.res="t"+to_string(cnt);ch.arg1=(yyvsp[-1]. str);ch.arg2=(yyvsp[0].str);ir.push_back(ch);ch_clear(ch);ch.op="=";ch.idx=to_string(line++);ch.res=(yyvsp[0].str);ch.arg1="t"+to_string(cnt);ir.push_back(ch);  sprintf((yyval.str),"t%d",cnt);cnt++;}
#line 2617 "y.tab.c"
    break;

  case 239:
#line 568 "javaR.y"
                                    {quad ch;ch.idx=to_string(line++);ch.op="unary-";ch.res="t"+to_string(cnt);ch.arg1=(yyvsp[-1]. str);ch.arg2=(yyvsp[0].str);ir.push_back(ch);ch_clear(ch);ch.op="=";ch.idx=to_string(line++);ch.res=(yyvsp[0].str);ch.arg1="t"+to_string(cnt);ir.push_back(ch);  sprintf((yyval.str),"t%d",cnt);cnt++;}
#line 2623 "y.tab.c"
    break;

  case 240:
#line 569 "javaR.y"
                                          {(yyval.str)=(yyvsp[0].str);}
#line 2629 "y.tab.c"
    break;

  case 241:
#line 573 "javaR.y"
                                     {quad ch;ch.idx=to_string(line++);ch.op="+";ch.res="t"+to_string(cnt);ch.arg1=(yyvsp[-1]. str);ch.arg2="1";ir.push_back(ch); ch.op="=";ch.idx=to_string(line++);ch.res=(yyvsp[-1]. str);ch.arg1="t"+to_string(cnt);ir.push_back(ch); sprintf((yyval.str),"t%d",cnt);cnt++;}
#line 2635 "y.tab.c"
    break;

  case 242:
#line 577 "javaR.y"
                                     {quad ch;ch.idx=to_string(line++);ch.op="-";ch.res="t"+to_string(cnt);ch.arg1=(yyvsp[-1]. str);ch.arg2="1";ir.push_back(ch); ch.op="=";ch.idx=to_string(line++);ch.res=(yyvsp[-1]. str);ch.arg1="t"+to_string(cnt);ir.push_back(ch); sprintf((yyval.str),"t%d",cnt);cnt++;}
#line 2641 "y.tab.c"
    break;

  case 243:
#line 581 "javaR.y"
                               {(yyval.str)=(yyvsp[0].str);}
#line 2647 "y.tab.c"
    break;

  case 250:
#line 593 "javaR.y"
                                                     {(yyval.str)=(yyvsp[0].str);}
#line 2653 "y.tab.c"
    break;

  case 251:
#line 597 "javaR.y"
                             {(yyval.str)=(yyvsp[0].str);}
#line 2659 "y.tab.c"
    break;

  case 252:
#line 598 "javaR.y"
                                                               {quad ch;ch.idx=to_string(line++);ch.op=(yyvsp[-1]. str);ch.arg1=(yyvsp[-2].str);ch.arg2=(yyvsp[0].str);ch.res="t"+to_string(cnt);sprintf((yyval.str),"t%d",cnt);cnt++;ir.push_back(ch);}
#line 2665 "y.tab.c"
    break;

  case 253:
#line 599 "javaR.y"
                                                               {quad ch;ch.idx=to_string(line++);ch.op=(yyvsp[-1]. str);ch.arg1=(yyvsp[-2].str);ch.arg2=(yyvsp[0].str);ch.res="t"+to_string(cnt);sprintf((yyval.str),"t%d",cnt);cnt++;ir.push_back(ch);}
#line 2671 "y.tab.c"
    break;

  case 254:
#line 600 "javaR.y"
                                                              {quad ch;ch.idx=to_string(line++);ch.op=(yyvsp[-1]. str);ch.arg1=(yyvsp[-2].str);ch.arg2=(yyvsp[0].str);ch.res="t"+to_string(cnt);sprintf((yyval.str),"t%d",cnt);cnt++;ir.push_back(ch);}
#line 2677 "y.tab.c"
    break;

  case 255:
#line 603 "javaR.y"
                                        {(yyval.str)=(yyvsp[0].str);}
#line 2683 "y.tab.c"
    break;

  case 256:
#line 607 "javaR.y"
                                     {(yyval.str)=(yyvsp[0].str);}
#line 2689 "y.tab.c"
    break;

  case 257:
#line 608 "javaR.y"
                                                                {quad ch;ch.idx=to_string(line++);ch.op=(yyvsp[-1]. str);ch.arg1=(yyvsp[-2].str);ch.arg2=(yyvsp[0].str);ch.res="t"+to_string(cnt);sprintf((yyval.str),"t%d",cnt);cnt++;ir.push_back(ch);}
#line 2695 "y.tab.c"
    break;

  case 258:
#line 609 "javaR.y"
                                                                {quad ch;ch.idx=to_string(line++);ch.op=(yyvsp[-1]. str);ch.arg1=(yyvsp[-2].str);ch.arg2=(yyvsp[0].str);ch.res="t"+to_string(cnt);sprintf((yyval.str),"t%d",cnt);cnt++;ir.push_back(ch);}
#line 2701 "y.tab.c"
    break;

  case 259:
#line 612 "javaR.y"
                                  {(yyval.str)=(yyvsp[0].str);}
#line 2707 "y.tab.c"
    break;

  case 260:
#line 616 "javaR.y"
                               {(yyval.str)=(yyvsp[0].str);}
#line 2713 "y.tab.c"
    break;

  case 261:
#line 617 "javaR.y"
                                                             {quad ch;ch.idx=to_string(line++);ch.op=(yyvsp[-1]. str);ch.arg1=(yyvsp[-2].str);ch.arg2=(yyvsp[0].str);ch.res="t"+to_string(cnt);sprintf((yyval.str),"t%d",cnt);cnt++;ir.push_back(ch);}
#line 2719 "y.tab.c"
    break;

  case 262:
#line 618 "javaR.y"
                                                              {quad ch;ch.idx=to_string(line++);ch.op=(yyvsp[-1]. str);ch.arg1=(yyvsp[-2].str);ch.arg2=(yyvsp[0].str);ch.res="t"+to_string(cnt);sprintf((yyval.str),"t%d",cnt);cnt++;ir.push_back(ch);}
#line 2725 "y.tab.c"
    break;

  case 263:
#line 619 "javaR.y"
                                                                {quad ch;ch.idx=to_string(line++);ch.op=(yyvsp[-1]. str);ch.arg1=(yyvsp[-2].str);ch.arg2=(yyvsp[0].str);ch.res="t"+to_string(cnt);sprintf((yyval.str),"t%d",cnt);cnt++;ir.push_back(ch);}
#line 2731 "y.tab.c"
    break;

  case 264:
#line 622 "javaR.y"
                                                {(yyval.str)=(yyvsp[0].str);}
#line 2737 "y.tab.c"
    break;

  case 265:
#line 626 "javaR.y"
                            {(yyval.str)=(yyvsp[0].str);}
#line 2743 "y.tab.c"
    break;

  case 266:
#line 627 "javaR.y"
                                                           {if(check==1){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat((yyvsp[-2].str),strConcat((yyvsp[-1]. str),(yyvsp[0].str))));ch.idx = to_string(label+1); label++;ir.push_back(ch);}
        else if(check==2){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat((yyvsp[-2].str),strConcat((yyvsp[-1]. str),(yyvsp[0].str))));ch.idx = to_string(label+1); label++;ir.push_back(ch);;}
        else if(check==3){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat((yyvsp[-2].str),strConcat((yyvsp[-1]. str),(yyvsp[0].str))));ch.idx = to_string(label+1); label++;ir.push_back(ch);;}
        else{{quad ch;ch.idx=to_string(line++);ch.op=(yyvsp[-1]. str);ch.res="t"+to_string(cnt);ch.arg1=(yyvsp[-2].str);ch.arg2=(yyvsp[0].str); sprintf((yyval.str),"t%d",cnt);cnt++;ir.push_back(ch);}}
        }
#line 2753 "y.tab.c"
    break;

  case 267:
#line 632 "javaR.y"
                                                             {if(check==1){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat((yyvsp[-2].str),strConcat((yyvsp[-1]. str),(yyvsp[0].str))));ch.idx = to_string(label+1); label++;ir.push_back(ch);}
        else if(check==2){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat((yyvsp[-2].str),strConcat((yyvsp[-1]. str),(yyvsp[0].str))));ch.idx = to_string(label+1); label++;ir.push_back(ch);;}
        else if(check==3){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat((yyvsp[-2].str),strConcat((yyvsp[-1]. str),(yyvsp[0].str))));ch.idx = to_string(label+1); label++;ir.push_back(ch);;}
        else{{quad ch;ch.idx=to_string(line++);ch.op=(yyvsp[-1]. str);ch.res="t"+to_string(cnt);ch.arg1=(yyvsp[-2].str);ch.arg2=(yyvsp[0].str); sprintf((yyval.str),"t%d",cnt);cnt++;ir.push_back(ch);}}
        }
#line 2763 "y.tab.c"
    break;

  case 268:
#line 637 "javaR.y"
                                                                {if(check==1){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat((yyvsp[-2].str),strConcat((yyvsp[-1]. str),(yyvsp[0].str))));ch.idx = to_string(label+1); label++;ir.push_back(ch);}
        else if(check==2){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat((yyvsp[-2].str),strConcat((yyvsp[-1]. str),(yyvsp[0].str))));ch.idx = to_string(label+1); label++;ir.push_back(ch);;}
        else if(check==3){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat((yyvsp[-2].str),strConcat((yyvsp[-1]. str),(yyvsp[0].str))));ch.idx = to_string(label+1); label++;ir.push_back(ch);;}
        else{{quad ch;ch.idx=to_string(line++);ch.op=(yyvsp[-1]. str);ch.res="t"+to_string(cnt);ch.arg1=(yyvsp[-2].str);ch.arg2=(yyvsp[0].str); sprintf((yyval.str),"t%d",cnt);cnt++;ir.push_back(ch);}}
        }
#line 2773 "y.tab.c"
    break;

  case 269:
#line 642 "javaR.y"
                                                                   {if(check==1){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat((yyvsp[-2].str),strConcat((yyvsp[-1]. str),(yyvsp[0].str))));ch.idx = to_string(label+1); label++;ir.push_back(ch);}
        else if(check==2){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat((yyvsp[-2].str),strConcat((yyvsp[-1]. str),(yyvsp[0].str))));ch.idx = to_string(label+1); label++;ir.push_back(ch);;}
        else if(check==3){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat((yyvsp[-2].str),strConcat((yyvsp[-1]. str),(yyvsp[0].str))));ch.idx = to_string(label+1); label++;ir.push_back(ch);;}
        else{{quad ch;ch.idx=to_string(line++);ch.op=(yyvsp[-1]. str);ch.res="t"+to_string(cnt);ch.arg1=(yyvsp[-2].str);ch.arg2=(yyvsp[0].str); sprintf((yyval.str),"t%d",cnt);cnt++;ir.push_back(ch);}}
        }
#line 2783 "y.tab.c"
    break;

  case 270:
#line 649 "javaR.y"
                                        {(yyval.str)=(yyvsp[0].str);}
#line 2789 "y.tab.c"
    break;

  case 271:
#line 653 "javaR.y"
                                  {(yyval.str)=(yyvsp[0].str);}
#line 2795 "y.tab.c"
    break;

  case 272:
#line 654 "javaR.y"
                                                                     {if(check==1){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat((yyvsp[-2].str),strConcat((yyvsp[-1]. str),(yyvsp[0].str))));ch.idx = to_string(label+1); label++;ir.push_back(ch);}
        else if(check==2){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat((yyvsp[-2].str),strConcat((yyvsp[-1]. str),(yyvsp[0].str))));ch.idx = to_string(label+1); label++;ir.push_back(ch);;}
        else if(check==3){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat((yyvsp[-2].str),strConcat((yyvsp[-1]. str),(yyvsp[0].str))));ch.idx = to_string(label+1); label++;ir.push_back(ch);;}
        else{{quad ch;ch.idx=to_string(line++);ch.op=(yyvsp[-1]. str);ch.res="t"+to_string(cnt);ch.arg1=(yyvsp[-2].str);ch.arg2=(yyvsp[0].str); sprintf((yyval.str),"t%d",cnt);cnt++;ir.push_back(ch);}}
        }
#line 2805 "y.tab.c"
    break;

  case 273:
#line 659 "javaR.y"
                                                                     {if(check==1){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat((yyvsp[-2].str),strConcat((yyvsp[-1]. str),(yyvsp[0].str))));ch.idx = to_string(label+1); label++;ir.push_back(ch);}
        else if(check==2){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat((yyvsp[-2].str),strConcat((yyvsp[-1]. str),(yyvsp[0].str))));ch.idx = to_string(label+1); label++;ir.push_back(ch);;}
        else if(check==3){quad ch;ch.idx=to_string(line++);ch.op="GOTO";ch.arg1="IF";ch.arg2 = strConcat("!",strConcat((yyvsp[-2].str),strConcat((yyvsp[-1]. str),(yyvsp[0].str))));ch.idx = to_string(label+1); label++;ir.push_back(ch);;}
        else{{quad ch;ch.idx=to_string(line++);ch.op=(yyvsp[-1]. str);ch.res="t"+to_string(cnt);ch.arg1=(yyvsp[-2].str);ch.arg2=(yyvsp[0].str); sprintf((yyval.str),"t%d",cnt);cnt++;ir.push_back(ch);}}
        }
#line 2815 "y.tab.c"
    break;

  case 274:
#line 665 "javaR.y"
                                {(yyval.str)=(yyvsp[0].str);}
#line 2821 "y.tab.c"
    break;

  case 275:
#line 668 "javaR.y"
                                {(yyval.str)=(yyvsp[0].str);}
#line 2827 "y.tab.c"
    break;

  case 276:
#line 669 "javaR.y"
                                                   {quad ch;ch.idx=to_string(line++);ch.op=(yyvsp[-1]. str);ch.arg1=(yyvsp[-2].str);ch.arg2=(yyvsp[0].str);ch.res="t"+to_string(cnt);sprintf((yyval.str),"t%d",cnt);cnt++;ir.push_back(ch);}
#line 2833 "y.tab.c"
    break;

  case 277:
#line 673 "javaR.y"
                                                {(yyval.str)=(yyvsp[0].str);}
#line 2839 "y.tab.c"
    break;

  case 278:
#line 676 "javaR.y"
                          {(yyval.str)=(yyvsp[0].str);}
#line 2845 "y.tab.c"
    break;

  case 279:
#line 677 "javaR.y"
                                                         {quad ch;ch.idx=to_string(line++);ch.op=(yyvsp[-1]. str);ch.arg1=(yyvsp[-2].str);ch.arg2=(yyvsp[0].str);ch.res="t"+to_string(cnt);sprintf((yyval.str),"t%d",cnt);cnt++;ir.push_back(ch);}
#line 2851 "y.tab.c"
    break;

  case 280:
#line 680 "javaR.y"
                                                {(yyval.str)=(yyvsp[0].str);}
#line 2857 "y.tab.c"
    break;

  case 281:
#line 684 "javaR.y"
                              {(yyval.str)=(yyvsp[0].str);}
#line 2863 "y.tab.c"
    break;

  case 282:
#line 685 "javaR.y"
                                                                {quad ch;ch.idx=to_string(line++);ch.op=(yyvsp[-1]. str);ch.arg1=(yyvsp[-2].str);ch.arg2=(yyvsp[0].str);ch.res="t"+to_string(cnt);sprintf((yyval.str),"t%d",cnt);cnt++;ir.push_back(ch);}
#line 2869 "y.tab.c"
    break;

  case 283:
#line 688 "javaR.y"
                                                        {(yyval.str)=(yyvsp[0].str);}
#line 2875 "y.tab.c"
    break;

  case 284:
#line 692 "javaR.y"
                                  {(yyval.str)=(yyvsp[0].str);}
#line 2881 "y.tab.c"
    break;

  case 285:
#line 693 "javaR.y"
                                                                   {quad ch;ch.idx=to_string(line++);ch.op=(yyvsp[-1]. str);ch.arg1=(yyvsp[-2].str);ch.arg2=(yyvsp[0].str);ch.res="t"+to_string(cnt);sprintf((yyval.str),"t%d",cnt);cnt++;ir.push_back(ch);}
#line 2887 "y.tab.c"
    break;

  case 286:
#line 696 "javaR.y"
                                                        {(yyval.str)=(yyvsp[0].str);}
#line 2893 "y.tab.c"
    break;

  case 287:
#line 700 "javaR.y"
                                     {(yyval.str)=(yyvsp[0].str);}
#line 2899 "y.tab.c"
    break;

  case 288:
#line 701 "javaR.y"
                                                                    {quad ch;ch.idx=to_string(line++);ch.op=(yyvsp[-1]. str);ch.arg1=(yyvsp[-2].str);ch.arg2=(yyvsp[0].str);ch.res="t"+to_string(cnt);sprintf((yyval.str),"t%d",cnt);cnt++;ir.push_back(ch);}
#line 2905 "y.tab.c"
    break;

  case 289:
#line 704 "javaR.y"
                                               {(yyval.str)=(yyvsp[0].str);}
#line 2911 "y.tab.c"
    break;

  case 290:
#line 707 "javaR.y"
                                                  {(yyval.str)=(yyvsp[0].str);}
#line 2917 "y.tab.c"
    break;

  case 292:
#line 712 "javaR.y"
                                 {(yyval.str)=(yyvsp[0].str);}
#line 2923 "y.tab.c"
    break;

  case 293:
#line 713 "javaR.y"
                            {(yyval.str)=(yyvsp[0].str);}
#line 2929 "y.tab.c"
    break;

  case 294:
#line 717 "javaR.y"
                                                             {quad ch;ch.idx=to_string(line++);ch.op=(yyvsp[-1].str);ch.res=(yyvsp[-2].str);ch.arg1=(yyvsp[0].str); ir.push_back(ch);  }
#line 2935 "y.tab.c"
    break;

  case 295:
#line 721 "javaR.y"
                      {(yyval.str)=(yyvsp[0].str);}
#line 2941 "y.tab.c"
    break;

  case 297:
#line 723 "javaR.y"
                            {string ch = word+"[" + array3ac((yyvsp[0].str)) + "]";(yyval.str)=strdup(ch.c_str());}
#line 2947 "y.tab.c"
    break;

  case 298:
#line 727 "javaR.y"
                     {(yyval.str)=(yyvsp[0]. str);}
#line 2953 "y.tab.c"
    break;

  case 304:
#line 736 "javaR.y"
                                  {(yyval.str)=(yyvsp[0].str);}
#line 2959 "y.tab.c"
    break;


#line 2963 "y.tab.c"

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
#line 739 "javaR.y"


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

/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

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

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     ID = 258,
     STRING = 259,
     INT = 260,
     COMMA = 261,
     COLON = 262,
     SEMICOLON = 263,
     LPAREN = 264,
     RPAREN = 265,
     LBRACK = 266,
     RBRACK = 267,
     LBRACE = 268,
     RBRACE = 269,
     DOT = 270,
     PLUS = 271,
     MINUS = 272,
     TIMES = 273,
     DIVIDE = 274,
     EQ = 275,
     NEQ = 276,
     LT = 277,
     LE = 278,
     GT = 279,
     GE = 280,
     AND = 281,
     OR = 282,
     ASSIGN = 283,
     ARRAY = 284,
     IF = 285,
     THEN = 286,
     ELSE = 287,
     WHILE = 288,
     FOR = 289,
     TO = 290,
     DO = 291,
     LET = 292,
     IN = 293,
     END = 294,
     OF = 295,
     BREAK = 296,
     NIL = 297,
     FUNCTION = 298,
     VAR = 299,
     TYPE = 300,
     LOWER = 301,
     LOW = 302,
     UMINUS = 303
   };
#endif
/* Tokens.  */
#define ID 258
#define STRING 259
#define INT 260
#define COMMA 261
#define COLON 262
#define SEMICOLON 263
#define LPAREN 264
#define RPAREN 265
#define LBRACK 266
#define RBRACK 267
#define LBRACE 268
#define RBRACE 269
#define DOT 270
#define PLUS 271
#define MINUS 272
#define TIMES 273
#define DIVIDE 274
#define EQ 275
#define NEQ 276
#define LT 277
#define LE 278
#define GT 279
#define GE 280
#define AND 281
#define OR 282
#define ASSIGN 283
#define ARRAY 284
#define IF 285
#define THEN 286
#define ELSE 287
#define WHILE 288
#define FOR 289
#define TO 290
#define DO 291
#define LET 292
#define IN 293
#define END 294
#define OF 295
#define BREAK 296
#define NIL 297
#define FUNCTION 298
#define VAR 299
#define TYPE 300
#define LOWER 301
#define LOW 302
#define UMINUS 303




/* Copy the first part of user declarations.  */
#line 1 "tiger.grm"

#include <stdio.h>
#include "util.h"
#include "errormsg.h"
#include "symbol.h"
#include "absyn.h"

int yylex(void); /* function prototype */

A_exp absyn_root;

void yyerror(char *s)
{
 EM_error(EM_tokPos, "%s", s);
}


/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 19 "tiger.grm"
{
	int pos;
	int ival;
	string sval;
        A_exp exp;
        A_expList explist;
        A_var var;
        A_decList declist;
        A_dec  dec;
        A_efieldList efieldlist;
        A_efield  efield;
        A_namety namety;
        A_nametyList nametylist;
        A_fieldList fieldlist;
        A_field field;
        A_fundecList fundeclist;
        A_fundec fundec;
        A_ty ty;
	}
/* Line 193 of yacc.c.  */
#line 229 "y.tab.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 242 "y.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

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
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
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
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
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
  yytype_int16 yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  40
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   306

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  49
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  26
/* YYNRULES -- Number of rules.  */
#define YYNRULES  77
/* YYNRULES -- Number of states.  */
#define YYNSTATES  151

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   303

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
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
      45,    46,    47,    48
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     5,     7,     8,    10,    12,    14,    16,
      20,    24,    28,    32,    36,    40,    44,    48,    52,    56,
      60,    64,    68,    71,    75,    80,    84,    89,    96,   101,
     110,   112,   118,   123,   130,   132,   134,   136,   140,   145,
     149,   154,   156,   157,   159,   163,   167,   171,   173,   177,
     179,   180,   182,   186,   188,   189,   191,   192,   194,   197,
     199,   201,   203,   205,   208,   213,   215,   219,   223,   225,
     226,   230,   236,   241,   248,   250,   253,   261
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      50,     0,    -1,    51,    -1,    52,    -1,    -1,     5,    -1,
       4,    -1,    53,    -1,    42,    -1,     9,    59,    10,    -1,
      52,    16,    52,    -1,    52,    17,    52,    -1,    52,    18,
      52,    -1,    52,    19,    52,    -1,    52,    20,    52,    -1,
      52,    21,    52,    -1,    52,    22,    52,    -1,    52,    23,
      52,    -1,    52,    24,    52,    -1,    52,    25,    52,    -1,
      52,    26,    52,    -1,    52,    27,    52,    -1,    17,    52,
      -1,     9,    51,    10,    -1,     3,     9,    61,    10,    -1,
      53,    28,    52,    -1,    30,    52,    31,    52,    -1,    30,
      52,    31,    52,    32,    52,    -1,    33,    52,    36,    52,
      -1,    34,     3,    28,    52,    35,    52,    36,    52,    -1,
      41,    -1,    37,    64,    38,    63,    39,    -1,     3,    13,
      56,    14,    -1,     3,    11,    52,    12,    40,    52,    -1,
       3,    -1,    54,    -1,    55,    -1,    54,    15,     3,    -1,
      54,    11,    52,    12,    -1,     3,    15,     3,    -1,     3,
      11,    52,    12,    -1,    57,    -1,    -1,    58,    -1,    58,
       6,    57,    -1,     3,    20,    52,    -1,    52,     8,    60,
      -1,    52,    -1,    52,     8,    60,    -1,    62,    -1,    -1,
      52,    -1,    52,     6,    62,    -1,    60,    -1,    -1,    65,
      -1,    -1,    66,    -1,    66,    65,    -1,    67,    -1,    72,
      -1,    73,    -1,    68,    -1,    68,    67,    -1,    45,     3,
      20,    69,    -1,     3,    -1,    13,    70,    14,    -1,    29,
      40,     3,    -1,    71,    -1,    -1,     3,     7,     3,    -1,
       3,     7,     3,     6,    71,    -1,    44,     3,    28,    52,
      -1,    44,     3,     7,     3,    28,    52,    -1,    74,    -1,
      74,    73,    -1,    43,     3,     9,    70,    10,    20,    52,
      -1,    43,     3,     9,    70,    10,     7,     3,    20,    52,
      -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    87,    87,    89,    90,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   124,   125,   128,   129,   130,   133,
     134,   138,   139,   142,   143,   146,   149,   152,   153,   157,
     158,   161,   162,   165,   166,   170,   171,   174,   175,   178,
     179,   180,   183,   184,   187,   190,   191,   192,   195,   196,
     199,   200,   203,   204,   207,   208,   211,   212
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "ID", "STRING", "INT", "COMMA", "COLON",
  "SEMICOLON", "LPAREN", "RPAREN", "LBRACK", "RBRACK", "LBRACE", "RBRACE",
  "DOT", "PLUS", "MINUS", "TIMES", "DIVIDE", "EQ", "NEQ", "LT", "LE", "GT",
  "GE", "AND", "OR", "ASSIGN", "ARRAY", "IF", "THEN", "ELSE", "WHILE",
  "FOR", "TO", "DO", "LET", "IN", "END", "OF", "BREAK", "NIL", "FUNCTION",
  "VAR", "TYPE", "LOWER", "LOW", "UMINUS", "$accept", "program", "exp_em",
  "exp", "lvalue", "oneormore", "one", "rec", "rec_nonempty", "rec_one",
  "sequencing", "sequencing_exps", "actuals", "nonemptyactuals", "expseq",
  "decs", "decs_nonempty", "decs_nonempty_s", "tydec", "tydec_one", "ty",
  "tyfields", "tyfields_nonempty", "vardec", "fundec", "fundec_one", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    49,    50,    51,    51,    52,    52,    52,    52,    52,
      52,    52,    52,    52,    52,    52,    52,    52,    52,    52,
      52,    52,    52,    52,    52,    52,    52,    52,    52,    52,
      52,    52,    52,    52,    53,    53,    54,    54,    54,    55,
      55,    56,    56,    57,    57,    58,    59,    60,    60,    61,
      61,    62,    62,    63,    63,    64,    64,    65,    65,    66,
      66,    66,    67,    67,    68,    69,    69,    69,    70,    70,
      71,    71,    72,    72,    73,    73,    74,    74
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     1,     0,     1,     1,     1,     1,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     2,     3,     4,     3,     4,     6,     4,     8,
       1,     5,     4,     6,     1,     1,     1,     3,     4,     3,
       4,     1,     0,     1,     3,     3,     3,     1,     3,     1,
       0,     1,     3,     1,     0,     1,     0,     1,     2,     1,
       1,     1,     1,     2,     4,     1,     3,     3,     1,     0,
       3,     5,     4,     6,     1,     2,     7,     9
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       4,    34,     6,     5,     4,     0,     0,     0,     0,    56,
      30,     8,     0,     2,     3,     7,    35,    36,    50,     0,
      42,     0,     0,     3,     0,    22,     0,     0,     0,     0,
       0,     0,     0,    55,    57,    59,    62,    60,    61,    74,
       1,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    51,     0,    49,     0,
       0,     0,    41,    43,    39,    23,     0,     9,     0,     0,
       0,     0,     0,     0,    54,    58,    63,    75,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      25,     0,    37,     0,    24,    40,     0,    32,     0,    47,
      46,    26,    28,     0,    69,     0,     0,     0,    53,     0,
      38,    52,     0,    45,    44,     0,     0,     0,     0,     0,
      68,     0,    72,    65,    69,     0,    64,    31,    33,    48,
      27,     0,     0,     0,     0,     0,     0,     0,    70,     0,
       0,    73,    66,    67,    29,     0,     0,    76,    71,     0,
      77
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,    12,    13,    99,    15,    16,    17,    61,    62,    63,
      24,   100,    57,    58,   109,    32,    33,    34,    35,    36,
     126,   119,   120,    37,    38,    39
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -61
static const yytype_int16 yypact[] =
{
      57,    92,   -61,   -61,    57,    57,    57,    57,    10,   -19,
     -61,   -61,    15,   -61,   248,   -11,    -3,   -61,    57,    57,
      18,    19,    30,    12,    46,   -61,   204,   102,    29,    55,
      56,    60,    26,   -61,   -19,   -61,    22,   -61,   -61,    28,
     -61,    57,    57,    57,    57,    57,    57,    57,    57,    57,
      57,    57,    57,    57,    57,    69,    59,    63,   -61,   220,
      68,    75,   -61,    86,   -61,   -61,    57,   -61,    57,    57,
      57,    88,    -5,    80,    57,   -61,   -61,   -61,    -9,    -9,
     -61,   -61,   271,   271,   271,   271,   271,   271,   281,   260,
     248,   236,   -61,    57,   -61,    62,    57,   -61,    18,   155,
     -61,   187,   248,   167,   101,   105,    57,    -2,   -61,    70,
     -61,   -61,    57,   248,   -61,    57,    57,    57,   103,    85,
     -61,    83,   248,   -61,   101,    73,   -61,   -61,   248,   -61,
     248,   134,   111,    -4,    57,   116,   112,    57,   125,   129,
      57,   248,   -61,   -61,   248,   101,   113,   248,   -61,    57,
     248
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -61,   -61,   131,     0,   -61,   -61,   -61,   -61,    38,   -61,
     -61,   -60,   -61,    48,   -61,   -61,   108,   -61,   107,   -61,
     -61,    20,    -6,   -61,   106,   -61
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_int16 yytable[] =
{
      14,   123,   105,   139,    23,    25,    26,    27,    54,    43,
      44,   124,    55,    28,   108,    40,   140,    53,    56,    59,
      66,    60,    64,   106,    29,    30,    31,   125,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      65,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,   129,    67,    70,    71,    72,
       1,     2,     3,    73,    74,    93,     4,    31,   101,   102,
     103,    29,    92,    94,     5,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,     6,    96,    97,
       7,     8,    98,    56,     9,   133,   113,   104,    10,    11,
     107,    18,   112,    19,   118,    20,   122,    21,   121,   127,
     132,   134,   128,   136,   138,   143,   130,   131,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
     142,   145,   146,   149,   141,    22,   114,   144,    69,   148,
     147,   111,    75,    76,   135,    77,     0,     0,     0,   150,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,     0,   115,     0,     0,     0,     0,     0,     0,
     137,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,     0,     0,     0,     0,     0,
       0,     0,   117,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,     0,     0,     0,     0,   116,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    95,     0,     0,    68,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,   110,     0,
       0,     0,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    41,    42,    43,
      44,    -1,    -1,    -1,    -1,    -1,    -1,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50
};

static const yytype_int16 yycheck[] =
{
       0,     3,     7,     7,     4,     5,     6,     7,    11,    18,
      19,    13,    15,     3,    74,     0,    20,    28,    18,    19,
       8,     3,     3,    28,    43,    44,    45,    29,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      10,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,   115,    10,    28,     3,     3,
       3,     4,     5,     3,    38,     6,     9,    45,    68,    69,
      70,    43,     3,    10,    17,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    30,    20,    14,
      33,    34,     6,    93,    37,    10,    96,     9,    41,    42,
      20,     9,    40,    11,     3,    13,   106,    15,     3,    39,
       7,    28,   112,    40,     3,     3,   116,   117,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      14,     6,     3,    20,   134,     4,    98,   137,    36,   145,
     140,    93,    34,    36,   124,    39,    -1,    -1,    -1,   149,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    -1,     8,    -1,    -1,    -1,    -1,    -1,    -1,
      36,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    35,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    -1,    -1,    -1,    -1,    32,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    12,    -1,    -1,    31,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    12,    -1,
      -1,    -1,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,     5,     9,    17,    30,    33,    34,    37,
      41,    42,    50,    51,    52,    53,    54,    55,     9,    11,
      13,    15,    51,    52,    59,    52,    52,    52,     3,    43,
      44,    45,    64,    65,    66,    67,    68,    72,    73,    74,
       0,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    11,    15,    52,    61,    62,    52,
       3,    56,    57,    58,     3,    10,     8,    10,    31,    36,
      28,     3,     3,     3,    38,    65,    67,    73,    52,    52,
      52,    52,    52,    52,    52,    52,    52,    52,    52,    52,
      52,    52,     3,     6,    10,    12,    20,    14,     6,    52,
      60,    52,    52,    52,     9,     7,    28,    20,    60,    63,
      12,    62,    40,    52,    57,     8,    32,    35,     3,    70,
      71,     3,    52,     3,    13,    29,    69,    39,    52,    60,
      52,    52,     7,    10,    28,    70,    40,    36,     3,     7,
      20,    52,    14,     3,    52,     6,     3,    52,    71,    20,
      52
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

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
#ifndef	YYINITDEPTH
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
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
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
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
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
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
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
	    /* Fall through.  */
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

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
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
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

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
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 87 "tiger.grm"
    {absyn_root = (yyvsp[(1) - (1)].exp);}
    break;

  case 3:
#line 89 "tiger.grm"
    {(yyval.exp)=(yyvsp[(1) - (1)].exp);}
    break;

  case 4:
#line 90 "tiger.grm"
    {(yyval.exp) = NULL;}
    break;

  case 5:
#line 93 "tiger.grm"
    {(yyval.exp) = A_IntExp(EM_tokPos,(yyvsp[(1) - (1)].ival));}
    break;

  case 6:
#line 94 "tiger.grm"
    {(yyval.exp) = A_StringExp(EM_tokPos,(yyvsp[(1) - (1)].sval));}
    break;

  case 7:
#line 95 "tiger.grm"
    {(yyval.exp) = A_VarExp(EM_tokPos,(yyvsp[(1) - (1)].var));}
    break;

  case 8:
#line 96 "tiger.grm"
    {(yyval.exp) = A_NilExp(EM_tokPos);}
    break;

  case 9:
#line 97 "tiger.grm"
    {(yyval.exp) = A_SeqExp(EM_tokPos,(yyvsp[(2) - (3)].explist));}
    break;

  case 10:
#line 98 "tiger.grm"
    {(yyval.exp) = A_OpExp(EM_tokPos,A_plusOp,(yyvsp[(1) - (3)].exp),(yyvsp[(3) - (3)].exp));}
    break;

  case 11:
#line 99 "tiger.grm"
    {(yyval.exp) = A_OpExp(EM_tokPos,A_minusOp,(yyvsp[(1) - (3)].exp),(yyvsp[(3) - (3)].exp));}
    break;

  case 12:
#line 100 "tiger.grm"
    {(yyval.exp) = A_OpExp(EM_tokPos,A_timesOp,(yyvsp[(1) - (3)].exp),(yyvsp[(3) - (3)].exp));}
    break;

  case 13:
#line 101 "tiger.grm"
    {(yyval.exp) = A_OpExp(EM_tokPos,A_divideOp,(yyvsp[(1) - (3)].exp),(yyvsp[(3) - (3)].exp));}
    break;

  case 14:
#line 102 "tiger.grm"
    {(yyval.exp) = A_OpExp(EM_tokPos,A_eqOp,(yyvsp[(1) - (3)].exp),(yyvsp[(3) - (3)].exp));}
    break;

  case 15:
#line 103 "tiger.grm"
    {(yyval.exp) = A_OpExp(EM_tokPos,A_neqOp,(yyvsp[(1) - (3)].exp),(yyvsp[(3) - (3)].exp));}
    break;

  case 16:
#line 104 "tiger.grm"
    {(yyval.exp) = A_OpExp(EM_tokPos,A_ltOp,(yyvsp[(1) - (3)].exp),(yyvsp[(3) - (3)].exp));}
    break;

  case 17:
#line 105 "tiger.grm"
    {(yyval.exp) = A_OpExp(EM_tokPos,A_leOp,(yyvsp[(1) - (3)].exp),(yyvsp[(3) - (3)].exp));}
    break;

  case 18:
#line 106 "tiger.grm"
    {(yyval.exp) = A_OpExp(EM_tokPos,A_gtOp,(yyvsp[(1) - (3)].exp),(yyvsp[(3) - (3)].exp));}
    break;

  case 19:
#line 107 "tiger.grm"
    {(yyval.exp) = A_OpExp(EM_tokPos,A_geOp,(yyvsp[(1) - (3)].exp),(yyvsp[(3) - (3)].exp));}
    break;

  case 20:
#line 108 "tiger.grm"
    {(yyval.exp) = A_IfExp(EM_tokPos,(yyvsp[(1) - (3)].exp),(yyvsp[(3) - (3)].exp),A_IntExp(EM_tokPos,0));}
    break;

  case 21:
#line 109 "tiger.grm"
    {(yyval.exp) = A_IfExp(EM_tokPos,(yyvsp[(1) - (3)].exp),A_IntExp(EM_tokPos,1),(yyvsp[(3) - (3)].exp));}
    break;

  case 22:
#line 110 "tiger.grm"
    {(yyval.exp) = A_OpExp(EM_tokPos,A_minusOp,A_IntExp(EM_tokPos,0),(yyvsp[(2) - (2)].exp));}
    break;

  case 23:
#line 111 "tiger.grm"
    {(yyval.exp) = (yyvsp[(2) - (3)].exp);}
    break;

  case 24:
#line 112 "tiger.grm"
    {(yyval.exp) = A_CallExp(EM_tokPos,S_Symbol((yyvsp[(1) - (4)].sval)),(yyvsp[(3) - (4)].explist));}
    break;

  case 25:
#line 113 "tiger.grm"
    {(yyval.exp) = A_AssignExp(EM_tokPos,(yyvsp[(1) - (3)].var),(yyvsp[(3) - (3)].exp));}
    break;

  case 26:
#line 114 "tiger.grm"
    {(yyval.exp) = A_IfExp(EM_tokPos,(yyvsp[(2) - (4)].exp),(yyvsp[(4) - (4)].exp),NULL);}
    break;

  case 27:
#line 115 "tiger.grm"
    {(yyval.exp) = A_IfExp(EM_tokPos,(yyvsp[(2) - (6)].exp),(yyvsp[(4) - (6)].exp),(yyvsp[(6) - (6)].exp));}
    break;

  case 28:
#line 116 "tiger.grm"
    {(yyval.exp) = A_WhileExp(EM_tokPos,(yyvsp[(2) - (4)].exp),(yyvsp[(4) - (4)].exp));}
    break;

  case 29:
#line 117 "tiger.grm"
    {(yyval.exp) = A_ForExp(EM_tokPos,S_Symbol((yyvsp[(2) - (8)].sval)),(yyvsp[(4) - (8)].exp),(yyvsp[(6) - (8)].exp),(yyvsp[(8) - (8)].exp));}
    break;

  case 30:
#line 118 "tiger.grm"
    {(yyval.exp) = A_BreakExp(EM_tokPos);}
    break;

  case 31:
#line 119 "tiger.grm"
    {(yyval.exp) = A_LetExp(EM_tokPos,(yyvsp[(2) - (5)].declist),(yyvsp[(4) - (5)].exp));}
    break;

  case 32:
#line 120 "tiger.grm"
    {(yyval.exp) = A_RecordExp(EM_tokPos,S_Symbol((yyvsp[(1) - (4)].sval)),(yyvsp[(3) - (4)].efieldlist));}
    break;

  case 33:
#line 121 "tiger.grm"
    {(yyval.exp) = A_ArrayExp(EM_tokPos,S_Symbol((yyvsp[(1) - (6)].sval)),(yyvsp[(3) - (6)].exp),(yyvsp[(6) - (6)].exp));}
    break;

  case 34:
#line 124 "tiger.grm"
    {(yyval.var) = A_SimpleVar(EM_tokPos,S_Symbol((yyvsp[(1) - (1)].sval)));}
    break;

  case 35:
#line 125 "tiger.grm"
    {(yyval.var) = (yyvsp[(1) - (1)].var);}
    break;

  case 36:
#line 128 "tiger.grm"
    {(yyval.var) = (yyvsp[(1) - (1)].var);}
    break;

  case 37:
#line 129 "tiger.grm"
    {(yyval.var) = A_FieldVar(EM_tokPos,(yyvsp[(1) - (3)].var),S_Symbol((yyvsp[(3) - (3)].sval)));}
    break;

  case 38:
#line 130 "tiger.grm"
    {(yyval.var) = A_SubscriptVar(EM_tokPos,(yyvsp[(1) - (4)].var),(yyvsp[(3) - (4)].exp));}
    break;

  case 39:
#line 133 "tiger.grm"
    {(yyval.var) = A_FieldVar(EM_tokPos,A_SimpleVar(EM_tokPos,S_Symbol((yyvsp[(1) - (3)].sval))),S_Symbol((yyvsp[(3) - (3)].sval)));}
    break;

  case 40:
#line 134 "tiger.grm"
    {(yyval.var) = A_SubscriptVar(EM_tokPos,A_SimpleVar(EM_tokPos,S_Symbol((yyvsp[(1) - (4)].sval))),(yyvsp[(3) - (4)].exp));}
    break;

  case 41:
#line 138 "tiger.grm"
    {(yyval.efieldlist) =(yyvsp[(1) - (1)].efieldlist);}
    break;

  case 42:
#line 139 "tiger.grm"
    {(yyval.efieldlist) = NULL;}
    break;

  case 43:
#line 142 "tiger.grm"
    {(yyval.efieldlist) = A_EfieldList((yyvsp[(1) - (1)].efield),NULL);}
    break;

  case 44:
#line 143 "tiger.grm"
    {(yyval.efieldlist) = A_EfieldList((yyvsp[(1) - (3)].efield),(yyvsp[(3) - (3)].efieldlist));}
    break;

  case 45:
#line 146 "tiger.grm"
    {(yyval.efield) = A_Efield(S_Symbol((yyvsp[(1) - (3)].sval)),(yyvsp[(3) - (3)].exp));}
    break;

  case 46:
#line 149 "tiger.grm"
    {(yyval.explist) = A_ExpList((yyvsp[(1) - (3)].exp),(yyvsp[(3) - (3)].explist));}
    break;

  case 47:
#line 152 "tiger.grm"
    {(yyval.explist) = A_ExpList((yyvsp[(1) - (1)].exp),NULL);}
    break;

  case 48:
#line 153 "tiger.grm"
    {(yyval.explist) = A_ExpList((yyvsp[(1) - (3)].exp),(yyvsp[(3) - (3)].explist));}
    break;

  case 49:
#line 157 "tiger.grm"
    {(yyval.explist) = (yyvsp[(1) - (1)].explist);}
    break;

  case 50:
#line 158 "tiger.grm"
    {(yyval.explist) = NULL;}
    break;

  case 51:
#line 161 "tiger.grm"
    {(yyval.explist) = A_ExpList((yyvsp[(1) - (1)].exp),NULL);}
    break;

  case 52:
#line 162 "tiger.grm"
    {(yyval.explist) = A_ExpList((yyvsp[(1) - (3)].exp),(yyvsp[(3) - (3)].explist));}
    break;

  case 53:
#line 165 "tiger.grm"
    {(yyval.exp) = A_SeqExp(EM_tokPos,(yyvsp[(1) - (1)].explist));}
    break;

  case 54:
#line 166 "tiger.grm"
    {(yyval.exp) = NULL;}
    break;

  case 55:
#line 170 "tiger.grm"
    {(yyval.declist) = (yyvsp[(1) - (1)].declist);}
    break;

  case 56:
#line 171 "tiger.grm"
    {(yyval.declist) = NULL;}
    break;

  case 57:
#line 174 "tiger.grm"
    {(yyval.declist) = A_DecList((yyvsp[(1) - (1)].dec),NULL);}
    break;

  case 58:
#line 175 "tiger.grm"
    {(yyval.declist) = A_DecList((yyvsp[(1) - (2)].dec),(yyvsp[(2) - (2)].declist));}
    break;

  case 59:
#line 178 "tiger.grm"
    {(yyval.dec) = A_TypeDec(EM_tokPos,(yyvsp[(1) - (1)].nametylist));}
    break;

  case 60:
#line 179 "tiger.grm"
    {(yyval.dec) = (yyvsp[(1) - (1)].dec);}
    break;

  case 61:
#line 180 "tiger.grm"
    {(yyval.dec) = A_FunctionDec(EM_tokPos,(yyvsp[(1) - (1)].fundeclist));}
    break;

  case 62:
#line 183 "tiger.grm"
    {(yyval.nametylist) = A_NametyList((yyvsp[(1) - (1)].namety),NULL);}
    break;

  case 63:
#line 184 "tiger.grm"
    {(yyval.nametylist) = A_NametyList((yyvsp[(1) - (2)].namety),(yyvsp[(2) - (2)].nametylist));}
    break;

  case 64:
#line 187 "tiger.grm"
    {(yyval.namety) = A_Namety(S_Symbol((yyvsp[(2) - (4)].sval)),(yyvsp[(4) - (4)].ty));}
    break;

  case 65:
#line 190 "tiger.grm"
    {(yyval.ty) = A_NameTy(EM_tokPos,S_Symbol((yyvsp[(1) - (1)].sval)));}
    break;

  case 66:
#line 191 "tiger.grm"
    {(yyval.ty) = A_RecordTy(EM_tokPos,(yyvsp[(2) - (3)].fieldlist));}
    break;

  case 67:
#line 192 "tiger.grm"
    {(yyval.ty) = A_ArrayTy(EM_tokPos,S_Symbol((yyvsp[(3) - (3)].sval)));}
    break;

  case 68:
#line 195 "tiger.grm"
    {(yyval.fieldlist) = (yyvsp[(1) - (1)].fieldlist);}
    break;

  case 69:
#line 196 "tiger.grm"
    {(yyval.fieldlist) = NULL;}
    break;

  case 70:
#line 199 "tiger.grm"
    {(yyval.fieldlist) = A_FieldList(A_Field(EM_tokPos,S_Symbol((yyvsp[(1) - (3)].sval)),S_Symbol((yyvsp[(3) - (3)].sval))),NULL);}
    break;

  case 71:
#line 200 "tiger.grm"
    {(yyval.fieldlist) = A_FieldList(A_Field(EM_tokPos,S_Symbol((yyvsp[(1) - (5)].sval)),S_Symbol((yyvsp[(3) - (5)].sval))),(yyvsp[(5) - (5)].fieldlist));}
    break;

  case 72:
#line 203 "tiger.grm"
    {(yyval.dec) = A_VarDec(EM_tokPos,S_Symbol((yyvsp[(2) - (4)].sval)),S_Symbol(""),(yyvsp[(4) - (4)].exp));}
    break;

  case 73:
#line 204 "tiger.grm"
    {(yyval.dec) = A_VarDec(EM_tokPos,S_Symbol((yyvsp[(2) - (6)].sval)),S_Symbol((yyvsp[(4) - (6)].sval)),(yyvsp[(6) - (6)].exp));}
    break;

  case 74:
#line 207 "tiger.grm"
    {(yyval.fundeclist) = A_FundecList((yyvsp[(1) - (1)].fundec),NULL);}
    break;

  case 75:
#line 208 "tiger.grm"
    {(yyval.fundeclist) = A_FundecList((yyvsp[(1) - (2)].fundec),(yyvsp[(2) - (2)].fundeclist));}
    break;

  case 76:
#line 211 "tiger.grm"
    {(yyval.fundec) = A_Fundec(EM_tokPos,S_Symbol((yyvsp[(2) - (7)].sval)),(yyvsp[(4) - (7)].fieldlist),S_Symbol(""),(yyvsp[(7) - (7)].exp));}
    break;

  case 77:
#line 212 "tiger.grm"
    {(yyval.fundec) = A_Fundec(EM_tokPos,S_Symbol((yyvsp[(2) - (9)].sval)),(yyvsp[(4) - (9)].fieldlist),S_Symbol((yyvsp[(7) - (9)].sval)),(yyvsp[(9) - (9)].exp));}
    break;


/* Line 1267 of yacc.c.  */
#line 1982 "y.tab.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
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

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
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
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
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

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


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

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
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
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}




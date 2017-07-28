
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
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

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.4.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Copy the first part of user declarations.  */

/* Line 189 of yacc.c  */
#line 21 "parser.y"



#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "ocesql.h"
#include "ocesqlutil.h"

	static void put_exec_list();
	int cb_get_level(int level);
	struct cb_field * cb_build_field_tree(int level, char *name , struct cb_field *last_field);
	int build_picture (const char *str,struct cb_field * pic);
	int check_has_occurs_children(struct cb_field *field);
	int check_host_has_multi_occurs(struct cb_field *field);

	static struct cb_field		*current_field;
	static struct cb_field		*description_field;
	int hostreferenceCount = 0;

	int yyerror(const char *msg)
	{
	  	printf("%06d:%s\n", yylineno,msg);
		return 0;
	}




/* Line 189 of yacc.c  */
#line 104 "parser.c"

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


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     SELECT = 258,
     SELECTFROM = 259,
     TOKEN = 260,
     CURNAME = 261,
     HOSTTOKEN = 262,
     WORD = 263,
     PICTURE = 264,
     INSERT = 265,
     UPDATE = 266,
     DISCONNECT = 267,
     DELETE = 268,
     EXECUTE = 269,
     OTHERFUNC = 270,
     INTO = 271,
     NUMERIC = 272,
     END_EXEC = 273,
     EXECSQL = 274,
     EXECSQL_INCLUDE = 275,
     PREPARE = 276,
     FROM = 277,
     DECLARE = 278,
     CURSOR = 279,
     FOR = 280,
     WORKINGBEGIN = 281,
     WORKINGEND = 282,
     HOSTVARIANTBEGIN = 283,
     HOSTVARIANTEND = 284,
     INCLUDE_FILE = 285,
     INCLUDE_SQLCA = 286,
     SQLCA = 287,
     IDENTIFIED_BY = 288,
     COMMIT_WORK = 289,
     ROLLBACK_WORK = 290,
     CONNECT = 291,
     USING = 292,
     OPEN = 293,
     CLOSE = 294,
     FETCH = 295,
     TRAILING = 296,
     COMP_1 = 297,
     COMP_2 = 298,
     COMP_3 = 299,
     USAGE = 300,
     SIGN = 301,
     LEADING = 302,
     SEPARATE = 303,
     AT = 304,
     IS = 305,
     ARE = 306,
     VALUE = 307,
     VARYING = 308,
     ALL = 309,
     OCCURS = 310,
     EXTERNAL = 311,
     TIMES = 312,
     CONST = 313,
     WHERECURRENTOF = 314
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 214 of yacc.c  */
#line 51 "parser.y"

	char *s;
	long int ld;
	struct cb_sql_list	*l;
	struct cb_hostreference_list *h;



/* Line 214 of yacc.c  */
#line 208 "parser.c"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 220 "parser.c"

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
# if YYENABLE_NLS
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
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
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
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
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
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  2
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   228

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  61
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  62
/* YYNRULES -- Number of rules.  */
#define YYNRULES  125
/* YYNRULES -- Number of states.  */
#define YYNSTATES  196

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   314

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,    60,     2,     2,     2,
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
      55,    56,    57,    58,    59
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     4,     7,     9,    11,    13,    15,    17,
      19,    21,    23,    25,    27,    29,    31,    33,    35,    37,
      39,    41,    47,    49,    55,    57,    63,    65,    71,    73,
      76,    81,    86,    93,    96,    98,   101,   104,   106,   109,
     112,   118,   124,   132,   133,   136,   138,   145,   151,   156,
     162,   165,   168,   171,   175,   179,   187,   195,   201,   211,
     219,   225,   231,   233,   235,   238,   243,   245,   248,   251,
     255,   257,   259,   261,   263,   265,   266,   271,   272,   275,
     278,   281,   285,   288,   291,   292,   297,   298,   302,   303,
     306,   308,   310,   312,   314,   316,   318,   320,   322,   324,
     328,   330,   332,   334,   336,   338,   343,   345,   347,   349,
     353,   357,   359,   362,   363,   365,   369,   372,   373,   375,
     376,   378,   380,   381,   383,   384
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      62,     0,    -1,    -1,    62,    63,    -1,    92,    -1,   100,
      -1,    87,    -1,    82,    -1,    89,    -1,    90,    -1,    79,
      -1,    78,    -1,    74,    -1,    73,    -1,    72,    -1,    91,
      -1,    70,    -1,    68,    -1,    64,    -1,    66,    -1,    83,
      -1,    19,    80,    65,    97,    18,    -1,    11,    -1,    19,
      80,    67,    97,    18,    -1,    12,    -1,    19,    80,    69,
      97,    18,    -1,    13,    -1,    19,    80,    71,    97,    18,
      -1,    10,    -1,    71,    16,    -1,    19,    80,    35,    18,
      -1,    19,    80,    34,    18,    -1,    19,    80,    75,    16,
      77,    18,    -1,    40,    99,    -1,    98,    -1,    76,     5,
      -1,    76,    98,    -1,    98,    -1,    77,     5,    -1,    77,
      98,    -1,    19,    80,    39,    99,    18,    -1,    19,    80,
      38,    99,    18,    -1,    19,    80,    38,    99,    37,    76,
      18,    -1,    -1,    49,    81,    -1,     7,    -1,    19,    84,
      85,    80,    86,    18,    -1,    19,    49,    81,    84,    18,
      -1,    19,    84,    80,    18,    -1,    19,    80,    15,    97,
      18,    -1,    36,    98,    -1,    33,    98,    -1,    37,    98,
      -1,    20,    30,    18,    -1,    20,    31,    18,    -1,    19,
      80,    21,    93,    22,    94,    18,    -1,    19,    80,    14,
      93,    37,    76,    18,    -1,    19,    80,    14,    93,    18,
      -1,    19,    80,     3,    97,    16,    77,     4,    97,    18,
      -1,    19,    80,     3,    97,    16,    77,    18,    -1,    19,
      80,    96,    95,    18,    -1,    19,    80,    96,    93,    18,
      -1,     5,    -1,     7,    -1,     3,    97,    -1,    23,    99,
      24,    25,    -1,    99,    -1,    97,    99,    -1,    97,    98,
      -1,    97,    59,     6,    -1,     7,    -1,     5,    -1,     3,
      -1,    25,    -1,    11,    -1,    -1,    26,   101,   102,    27,
      -1,    -1,   102,    87,    -1,   102,    88,    -1,   102,    96,
      -1,   102,   103,    60,    -1,   102,    28,    -1,   102,    29,
      -1,    -1,    17,     8,   104,   106,    -1,    -1,    17,   105,
     106,    -1,    -1,   106,   107,    -1,   108,    -1,   109,    -1,
     114,    -1,   117,    -1,   112,    -1,   118,    -1,   111,    -1,
       9,    -1,   110,    -1,    45,   119,   110,    -1,    42,    -1,
      43,    -1,    44,    -1,     8,    -1,    53,    -1,    52,   120,
     121,   113,    -1,    17,    -1,     8,    -1,    58,    -1,   115,
      47,   116,    -1,   115,    41,   116,    -1,    46,    -1,    46,
      50,    -1,    -1,    48,    -1,    55,    17,   122,    -1,   119,
      56,    -1,    -1,    50,    -1,    -1,    50,    -1,    51,    -1,
      -1,    54,    -1,    -1,    57,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   122,   122,   123,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,   134,   135,   136,   137,   138,   139,
     140,   144,   153,   157,   165,   168,   177,   180,   188,   189,
     194,   199,   206,   210,   213,   214,   215,   218,   219,   220,
     223,   229,   233,   238,   239,   242,   247,   248,   249,   252,
     258,   263,   268,   273,   278,   283,   288,   291,   296,   302,
     309,   310,   313,   316,   319,   322,   325,   326,   327,   330,
     337,   339,   340,   341,   342,   344,   344,   357,   358,   359,
     360,   361,   362,   363,   367,   367,   382,   382,   400,   401,
     406,   407,   408,   409,   410,   411,   412,   416,   420,   421,
     425,   426,   427,   428,   432,   446,   448,   449,   450,   453,
     457,   463,   464,   466,   467,   471,   478,   481,   481,   482,
     482,   482,   483,   483,   484,   484
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "SELECT", "SELECTFROM", "TOKEN",
  "CURNAME", "HOSTTOKEN", "WORD", "PICTURE", "INSERT", "UPDATE",
  "DISCONNECT", "DELETE", "EXECUTE", "OTHERFUNC", "INTO", "NUMERIC",
  "END_EXEC", "EXECSQL", "EXECSQL_INCLUDE", "PREPARE", "FROM", "DECLARE",
  "CURSOR", "FOR", "WORKINGBEGIN", "WORKINGEND", "HOSTVARIANTBEGIN",
  "HOSTVARIANTEND", "INCLUDE_FILE", "INCLUDE_SQLCA", "SQLCA",
  "IDENTIFIED_BY", "COMMIT_WORK", "ROLLBACK_WORK", "CONNECT", "USING",
  "OPEN", "CLOSE", "FETCH", "TRAILING", "COMP_1", "COMP_2", "COMP_3",
  "USAGE", "SIGN", "LEADING", "SEPARATE", "AT", "IS", "ARE", "VALUE",
  "VARYING", "ALL", "OCCURS", "EXTERNAL", "TIMES", "CONST",
  "WHERECURRENTOF", "'.'", "$accept", "sqlstate_list", "sqlstate",
  "updatesql", "update", "disconnectsql", "disconnect", "deletesql",
  "delete", "insertsql", "insert", "rollbacksql", "commitsql", "fetchsql",
  "fetch", "host_references", "res_host_references", "closesql", "opensql",
  "otherdb", "dbid", "connectsql", "othersql", "connect", "identified",
  "using", "incfile", "includesql", "preparesql", "execsql",
  "selectintosql", "declaresql", "prepared_stname", "statement_id",
  "select", "declare_for", "token_list", "host_reference", "expr",
  "sqlvariantstates", "$@1", "sqlvariantstate_list", "sqlvariantstate",
  "$@2", "$@3", "data_description_clause_sequence",
  "data_description_clause", "picture_clause", "usage_clause", "usage",
  "varying_clause", "value_clause", "const_clause", "sign_clause",
  "_sign_is", "flag_separate", "occurs_clause", "external_clause", "_is",
  "_is_are", "_all", "_times", 0
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
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
      46
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    61,    62,    62,    63,    63,    63,    63,    63,    63,
      63,    63,    63,    63,    63,    63,    63,    63,    63,    63,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    71,
      72,    73,    74,    75,    76,    76,    76,    77,    77,    77,
      78,    79,    79,    80,    80,    81,    82,    82,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    90,    91,    91,
      92,    92,    93,    94,    95,    96,    97,    97,    97,    97,
      98,    99,    99,    99,    99,   101,   100,   102,   102,   102,
     102,   102,   102,   102,   104,   103,   105,   103,   106,   106,
     107,   107,   107,   107,   107,   107,   107,   108,   109,   109,
     110,   110,   110,   110,   111,   112,   113,   113,   113,   114,
     114,   115,   115,   116,   116,   117,   118,   119,   119,   120,
     120,   120,   121,   121,   122,   122
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     5,     1,     5,     1,     5,     1,     5,     1,     2,
       4,     4,     6,     2,     1,     2,     2,     1,     2,     2,
       5,     5,     7,     0,     2,     1,     6,     5,     4,     5,
       2,     2,     2,     3,     3,     7,     7,     5,     9,     7,
       5,     5,     1,     1,     2,     4,     1,     2,     2,     3,
       1,     1,     1,     1,     1,     0,     4,     0,     2,     2,
       2,     3,     2,     2,     0,     4,     0,     3,     0,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     3,
       1,     1,     1,     1,     1,     4,     1,     1,     1,     3,
       3,     1,     2,     0,     1,     3,     2,     0,     1,     0,
       1,     1,     0,     1,     0,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,     0,     1,    43,     0,    75,     3,    18,    19,    17,
      16,    14,    13,    12,    11,    10,     7,    20,     6,     8,
       9,    15,     4,     5,     0,     0,     0,    43,     0,    77,
      70,    50,    45,    44,     0,    28,    22,    24,    26,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    43,    53,     0,
       0,    72,    71,    74,    73,     0,    66,    62,     0,     0,
       0,     0,    31,    30,     0,     0,    33,     0,     0,     0,
      29,     0,     0,     0,     0,     0,    51,    44,    48,     0,
      86,     0,    76,    82,    83,    78,    79,    80,     0,    47,
       0,     0,    68,    67,    57,     0,    49,     0,     0,    41,
       0,    40,    21,    23,    25,    27,     0,    37,    64,    61,
      60,     0,     0,    84,    88,     0,    81,     0,    69,     0,
      34,    63,     0,    65,     0,    38,    32,    39,    52,    46,
      88,    87,    54,     0,    59,    35,    56,    36,    55,    42,
      85,   103,    97,   100,   101,   102,   117,   111,   118,   119,
     104,     0,    89,    90,    91,    98,    96,    94,    92,     0,
      93,    95,     0,     0,     0,   112,   120,   121,   122,   124,
     113,   113,   116,    58,    99,   123,     0,   125,   115,   114,
     110,   109,   107,   106,   108,   105
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     6,     7,    48,     8,    49,     9,    50,    10,
      51,    11,    12,    13,    52,   129,   116,    14,    15,    26,
      33,    16,    17,    27,    57,   122,    18,    96,    19,    20,
      21,    22,    68,   132,    85,    53,    65,   102,    66,    23,
      29,    59,    98,   140,   124,   141,   162,   163,   164,   165,
     166,   167,   195,   168,   169,   190,   170,   171,   172,   178,
     186,   188
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -43
static const yytype_int16 yypact[] =
{
     -43,    64,   -43,   -27,    -2,   -43,   -43,   -43,   -43,   -43,
     -43,   -43,   -43,   -43,   -43,   -43,   -43,   -43,   -43,   -43,
     -43,   -43,   -43,   -43,    46,    50,   188,   -19,    29,   -43,
     -43,   -43,   -43,    25,   130,   -43,   -43,   -43,   -43,    68,
     130,    68,   130,    63,    67,   130,   130,   130,   130,   130,
     130,    75,    72,    38,    46,    50,    71,    43,   -43,   142,
      76,   -43,   -43,   -43,   -43,    13,   -43,   -43,   -12,    37,
      85,    89,   -43,   -43,   -11,    91,   -43,    49,    92,   109,
     -43,   118,    46,   130,    97,   100,   -43,   -43,   -43,    82,
     120,    40,   -43,   -43,   -43,   -43,   -43,   -43,    79,   -43,
      46,   126,   -43,   -43,   -43,    46,   -43,   138,   122,   -43,
      46,   -43,   -43,   -43,   -43,   -43,    14,   -43,   135,   -43,
     -43,    46,   139,   -43,   -43,   143,   -43,    61,   -43,   145,
     -43,   -43,   146,   -43,   149,   -43,   -43,   -43,   -43,   -43,
     -43,   140,   -43,   130,   -43,   -43,   -43,   -43,   -43,   -43,
     140,   -43,   -43,   -43,   -43,   -43,   103,   108,   -43,    51,
     -43,   155,   -43,   -43,   -43,   -43,   -43,   -43,   -43,    28,
     -43,   -43,   110,   119,     7,   -43,   -43,   -43,   121,   116,
     128,   128,   -43,   -43,   -43,   -43,     0,   -43,   -43,   -43,
     -43,   -43,   -43,   -43,   -43,   -43
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -43,   -43,   -43,   -43,   -43,   -43,   -43,   -43,   -43,   -43,
     -43,   -43,   -43,   -43,   -43,    69,    74,   -43,   -43,     6,
     125,   -43,   -43,   148,   -43,   -43,   129,   -43,   -43,   -43,
     -43,   -43,    -7,   -43,   -43,   147,   -38,   -23,   -42,   -43,
     -43,   -43,   -43,   -43,   -43,    47,   -43,   -43,   -43,    15,
     -43,   -43,   -43,   -43,   -43,    16,   -43,   -43,    48,   -43,
     -43,   -43
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -118
static const yytype_int16 yytable[] =
{
      71,    31,    69,    74,    75,    76,   104,   109,   192,    24,
      77,    78,    79,    81,    54,   151,    61,   193,    62,   135,
      30,    30,    25,   103,    63,   105,   110,   103,    28,   100,
      55,    86,   136,    56,    70,   103,   103,   103,    64,   103,
      61,    83,    62,    67,    30,   118,    84,    58,    63,   153,
     154,   155,    61,    30,    62,   106,    30,    32,   194,   117,
      63,    24,    64,    89,     2,   143,   135,   112,    30,   180,
      28,   125,   101,    67,    64,   181,   103,   117,    61,   144,
      62,    72,   130,     3,     4,    73,    63,   130,    82,    88,
       5,    80,    55,   137,    99,    61,   101,    62,   138,    30,
      64,   176,   177,    63,   137,   173,   147,   107,   101,   111,
     113,   147,    61,   108,    62,   119,    30,    64,   120,   121,
      63,    61,    61,    62,    62,    30,    30,   114,   123,    63,
      63,   103,   128,    61,    64,    62,   115,   183,    61,   126,
      62,    63,    30,    64,    64,   131,    63,   133,   151,   152,
     145,   101,    30,   158,   145,    64,    30,   139,   175,    90,
      64,   142,    91,   146,   148,    42,   182,   149,   101,    92,
      93,    94,   179,   187,   127,   185,   189,   101,   101,   134,
      87,    60,   153,   154,   155,   156,   157,   150,    95,   184,
     158,    34,   159,   160,   101,   161,  -117,   191,    35,    36,
      37,    38,    39,    40,   174,     0,    97,     0,     0,    41,
       0,    42,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    43,    44,     0,     0,    45,    46,    47
};

static const yytype_int16 yycheck[] =
{
      42,    24,    40,    45,    46,    47,    18,    18,     8,    36,
      48,    49,    50,    51,    33,     8,     3,    17,     5,     5,
       7,     7,    49,    65,    11,    37,    37,    69,    30,    16,
      49,    54,    18,    27,    41,    77,    78,    79,    25,    81,
       3,     3,     5,     5,     7,    83,    53,    18,    11,    42,
      43,    44,     3,     7,     5,    18,     7,     7,    58,    82,
      11,    36,    25,    57,     0,     4,     5,    18,     7,    41,
      30,    31,    59,     5,    25,    47,   118,   100,     3,    18,
       5,    18,   105,    19,    20,    18,    11,   110,    16,    18,
      26,    16,    49,   116,    18,     3,    59,     5,   121,     7,
      25,    50,    51,    11,   127,   143,   129,    22,    59,    18,
      18,   134,     3,    24,     5,    18,     7,    25,    18,    37,
      11,     3,     3,     5,     5,     7,     7,    18,     8,    11,
      11,   173,     6,     3,    25,     5,    18,    18,     3,    60,
       5,    11,     7,    25,    25,     7,    11,    25,     8,     9,
       5,    59,     7,    50,     5,    25,     7,    18,    50,    17,
      25,    18,    20,    18,    18,    23,    56,    18,    59,    27,
      28,    29,    17,    57,   100,    54,    48,    59,    59,   110,
      55,    33,    42,    43,    44,    45,    46,   140,    59,   174,
      50,     3,    52,    53,    59,    55,    56,   181,    10,    11,
      12,    13,    14,    15,   156,    -1,    59,    -1,    -1,    21,
      -1,    23,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    34,    35,    -1,    -1,    38,    39,    40
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    62,     0,    19,    20,    26,    63,    64,    66,    68,
      70,    72,    73,    74,    78,    79,    82,    83,    87,    89,
      90,    91,    92,   100,    36,    49,    80,    84,    30,   101,
       7,    98,     7,    81,     3,    10,    11,    12,    13,    14,
      15,    21,    23,    34,    35,    38,    39,    40,    65,    67,
      69,    71,    75,    96,    33,    49,    80,    85,    18,   102,
      84,     3,     5,    11,    25,    97,    99,     5,    93,    97,
      93,    99,    18,    18,    99,    99,    99,    97,    97,    97,
      16,    97,    16,     3,    93,    95,    98,    81,    18,    80,
      17,    20,    27,    28,    29,    87,    88,    96,   103,    18,
      16,    59,    98,    99,    18,    37,    18,    22,    24,    18,
      37,    18,    18,    18,    18,    18,    77,    98,    97,    18,
      18,    37,    86,     8,   105,    31,    60,    77,     6,    76,
      98,     7,    94,    25,    76,     5,    18,    98,    98,    18,
     104,   106,    18,     4,    18,     5,    18,    98,    18,    18,
     106,     8,     9,    42,    43,    44,    45,    46,    50,    52,
      53,    55,   107,   108,   109,   110,   111,   112,   114,   115,
     117,   118,   119,    97,   119,    50,    50,    51,   120,    17,
      41,    47,    56,    18,   110,    54,   121,    57,   122,    48,
     116,   116,     8,    17,    58,   113
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
# if YYLTYPE_IS_TRIVIAL
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
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
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
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      YYFPRINTF (stderr, "\n");
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


/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*-------------------------.
| yyparse or yypush_parse.  |
`-------------------------*/

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
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

       Refer to the stacks thru separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

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
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
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
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
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

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
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
        case 21:

/* Line 1455 of yacc.c  */
#line 146 "parser.y"
    {
	(yyval.l) = cb_add_text_list ((yyvsp[(3) - (5)].l), (yyvsp[(4) - (5)].l));
	put_exec_list();
;}
    break;

  case 22:

/* Line 1455 of yacc.c  */
#line 153 "parser.y"
    {(yyval.l) = cb_text_list_add (NULL, (yyvsp[(1) - (1)].s));;}
    break;

  case 23:

/* Line 1455 of yacc.c  */
#line 159 "parser.y"
    {
	(yyval.l) = cb_add_text_list ((yyvsp[(3) - (5)].l), (yyvsp[(4) - (5)].l));
	put_exec_list();
;}
    break;

  case 24:

/* Line 1455 of yacc.c  */
#line 165 "parser.y"
    {(yyval.l) = cb_text_list_add (NULL, (yyvsp[(1) - (1)].s));;}
    break;

  case 25:

/* Line 1455 of yacc.c  */
#line 170 "parser.y"
    {
	(yyval.l) = cb_add_text_list ((yyvsp[(3) - (5)].l), (yyvsp[(4) - (5)].l));
	put_exec_list();
;}
    break;

  case 26:

/* Line 1455 of yacc.c  */
#line 177 "parser.y"
    {(yyval.l) = cb_text_list_add (NULL, (yyvsp[(1) - (1)].s));;}
    break;

  case 27:

/* Line 1455 of yacc.c  */
#line 182 "parser.y"
    {
	(yyval.l) = cb_add_text_list ((yyvsp[(3) - (5)].l), (yyvsp[(4) - (5)].l));
	put_exec_list();
;}
    break;

  case 28:

/* Line 1455 of yacc.c  */
#line 188 "parser.y"
    {(yyval.l) = cb_text_list_add (NULL, (yyvsp[(1) - (1)].s));;}
    break;

  case 29:

/* Line 1455 of yacc.c  */
#line 189 "parser.y"
    {(yyval.l) = cb_text_list_add ((yyvsp[(1) - (2)].l), (yyvsp[(2) - (2)].s));;}
    break;

  case 30:

/* Line 1455 of yacc.c  */
#line 194 "parser.y"
    {
	put_exec_list();
;}
    break;

  case 31:

/* Line 1455 of yacc.c  */
#line 199 "parser.y"
    {
	put_exec_list();
;}
    break;

  case 32:

/* Line 1455 of yacc.c  */
#line 206 "parser.y"
    {
	put_exec_list();
;}
    break;

  case 33:

/* Line 1455 of yacc.c  */
#line 210 "parser.y"
    { cb_set_cursorname((yyvsp[(2) - (2)].s));;}
    break;

  case 34:

/* Line 1455 of yacc.c  */
#line 213 "parser.y"
    {cb_host_list_add (host_reference_list, (yyvsp[(1) - (1)].s));;}
    break;

  case 36:

/* Line 1455 of yacc.c  */
#line 215 "parser.y"
    {cb_host_list_add (host_reference_list, (yyvsp[(2) - (2)].s));;}
    break;

  case 37:

/* Line 1455 of yacc.c  */
#line 218 "parser.y"
    {cb_res_host_list_add (res_host_reference_list, (yyvsp[(1) - (1)].s));;}
    break;

  case 39:

/* Line 1455 of yacc.c  */
#line 220 "parser.y"
    {cb_res_host_list_add (res_host_reference_list, (yyvsp[(2) - (2)].s));;}
    break;

  case 40:

/* Line 1455 of yacc.c  */
#line 223 "parser.y"
    {
	cb_set_cursorname((yyvsp[(4) - (5)].s));
	put_exec_list();
;}
    break;

  case 41:

/* Line 1455 of yacc.c  */
#line 229 "parser.y"
    {
	cb_set_cursorname((yyvsp[(4) - (5)].s));
	put_exec_list();
;}
    break;

  case 42:

/* Line 1455 of yacc.c  */
#line 233 "parser.y"
    {
	cb_set_cursorname((yyvsp[(4) - (7)].s));
	put_exec_list();
;}
    break;

  case 44:

/* Line 1455 of yacc.c  */
#line 239 "parser.y"
    { ;}
    break;

  case 45:

/* Line 1455 of yacc.c  */
#line 242 "parser.y"
    {
	cb_set_dbname((yyvsp[(1) - (1)].s));
;}
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 247 "parser.y"
    { put_exec_list(); ;}
    break;

  case 47:

/* Line 1455 of yacc.c  */
#line 248 "parser.y"
    { put_exec_list(); ;}
    break;

  case 48:

/* Line 1455 of yacc.c  */
#line 249 "parser.y"
    { put_exec_list(); ;}
    break;

  case 49:

/* Line 1455 of yacc.c  */
#line 252 "parser.y"
    {
	(yyval.l) = cb_add_text_list(cb_text_list_add(NULL, (yyvsp[(3) - (5)].s)), (yyvsp[(4) - (5)].l));
	put_exec_list();
;}
    break;

  case 50:

/* Line 1455 of yacc.c  */
#line 258 "parser.y"
    {
	cb_host_list_add (host_reference_list, (yyvsp[(2) - (2)].s));
;}
    break;

  case 51:

/* Line 1455 of yacc.c  */
#line 263 "parser.y"
    {
	cb_host_list_add (host_reference_list, (yyvsp[(2) - (2)].s));
;}
    break;

  case 52:

/* Line 1455 of yacc.c  */
#line 268 "parser.y"
    {
	cb_host_list_add (host_reference_list, (yyvsp[(2) - (2)].s));
;}
    break;

  case 53:

/* Line 1455 of yacc.c  */
#line 273 "parser.y"
    {
	put_exec_list();
;}
    break;

  case 54:

/* Line 1455 of yacc.c  */
#line 278 "parser.y"
    {
	put_exec_list();
;}
    break;

  case 55:

/* Line 1455 of yacc.c  */
#line 283 "parser.y"
    {
	put_exec_list();
;}
    break;

  case 56:

/* Line 1455 of yacc.c  */
#line 288 "parser.y"
    {
	put_exec_list();
;}
    break;

  case 57:

/* Line 1455 of yacc.c  */
#line 291 "parser.y"
    {
	put_exec_list();
;}
    break;

  case 58:

/* Line 1455 of yacc.c  */
#line 296 "parser.y"
    {
	(yyval.l) = cb_add_text_list(cb_text_list_add(NULL, (yyvsp[(3) - (9)].s)), (yyvsp[(4) - (9)].l));
	cb_add_text_list((yyval.l), cb_text_list_add(NULL, (yyvsp[(7) - (9)].s)));
	cb_add_text_list((yyval.l), (yyvsp[(8) - (9)].l));
	put_exec_list();
;}
    break;

  case 59:

/* Line 1455 of yacc.c  */
#line 302 "parser.y"
    {
	(yyval.l) = cb_add_text_list(cb_text_list_add(NULL, (yyvsp[(3) - (7)].s)), (yyvsp[(4) - (7)].l));
	put_exec_list();
;}
    break;

  case 60:

/* Line 1455 of yacc.c  */
#line 309 "parser.y"
    { put_exec_list(); ;}
    break;

  case 61:

/* Line 1455 of yacc.c  */
#line 310 "parser.y"
    { put_exec_list(); ;}
    break;

  case 62:

/* Line 1455 of yacc.c  */
#line 313 "parser.y"
    { cb_set_prepname((yyvsp[(1) - (1)].s)); ;}
    break;

  case 63:

/* Line 1455 of yacc.c  */
#line 316 "parser.y"
    { cb_host_list_add (host_reference_list, (yyvsp[(1) - (1)].s)); ;}
    break;

  case 64:

/* Line 1455 of yacc.c  */
#line 319 "parser.y"
    { (yyval.l) = cb_add_text_list (cb_text_list_add (NULL, (yyvsp[(1) - (2)].s)), (yyvsp[(2) - (2)].l));;}
    break;

  case 65:

/* Line 1455 of yacc.c  */
#line 322 "parser.y"
    { cb_set_cursorname((yyvsp[(2) - (4)].s));;}
    break;

  case 66:

/* Line 1455 of yacc.c  */
#line 325 "parser.y"
    {      (yyval.l) = cb_text_list_add (NULL, (yyvsp[(1) - (1)].s));;}
    break;

  case 67:

/* Line 1455 of yacc.c  */
#line 326 "parser.y"
    {      (yyval.l) = cb_text_list_add ((yyvsp[(1) - (2)].l), (yyvsp[(2) - (2)].s));;}
    break;

  case 68:

/* Line 1455 of yacc.c  */
#line 327 "parser.y"
    {
	(yyval.l) = cb_text_list_add ((yyvsp[(1) - (2)].l), cb_host_list_add (host_reference_list, (yyvsp[(2) - (2)].s)));
;}
    break;

  case 69:

/* Line 1455 of yacc.c  */
#line 330 "parser.y"
    {
	     (yyval.l) = cb_text_list_add((yyvsp[(1) - (3)].l), "WHERE CURRENT OF");
	     cb_set_cursorname((yyvsp[(3) - (3)].s));
	     (yyval.l) = cb_text_list_add((yyvsp[(1) - (3)].l), cursorname);
;}
    break;

  case 70:

/* Line 1455 of yacc.c  */
#line 337 "parser.y"
    {;}
    break;

  case 71:

/* Line 1455 of yacc.c  */
#line 339 "parser.y"
    {;}
    break;

  case 72:

/* Line 1455 of yacc.c  */
#line 340 "parser.y"
    {;}
    break;

  case 73:

/* Line 1455 of yacc.c  */
#line 341 "parser.y"
    {;}
    break;

  case 74:

/* Line 1455 of yacc.c  */
#line 342 "parser.y"
    {;}
    break;

  case 75:

/* Line 1455 of yacc.c  */
#line 344 "parser.y"
    {
	current_field = NULL;
	description_field = NULL;
	put_exec_list();
;}
    break;

  case 76:

/* Line 1455 of yacc.c  */
#line 351 "parser.y"
    {
	// check host_variable
	put_exec_list();
;}
    break;

  case 82:

/* Line 1455 of yacc.c  */
#line 362 "parser.y"
    { put_exec_list(); ;}
    break;

  case 83:

/* Line 1455 of yacc.c  */
#line 363 "parser.y"
    { put_exec_list(); ;}
    break;

  case 84:

/* Line 1455 of yacc.c  */
#line 367 "parser.y"
    {
	struct cb_field *x;

	x =  cb_build_field_tree( (yyvsp[(1) - (2)].ld), (yyvsp[(2) - (2)].s) , current_field);
	if( x != NULL)
	{
		if( x->level != 78)
			current_field = x;
	}
;}
    break;

  case 85:

/* Line 1455 of yacc.c  */
#line 378 "parser.y"
    {
	if (description_field == NULL)
		description_field = current_field;
;}
    break;

  case 86:

/* Line 1455 of yacc.c  */
#line 382 "parser.y"
    {
	struct cb_field *x;

	x =  cb_build_field_tree( (yyvsp[(1) - (1)].ld), "" , current_field); // regist dummy name
	if( x != NULL){
		if( x->level != 78)
			current_field = x;
	}
;}
    break;

  case 87:

/* Line 1455 of yacc.c  */
#line 392 "parser.y"
    {
	if (description_field == NULL)
		description_field = current_field;
;}
    break;

  case 88:

/* Line 1455 of yacc.c  */
#line 400 "parser.y"
    {;}
    break;

  case 89:

/* Line 1455 of yacc.c  */
#line 402 "parser.y"
    {;}
    break;

  case 97:

/* Line 1455 of yacc.c  */
#line 416 "parser.y"
    {  build_picture( (yyvsp[(1) - (1)].s),current_field); ;}
    break;

  case 100:

/* Line 1455 of yacc.c  */
#line 425 "parser.y"
    { current_field->usage = USAGE_FLOAT;   ;}
    break;

  case 101:

/* Line 1455 of yacc.c  */
#line 426 "parser.y"
    { current_field->usage = USAGE_DOUBLE; ;}
    break;

  case 102:

/* Line 1455 of yacc.c  */
#line 427 "parser.y"
    { current_field->usage = USAGE_PACKED; ;}
    break;

  case 103:

/* Line 1455 of yacc.c  */
#line 428 "parser.y"
    { current_field->usage = USAGE_OTHER; ;}
    break;

  case 104:

/* Line 1455 of yacc.c  */
#line 433 "parser.y"
    {
	if(current_field->pictype != PIC_ALPHANUMERIC &&
		current_field->pictype != PIC_NATIONAL){
		printf("parse error: %s specified the data types are not available to VARYING\n",
		       current_field->sname);
		exit(-1);
	}

	var_varying = current_field;
	put_exec_list();
;}
    break;

  case 105:

/* Line 1455 of yacc.c  */
#line 446 "parser.y"
    {;}
    break;

  case 106:

/* Line 1455 of yacc.c  */
#line 448 "parser.y"
    {;}
    break;

  case 107:

/* Line 1455 of yacc.c  */
#line 449 "parser.y"
    {;}
    break;

  case 108:

/* Line 1455 of yacc.c  */
#line 450 "parser.y"
    {;}
    break;

  case 109:

/* Line 1455 of yacc.c  */
#line 454 "parser.y"
    {
	current_field->sign_leading = SIGNLEADING;
;}
    break;

  case 110:

/* Line 1455 of yacc.c  */
#line 458 "parser.y"
    {

;}
    break;

  case 111:

/* Line 1455 of yacc.c  */
#line 463 "parser.y"
    {;}
    break;

  case 112:

/* Line 1455 of yacc.c  */
#line 464 "parser.y"
    {;}
    break;

  case 114:

/* Line 1455 of yacc.c  */
#line 467 "parser.y"
    { current_field->separate = SIGN_SEPARATE; ;}
    break;

  case 115:

/* Line 1455 of yacc.c  */
#line 472 "parser.y"
    {
	current_field->occurs = (int)(yyvsp[(2) - (3)].ld);
;}
    break;

  case 116:

/* Line 1455 of yacc.c  */
#line 478 "parser.y"
    {;}
    break;



/* Line 1455 of yacc.c  */
#line 2284 "parser.c"
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

#if !defined(yyoverflow) || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
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



/* Line 1675 of yacc.c  */
#line 486 "parser.y"



static void
put_exec_list()
{
	struct cb_exec_list *l;
	struct cb_exec_list *p;

	struct cb_hostreference_list *h;
	h = host_reference_list;

	for(; h; h = h->next)
	{
		hostreferenceCount++;
	}

	l = malloc (sizeof (struct cb_exec_list));

	l->startLine = startlineno;
	l->endLine = endlineno;
	l->period = period;
	l->host_list = host_reference_list;
	l->hostreferenceCount =hostreferenceCount;
	l->res_host_list = res_host_reference_list;
	l->conn_use_other_db = conn_use_other_db;
	l->sql_list = sql_list;
	l->dbName = com_strdup(dbname);
	l->prepareName = com_strdup(prepname);
	l->cursorName = com_strdup(cursorname);
	l->commandName = com_strdup(commandname);
	l->command_putother = command_putother;
	l->sqlName = com_strdup(sqlname);
	l->incfileName = com_strdup(incfilename);
	l->varname = var_varying;
	l->next = NULL;

	if (exec_list == NULL)
	{
		exec_list = l;
	}else{
		p = exec_list;
		for (; p->next ; p = p->next);
		p->next = l;

	}

}


static  int  xxx =0;

struct cb_field *getfieldbynamefrom(char *name , struct cb_field *field)
{
	struct cb_field * p;

	if( field == NULL)
		return NULL;

	xxx++;

	if(strcmp(name,field->sname) == 0 ){
		return field;
	}

	p = getfieldbynamefrom(name, field->sister);
	if( p != NULL )
		return p;


	p = getfieldbynamefrom(name, field->children);
	if( p != NULL )
		return p;

	return NULL;

}

struct cb_field * getfieldbyname(char *name )
{
	return getfieldbynamefrom(name, description_field);
}

int gethostvarianttype(char *name,  int *type, int *digits, int *scale)
{
	struct cb_field * p;
	int tmp_type,tmp_dig,tmp_scl;
	p = getfieldbyname(name);
	if( p == NULL){
		return ERR_NOTDEF_WORKING;
	}
	*digits = tmp_dig = p->picnsize;
	*scale = tmp_scl = -(p->scale);
	if(  p->pictype != 0 ){
		switch(p->pictype){
		case PIC_ALPHANUMERIC:
			tmp_type =  HVARTYPE_ALPHABETIC;
			break;
		case PIC_NATIONAL:
			tmp_type = HVARTYPE_NATIONAL;
			break;
		case PIC_NUMERIC:
			if(p->have_sign){
				if(p->usage){
					switch(p->usage){
					case USAGE_PACKED:
						tmp_type = HVARTYPE_SIGNED_PACKED;
						break;
					case USAGE_BINARY_NATIVE:
						tmp_type = HVARTYPE_SIGNED_BINARY_NATIVE;
						break;
					default:
						return ERR_NOT_SUPPORTED_USAGE;
					}
				}else if(p->sign_leading){
					if(p->separate){
						tmp_type = HVARTYPE_SIGNED_LEADING_SEPARATE;
					}else{
						tmp_type = HVARTYPE_SIGNED_LEADING_COMBINED;
					}
				}else{
					if(p->separate){
						tmp_type = HVARTYPE_SIGNED_TRAILING_SEPARATE;
					}else{
						tmp_type = HVARTYPE_SIGNED_TRAILING_COMBINED;
					}
				}
			}else{
				if(p->usage){
					switch(p->usage){
					case USAGE_PACKED:
						tmp_type = HVARTYPE_UNSIGNED_PACKED;
						break;
					case USAGE_BINARY_NATIVE:
						tmp_type = HVARTYPE_UNSIGNED_BINARY_NATIVE;
						break;
					default:
						return ERR_NOT_SUPPORTED_USAGE;
					}
				}else{
					tmp_type = HVARTYPE_UNSIGNED_NUMERIC;
				}
			}
			break;
		case PIC_ALPHANUMERIC_VARYING:
			tmp_type =  HVARTYPE_ALPHANUMERIC_VARYING;
			break;
		case PIC_NATIONAL_VARYING:
			tmp_type =  HVARTYPE_JAPANESE_VARYING;
			break;
		default:
			break;
		}
		*type = tmp_type;
		return 0;
	} else { // Group data
		if(p->occurs > 0){
			struct cb_field * c;

			c = p->children;
			while(c != NULL){
				if(c->children){
					return ERR_NOTDEF_CONVERSION;
				}
				c = c->sister;
			}
		}
		*type = HVARTYPE_GROUP;
		return 0;
	}
	if(p->usage){
		switch(p->usage){
		case USAGE_FLOAT:
			tmp_type = HVARTYPE_FLOAT;
			break;
		case USAGE_DOUBLE:
			tmp_type = HVARTYPE_FLOAT;
			break;
		default:
			return ERR_NOT_SUPPORTED_USAGE;
		}
		*type = tmp_type;
		return 0;
	}
	return ERR_NOTDEF_CONVERSION;
}

int cb_get_level (int val)
{
	int level = val;

	/* check level */
	switch (level) {
	case 66:
	case 77:
	case 78:
	case 88:
		break;
	default:
		if (level < 1 || level > 49) {
			goto level_error;
		}
		break;
	}

	return level;

	level_error:

	return 0;
}

struct cb_field *
cb_field_founder (struct cb_field *f)
{
     while (f->parent) {
		f = f->parent;
	}
	return f;
}
struct cb_field * cb_build_field_tree(int level, char *name , struct cb_field *last_field)
{
	int lv;
	struct cb_field *f, *p;

	if(name == NULL)
		return NULL;

	lv = cb_get_level (level);
	if (!lv) {
		return NULL;
	}

	f = malloc(sizeof(struct  cb_field));
	if( f == NULL )
		return NULL;

	memset(f, 0 ,sizeof(struct cb_field));

	f->sname = com_strdup(name);

	if (lv == 78) {
		f->level = 1;
	} else{
		f->level = lv;
	}

	if (last_field) {
		if (last_field->level == 77 && f->level != 01 &&
				f->level != 77 && f->level != 66 && f->level != 88) {
			return NULL;
		}
	}

	if (f->level == 1 || f->level == 77) {
		/* top level */
		if (last_field) {
			cb_field_founder (last_field)->sister = f;
		}
	} else {
		if(last_field == NULL){
			printf("parse error: %s level should start from 01 or 66 or 77 or 88\n", name);
			exit(-1);
			return NULL;
		}

		if (f->level == 66) {
			/* level 66 */
			f->parent = cb_field_founder (last_field);
			for (p = f->parent->children; p && p->sister; p = p->sister) ;
			if (p) {
				p->sister = f;
			}
		} else if (f->level == 88) {
			/* level 88 */
			f->parent = last_field;
		}else if (f->level > last_field->level) {
			/* lower level */
			last_field->children = f;
			f->parent = last_field;
		} else if (f->level == last_field->level) {
			/* same level */
			same_level:
			last_field->sister = f;
			f->parent = last_field->parent;
		} else {
			/* upper level */
			for (p = last_field->parent; p; p = p->parent) {
				if (p->level == f->level) {
					last_field = p;
					goto same_level;
				}
				if ( p->level < f->level) {
				     break;
				}
			}
			return NULL;
		}
	}

	return f;
}

int  build_picture (const char *str,struct cb_field * pic){
	const char		*p;

	int			i;
	int			n;
	unsigned char		c;

	int	category = 0;
	int s_count = 0;
	int v_count = 0;
	int idx = 0;
	int digits = 0;
	int scale = 0;
	int allocated = 0;

	if (strlen (str) > 50) {
		return 0;
	}

	for(p = str; *p; p++){
		n=1;
		c=*p;
	while(1){
		while(p[1]==c){
			p++; n++;
		}

		if(p[1] == '('){
			i=0;
			p += 2;
			allocated = 0;
			for(;*p == '0';p++){
				;
			}
			for(;*p != ')';p++){
				if(!isdigit(*p)){
					return 0;
				} else {
					allocated++;
					if(allocated > 9){
						return 0;
					}
					i = i * 10 + (*p - '0');
				}
			}
			if(i==0){
				return 0;
			}
			n+=i-1;
			continue;
		}
		break;
		}


		switch(c){
		case 'X':
			if(s_count | v_count){
				return 0;
			}
			category |=  PIC_ALPHANUMERIC;
			digits += n;
			break;
		case '9':
			category |= PIC_NUMERIC;
			digits += n;
			if(v_count){
				scale += n;
			}
			break;
		case 'N':
			if(s_count | v_count){
				return 0;
			}
			category |=  PIC_NATIONAL;
			digits += n;
			break;
		case 'S':
			category |= PIC_NUMERIC;
			if(category & PIC_ALPHABETIC) {
				return 0;
			}
			s_count += n;
			if(s_count > 1 || idx !=0){
				return 0;
			}
			continue;
		case 'V':
			category |= PIC_NUMERIC;
			if(category & PIC_ALPHABETIC) {
				return 0;
			}
			v_count += n;
			if(v_count > 1){
				return 0;
			}
			break;
		default:
			break;
		}
		idx += sizeof(int);
	}

	pic->picnsize = digits;
	pic->scale = scale;
	pic->have_sign = (unsigned char)s_count;
	pic->pictype = category;
	return 1;
}

int
check_has_occurs_children(struct cb_field *field){
	int ret;

	if(field == NULL)
		return 0;

	printf("CHILDR:sname=%s, level=%d, occurs=%d, children=%d",
	       field->sname, field->level, field->occurs, field->children);

	if(field->occurs != 0){
		return 1;
	}

	if(field->children != NULL){
		return 2;
	}

	ret = check_has_occurs_children(field->sister);
	if(ret) return ret;

	return 0;
}

int
check_host_has_multi_occurs(struct cb_field *field){
	int ret;

	if(field == NULL)
		return 0;

	if(field->occurs != 0){
		ret = check_has_occurs_children(field->children);
		if(ret) return ret;
	}

	ret = check_host_has_multi_occurs(field->children);
	if(ret) return ret;

	ret = check_host_has_multi_occurs(field->sister);
	if(ret) return ret;

	return 0;
}



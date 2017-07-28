
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton interface for Bison's Yacc-like parsers in C
   
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

/* Line 1676 of yacc.c  */
#line 51 "parser.y"

	char *s;
	long int ld;
	struct cb_sql_list	*l;
	struct cb_hostreference_list *h;



/* Line 1676 of yacc.c  */
#line 120 "parser.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE yylval;



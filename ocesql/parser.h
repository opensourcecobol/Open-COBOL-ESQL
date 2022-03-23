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

#ifndef YY_YY_PARSER_H_INCLUDED
# define YY_YY_PARSER_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
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
    COMP_5 = 300,
    USAGE = 301,
    SIGN = 302,
    LEADING = 303,
    SEPARATE = 304,
    AT = 305,
    IS = 306,
    ARE = 307,
    VALUE = 308,
    VARYING = 309,
    ALL = 310,
    OCCURS = 311,
    EXTERNAL = 312,
    TIMES = 313,
    CONST = 314,
    WHERECURRENTOF = 315
  };
#endif
/* Tokens.  */
#define SELECT 258
#define SELECTFROM 259
#define TOKEN 260
#define CURNAME 261
#define HOSTTOKEN 262
#define WORD 263
#define PICTURE 264
#define INSERT 265
#define UPDATE 266
#define DISCONNECT 267
#define DELETE 268
#define EXECUTE 269
#define OTHERFUNC 270
#define INTO 271
#define NUMERIC 272
#define END_EXEC 273
#define EXECSQL 274
#define EXECSQL_INCLUDE 275
#define PREPARE 276
#define FROM 277
#define DECLARE 278
#define CURSOR 279
#define FOR 280
#define WORKINGBEGIN 281
#define WORKINGEND 282
#define HOSTVARIANTBEGIN 283
#define HOSTVARIANTEND 284
#define INCLUDE_FILE 285
#define INCLUDE_SQLCA 286
#define SQLCA 287
#define IDENTIFIED_BY 288
#define COMMIT_WORK 289
#define ROLLBACK_WORK 290
#define CONNECT 291
#define USING 292
#define OPEN 293
#define CLOSE 294
#define FETCH 295
#define TRAILING 296
#define COMP_1 297
#define COMP_2 298
#define COMP_3 299
#define COMP_5 300
#define USAGE 301
#define SIGN 302
#define LEADING 303
#define SEPARATE 304
#define AT 305
#define IS 306
#define ARE 307
#define VALUE 308
#define VARYING 309
#define ALL 310
#define OCCURS 311
#define EXTERNAL 312
#define TIMES 313
#define CONST 314
#define WHERECURRENTOF 315

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 50 "parser.y"

	char *s;
	long int ld;
	struct cb_sql_list	*l;
	struct cb_hostreference_list *h;

#line 184 "parser.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_PARSER_H_INCLUDED  */

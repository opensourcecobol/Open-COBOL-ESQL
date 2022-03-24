/*
 * Copyright (C) 2022 Tokyo System House Co.,Ltd.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */


%option 8bit
%option caseless
%option noyywrap
%option never-interactive
%option yylineno
%option stack

%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "ocesql.h"
#include "ocesqlutil.h"
#include "parser.h"

int startlineno = 0;
int endlineno = 0;
int hostlineno = 0;
int includelinenum = 0;
int includeflag = 0;
int period = 0;
int conn_use_other_db = 0;
int command_putother = 0;
struct cb_exec_list *exec_list = NULL;
struct cb_hostreference_list *host_reference_list;
struct cb_res_hostreference_list *res_host_reference_list;
struct cb_sql_list *sql_list;

char dbname[BUFFSIZE];
char prepname[BUFFSIZE];
char commandname[BUFFSIZE];
char cursorname[BUFFSIZE];
char sqlname[BUFFSIZE];
int sqlnum = 0;
char incfilename[BUFFSIZE];
struct cb_field *var_varying;

int flag_insqlstring = 0;
int flag_selectcommand = 0;

#define YY_INPUT(buf,result,max_size) result = yyinput (buf, max_size);
static int
yyinput (char *buf, int max_size);

#define SET_LOCATION(x)				\
  (x)->source_file = (unsigned char *)cb_source_file;		\

%}

%s PICTURE_STATE WORKING_STATE

%x ESQL_FUNC_STATE ESQL_INCLUDE_STATE ESQL_SELECT_STATE ESQL_STATE ESQL_DBNAME_STATE WHERE_CURRENT_OF

JPNWORD [\xA0-\xDF]|([\x81-\x9F\xE0-\xFC][\x40-\x7E\x80-\xFC])
DIGIT [0-9]
WORD ([A-Za-z\+\-0-9_]|[(]|[)]|[\'])
INCFILE [A-Za-z0-9_\+\-]+
FILENAME [A-Za-z0-9_\+\-\.]+
STRVALUE ("\""[^\"]*"\""|"\'"[^\']*"\'")
HEXVALUE "X"("\""[^\"]+"\""|"\'"[^\']+"\'")
SELF [,()\[\].;\:\+\-\*\/\%\^\<\>\=]
OP_CHARS [\~\!\@\#\^\&\|\`\?\+\-\*\/\%\<\>\=]
OPERATOR {OP_CHARS}+
COMPARISON "="|"<>"|"<"|">"|"<="|">="
COMMA ","
HOSTWORD ":"([A-Za-z\-0-9_]*([\xA0-\xDF]|([\x81-\x9F\xE0-\xFC][\x40-\x7E\x80-\xFC]))*[A-Za-z\-0-9_]*)

INT_CONSTANT {digit}+

%%


"EXEC"[ ]+"SQL"		{
					BEGIN ESQL_FUNC_STATE;

					startlineno = yylineno;
					host_reference_list = NULL;
					res_host_reference_list = NULL;
					memset(dbname,0,sizeof(dbname));
					memset(prepname,0,sizeof(prepname));
					memset(commandname,0,sizeof(commandname));
					memset(cursorname,0,sizeof(cursorname));
					memset(sqlname,0,sizeof(sqlname));
					memset(incfilename,0,sizeof(incfilename));
					hostreferenceCount = 0;
					sql_list = NULL;
					var_varying = NULL;            
					period = 0;
					conn_use_other_db = 0;
					command_putother = 0;
					return EXECSQL;
}

<ESQL_FUNC_STATE>{
	"SELECT" {
		BEGIN ESQL_STATE;
		flag_insqlstring = 1;
		flag_selectcommand = 1;

		yylval.s = com_strdup (yytext);
		memset(commandname,0,sizeof(commandname));
		com_strcpy(commandname,sizeof(commandname),yylval.s);

		sqlnum++;
     		memset(sqlname, 0, sizeof(sqlname));
     		com_sprintf(sqlname, sizeof(sqlname), "SQ%04d", sqlnum);

		return SELECT;
	}

	"INSERT" {
		BEGIN ESQL_STATE;
		flag_insqlstring = 1;

		yylval.s = com_strdup (yytext);
		memset(commandname,0,sizeof(commandname));
		com_strcpy(commandname,sizeof(commandname),yylval.s);

		sqlnum++;
     		memset(sqlname, 0, sizeof(sqlname));
     		com_sprintf(sqlname, sizeof(sqlname), "SQ%04d", sqlnum);

		return INSERT;
	}

	"DELETE" {
		BEGIN ESQL_STATE;
		flag_insqlstring = 1;

		yylval.s = com_strdup (yytext);
		memset(commandname,0,sizeof(commandname));
		com_strcpy(commandname,sizeof(commandname),yylval.s);

		sqlnum++;
     		memset(sqlname, 0, sizeof(sqlname));
     		com_sprintf(sqlname, sizeof(sqlname), "SQ%04d", sqlnum);

		return DELETE;
	}

	"CONNECT" {
		BEGIN ESQL_STATE;

		com_strcpy(commandname,sizeof(commandname),"CONNECT");
		return CONNECT;
	}

	"DISCONNECT" {
		BEGIN ESQL_STATE;
		flag_insqlstring = 1;

		yylval.s = com_strdup (yytext);
		memset(commandname,0,sizeof(commandname));
		com_strcpy(commandname,sizeof(commandname),yylval.s);

		sqlnum++;
     		memset(sqlname, 0, sizeof(sqlname));
     		com_sprintf(sqlname, sizeof(sqlname), "SQ%04d", sqlnum);

		return DISCONNECT;
	}

	"UPDATE" {
		BEGIN ESQL_STATE;
		flag_insqlstring = 1;

		yylval.s = com_strdup (yytext);
		memset(commandname,0,sizeof(commandname));
		com_strcpy(commandname,sizeof(commandname),yylval.s);

		sqlnum++;
     		memset(sqlname, 0, sizeof(sqlname));
     		com_sprintf(sqlname, sizeof(sqlname), "SQ%04d", sqlnum);

		return UPDATE;
	}

	"DECLARE" {
		BEGIN ESQL_STATE;

		return DECLARE;
	}

	"OPEN" {
		BEGIN ESQL_STATE;

    	    	com_strcpy(commandname,sizeof(commandname),"OPEN");
    	    	return OPEN;
        }

	"PREPARE" {
		BEGIN ESQL_STATE;

    	    	com_strcpy(commandname,sizeof(commandname),"PREPARE");
		return PREPARE;
	}

	"EXECUTE" {
		BEGIN ESQL_STATE;

		com_strcpy(commandname,sizeof(commandname),"EXECUTE");
		return EXECUTE;
	}

	"CLOSE" {
		BEGIN ESQL_STATE;

		com_strcpy(commandname,sizeof(commandname),"CLOSE");
		return CLOSE;
	}

	"FETCH" {
		BEGIN ESQL_STATE;

		com_strcpy(commandname,sizeof(commandname),"FETCH");
		return FETCH;
	}

	"COMMIT"[ ]+"WORK"+[ ]+"RELEASE" {
		BEGIN ESQL_STATE;

		com_strcpy(commandname,sizeof(commandname),"COMMIT_RELEASE");
		return COMMIT_WORK;
	}

	"COMMIT"[ ]+"WORK"+[ ]+"WITH"+[ ]+"RELEASE" {
		BEGIN ESQL_STATE;

		com_strcpy(commandname,sizeof(commandname),"COMMIT_RELEASE");
		return COMMIT_WORK;
	}

	"COMMIT"[ ]+"WORK" {
		BEGIN ESQL_STATE;

		com_strcpy(commandname,sizeof(commandname),"COMMIT");
		return COMMIT_WORK;
	}

	"COMMIT" {
		BEGIN ESQL_STATE;

		com_strcpy(commandname,sizeof(commandname),"COMMIT");
		return COMMIT_WORK;
	}

	"ROLLBACK"[ ]+"WORK"+[ ]+"RELEASE" {
		BEGIN ESQL_STATE;

		com_strcpy(commandname,sizeof(commandname),"ROLLBACK_RELEASE");
		return ROLLBACK_WORK;
	}

	"ROLLBACK"[ ]+"WORK"+[ ]+"WITH"+[ ]+"RELEASE" {
		BEGIN ESQL_STATE;

		com_strcpy(commandname,sizeof(commandname),"ROLLBACK_RELEASE");
		return ROLLBACK_WORK;
	}

	"ROLLBACK"[ ]+"WORK" {
		BEGIN ESQL_STATE;

		com_strcpy(commandname,sizeof(commandname),"ROLLBACK");
		return ROLLBACK_WORK;
	}

	"ROLLBACK" {
		BEGIN ESQL_STATE;

		com_strcpy(commandname,sizeof(commandname),"ROLLBACK");
		return ROLLBACK_WORK;
	}

	"AT" {
		if(flag_insqlstring || conn_use_other_db){
			yylval.s = com_strdup (yytext);
			return TOKEN;
		}

		yy_push_state(ESQL_DBNAME_STATE);
		conn_use_other_db = 1;
		return AT;
	}

	({WORD}|{JPNWORD})+ {
		BEGIN ESQL_STATE;
		yylval.s = com_strdup (yytext);
		memset(commandname,0,sizeof(commandname));
		com_strcpy(commandname,sizeof(commandname),yylval.s);

		sqlnum++;
     		memset(sqlname, 0, sizeof(sqlname));
     		com_sprintf(sqlname, sizeof(sqlname), "SQ%04d", sqlnum);

		command_putother = 1;
		return OTHERFUNC;
	}
}

<ESQL_DBNAME_STATE>{
	{HOSTWORD} {
		yylval.s = com_strdup (yytext + 1);
		hostlineno = yylineno - includelinenum;

		yy_pop_state();
		return HOSTTOKEN;
	}
}

<ESQL_STATE>{
	{COMMA}   {
	          yylval.s = com_strdup (yytext);
	          return TOKEN;
	          }
	(\r\n|\n) {   }



	[;]?(\r\n|\n)		{
				ECHO;
				}
	"AT" {
			if(flag_insqlstring || conn_use_other_db){
				yylval.s = com_strdup (yytext);
				return TOKEN;
			}

			yy_push_state(ESQL_DBNAME_STATE);
                        conn_use_other_db = 1;
			return AT;
	}

	"SELECT" {
			if(flag_insqlstring){
	      			yylval.s = com_strdup (yytext);
	      			return TOKEN;
			}
			flag_insqlstring = 1;
			yylval.s = com_strdup (yytext);
			memset(commandname,0,sizeof(commandname));
			com_strcpy(commandname,sizeof(commandname),yylval.s);

			sqlnum++;
     			memset(sqlname, 0, sizeof(sqlname));
     			com_sprintf(sqlname, sizeof(sqlname), "SQ%04d", sqlnum);

			return SELECT;
	}

	"FROM" {
			if(flag_insqlstring){
	      			yylval.s = com_strdup (yytext);
				if(!flag_selectcommand){
		      			return TOKEN;
				} else {
					flag_selectcommand = 0;
		      			return SELECTFROM;
				}
			}
			return FROM;
	}

	"CURSOR" {
			if(flag_insqlstring){
	      			yylval.s = com_strdup (yytext);
	      			return TOKEN;
			}
		       return CURSOR;
	 }

	"FOR" {
			if(flag_insqlstring){
				yylval.s = com_strdup (yytext);
				return TOKEN;
			}
			return FOR;
	}

	"IDENTIFIED"[ ]+"BY" {
			if(flag_insqlstring){
	      			yylval.s = com_strdup (yytext);
	      			return TOKEN;
			}
			return IDENTIFIED_BY;
	}

	"USING" {
			if(flag_insqlstring){
	      			yylval.s = com_strdup (yytext);
	      			return TOKEN;
			}
			return USING;
	}

	"INTO" {
			if(flag_insqlstring && !flag_selectcommand){
	      			yylval.s = com_strdup (yytext);
	      			return TOKEN;
			}
		     	yylval.s = com_strdup (yytext);
		     	return INTO;
	}

	"WHERE"[ \r\n]+"CURRENT"[ \r\n]+"OF" {
			yy_push_state(WHERE_CURRENT_OF);
			return WHERECURRENTOF;
	}

	{OPERATOR} {
			yylval.s = com_strdup (yytext);
			return TOKEN;
	}

	{HOSTWORD} {
			yylval.s = com_strdup (yytext + 1);
			hostlineno = yylineno - includelinenum;

			return HOSTTOKEN;
	}

	"END-EXEC"[ \r\n]*"." {
			flag_insqlstring = 0;
			flag_selectcommand = 0;
			period = 1;
			endlineno = yylineno;
			BEGIN INITIAL;
			return END_EXEC;
	}

	"END-EXEC" {
			flag_insqlstring = 0;
			flag_selectcommand = 0;
			endlineno = yylineno;
			BEGIN INITIAL;
			return END_EXEC;
	}

	"'"({WORD}|{JPNWORD}|{SELF}|[ ])+"'" {
			yylval.s = com_strdup (yytext);
			return TOKEN;
	}

	({WORD}|{JPNWORD})+("."("*"|({WORD}|{JPNWORD})+))? {
			yylval.s = com_strdup (yytext);
			return TOKEN;
	}

	{SELF} {
			yylval.s = com_strdup (yytext);
			return TOKEN;
	}
}

<WHERE_CURRENT_OF>{
	(\r\n|\n) {   }

	({WORD}|{JPNWORD})+ {
			yylval.s = com_strdup (yytext);
			yy_pop_state();
			return CURNAME;
	}
}

"EXEC"[ ]+"SQL"[ ]+"INCLUDE" {
        period = 0;
	startlineno = yylineno;
	host_reference_list = NULL;
	res_host_reference_list = NULL;
	memset(dbname,0,sizeof(dbname));
	memset(prepname,0,sizeof(prepname));
	memset(cursorname,0,sizeof(cursorname));
	memset(sqlname,0,sizeof(sqlname));
	memset(incfilename,0,sizeof(incfilename));
	hostreferenceCount = 0;
	conn_use_other_db = 0;
	command_putother = 0;
	sql_list = NULL;
	var_varying = NULL;            
	yy_push_state(ESQL_INCLUDE_STATE);
 	return EXECSQL_INCLUDE;
}
<ESQL_INCLUDE_STATE>{
	(\r\n|\n) {   }
	"SQLCA" {
		memset(commandname,0,sizeof(commandname));
		com_strcpy(commandname,sizeof(commandname),"INCLUDE");
    		return INCLUDE_SQLCA;
	}
	"END-EXEC" {
		endlineno = yylineno;
		yy_pop_state();
		return END_EXEC;
	}
	{INCFILE} {
		memset(commandname,0,sizeof(commandname));
		com_strcpy(commandname,sizeof(commandname),"INCFILE");
		yylval.s = com_strdup (yytext);
		com_strcpy(incfilename,sizeof(incfilename),yylval.s);
	      	return INCLUDE_FILE;
	}
	"END-EXEC"[ \r\n]*"." {
		period = 1;
		endlineno = yylineno;
		yy_pop_state();
		return END_EXEC;
	}
}

"WORKING-STORAGE"[ ]+"SECTION"[ ]*"." {
        BEGIN WORKING_STATE;
        startlineno = yylineno;
        endlineno = yylineno;
	host_reference_list = NULL;
	res_host_reference_list = NULL;
	memset(dbname,0,sizeof(dbname));
	memset(prepname,0,sizeof(prepname));
	memset(commandname,0,sizeof(commandname));
	com_strcpy(commandname,sizeof(commandname),"WORKING_BEGIN");
	memset(cursorname,0,sizeof(cursorname));
        memset(sqlname,0,sizeof(sqlname));
	memset(incfilename,0,sizeof(incfilename));
        hostreferenceCount = 0;
	conn_use_other_db = 0;
	command_putother = 0;
        sql_list = NULL;
	var_varying = NULL;            

        return WORKINGBEGIN;
}
<WORKING_STATE>{

      "EXEC"[ ]+"SQL"[ ]+"BEGIN"[ ]+"DECLARE"[ ]+"SECTION"[ ]+"END-EXEC"[ ]*"." {
        startlineno = yylineno;
        endlineno = yylineno;
	host_reference_list = NULL;
	res_host_reference_list = NULL;
	memset(dbname,0,sizeof(dbname));
	memset(prepname,0,sizeof(prepname));
	memset(commandname,0,sizeof(commandname));
	com_strcpy(commandname,sizeof(commandname),"HOST_BEGIN");
	memset(cursorname,0,sizeof(cursorname));
        memset(sqlname,0,sizeof(sqlname));
	memset(incfilename,0,sizeof(incfilename));
        hostreferenceCount = 0;
	conn_use_other_db = 0;
	command_putother = 0;
        sql_list = NULL;
	var_varying = NULL;            

        return HOSTVARIANTBEGIN;
      }
      "EXEC"[ ]+"SQL"[ ]+"END"[ ]+"DECLARE"[ ]+"SECTION"[ ]+"END-EXEC"[ ]*"." {
        startlineno = yylineno;
        endlineno = yylineno;
	host_reference_list = NULL;
	res_host_reference_list = NULL;
	memset(dbname,0,sizeof(dbname));
	memset(prepname,0,sizeof(prepname));
	memset(commandname,0,sizeof(commandname));
	com_strcpy(commandname,sizeof(commandname),"HOST_END");
	memset(cursorname,0,sizeof(cursorname));
        memset(sqlname,0,sizeof(sqlname));
	memset(incfilename,0,sizeof(incfilename));
        hostreferenceCount = 0;
	conn_use_other_db = 0;
	command_putother = 0;
        sql_list = NULL;
	var_varying = NULL;            

        return HOSTVARIANTEND;
      }

      "COPY"[ ]+"\"".+"\""([ ]+("==".*"=="|[^\.]*))*"." {}
      "COPY"[ ]+[^\.]+([ ]+("==".*"=="|[^\.]*))*"." {}
      "INCLUDE"[ ]+"\"".+"\""([ ]+("==".*"=="|[^\.]*))*"." {}
      "INCLUDE"[ ]+[^\.]+([ ]+("==".*"=="|[^\.]*))*"." {}

      ("66"|"77"|"78"|"88")[^\.]*"." {}

      "OBJECT-STORAGE"[ ]+"SECTION"[ ]*"." |
      "LOCAL-STORAGE"[ ]+"SECTION"[ ]*"." |
      "LINKAGE"[ ]+"SECTION"[ ]*"." |
      "COMMUNICATION"[ ]+"SECTION"[ ]*"." |
      "REPORT"[ ]+"SECTION"[ ]*"." |
      "SCREEN"[ ]+"SECTION"[ ]*"." |
      "PROCEDURE"[ ]+"DIVISION"[^\.]*"." {
        startlineno = yylineno;
        endlineno = yylineno;
	host_reference_list = NULL;
	res_host_reference_list = NULL;
	memset(dbname,0,sizeof(dbname));
	memset(prepname,0,sizeof(prepname));
	memset(commandname,0,sizeof(commandname));
	com_strcpy(commandname,sizeof(commandname),"WORKING_END");
	memset(cursorname,0,sizeof(cursorname));
        memset(sqlname,0,sizeof(sqlname));
	memset(incfilename,0,sizeof(incfilename));
        hostreferenceCount = 0;
	conn_use_other_db = 0;
	command_putother = 0;
        sql_list = NULL;
	var_varying = NULL;            

        BEGIN INITIAL;
        return WORKINGEND;
      }
      "PIC" |
      "PICTURE" {
		BEGIN PICTURE_STATE;
       }

	([0-9]+)|([0-9]+\.[0-9]+) {
                yylval.ld = atol(yytext);
		return NUMERIC;
	}

        "OCCURS"  {return OCCURS;}
        "USAGE"  {return USAGE;}
        "COMP-1"  {
            return  COMP_1;
        }
        "COMP-2"  {
            return  COMP_2;
        }
        "COMP-3" {
        	return COMP_3;
        }
        "COMP-5" {
        	return COMP_5;
        }

        "SIGN"  { return SIGN ;}
         "LEADING" { return LEADING;}
         "SEPARATE" { return SEPARATE; }
         "TRAILING" { return TRAILING; }
         "EXTERNAL"  { return EXTERNAL;}
         "IS"  { return IS;}
         "ARE"  { return ARE;}
         "VALUE"|"VALUES" { return VALUE;}
         "VARYING"  {
                startlineno = yylineno;
                endlineno = yylineno;
        	host_reference_list = NULL;
        	res_host_reference_list = NULL;
        	memset(dbname,0,sizeof(dbname));
        	memset(prepname,0,sizeof(prepname));
        	memset(commandname,0,sizeof(commandname));
        	com_strcpy(commandname,sizeof(commandname),"VARYING_PARAM");
        	memset(cursorname,0,sizeof(cursorname));
                memset(sqlname,0,sizeof(sqlname));
        	memset(incfilename,0,sizeof(incfilename));
                hostreferenceCount = 0;
        	conn_use_other_db = 0;
        	command_putother = 0;
                sql_list = NULL;
		var_varying = NULL;            
        
                return VARYING;
         }
         "TIMES"  { return TIMES;}
         "ALL"  { return ALL ;}
         {STRVALUE}|{HEXVALUE} { return CONST;}
        ([A-Za-z\-0-9_]|{JPNWORD})+ {
                        yylval.s = com_strdup(yytext);
                        return WORD;
                   }
        "."   {    return '.';}
        (\r\n|\n) { }

         . {}
}


<PICTURE_STATE>{
  "IS" {
	/* ignore */
  }
  [^ \r\n;\.]+(\.[^ \r\n;\.]+)* {
	yylval.s = com_strdup(yytext);

	BEGIN WORKING_STATE;
	return PICTURE;
  }

}

(\r\n|\n) {   }

<*>(\r\n|\n) {

}


<*>[ \t]+ {

	//Ignore
}

. {}


<<EOF>> {

	yyterminate ();
}

%%

static int
yyinput(char *buff,int max_size)
{
	char *bp;
	char *comment;

	memset(buff,0,max_size);
	while(fgets(buff,max_size,yyin))
	{
		if(strlen(buff) > 7)
		{
			if(strstr(buff, INC_START_MARK) != NULL){
				includeflag = 1;
				includelinenum++;
				/* ignore line */
				com_strcpy(buff,max_size,"\n");
	    		return strlen(buff);
			}
			if(includeflag){
				includelinenum++;
			}
			if(strstr(buff, INC__END__MARK) != NULL){
				includeflag = 0;
				/* ignore line */
				com_strcpy(buff,max_size,"\n");
	    		return strlen(buff);
			}
			
			
		
			bp = buff + 7;

			switch (buff[6]) {
			case ' ':
				break;
			case '-':
				break;

			case '\r':
			case '\n':
			case '\0':
				/* ignore line */
				com_strcpy(buff,max_size,"\n");
	    			return strlen(buff);

			case '*':
				/* comment line */
				com_strcpy(buff,max_size,"\n");
	    			return strlen(buff);

			case '/':
				/* comment line */
				com_strcpy(buff,max_size,"\n");
    				return strlen(buff);

			case 'D':
				/* comment line */
				com_strcpy(buff,max_size,"\n");
    				return strlen(buff);

			case 'd':
				/* comment line */
				com_strcpy(buff,max_size,"\n");
    				return strlen(buff);

			case '$':
				/* comment line */
				com_strcpy(buff,max_size,"\n");
    				return strlen(buff);

			default:
				printmsg("EOF:%s\n", buff);
				return YY_NULL;
			}
			if(strlen(buff) > 72){
				memmove(buff, bp, 65);
				com_strcpy(buff + 65,max_size-65,"\n");
			}else{
				memmove(buff,bp,strlen(bp) + 1);
			}

			comment = strstr(buff, "*>");
                        if(comment) com_strcpy(comment, max_size,"\n");
			return strlen(buff);
	    	}
	    	com_strcpy(buff,max_size,"\n");
	    	return strlen(buff);
	}

	return 0;

}


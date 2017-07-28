/*
 * Copyright (C) 2013 Tokyo System House Co.,Ltd.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
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

#include "define.h"
#include "parser.h"

int startlineno = 0;
int endlineno = 0;
int hostlineno = 0;
int period = 0;
int command_putother = 0;
struct cb_exec_list *exec_list = NULL;
struct cb_hostreference_list *host_reference_list;
struct cb_res_hostreference_list *res_host_reference_list;
struct cb_sql_list *sql_list;

char commandname[BUFFSIZE];
char cursorname[BUFFSIZE];
char sqlname[BUFFSIZE];
int sqlnum = 0;
char incfilename[BUFFSIZE];

int flag_insqlstring = 0;
int flag_selectcommand = 0;

#define YY_INPUT(buf,result,max_size) result = yyinput (buf, max_size);
static int 
yyinput (char *buf, int max_size);

#define SET_LOCATION(x)				\
  (x)->source_file = (unsigned char *)cb_source_file;		\

%}

%s PICTURE_STATE WORKING_STATE

%x ESQL_FUNC_STATE ESQL_INCLUDE_STATE ESQL_SELECT_STATE ESQL_STATE

JPNWORD [\xA0-\xDF]|([\x81-\x9F\xE0-\xFC][\x40-\x7E\x80-\xFC])
DIGIT [0-9]
WORD ([A-Za-z\+\-0-9_]|[(]|[)]|[\'])
INCFILE [A-Za-z0-9_\+\-]+
FILENAME [A-Za-z0-9_\+\-\.]+
STRVALUE "\""[^\"]+"\""
HEXVALUE "X\""[^\"]+"\""
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
					memset(commandname,0,sizeof(commandname));
					memset(cursorname,0,sizeof(cursorname));
					memset(sqlname,0,sizeof(sqlname));	
					memset(incfilename,0,sizeof(incfilename));
					hostreferenceCount = 0;
					sql_list = NULL;
					period = 0;
					command_putother = 0;
					return EXECSQL; 
}

<ESQL_FUNC_STATE>{
	"SELECT" {
		BEGIN ESQL_STATE;
		flag_insqlstring = 1;
		flag_selectcommand = 1;

		yylval.s = strdup (yytext);
		memset(commandname,0,sizeof(commandname));
		strcpy(commandname,yylval.s);

		sqlnum++;
     		memset(sqlname, 0, sizeof(sqlname));
     		sprintf(sqlname, "SQ%04d", sqlnum);
					
		return SELECT;
	}

	"INSERT" {
		BEGIN ESQL_STATE;
		flag_insqlstring = 1;

		yylval.s = strdup (yytext);
		memset(commandname,0,sizeof(commandname));
		strcpy(commandname,yylval.s);
						
		sqlnum++;
     		memset(sqlname, 0, sizeof(sqlname));
     		sprintf(sqlname, "SQ%04d", sqlnum);
					
		return INSERT;
	}

	"DELETE" {
		BEGIN ESQL_STATE;
		flag_insqlstring = 1;

		yylval.s = strdup (yytext);
		memset(commandname,0,sizeof(commandname));
		strcpy(commandname,yylval.s);
					
		sqlnum++;
     		memset(sqlname, 0, sizeof(sqlname));
     		sprintf(sqlname, "SQ%04d", sqlnum);
					
		return DELETE;
	}		

	"CONNECT" {
		BEGIN ESQL_STATE;

		strcpy(commandname,"CONNECT");
		return CONNECT;
	}
     
	"DISCONNECT" {
		BEGIN ESQL_STATE;
		flag_insqlstring = 1;

		yylval.s = strdup (yytext);
		memset(commandname,0,sizeof(commandname));
		strcpy(commandname,yylval.s);
					
		sqlnum++;
     		memset(sqlname, 0, sizeof(sqlname));
     		sprintf(sqlname, "SQ%04d", sqlnum);
					
		return DISCONNECT;
	}

	"UPDATE" {
		BEGIN ESQL_STATE;
		flag_insqlstring = 1;

		yylval.s = strdup (yytext);
		memset(commandname,0,sizeof(commandname));
		strcpy(commandname,yylval.s);
					
		sqlnum++;
     		memset(sqlname, 0, sizeof(sqlname));
     		sprintf(sqlname, "SQ%04d", sqlnum);
					
		return UPDATE;
	}	
	
	"DECLARE" {
		BEGIN ESQL_STATE;

		return DECLARE;
	}

	"OPEN" {
		BEGIN ESQL_STATE;

    	    	strcpy(commandname,"OPEN");
    	    	return OPEN;
        }
     
	"EXECUTE" {
		BEGIN ESQL_STATE;

		strcpy(commandname,"EXECUTE");
		return EXECUTE;
	}
      
	"CLOSE" {
		BEGIN ESQL_STATE;

		strcpy(commandname,"CLOSE");
		return CLOSE;
	}  
            
	"FETCH" {
		BEGIN ESQL_STATE;

		strcpy(commandname,"FETCH");
		return FETCH;
	}
      
	"COMMIT"[ ]+"WORK"+[ ]+"RELEASE" {
		BEGIN ESQL_STATE;

		strcpy(commandname,"COMMIT_RELEASE");
		return COMMIT_WORK;
	}

	"COMMIT"[ ]+"WORK"+[ ]+"WITH"+[ ]+"RELEASE" {
		BEGIN ESQL_STATE;

		strcpy(commandname,"COMMIT_RELEASE");
		return COMMIT_WORK;
	}

	"COMMIT"[ ]+"WORK" {
		BEGIN ESQL_STATE;

		strcpy(commandname,"COMMIT");
		return COMMIT_WORK;
	}
     
	"COMMIT" {
		BEGIN ESQL_STATE;

		strcpy(commandname,"COMMIT");
		return COMMIT_WORK;
	}
     
	"ROLLBACK"[ ]+"WORK"+[ ]+"RELEASE" {
		BEGIN ESQL_STATE;

		strcpy(commandname,"ROLLBACK_RELEASE");
		return ROLLBACK_WORK;
	}

	"ROLLBACK"[ ]+"WORK"+[ ]+"WITH"+[ ]+"RELEASE" {
		BEGIN ESQL_STATE;

		strcpy(commandname,"ROLLBACK_RELEASE");
		return ROLLBACK_WORK;
	}

	"ROLLBACK"[ ]+"WORK" {
		BEGIN ESQL_STATE;

		strcpy(commandname,"ROLLBACK");
		return ROLLBACK_WORK;
	}     

	"ROLLBACK" {
		BEGIN ESQL_STATE;

		strcpy(commandname,"ROLLBACK");
		return ROLLBACK_WORK;
	}     

	({WORD}|{JPNWORD})+ {
		BEGIN ESQL_STATE;
		yylval.s = strdup (yytext);
		memset(commandname,0,sizeof(commandname));
		strcpy(commandname,yylval.s);

		sqlnum++;
     		memset(sqlname, 0, sizeof(sqlname));
     		sprintf(sqlname, "SQ%04d", sqlnum);
					
		command_putother = 1;
		return OTHERFUNC;
	}
}

<ESQL_STATE>{
	{COMMA}   {
	          yylval.s = strdup (yytext); 
	          return TOKEN;
	          }
	(\r\n|\n) {   }
         


	[;]?(\r\n|\n)		{ 
				ECHO; 
				}
	"SELECT" {
			if(flag_insqlstring){
	      			yylval.s = strdup (yytext);
	      			return TOKEN;       
			}
			flag_insqlstring = 1;
			yylval.s = strdup (yytext);
			memset(commandname,0,sizeof(commandname));
			strcpy(commandname,yylval.s);
						
			sqlnum++;
     			memset(sqlname, 0, sizeof(sqlname));
     			sprintf(sqlname, "SQ%04d", sqlnum);
					
			return SELECT;
	}
	
	"FROM" {
			if(flag_insqlstring){
	      			yylval.s = strdup (yytext);
				if(!flag_selectcommand){
		      			return TOKEN;       
				} else {
		      			return SELECTFROM;
				}
			}
			return FROM;
	}  
     
	"CURSOR" {
			if(flag_insqlstring){
	      			yylval.s = strdup (yytext);
	      			return TOKEN;       
			}
		       return CURSOR;
	 }

	"FOR" {
			if(flag_insqlstring){
				yylval.s = strdup (yytext);
				return TOKEN;       
			}
			return FOR;
	}      

	"IDENTIFIED"[ ]+"BY" {
			if(flag_insqlstring){
	      			yylval.s = strdup (yytext);
	      			return TOKEN;       
			}
			return IDENTIFIED_BY;
	}
  
	"USING" {
			if(flag_insqlstring){
	      			yylval.s = strdup (yytext);
	      			return TOKEN;       
			}
			return USING;
	} 
     
	"INTO" {
			if(flag_insqlstring && !flag_selectcommand){
	      			yylval.s = strdup (yytext);
	      			return TOKEN;       
			}
		     	yylval.s = strdup (yytext);
		     	return INTO;
	} 

	{OPERATOR} {
			yylval.s = strdup (yytext);
			return TOKEN;
	}
	
	{HOSTWORD} {
			yylval.s = strdup (yytext + 1);
			hostlineno = yylineno;
	      
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
	
	({WORD}|{JPNWORD})+("."(("*")|({WORD}|{JPNWORD})+))? {
		      yylval.s = strdup (yytext);
		      return TOKEN;       
	}

	{SELF} {
		      yylval.s = strdup (yytext);
		      return TOKEN;       
	}
}

"EXEC"[ ]+"SQL"[ ]+"INCLUDE" {
        period = 0;
	startlineno = yylineno;
	host_reference_list = NULL;
	res_host_reference_list = NULL;
	memset(cursorname,0,sizeof(cursorname));
	memset(sqlname,0,sizeof(sqlname));	
	memset(incfilename,0,sizeof(incfilename));
	hostreferenceCount = 0;
	command_putother = 0;
	sql_list = NULL;
	yy_push_state(ESQL_INCLUDE_STATE); 
 	return EXECSQL_INCLUDE;
}
<ESQL_INCLUDE_STATE>{
	(\r\n|\n) {   }
	"SQLCA" {
		memset(commandname,0,sizeof(commandname));
		strcpy(commandname,"INCLUDE");		
    		return INCLUDE_SQLCA;
	}
	{INCFILE} {
		memset(commandname,0,sizeof(commandname));
		strcpy(commandname,"INCFILE");		
		yylval.s = strdup (yytext);
		strcpy(incfilename,yylval.s);
	      	return INCLUDE_FILE;
	}
	"END-EXEC"[ \r\n]*"." {
		period = 1;
		endlineno = yylineno;
		yy_pop_state(); 
		return END_EXEC;
	}
	
	"END-EXEC" {
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
	memset(commandname,0,sizeof(commandname));
	strcpy(commandname,"WORKING_BEGIN");		
	memset(cursorname,0,sizeof(cursorname));	
        memset(sqlname,0,sizeof(sqlname));	
	memset(incfilename,0,sizeof(incfilename));
        hostreferenceCount = 0;
	command_putother = 0;
        sql_list = NULL;
        
        return WORKINGBEGIN;
}
<WORKING_STATE>{
      
      "EXEC"[ ]+"SQL"[ ]+"BEGIN"[ ]+"DECLARE"[ ]+"SECTION"[ ]+"END-EXEC"[ ]*"." {
        startlineno = yylineno;
        endlineno = yylineno;
	host_reference_list = NULL;
	res_host_reference_list = NULL;
	memset(commandname,0,sizeof(commandname));
	strcpy(commandname,"HOST_BEGIN");		
	memset(cursorname,0,sizeof(cursorname));	
        memset(sqlname,0,sizeof(sqlname));	
	memset(incfilename,0,sizeof(incfilename));
        hostreferenceCount = 0;
	command_putother = 0;
        sql_list = NULL;

        return HOSTVARIANTBEGIN;
      }
      "EXEC"[ ]+"SQL"[ ]+"END"[ ]+"DECLARE"[ ]+"SECTION"[ ]+"END-EXEC"[ ]*"." {
        startlineno = yylineno;
        endlineno = yylineno;
	host_reference_list = NULL;
	res_host_reference_list = NULL;
	memset(commandname,0,sizeof(commandname));
	strcpy(commandname,"HOST_END");		
	memset(cursorname,0,sizeof(cursorname));	
        memset(sqlname,0,sizeof(sqlname));	
	memset(incfilename,0,sizeof(incfilename));
        hostreferenceCount = 0;
	command_putother = 0;
        sql_list = NULL;

        return HOSTVARIANTEND;
      }

      "COPY"[ ]+"\"".+"\""([ ]+("==".*"=="|[^\.]*))*"." {}
      "COPY"[ ]+[^\.]+([ ]+("==".*"=="|[^\.]*))*"." {}

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
	memset(commandname,0,sizeof(commandname));
	strcpy(commandname,"WORKING_END");		
	memset(cursorname,0,sizeof(cursorname));	
        memset(sqlname,0,sizeof(sqlname));	
	memset(incfilename,0,sizeof(incfilename));
        hostreferenceCount = 0;
	command_putother = 0;
        sql_list = NULL;
   
        BEGIN INITIAL;
        return WORKINGEND;
      }
      "PIC" |
      "PICTURE" {
		BEGIN PICTURE_STATE;
       }
        "OCCURS"  {return OCCURS;}

	([0-9]+)|([0-9]+\.[0-9]+) {
		sscanf(yytext, "%ld", &yylval.ld);
		return NUMERIC;
	}

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
        
        "SIGN"  { return SIGN ;} 
         "LEADING" { return LEADING;}
         "SEPARATE" { return SEPARATE; }
         "TRAILING" { return TRAILING; }
         "EXTERNAL"  { return EXTERNAL;}
         "IS"  { return IS;}
         "ARE"  { return ARE;}
         "TIMES"  { return TIMES;}
         "VALUE"|"VALUES" { return VALUE;}
         "ALL"  { return ALL ;} 
         {STRVALUE}|{HEXVALUE} { return CONST;}
        ([A-Za-z\-0-9_]|{JPNWORD})+ {
                        yylval.s = strdup(yytext); 
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
	yylval.s = strdup(yytext);
	
	BEGIN WORKING_STATE;
	return PICTURE;
  }
  
}



<*>(\r\n|\n) {
	
}


<*>[ \t]+ {
     
	//Ignore 
}
(\r\n|\n) {   }
 
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
				strcpy(buff,"\n");
	    			return strlen(buff);

			case '*':
				/* comment line */
				strcpy(buff,"\n");
	    			return strlen(buff);

			case '/':
				/* comment line */
				strcpy(buff,"\n");
    				return strlen(buff);

			case 'D':
				/* comment line */
				strcpy(buff,"\n");
    				return strlen(buff);

			case 'd':
				/* comment line */
				strcpy(buff,"\n");
    				return strlen(buff);

			case '$':
				/* comment line */
				strcpy(buff,"\n");
    				return strlen(buff);

			default:					
	                        printf("EOF:%s\n", buff);
				return YY_NULL;
			}
			if(strlen(buff) > 72){
				memmove(buff, bp, 65);
				strcpy(buff + 65,"\n");
			}else{
				memmove(buff,bp,strlen(bp) + 1);
			}

			comment = strstr(buff, "*>");
                        if(comment) strcpy(comment, "\n");
			return strlen(buff);	
	    	}
	    	strcpy(buff,"\n");
	    	return strlen(buff);
	}
	
	return 0;

}


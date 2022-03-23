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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include "ocesqlutil.h"

#define MAXBUFFSIZE  1024
#define ERRORMSGNUM 10
static char errormsg[ERRORMSGNUM][128] = {
	{"E001: is not defined in the working-storage !"},
	{"E002: is not defined in c and cobol conversion rules!"},
	{"E011: child element can't have OCCURS items !"},
	{"E012: OCCURS item can't have multi item-layer !"},
	{"E013: variable for PREPARE should be GROUP."},
	{"E014: invalid parameter for PREPARE."},
	{"E030:'s usage option not supported."},
	{"E901: exceed limit line length(128 characters)"},
	{"E990: usage error"},
	{"E999: unexpected error"}
};
static FILE *pfile;

int openerrorfile(char *filename){
	if( filename != NULL){
		com_fopen(&pfile, filename, "a+");
		if(pfile == NULL){
			printf("errormsgshow: could not open %s.\n", filename);
		}
	}

	if(pfile == NULL){
		pfile = stdout;
	}
	return 1;
}

int closeerrorfile(){
	if(pfile != NULL && pfile != stdout){
		fclose(pfile);
	}
	return 1;
}

int spreadchar(char * code , char* msg, char *ret){
	char *p ;
	if(code  == NULL || msg == NULL || ret == NULL)
	    return 0;

	if(strlen(msg) <= strlen(code))
		return 0;

	if(memcmp(code, msg, strlen(code)) != 0)
		return 0;

	p = msg + strlen(code) + 1;

	if(p == NULL)
		return 0;

	com_strcpy(ret, sizeof(ret), p);
	return 1;
}

int geterrormsg(char *code , char *msg, int len){
	char buf[MAXBUFFSIZE];
	int i;

	if( code == NULL || msg == NULL )
	   return 0;

	 memset(msg, 0, len);

	 memset(buf, 0, sizeof(buf));
	 for(i=0; i<ERRORMSGNUM; i++){
	 	if (spreadchar(code ,errormsg[i], msg) == 1 ){
	 		return 1;
	 	}
	 	memset(buf, 0, sizeof(buf));
	 }
	 return 0;
}

int printerrormsg(char *name, int line, char * code){
	char buff[MAXBUFFSIZE];
	int ilen ;
	char *p;

	if( code == NULL || line <=0 || name == NULL)
		return 0;
	ilen = sizeof(buff);
	memset(buff,0, ilen);

	com_sprintf(buff,sizeof(buff), "%06d:%4s:%s", line, code, name);

	p = buff + strlen(buff);
	ilen -= strlen(buff)+1;

	if( geterrormsg(code,p, ilen) == 0){
		printf("printerrmsg: no error message for '%4s'\n",  code);
		return 0;
	}

	fputs(buff, pfile);
	fputs("\n", pfile);

	return 1;
}

int printmsg(char *format, ...){
	va_list list;
	va_start(list, format);
	vfprintf(pfile, format, list);
	va_end(list);
	return 1;
}

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
#include <stdlib.h>
#include "ocesql.h"
#include "ocesqlutil.h"

char inbuff[256];
char out[256];
struct cb_exec_list *head;
char *outbuff;
FILE *outfile;
int EOFflg = 0;
int EOFFLG = 0;
int lineNUM = 0;

int japflg = 0;
int charcount = 0;

char *SQcount(int i){
	char NUM[4];
	NUM[0] = '0';
	NUM[1] = '0';
	NUM[2] = '0';
	NUM[3] = 0;
	if(i<10)
		NUM[2] = i+48;
	if(i>=10&&i<=99){
		NUM[1] = (i/10)+48;
		NUM[2] = (i%10)+48;
	}
	if(i>=100&&i<=999){
		NUM[0] = (i/100)+48;
		NUM[1] = (i%100/10)+48;
		NUM[2] = (i%100%10)+48;
	}
	return com_strdup(NUM);
}

char *substring(int len,char *wk_str,int flag_end){
	char wkstr[70];
	int n = 0;
	int k;
	int dex;
	int h;
	int len2;

	for(n=0;n<70;n++){
		wkstr[n]='\0';
	}
	dex = len;

	len2 = strlen(wk_str);
	if(len >= len2){
		EOFFLG = 1;
		dex = strlen(wk_str);
	}
	h=1;
	if(!EOFFLG){
		for(h=1;h<dex;h++){
			if(!check_Dchar(wk_str[dex-h])){
				break;
			}
		}
	}
	if(h>=2){
		if(!(h%2)){
			if(flag_end==1){
				dex--;
			}else{
				dex++;
			}
		}
	}

	for(n = 0;n<dex;n++){
		wkstr[n] = wk_str[n];
	}
	for( k=n;wk_str[k]!='\0';k++){
		wk_str[k-n]=wk_str[k];
	}
	wk_str[k-n]='\0';
	return com_strdup(wkstr);
}

void sql_string(struct cb_exec_list *wk_text){
	char *intNUM;
	char sqlstr[5][256];
	char compsql[15] = "OCESQL  &  \"\".";
	char terminal[13] = "OCESQL     .";

	char *sqlloop;
	int sqlloop_len;

	struct cb_sql_list *wk_sql;
	int len;

	wk_sql = wk_text->sql_list;

	sqlloop_len = 0;
	for(;wk_sql->next!=NULL;){
		sqlloop_len += strlen(wk_sql->sqltext);
		if(strcmp(wk_sql->next->sqltext,",")!=0){
			sqlloop_len += strlen(" ");
		}
		wk_sql = wk_sql->next;
	}
	sqlloop_len += strlen(wk_sql->sqltext);

	sqlloop = (char *)malloc((sqlloop_len + 1) * sizeof(char));
	if(sqlloop == NULL){
		_printlog("memory allocation failed.\n");
		return;
	}
	memset(sqlloop, 0, sqlloop_len + 1);

	wk_sql = wk_text->sql_list;
	for(;wk_sql->next!=NULL;){
		com_strcat(sqlloop,sqlloop_len,wk_sql->sqltext);
		if(strcmp(wk_sql->next->sqltext,",")!=0){
			com_strcat(sqlloop,sqlloop_len," ");
		}
		wk_sql = wk_sql->next;
	}
	com_strcat(sqlloop, sqlloop_len+1, wk_sql->sqltext);

	for(;;){
		charcount=0;
		com_strcpy(sqlstr[0],256,"OCESQL     02  FILLER PIC X(000) VALUE \"");
		com_strcpy(sqlstr[1],256,"OCESQL  &  \"");
		com_strcpy(sqlstr[2],256,"OCESQL  &  \"");
		com_strcpy(sqlstr[3],256,"OCESQL  &  \"");
		com_strcpy(sqlstr[4],256,"OCESQL  &  \"");
		len = strlen(sqlloop);
		if(len<256){
			intNUM = SQcount(len);
			sqlstr[0][28] = intNUM[0];
			sqlstr[0][29] = intNUM[1];
			sqlstr[0][30] = intNUM[2];
		}else{
			sqlstr[0][28] = '2';
			sqlstr[0][29] = '5';
			sqlstr[0][30] = '6';
		}
		com_strcat(sqlstr[0], sizeof(sqlstr[0]), substring(30, sqlloop, 0));
		com_strcat(sqlstr[0], sizeof(sqlstr[0]), "\"");

		if((strlen(sqlstr[0]) == 72)){
			charcount++;
		}
		sqlstr[0][72]='\0';
		if(EOFFLG == 1){
			com_strcat(sqlstr[0], sizeof(sqlstr[0]), ".");
		}
		if(!EOFFLG){
			com_strcat(sqlstr[1], sizeof(sqlstr[1]), substring(58, sqlloop, 0));
			com_strcat(sqlstr[1], sizeof(sqlstr[1]), "\"");
			if ((strlen(sqlstr[1]) == 72)){
				charcount++;
			}

			sqlstr[1][72]='\0';

			if(EOFFLG == 1){
				com_strcat(sqlstr[1], sizeof(sqlstr[1]), ".");
			}
		}
		if(!EOFFLG){
			com_strcat(sqlstr[2], sizeof(sqlstr[2]), substring(58, sqlloop, 0));
			com_strcat(sqlstr[2], sizeof(sqlstr[2]), "\"");
			if ((strlen(sqlstr[2]) == 72)){
				charcount++;
			}

			sqlstr[2][72]='\0';

			if(EOFFLG == 1){
				com_strcat(sqlstr[2], sizeof(sqlstr[2]), ".");
			}
		}
		if(!EOFFLG){
			com_strcat(sqlstr[3], sizeof(sqlstr[3]), substring(58, sqlloop, 0));
			com_strcat(sqlstr[3], sizeof(sqlstr[3]), "\"");
			if ((strlen(sqlstr[3]) == 72)){
				charcount++;
			}

			sqlstr[3][72]='\0';

			if(EOFFLG == 1){
				com_strcat(sqlstr[3], sizeof(sqlstr[3]), ".");
			}
		}
		if(!EOFFLG){
			com_strcat(sqlstr[4], sizeof(sqlstr[4]), substring(52 - charcount, sqlloop, 1));
			com_strcat(sqlstr[4], sizeof(sqlstr[4]), "\"");
			sqlstr[4][72] = '\0';

			if(strlen(sqlstr[4]) != ( 65 - charcount ) && !EOFFLG){
				sqlstr[0][28] = '2';
				sqlstr[0][29] = '5';
				sqlstr[0][30] = '5';
			}

			com_strcat(sqlstr[4], sizeof(sqlstr[4]), ".");
		}

		com_strcpy(out,sizeof(out),sqlstr[0]);
		outwrite();
		if(strncmp(compsql, sqlstr[1], 14) == 0){
			com_strcpy(out,sizeof(out),terminal);
			outwrite();
		} else if(sqlstr[1][12] !='\0'){
			com_strcpy(out,sizeof(out),sqlstr[1]);
			outwrite();
		}
		if(strncmp(compsql, sqlstr[2], 14) == 0){
			com_strcpy(out,sizeof(out),terminal);
			outwrite();
		} else if(sqlstr[2][12] !='\0'){
			com_strcpy(out,sizeof(out),sqlstr[2]);
			outwrite();
		}
		if(strncmp(compsql, sqlstr[3], 14) == 0){
			com_strcpy(out,sizeof(out),terminal);
			outwrite();
		} else if(sqlstr[3][12] !='\0'){
			com_strcpy(out,sizeof(out),sqlstr[3]);
			outwrite();
		}
		if(strncmp(compsql, sqlstr[4], 14) == 0){
			com_strcpy(out,sizeof(out),terminal);
			outwrite();
		} else if(sqlstr[4][12] !='\0'){
			com_strcpy(out,sizeof(out),sqlstr[4]);
			outwrite();
		}
		if(EOFFLG == 1){
			EOFFLG = 0;
			com_strcpy(out,sizeof(out),"OCESQL     02  FILLER PIC X(1) VALUE X\"00\".");
			outwrite();
			break;
		}
	}
	free(sqlloop);
}

void outsqlfiller(struct cb_exec_list *wk_head_p){
	int i;
	i = 0;
	com_strcpy(out,sizeof(out),"OCESQL*");
	outwrite();
	for(;wk_head_p;wk_head_p = wk_head_p->next){

		if (wk_head_p->sql_list){

			i++;
			com_strcpy(out,sizeof(out),"OCESQL 01  ");

			com_strcat(out,sizeof(out),wk_head_p->sqlName);
			com_strcat(out,sizeof(out),".");
			outwrite();

			sql_string(wk_head_p);

			com_strcpy(out,sizeof(out),"OCESQL*");
			outwrite();
		}

	}

	return;
}


void ppoutputendcall(struct cb_exec_list *list){
	char buff[256];
	if( list == NULL)
		return ;

	memset(buff, 0, sizeof(buff));
	if( list->period)
		com_sprintf(buff,sizeof(buff), "OCESQL%5sEND-CALL.\n", " ");
	else
		com_sprintf(buff,sizeof(buff), "OCESQL%5sEND-CALL\n", " ");
	fputs(buff, outfile);
	return ;

}

void ppoutputopen(struct cb_exec_list *list){
	struct cb_hostreference_list *wk_host;
	struct cb_exec_list *l;
	char str_type[BUFFSIZE];

	l = list;

	if(l->hostreferenceCount != 0){
		com_strcpy(out,sizeof(out),"OCESQL ");
		com_strcat(out,sizeof(out),"   ");
		com_strcat(out,sizeof(out),strcall);
		com_strcat(out,sizeof(out),"\"OCESQLStartSQL\"");
		outwrite();

		com_strcpy(out,sizeof(out),"OCESQL ");
		com_strcat(out,sizeof(out),"   ");
		com_strcat(out,sizeof(out),strend);
		outwrite();
		wk_host = l->host_list;
		int count = 0;
		for(; wk_host ; wk_host = wk_host->next){
			count += ppoutputparam(wk_host,0);
		}

		com_strcpy(out,sizeof(out),"OCESQL ");
		com_strcat(out,sizeof(out),"   ");
		com_strcat(out,sizeof(out),strcall);
		com_strcat(out,sizeof(out),"\"OCESQLCursorOpenParams\"");
		com_strcat(out,sizeof(out),strusing);
		outwrite();
		_printlog("Generate:OCESQLCursorOpenParams");

		com_strcpy(out,sizeof(out),"OCESQL ");
		com_strcat(out,sizeof(out),"       ");
		com_strcat(out,sizeof(out),strsqlca);
		outwrite();

		if( list->cursorName == NULL)
			return ;
		com_strcpy(out,sizeof(out),"OCESQL ");
		com_strcat(out,sizeof(out),"       ");
		com_strcat(out,sizeof(out),strreference);
		com_strcat(out,sizeof(out),"\"");
		com_strcat(out,sizeof(out),list->cursorName);
		com_strcat(out,sizeof(out),"\"");
		com_strcat(out,sizeof(out)," & x\"00\"");
		outwrite();

		com_strcpy(out,sizeof(out),"OCESQL ");
		com_strcat(out,sizeof(out),"       ");
		com_strcat(out,sizeof(out),strbyvalue);
		com_sprintf(str_type,sizeof(str_type),"%d",l->hostreferenceCount);
		com_strcat(out,sizeof(out),str_type);
		outwrite();

		com_strcpy(out,sizeof(out),"OCESQL ");
		com_strcat(out,sizeof(out),"   ");
		com_strcat(out,sizeof(out),strend);
		outwrite();

		com_strcpy(out,sizeof(out),"OCESQL ");
		com_strcat(out,sizeof(out),"   ");
		com_strcat(out,sizeof(out),strcall);
		com_strcat(out,sizeof(out),"\"OCESQLEndSQL\"");
		outwrite();
	} else {
		com_strcpy(out,sizeof(out),"OCESQL ");
		com_strcat(out,sizeof(out),"   ");
		com_strcat(out,sizeof(out),strcall);
		com_strcat(out,sizeof(out),"\"OCESQLCursorOpen\"");
		com_strcat(out,sizeof(out),strusing);
		outwrite();
		_printlog("Generate:OCESQLCursorOpen");

		com_strcpy(out,sizeof(out),"OCESQL ");
		com_strcat(out,sizeof(out),"       ");
		com_strcat(out,sizeof(out),strsqlca);
		outwrite();

		if( list->cursorName == NULL)
			return ;
		com_strcpy(out,sizeof(out),"OCESQL ");
		com_strcat(out,sizeof(out),"       ");
		com_strcat(out,sizeof(out),strreference);
		com_strcat(out,sizeof(out),"\"");
		com_strcat(out,sizeof(out),list->cursorName);
		com_strcat(out,sizeof(out),"\"");
		com_strcat(out,sizeof(out)," & x\"00\"");
		outwrite();
	}

	ppoutputendcall(list);

	return ;

}

void ppoutputconnect(struct cb_exec_list *list){
	char buff[256];
	struct cb_hostreference_list *host_list;
	int l,m,n;
	int iret;
	int list_count = 0;

	host_list = list->host_list;
	while(host_list)
	{
		list_count++;
		host_list = host_list->next;
	}
	if(list_count == 0){
		if(list->conn_use_other_db){
			memset(buff, 0, sizeof(buff));
			com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLIDConnectShort\" USING\n"," ");
			fputs(buff, outfile);
			_printlog("Generate:OCESQLIDConnectShort");
		} else {
			memset(buff, 0, sizeof(buff));
			com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLConnectShort\" USING\n"," ");
			fputs(buff, outfile);
			_printlog("Generate:OCESQLConnectShort");
		}
	}else if(list_count == 1){
		if(list->conn_use_other_db){
			memset(buff, 0, sizeof(buff));
			com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLIDConnectInformal\" USING\n"," ");
			fputs(buff, outfile);
			_printlog("Generate:OCESQLIDConnectInformal");
		} else {
			memset(buff, 0, sizeof(buff));
			com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLConnectInformal\" USING\n"," ");
			fputs(buff, outfile);
			_printlog("Generate:OCESQLConnectInformal");
		}
	} else {
		if(list->conn_use_other_db){
			memset(buff, 0, sizeof(buff));
			com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLIDConnect\" USING\n"," ");
			fputs(buff, outfile);
			_printlog("Generate:OCESQLIDConnect");
		} else {
			memset(buff, 0, sizeof(buff));
			com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLConnect\" USING\n"," ");
			fputs(buff, outfile);
			_printlog("Generate:OCESQLConnect");
		}
	}

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE SQLCA\n"," ");
	fputs(buff, outfile);

	if(list->conn_use_other_db){
		iret = gethostvarianttype(list->dbName,&l,&m,&n);
		if(iret != 0){
			memset(buff, 0, sizeof(buff));
			com_sprintf(buff,sizeof(buff), "E%03d",iret);
			printerrormsg(list->dbName, lineNUM, buff);
			return;
		}
		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE %s\n"," ", list->dbName);
		fputs(buff, outfile);
		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n"," ", m);
		fputs(buff, outfile);
	}

	host_list = list->host_list;
	while(host_list)
	{
		iret = gethostvarianttype( host_list->hostreference,&l,&m,&n);
		if(iret!= 0)
		{
			memset(buff, 0, sizeof(buff));
			com_sprintf(buff,sizeof(buff), "E%03d",iret);
			printerrormsg(host_list->hostreference, host_list->lineno, buff);
			return;
		}
		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE %s\n"," ",host_list->hostreference);
		fputs(buff, outfile);
		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n"," ",m);
		fputs(buff, outfile);
		host_list = host_list->next;
	}
	ppoutputendcall(list);
	return ;

}

void _ppoutputparam(char *varface, int type, int digits, int scale, int iteration){
	char buff[256];

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLSetSQLParams\" USING\n" ," ");
	fputs(buff, outfile);

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n"," ", type);
	fputs(buff, outfile);

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n"," ",digits );
	fputs(buff, outfile);

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n"," ",scale );
	fputs(buff, outfile);
	memset(buff, 0, sizeof(buff));
	if(iteration>0){
		com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE %s(1)\n"," ",varface);
	}else{
		com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE %s\n"," ",varface);
	}
	fputs(buff, outfile);

	memset(buff, 0, sizeof(buff));

	com_sprintf(buff,sizeof(buff), "OCESQL%5sEND-CALL\n", " ");
	fputs(buff, outfile);
	return ;
}

int ppoutputparam(struct cb_hostreference_list *host_list, int iteration){
	char buff[256];
	int type, digits, scale ;
	int iret;
	int count = 0;

	if( host_list == NULL )
		return count;
	iret = gethostvarianttype( host_list->hostreference,&type,&digits,&scale);
	if(iret  != 0)
	{
		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "E%03d",iret);
		printerrormsg(host_list->hostreference, host_list->lineno, buff);
		return 0;
	}

	if(type == HVARTYPE_GROUP){
		struct cb_field *f;

		f = getfieldbyname(host_list->hostreference);
		if(f == NULL){
			printmsg("%s:%d\n", host_list->hostreference, ERR_NOTDEF_WORKING);
			memset(buff, 0, sizeof(buff));
			com_sprintf(buff,sizeof(buff), "E%03d",ERR_NOTDEF_WORKING);
			printerrormsg(host_list->hostreference, host_list->lineno,
					buff);
			return count;
		}

		f = f->children;

		while(f){
			iret = gethostvarianttype(f->sname,&type,&digits,&scale);
			if(iret  != 0)
			{
				memset(buff, 0, sizeof(buff));
				com_sprintf(buff,sizeof(buff), "E%03d",iret);
				printerrormsg(f->sname, host_list->lineno, buff);
				return count;
			}
			_ppoutputparam(f->sname, type, digits, scale, iteration);
			count++;
			f = f->sister;
		}
	}else{
		_ppoutputparam(host_list->hostreference, type, digits, scale, iteration);
		count++;
	}

	return count;
}

void ppoutputresparam(char *varface, int type, int digits, int scale, int iteration){
	char buff[256];

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLSetResultParams\" USING\n" ," ");
	fputs(buff, outfile);

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n"," ", type);
	fputs(buff, outfile);

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n"," ",digits);
	fputs(buff, outfile);

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n"," ",scale);
	fputs(buff, outfile);
	memset(buff, 0, sizeof(buff));
	if(iteration > 0){
		com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE %s(1)\n"," ",varface);
	}else{
		com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE %s\n"," ",varface);
	}
	fputs(buff, outfile);

	memset(buff, 0, sizeof(buff));

	com_sprintf(buff,sizeof(buff), "OCESQL%5sEND-CALL\n", " ");
	fputs(buff, outfile);
	return ;
}

void ppoutputresgroup(struct cb_field *cf, int lineno, int iteration){
	char buff[256];
	int type, digits, scale;
	int iret;

	if(cf == NULL)
		return;

	iret = gethostvarianttype(cf->sname, &type, &digits, &scale);
	if(iret != 0){
		printmsg("%s:%d\n", cf->sname, iret);
		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "E%03d",iret);
		printerrormsg(cf->sname, lineno, buff);
		return;
	}

	if(type == HVARTYPE_GROUP){
		ppoutputresgroup(cf->children, lineno, iteration);
	} else {
		ppoutputresparam(cf->sname, type, digits, scale, iteration);
	}

	if(cf->sister != NULL){
		ppoutputresgroup(cf->sister, lineno, iteration);
	}

	return ;
}

void ppoutputexecprepare(struct cb_exec_list *list){
	char buff[256];
	struct cb_hostreference_list *host_list;
	int type, digits, scale;
	int iret;
	char str_type[BUFFSIZE];

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLStartSQL\"\nOCESQL%5sEND-CALL\n"," "," ");
	fputs(buff, outfile);

	host_list = list->host_list;
	int count = 0;
	if(host_list){
		iret = gethostvarianttype(host_list->hostreference, &type, &digits, &scale);
		if(iret != 0){
			printmsg("%s:%d\n", host_list->hostreference,iret);
			memset(buff, 0, sizeof(buff));
			com_sprintf(buff,sizeof(buff), "E%03d",iret);
			printerrormsg(host_list->hostreference, host_list->lineno,
					buff);
			return;
		}

		while(host_list){
			count += ppoutputparam(host_list,0);
			host_list = host_list->next;
		}
	}
	if(!list->conn_use_other_db){
		com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLExecPrepare\" USING\n" ," ");
		fputs(buff, outfile);
		_printlog("Generate:OCESQLExecPrepare");
	} else {
		com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLIDExecPrepare\" USING\n" ," ");
		fputs(buff, outfile);
		_printlog("Generate:OCESQLIDExecPrepare");
	}

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE SQLCA\n"," ");
	fputs(buff, outfile);

	if(list->conn_use_other_db){
		int iret;
		int var_type;
		int var_len;
		int var_scale;

		iret = gethostvarianttype(list->dbName,&var_type, &var_len, &var_scale);
		if(iret != 0)
		{
			memset(buff, 0, sizeof(buff));
			com_sprintf(buff,sizeof(buff), "E%03d",iret);
			printerrormsg(list->dbName, lineNUM, buff);
			return;
		}

		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE %s\n"," ", list->dbName);
		fputs(buff, outfile);
		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n"," ", var_len);
		fputs(buff, outfile);
	}

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE \"%s\" & x\"00\"\n"," ",list->prepareName);
	fputs(buff, outfile);

	com_strcpy(out,sizeof(out),"OCESQL ");
	com_strcat(out,sizeof(out),"       ");
	com_strcat(out,sizeof(out),strbyvalue);
	com_sprintf(str_type,sizeof(str_type),"%d",count);
	com_strcat(out,sizeof(out),str_type);
	outwrite();

	com_strcpy(out,sizeof(out),"OCESQL ");
	com_strcat(out,sizeof(out),"   ");
	com_strcat(out,sizeof(out),strend);
	outwrite();

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLEndSQL\"\n"," ");
	fputs(buff, outfile);

	ppoutputendcall(list);

}

void ppoutputfetch(struct cb_exec_list *list){
	char buff[256];
	struct cb_res_hostreference_list *res_host_list;
	int type, digits, scale;
	int iret;

 	int occurs_is_parent = 0;
	int length = 0;
	int iteration = 0;

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLStartSQL\"\nOCESQL%5sEND-CALL\n"," "," ");
	fputs(buff, outfile);

	res_host_list = list->res_host_list;

	iret = gethostvarianttype(res_host_list->hostreference, &type, &digits, &scale);
	if(iret != 0){
		printmsg("%s:%d\n", res_host_list->hostreference,iret);
		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "E%03d",iret);
		printerrormsg(res_host_list->hostreference, res_host_list->lineno,
					  buff);
		return;
	}

	if(type == HVARTYPE_GROUP){
		struct cb_field *parent, *child;

		parent = getfieldbyname(res_host_list->hostreference);
		if(parent == NULL){
			printmsg("%s:%d\n", res_host_list->hostreference, ERR_NOTDEF_WORKING);
			memset(buff, 0, sizeof(buff));
			com_sprintf(buff,sizeof(buff), "E%03d",ERR_NOTDEF_WORKING);
			printerrormsg(res_host_list->hostreference, res_host_list->lineno,
						  buff);
			return;
		}

		child = parent->children;
		if(parent->occurs){
			iteration = parent->occurs;
			occurs_is_parent = 1;
			ppoutputresgroup(child, res_host_list->lineno, iteration);
			iret = get_host_group_length(child, &length);
			if(iret != 0){
				memset(buff, 0, sizeof(buff));
				com_sprintf(buff,sizeof(buff), "E%03d",iret);
				printerrormsg(res_host_list->hostreference, res_host_list->lineno, buff);
				return;
			}
		} else {
			iteration = -1;
			occurs_is_parent = 0;

			iret = get_host_group_table_info(child, &iteration, &length);
			ppoutputresgroup(child, res_host_list->lineno, iteration);
			if(iret != 0){
				memset(buff, 0, sizeof(buff));
				com_sprintf(buff,sizeof(buff), "E%03d",iret);
				printerrormsg(res_host_list->hostreference, res_host_list->lineno, buff);
				return;
			}
		}
	} else {
		while(res_host_list)
		{
			iret = gethostvarianttype(res_host_list->hostreference, &type, &digits, &scale);
			if(iret != 0){
				memset(buff, 0, sizeof(buff));
				com_sprintf(buff,sizeof(buff), "E%03d",iret);
				printerrormsg(res_host_list->hostreference, res_host_list->lineno,
							  buff);
				return;
			}
			ppoutputresparam(res_host_list->hostreference, type, digits, scale,iteration);
			res_host_list = res_host_list->next;
		}
	}

	if(iteration){
		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLSetHostTable\" USING\n" ," ");
		fputs(buff, outfile);

		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n"," ", iteration);
		fputs(buff, outfile);

		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n"," ", length);
		fputs(buff, outfile);

		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n"," ", occurs_is_parent);
		fputs(buff, outfile);

		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%5sEND-CALL\n", " ");
		fputs(buff, outfile);

		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLCursorFetchOccurs\" USING\n" ," ");
		fputs(buff, outfile);
		_printlog("Generate:OCESQLCursorFetchOccurs");
	} else {
		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLCursorFetchOne\" USING\n" ," ");
		fputs(buff, outfile);
		_printlog("Generate:OCESQLCursorFetchOne");
	}

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE SQLCA\n"," ");
	fputs(buff, outfile);

	if( list->cursorName == NULL )
		return ;

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE \"%s\" & x\"00\"\n"," ",list->cursorName);
	fputs(buff, outfile);

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%5sEND-CALL\n", " ");
	fputs(buff, outfile);

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLEndSQL\"\n"," ");
	fputs(buff, outfile);

	ppoutputendcall(list);

}

void ppoutputcommit(struct cb_exec_list *list){
	char buff[256];

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLStartSQL\"\n"," ");
	fputs(buff, outfile);

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%5sEND-CALL\n"," ");
	fputs(buff, outfile);

	memset(buff, 0, sizeof(buff));
	if(!list->conn_use_other_db){
		com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLExec\" USING\n", " ");
		_printlog("Generate:COMMIT");
	} else {
		com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLIDExec\" USING\n", " ");
		_printlog("Generate:COMMITuseDBNAME");
	}
	fputs(buff, outfile);

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE SQLCA\n"," ");
	fputs(buff, outfile);

	if(list->conn_use_other_db){
		int iret;
		int var_type;
		int var_len;
		int var_scale;

		iret = gethostvarianttype(list->dbName,&var_type, &var_len, &var_scale);
		if(iret != 0)
		{
			memset(buff, 0, sizeof(buff));
			com_sprintf(buff,sizeof(buff), "E%03d",iret);
			printerrormsg(list->dbName, lineNUM, buff);
			return;
		}

		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE %s\n"," ", list->dbName);
		fputs(buff, outfile);
		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n"," ", var_len);
		fputs(buff, outfile);
	}

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE \"COMMIT\" & x\"00\"\n"," ");
	fputs(buff, outfile);

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%5sEND-CALL\n"," ");
	fputs(buff, outfile);

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLEndSQL\"\n"," ");
	fputs(buff, outfile);

	ppoutputendcall(list);
	return ;
}


void ppoutputrollback(struct cb_exec_list *list){
	char buff[256];

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLStartSQL\"\n"," ");
	fputs(buff, outfile);

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%5sEND-CALL\n"," ");
	fputs(buff, outfile);

	memset(buff, 0, sizeof(buff));
	if(!list->conn_use_other_db){
		com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLExec\" USING\n", " ");
		_printlog("Generate:ROLLBACK");
	} else {
		com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLIDExec\" USING\n", " ");
		_printlog("Generate:ROLLBACKuseDBNAME");
	}
	fputs(buff, outfile);

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE SQLCA\n"," ");
	fputs(buff, outfile);

	if(list->conn_use_other_db){
		int iret;
		int var_type;
		int var_len;
		int var_scale;

		iret = gethostvarianttype(list->dbName,&var_type, &var_len, &var_scale);
		if(iret != 0)
		{
			memset(buff, 0, sizeof(buff));
			com_sprintf(buff,sizeof(buff), "E%03d",iret);
			printerrormsg(list->dbName, lineNUM, buff);
			return;
		}

		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE %s\n"," ", list->dbName);
		fputs(buff, outfile);
		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n"," ", var_len);
		fputs(buff, outfile);
	}

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE \"ROLLBACK\" & x\"00\"\n"," ");
	fputs(buff, outfile);

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%5sEND-CALL\n"," ");
	fputs(buff, outfile);

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLEndSQL\"\n"," ");
	fputs(buff, outfile);

	ppoutputendcall(list);

}

void ppoutputprepare(struct cb_exec_list *list){
	char buff[256];
	int l,m,n;
	int iret;
	struct cb_field *parent, *child;
	char *comp_varname;
	int comp_varname_len;

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLPrepare\" USING\n", " ");
	fputs(buff, outfile);
	_printlog("Generate:OCESQLPrepare");

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE SQLCA\n"," ");
	fputs(buff, outfile);

	if( list->cursorName == NULL)
		return ;
	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE \"%s\" & x\"00\"\n"," ",list->prepareName);
	fputs(buff, outfile);

	iret = gethostvarianttype(list->host_list->hostreference,&l,&m,&n);
	if(iret!= 0)
	{
		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "E%03d",iret);
		printerrormsg(list->host_list->hostreference, list->host_list->lineno, buff);
		return;
	} else if(l != HVARTYPE_GROUP){
		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "E%03d",ERR_PREPARE_ISNT_GROUP);
		printerrormsg(list->host_list->hostreference, list->host_list->lineno, buff);
		return;
	}

	parent = getfieldbyname(list->host_list->hostreference);
	if(parent == NULL){
	     printmsg("%s:%d\n", list->host_list->hostreference, ERR_NOTDEF_WORKING);
	     memset(buff, 0, sizeof(buff));
	     com_sprintf(buff,sizeof(buff), "E%03d",ERR_NOTDEF_WORKING);
	     printerrormsg(list->host_list->hostreference, list->host_list->lineno,
			   buff);
	     return;
	}

	child = parent->children;
	comp_varname_len = strlen(parent->sname) + 4 + TERMINAL_LENGTH;
	comp_varname = (char *)malloc(comp_varname_len * sizeof(char));
	if(comp_varname == NULL){
	     return;
	}
	memset(comp_varname, 0, comp_varname_len);

	com_sprintf(comp_varname, comp_varname_len, "%s-LEN", parent->sname);
	if(strcmp(comp_varname, child->sname) != 0 ||
	   child->sister == NULL){
	     memset(buff, 0, sizeof(buff));
	     com_sprintf(buff,sizeof(buff), "E%03d",ERR_PREPARE_INVALID_PARAM);
	     printerrormsg(list->host_list->hostreference, list->host_list->lineno,
			   buff);
	     free(comp_varname);
	     return;
	}

	memset(comp_varname, 0, comp_varname_len);
	com_sprintf(comp_varname, comp_varname_len, "%s-ARR", parent->sname);
	if(strcmp(comp_varname, child->sister->sname) != 0){
	     memset(buff, 0, sizeof(buff));
	     com_sprintf(buff,sizeof(buff), "E%03d",ERR_PREPARE_INVALID_PARAM);
	     printerrormsg(list->host_list->hostreference, list->host_list->lineno,
			   buff);
	     free(comp_varname);
	     return;
	}
	free(comp_varname);

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE %s\n"," ",child->sister->sname);
	fputs(buff, outfile);
	com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %s\n"," ",child->sname);
	fputs(buff, outfile);

	ppoutputendcall(list);
}

void ppoutputdisconnect(struct cb_exec_list *list){
	char buff[256];

	memset(buff, 0, sizeof(buff));
	if(!list->conn_use_other_db){
		com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLDisconnect\" USING\n", " ");
		_printlog("Generate:OCESQLDisconnect");
	} else {
		com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLIDDisconnect\" USING\n", " ");
		_printlog("Generate:OCESQLDisconnect");
	}
	fputs(buff, outfile);

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE SQLCA\n"," ");
	fputs(buff, outfile);

	if(list->conn_use_other_db){
		int iret;
		int var_type;
		int var_len;
		int var_scale;

		iret = gethostvarianttype(list->dbName,&var_type, &var_len, &var_scale);
		if(iret != 0)
		{
			memset(buff, 0, sizeof(buff));
			com_sprintf(buff,sizeof(buff), "E%03d",iret);
			printerrormsg(list->dbName, lineNUM, buff);
			return;
		}

		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE %s\n"," ", list->dbName);
		fputs(buff, outfile);
		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n"," ", var_len);
		fputs(buff, outfile);
	}

	ppoutputendcall(list);
}


void ppoutputother(struct cb_exec_list *list){
	char buff[256];
	struct cb_hostreference_list *host_list;

	if( list->hostreferenceCount ==  0)
	{
		memset(buff, 0, sizeof(buff));
		if(!list->conn_use_other_db){
			com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLExec\" USING\n", " ");
			_printlog("Generate:OCESQLExec");
		} else {
			com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLIDExec\" USING\n", " ");
			_printlog("Generate:OCESQLIDExec");
		}
		fputs(buff, outfile);

		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE SQLCA\n"," ");
		fputs(buff, outfile);

		if(list->conn_use_other_db){
			int iret;
			int var_type;
			int var_len;
			int var_scale;

			iret = gethostvarianttype(list->dbName,&var_type, &var_len, &var_scale);
			if(iret != 0)
			{
				memset(buff, 0, sizeof(buff));
				com_sprintf(buff,sizeof(buff), "E%03d",iret);
				printerrormsg(list->dbName, lineNUM, buff);
				return;
			}

			memset(buff, 0, sizeof(buff));
			com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE %s\n"," ", list->dbName);
			fputs(buff, outfile);
			memset(buff, 0, sizeof(buff));
			com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n"," ", var_len);
			fputs(buff, outfile);
		}

		if( list->sqlName == NULL )
			return ;
		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE %s\n"," ", list->sqlName);
		fputs(buff, outfile);

		ppoutputendcall(list);
		return ;
	}

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLStartSQL\"\nOCESQL%5sEND-CALL\n"," "," ");
	fputs(buff, outfile);

	int length = 0;
	int iteration = 0;
	int occurs_is_parent = 0;

	if((com_stricmp(list->commandName,"INSERT")==0) ||
			(com_stricmp(list->commandName,"DELETE")==0) ||
			(com_stricmp(list->commandName,"UPDATE")==0)){
		struct cb_hostreference_list *res_host_list;
		int iret;

		res_host_list = list->host_list;
		struct cb_field *parent, *child, *f;
		f = getfieldbyname(res_host_list->hostreference);
		if (f == NULL){
			goto exit_occurs_check;
		}

		parent = f->parent;
		if(parent == NULL){
			goto exit_occurs_check;
		}

		child = parent->children;
		if(parent->occurs){
			iteration = parent->occurs;
			occurs_is_parent = 1;

			iret = get_host_group_length(child, &length);
			if(iret != 0){
				goto exit_occurs_check;
			}
		} else {
			iteration = -1;
			occurs_is_parent = 0;

			iret = get_host_group_table_info(child, &iteration, &length);
			if(iret != 0){
				goto exit_occurs_check;
			}
		}
	}
exit_occurs_check:
	host_list = list->host_list;
	int count = 0;
	while( host_list)
	{
		count += ppoutputparam(host_list,iteration);
		host_list = host_list->next;
	}

	memset(buff, 0, sizeof(buff));
	if(iteration>0){
		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLSetHostTable\" USING\n" ," ");
		fputs(buff, outfile);

		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n"," ", iteration);
		fputs(buff, outfile);

		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n"," ", length);
		fputs(buff, outfile);

		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n"," ", occurs_is_parent);
		fputs(buff, outfile);

		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%5sEND-CALL\n", " ");
		fputs(buff, outfile);
		if(!list->conn_use_other_db){
			com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLExecParamsOccurs\" USING\n" ," ");
			_printlog("Generate:OCESQLExecParamsOccurs");
		} else {
			com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLIDExecParamsOccurs\" USING\n" ," ");
			_printlog("Generate:OCESQLIDExecParamsOccurs");
		}
	}else{
		if(!list->conn_use_other_db){
			com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLExecParams\" USING\n" ," ");
			_printlog("Generate:OCESQLExecParams");
		} else {
			com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLIDExecParams\" USING\n" ," ");
			_printlog("Generate:OCESQLIDExecParams");
		}
	}

	fputs(buff, outfile);
	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE SQLCA\n"," ");
	fputs(buff, outfile);

	if(list->conn_use_other_db){
		int iret;
		int var_type;
		int var_len;
		int var_scale;

		iret = gethostvarianttype(list->dbName,&var_type, &var_len, &var_scale);
		if(iret != 0)
		{
			memset(buff, 0, sizeof(buff));
			com_sprintf(buff,sizeof(buff), "E%03d",iret);
			printerrormsg(list->dbName, lineNUM, buff);
			return;
		}

		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE %s\n"," ", list->dbName);
		fputs(buff, outfile);
		memset(buff, 0, sizeof(buff));
		com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n"," ", var_len);
		fputs(buff, outfile);
	}

	if( list->sqlName == NULL )
		return ;

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE %s\n"," ",list->sqlName);
	fputs(buff, outfile);

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n", " ", count);
	fputs(buff, outfile);

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%5sEND-CALL\n", " ");
	fputs(buff, outfile);

	memset(buff, 0, sizeof(buff));
	com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLEndSQL\"\n"," ");
	fputs(buff, outfile);

	ppoutputendcall(list);
}

int ppoutcontext(struct cb_exec_list *list){
	if( list  == NULL)
		return 1;

	if(  strcmp(list->commandName,"CONNECT")==0){

		ppoutputconnect(list);
		return 1;
	}

	if(  strcmp(list->commandName,"OPEN")==0){

		ppoutputopen(list);
		return 1;
	}

	if(  strcmp(list->commandName,"EXECUTE")==0){

		ppoutputexecprepare(list);
		return 1;
	}

	if(  strcmp(list->commandName,"FETCH")==0){

		ppoutputfetch(list);
		return 1;
	}

	if(  strcmp(list->commandName,"COMMIT")==0){

		ppoutputcommit(list);
		return 1;
	}

	if(  strcmp(list->commandName,"COMMIT_RELEASE")==0){

		ppoutputcommit(list);
		ppoutputdisconnect(list);
		return 1;
	}

	if(  strcmp(list->commandName,"ROLLBACK")==0){

		ppoutputrollback(list);
		return 1;
	}
	if(  strcmp(list->commandName,"ROLLBACK_RELEASE")==0){

		ppoutputrollback(list);
		ppoutputdisconnect(list);
		return 1;
	}
	if(  strcmp(list->commandName,"INSERT")==0){

		ppoutputother(list);
		return 1;
	}
	if(  strcmp(list->commandName,"UPDATE")==0){

		ppoutputother(list);
		return 1;
	}
	if(  strcmp(list->commandName,"DELETE")==0){

		ppoutputother(list);
		return 1;
	}
	if(  strcmp(list->commandName,"PREPARE")==0){

		ppoutputprepare(list);
		return 1;
	}
	if(  strcmp(list->commandName,"DISCONNECT")==0){

		ppoutputdisconnect(list);
		return 1;
	}
	if(  list->command_putother){

		ppoutputother(list);
		return 1;
	}

	return 0;
}

void ppbuff(struct cb_exec_list *list){
	int var_type;
	int var_len;
	int var_scale;
	int count;
	struct cb_exec_list *wk_head;
	struct cb_hostreference_list *wk_host;
	char str_type[BUFFSIZE];
	struct cb_exec_list *l;
	int iret;

	char buff[60];

	l = list;
	if(ppoutcontext(list) == 1 )
		return ;

	if(strcmp(l->commandName,"INCLUDE")==0){
		com_strcpy(out,sizeof(out),"OCESQL ");
		com_strcat(out,sizeof(out),"    ");
		com_strcat(out,sizeof(out),"copy ");
		com_strcat(out,sizeof(out),"\"");
		com_strcat(out,sizeof(out),copypath);
		com_strcat(out,sizeof(out),"\".");
		outwrite();
	}
	if(strcmp(l->commandName,"INCFILE")==0){
		return;
	}
	if(strcmp(l->commandName,"HOST_BEGIN")==0 ||
	   strcmp(l->commandName,"HOST_END")==0 ||
	   strcmp(l->commandName,"WORKING_BEGIN")==0){
		return;
	}

	if(strcmp(l->commandName,"WORKING_END")==0){
		wk_head = l;
		outsqlfiller(wk_head);
	}

	if(strcmp(l->commandName,"VARYING_PARAM")==0){
	     // modify cb_field
	     struct cb_field *vp_parent, *vp_len, *vp_arr;
	     int pstart;
	     int istart;
	     char vtmp[256];
	     vp_parent = l->varname;

	     parameter_split(vp_parent);

	     vp_len = vp_parent->children;
	     vp_arr = vp_len->sister;

	     // get start position
	     for(pstart=6;inbuff[pstart]!='\0';pstart++){
		  if(inbuff[pstart] != ' ')
		       break;
	     }

	     com_strcpy(out,sizeof(out),"OCESQL ");
	     for(istart=7; istart<pstart; istart++){
		  com_strcat(out,sizeof(out)," ");
	     }
	     com_sprintf(vtmp,sizeof(vtmp), "%02d", vp_parent->level);
	     com_strcat(out,sizeof(out), vtmp);
	     com_strcat(out,sizeof(out)," ");
	     com_strcat(out,sizeof(out), vp_parent->sname);
	     com_strcat(out,sizeof(out), ".");
	     outwrite();

	     com_strcpy(out,sizeof(out),"OCESQL ");
	     for(istart=7; istart<pstart; istart++){
		  com_strcat(out,sizeof(out)," ");
	     }
	     com_strcat(out,sizeof(out),"  ");
	     com_sprintf(vtmp,sizeof(vtmp), "%02d", vp_len->level);
	     com_strcat(out,sizeof(out), vtmp);
	     com_strcat(out,sizeof(out)," ");
	     com_strcat(out,sizeof(out), vp_len->sname);
	     com_strcat(out,sizeof(out), " PIC S9(8) COMP-5.");
	     outwrite();

	     com_strcpy(out,sizeof(out),"OCESQL ");
	     for(istart=7; istart<pstart; istart++){
		  com_strcat(out,sizeof(out)," ");
	     }
	     com_strcat(out,sizeof(out),"  ");
	     com_sprintf(vtmp,sizeof(vtmp), "%02d", vp_arr->level);
	     com_strcat(out,sizeof(out), vtmp);
	     com_strcat(out,sizeof(out)," ");
	     com_strcat(out,sizeof(out), vp_arr->sname);
	     com_strcat(out,sizeof(out), " PIC X(");
	     com_sprintf(vtmp,sizeof(vtmp), "%d", vp_arr->picnsize);
	     com_strcat(out,sizeof(out), vtmp);
	     com_strcat(out,sizeof(out), ").");
	     outwrite();
	}

	if(strcmp(l->commandName,"SELECT")==0){
		if(l->res_host_list == NULL){
			if(l->hostreferenceCount != 0){
				com_strcpy(out,sizeof(out),"OCESQL ");
				com_strcat(out,sizeof(out),"   ");
				com_strcat(out,sizeof(out),strcall);
				com_strcat(out,sizeof(out),"\"OCESQLStartSQL\"");
				outwrite();

				com_strcpy(out,sizeof(out),"OCESQL ");
				com_strcat(out,sizeof(out),"   ");
				com_strcat(out,sizeof(out),strend);
				outwrite();
				wk_host = l->host_list;
				count = 0;
				for(; wk_host ; wk_host = wk_host->next){
					count += ppoutputparam(wk_host,0);
				}

				com_strcpy(out,sizeof(out),"OCESQL ");
				com_strcat(out,sizeof(out),"   ");
				com_strcat(out,sizeof(out),strcall);
				if(strlen(l->cursorName)>0){
					if(l->conn_use_other_db){
						com_strcat(out,sizeof(out),"\"OCESQLIDCursorDeclareParams\"");
						_printlog("Generate:OCESQLIDCursorDeclareParams");
					} else {
						com_strcat(out,sizeof(out),"\"OCESQLCursorDeclareParams\"");
						_printlog("Generate:OCESQLCursorDeclareParams");
					}
				} else{
					if(l->conn_use_other_db){
						com_strcat(out,sizeof(out),"\"OCESQLIDExecParams\"");
						_printlog("Generate:OCESQLIDExecParams");
					} else {
						com_strcat(out,sizeof(out),"\"OCESQLExecParams\"");
						_printlog("Generate:OCESQLExecParams");
					}
				}
				com_strcat(out,sizeof(out),strusing);

			} else {
				com_strcpy(out,sizeof(out),"OCESQL ");
				com_strcat(out,sizeof(out),"   ");
				com_strcat(out,sizeof(out),strcall);
				if(strlen(l->cursorName)>0){
					if(l->conn_use_other_db){
						com_strcat(out,sizeof(out),"\"OCESQLIDCursorDeclare\"");
						_printlog("Generate:OCESQLIDCursorDeclare");
					} else {
						com_strcat(out,sizeof(out),"\"OCESQLCursorDeclare\"");
						_printlog("Generate:OCESQLCursorDeclare");
					}
				} else {
					if(l->conn_use_other_db){
						com_strcat(out,sizeof(out),"\"OCESQLIDExec\"");
						_printlog("Generate:OCESQLIDExec");
					} else {
						com_strcat(out,sizeof(out),"\"OCESQLExec\"");
						_printlog("Generate:OCESQLExec");
					}
				}
				com_strcat(out,sizeof(out),strusing);
			}
			outwrite();

			com_strcpy(out,sizeof(out),"OCESQL ");
			com_strcat(out,sizeof(out),"       ");
			com_strcat(out,sizeof(out),strsqlca);
			outwrite();

			if(list->conn_use_other_db){
				iret = gethostvarianttype(list->dbName,&var_type, &var_len, &var_scale);
				if(iret != 0)
				{
					memset(buff, 0, sizeof(buff));
					com_sprintf(buff,sizeof(buff), "E%03d",iret);
					printerrormsg(list->dbName, lineNUM, buff);
					return;
				}

				memset(buff, 0, sizeof(buff));
				com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE %s\n"," ", l->dbName);
				fputs(buff, outfile);
				memset(buff, 0, sizeof(buff));
				com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n"," ", var_len);
				fputs(buff, outfile);
			}

			if(strlen(l->cursorName)>0){
				com_strcpy(out,sizeof(out),"OCESQL ");
				com_strcat(out,sizeof(out),"       ");
				com_strcat(out,sizeof(out),strreference);
				com_strcat(out,sizeof(out),"\"");
				com_strcat(out,sizeof(out),l->cursorName);
				com_strcat(out,sizeof(out),"\"");
				com_strcat(out,sizeof(out)," & x\"00\"");
				outwrite();
			}

			com_strcpy(out,sizeof(out),"OCESQL ");
			com_strcat(out,sizeof(out),"       ");
			com_strcat(out,sizeof(out),strreference);
			com_strcat(out,sizeof(out),l->sqlName);
			outwrite();
			if(l->hostreferenceCount != 0){
				com_strcpy(out,sizeof(out),"OCESQL ");
				com_strcat(out,sizeof(out),"       ");
				com_strcat(out,sizeof(out),strbyvalue);
				com_sprintf(str_type,sizeof(str_type),"%d",count);
				com_strcat(out,sizeof(out),str_type);
				outwrite();

				com_strcpy(out,sizeof(out),"OCESQL ");
				com_strcat(out,sizeof(out),"   ");
				com_strcat(out,sizeof(out),strend);
				outwrite();

				com_strcpy(out,sizeof(out),"OCESQL ");
				com_strcat(out,sizeof(out),"   ");
				com_strcat(out,sizeof(out),strcall);
				com_strcat(out,sizeof(out),"\"OCESQLEndSQL\"");
				outwrite();
			}
			com_strcpy(out,sizeof(out),"OCESQL ");
			com_strcat(out,sizeof(out),"   ");
			com_strcat(out,sizeof(out),strend);
			if( l->period)
				com_strcat(out,sizeof(out),".");

			outwrite();
		} else { // SELECT INTO
			struct cb_res_hostreference_list *wk_res_host;
			int length = 0;
			int iteration = 0;
			int reshostreferenceCount = 0;
			int occurs_is_parent = 0;

			com_strcpy(out,sizeof(out),"OCESQL ");
			com_strcat(out,sizeof(out),"   ");
			com_strcat(out,sizeof(out),strcall);
			com_strcat(out,sizeof(out),"\"OCESQLStartSQL\"");
			outwrite();

			com_strcpy(out,sizeof(out),"OCESQL ");
			com_strcat(out,sizeof(out),"   ");
			com_strcat(out,sizeof(out),strend);
			outwrite();

			wk_res_host = l->res_host_list;
			iret = gethostvarianttype(wk_res_host->hostreference ,
									  &var_type, &var_len, &var_scale);

			if(iret != 0){
				memset(buff, 0, sizeof(buff));
				com_sprintf(buff,sizeof(buff), "E%03d",iret);
				printerrormsg(wk_res_host->hostreference, wk_res_host->lineno,
							  buff);
				return;
			}

			if(var_type == HVARTYPE_GROUP){
				struct cb_field *parent, *child;

				parent = getfieldbyname(wk_res_host->hostreference);
				if(parent == NULL){
					printmsg("%s:%d\n", wk_res_host->hostreference, ERR_NOTDEF_WORKING);
					memset(buff, 0, sizeof(buff));
					com_sprintf(buff,sizeof(buff), "E%03d",ERR_NOTDEF_WORKING);
					printerrormsg(wk_res_host->hostreference, wk_res_host->lineno,
								  buff);
					return;
				}

				child = parent->children;

				if(parent->occurs){
					iteration = parent->occurs;
					occurs_is_parent = 1;
					iret = get_host_group_length(child, &length);
					if(iret != 0){
						memset(buff, 0, sizeof(buff));
						com_sprintf(buff,sizeof(buff), "E%03d",iret);
						printerrormsg(wk_res_host->hostreference, wk_res_host->lineno,
									  buff);
						return;
					}
				} else {
					occurs_is_parent = 0;
					iteration = -1;

					iret = get_host_group_table_info(child, &iteration, &length);
					if(iret != 0){
						memset(buff, 0, sizeof(buff));
						com_sprintf(buff,sizeof(buff), "E%03d",iret);
						printerrormsg(wk_res_host->hostreference, wk_res_host->lineno,
									  buff);
						return;
					}
				}

				while(child != NULL){
					iret = gethostvarianttype(child->sname, &var_type, &var_len, &var_scale);
					if(iret != 0){
						memset(buff, 0, sizeof(buff));
						com_sprintf(buff,sizeof(buff), "E%03d",iret);
						printerrormsg(child->sname, wk_res_host->lineno, buff);
						return;
					}
					ppoutputresparam(child->sname, var_type, var_len, var_scale,iteration);
					child = child->sister;
					reshostreferenceCount++;
				}
			} else {
				while(wk_res_host)
				{
					iret = gethostvarianttype(wk_res_host->hostreference,
											  &var_type, &var_len, &var_scale);
					if(iret != 0){
						memset(buff, 0, sizeof(buff));
						com_sprintf(buff,sizeof(buff), "E%03d",iret);
						printerrormsg(wk_res_host->hostreference, wk_res_host->lineno,
									  buff);
						return;
					}

					ppoutputresparam(wk_res_host->hostreference, var_type, var_len, var_scale, 0);
					reshostreferenceCount++;
					wk_res_host = wk_res_host->next;
				}
			}
			count = 0;
			wk_host = l->host_list;
			for(; wk_host ; wk_host = wk_host->next){
				count += ppoutputparam(wk_host,iteration);
			}

			if(iteration){
				memset(buff, 0, sizeof(buff));
				com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLSetHostTable\" USING\n" ," ");
				fputs(buff, outfile);

				memset(buff, 0, sizeof(buff));
				com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n"," ", iteration);
				fputs(buff, outfile);

				memset(buff, 0, sizeof(buff));
				com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n"," ", length);
				fputs(buff, outfile);

				memset(buff, 0, sizeof(buff));
				com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n"," ", occurs_is_parent);
				fputs(buff, outfile);

				memset(buff, 0, sizeof(buff));
				com_sprintf(buff,sizeof(buff), "OCESQL%5sEND-CALL\n", " ");
				fputs(buff, outfile);

				memset(buff, 0, sizeof(buff));
				if(l->conn_use_other_db){
					com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLIDExecSelectIntoOccurs\" USING\n" ," ");
					_printlog("Generate:OCESQLIDExecSelectIntoOccurs");
				} else {
					com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLExecSelectIntoOccurs\" USING\n" ," ");
					_printlog("Generate:OCESQLExecSelectIntoOccurs");
				}
				fputs(buff, outfile);
			} else {
				memset(buff, 0, sizeof(buff));
				if(l->conn_use_other_db){
					com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLIDExecSelectIntoOne\" USING\n" ," ");
					_printlog("Generate:OCESQLIDExecSelectIntoOne");
				} else {
					com_sprintf(buff,sizeof(buff), "OCESQL%5sCALL \"OCESQLExecSelectIntoOne\" USING\n" ," ");
					_printlog("Generate:OCESQLExecSelectIntoOne");
				}
				fputs(buff, outfile);
			}

			com_strcpy(out,sizeof(out),"OCESQL ");
			com_strcat(out,sizeof(out),"       ");
			com_strcat(out,sizeof(out),strsqlca);
			outwrite();

			if(list->conn_use_other_db) {
				iret = gethostvarianttype(list->dbName,&var_type, &var_len, &var_scale);
				if(iret != 0)
				{
					memset(buff, 0, sizeof(buff));
					com_sprintf(buff,sizeof(buff), "E%03d",iret);
					printerrormsg(list->dbName, lineNUM, buff);
					return;
				}

				memset(buff, 0, sizeof(buff));
				com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE %s\n"," ", l->dbName);
				fputs(buff, outfile);
				memset(buff, 0, sizeof(buff));
				com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n"," ", var_len);
				fputs(buff, outfile);
			}

			com_strcpy(out,sizeof(out),"OCESQL ");
			com_strcat(out,sizeof(out),"       ");
			com_strcat(out,sizeof(out),strreference);
			com_strcat(out,sizeof(out),l->sqlName);
			outwrite();

			memset(buff, 0, sizeof(buff));
			com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n", " ", count);
			fputs(buff, outfile);

			memset(buff, 0, sizeof(buff));
			com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n", " ", reshostreferenceCount);
			fputs(buff, outfile);

			com_strcpy(out,sizeof(out),"OCESQL ");
			com_strcat(out,sizeof(out),"   ");
			com_strcat(out,sizeof(out),strend);
			outwrite();

			com_strcpy(out,sizeof(out),"OCESQL ");
			com_strcat(out,sizeof(out),"   ");
			com_strcat(out,sizeof(out),strcall);
			com_strcat(out,sizeof(out),"\"OCESQLEndSQL\"");
			outwrite();

			com_strcpy(out,sizeof(out),"OCESQL ");
			com_strcat(out,sizeof(out),"   ");
			com_strcat(out,sizeof(out),strend);
			if( l->period)
				com_strcat(out,sizeof(out),".");

			outwrite();
		}
	} else if(l->prepareName[0] != '\0'){
		// DECLARE cursor for prepare
		com_strcpy(out,sizeof(out),"OCESQL ");
		com_strcat(out,sizeof(out),"   ");
		com_strcat(out,sizeof(out),strcall);
		if(l->conn_use_other_db){
			com_strcat(out,sizeof(out),"\"OCESQLIDPreparedCursorDeclare\"");
			_printlog("Generate:OCESQLIDPreparedCursorDeclare");
		} else {
			com_strcat(out,sizeof(out),"\"OCESQLPreparedCursorDeclare\"");
			_printlog("Generate:OCESQLPreparedCursorDeclare");
		}
		com_strcat(out,sizeof(out),strusing);
		outwrite();

		com_strcpy(out,sizeof(out),"OCESQL ");
		com_strcat(out,sizeof(out),"       ");
		com_strcat(out,sizeof(out),strsqlca);
		outwrite();

		if(list->conn_use_other_db){
			iret = gethostvarianttype(list->dbName,&var_type, &var_len, &var_scale);
			if(iret != 0)
			{
				memset(buff, 0, sizeof(buff));
				com_sprintf(buff,sizeof(buff), "E%03d",iret);
				printerrormsg(list->dbName, lineNUM, buff);
				return;
			}

			memset(buff, 0, sizeof(buff));
			com_sprintf(buff,sizeof(buff), "OCESQL%10sBY REFERENCE %s\n"," ", l->dbName);
			fputs(buff, outfile);
			memset(buff, 0, sizeof(buff));
			com_sprintf(buff,sizeof(buff), "OCESQL%10sBY VALUE %d\n"," ", var_len);
			fputs(buff, outfile);
		}

		com_strcpy(out,sizeof(out),"OCESQL ");
		com_strcat(out,sizeof(out),"       ");
		com_strcat(out,sizeof(out),strreference);
		com_strcat(out,sizeof(out),"\"");
		com_strcat(out,sizeof(out),l->cursorName);
		com_strcat(out,sizeof(out),"\"");
		com_strcat(out,sizeof(out)," & x\"00\"");
		outwrite();

		com_strcpy(out,sizeof(out),"OCESQL ");
		com_strcat(out,sizeof(out),"       ");
		com_strcat(out,sizeof(out),strreference);
		com_strcat(out,sizeof(out),"\"");
		com_strcat(out,sizeof(out),l->prepareName);
		com_strcat(out,sizeof(out),"\"");
		com_strcat(out,sizeof(out)," & x\"00\"");
		outwrite();

		com_strcpy(out,sizeof(out),"OCESQL ");
		com_strcat(out,sizeof(out),"   ");
		com_strcat(out,sizeof(out),strend);
		if( l->period)
			com_strcat(out,sizeof(out),".");
		outwrite();
	}

	if(strcmp(l->commandName, "CLOSE")==0){


		com_strcpy(out,sizeof(out),"OCESQL ");
		com_strcat(out,sizeof(out),"   ");
		com_strcat(out,sizeof(out),strcall);
		com_strcat(out,sizeof(out),"\"OCESQLCursorClose\" ");
		com_strcat(out,sizeof(out),strusing);
		outwrite();
		_printlog("Generate:OCESQLCursorClose");

		com_strcpy(out,sizeof(out),"OCESQL ");
		com_strcat(out,sizeof(out),"       ");
		com_strcat(out,sizeof(out),strsqlca);
		outwrite();

		com_strcpy(out,sizeof(out),"OCESQL ");
		com_strcat(out,sizeof(out),"       ");
		com_strcat(out,sizeof(out),strreference);
		com_strcat(out,sizeof(out),"\"");
		com_strcat(out,sizeof(out),l->cursorName);
		com_strcat(out,sizeof(out),"\"");
		com_strcat(out,sizeof(out)," & x\"00\"");
		outwrite();

		com_strcpy(out,sizeof(out),"OCESQL ");

		com_strcat(out, sizeof(out), "   ");
		com_strcat(out, sizeof(out), strend);

		outwrite();

		com_strcpy(out,sizeof(out),"OCESQL ");
		com_strcat(out,sizeof(out),"   ");
		if( l->period)
			com_strcat(out,sizeof(out),".");
		outwrite();
	}
	return;
}

void ppbuff_incfile(struct cb_exec_list *list){
	struct cb_exec_list *l;
	char buff[10];
	char incmsg[256];
	int len2;

	l = list;

	if(strcmp(l->commandName,"INCFILE")==0){
		char filename[512];
		FILE *incf;
		char incf_buff[BUFFSIZE + 1];
		int retcode;

		memset(filename, 0, 512);

		if(include_path){
			com_sprintf(filename,sizeof(filename), "%s/", include_path);
		}
		com_strcat(filename,sizeof(filename), l->incfileName);
 
 		incf = fopen_or_die(filename, "r");

		memset(incmsg, 0, 256);
		sprintf(incmsg, "%s incfile start:%s", INC_START_MARK, filename);
		com_strcpy(out,sizeof(out),incmsg);
		outwrite();

		while(1){
			memset(incf_buff, 0, BUFFSIZE + 1);
			fgets(incf_buff, BUFFSIZE, incf);
			if(feof(incf)) break;

			if(strlen(incf_buff) > MAX_LINESIZE){
				memset(buff, 0, sizeof(buff));
				com_sprintf(buff,sizeof(buff), "E%03d",ERR_EXCEED_LIMIT_LINE_LENGTH);
				printerrormsg("", lineNUM, buff);
			}

			com_strcpy(out,sizeof(out),"OCESQL");
			com_strcat(out,sizeof(out), incf_buff + strlen("OCESQL"));
			retcode = strlen(incf_buff);
			len2 = strlen("OCESQL");
			if(retcode > len2){
				out[retcode-1] = '\0';
			}
			outwrite();
		}

		memset(incmsg, 0, 256);
		sprintf(incmsg, "%s incfile end:%s",INC__END__MARK , filename);
		com_strcpy(out,sizeof(out),incmsg);
		outwrite();

		return;
	}
	return;
}

void outwrite(){
	size_t len;

	outbuff = out;
	len = strlen(outbuff);
	fwrite (outbuff ,len, 1 , outfile );
	fputc('\n',outfile);
}

void ppoutput(char *ppin,char *ppout,struct cb_exec_list *head){
	FILE *readfile;
	size_t len;

	struct cb_exec_list *l;
	l = head;
	lineNUM = 0;
	EOFflg = 0;

 	readfile = fopen_or_die(ppin,"r");
 	outfile  = fopen_or_die(ppout,"w");

	EOFFLG = 0;
	if (readfile && outfile){
		for(;EOFflg != 1;){
			com_readline(readfile, inbuff, &lineNUM, &EOFflg);
			if(strstr(inbuff, INC_START_MARK) != NULL ||
			strstr(inbuff, INC__END__MARK) != NULL){
				continue;
			}
			if(head){
				if (l->startLine<= lineNUM && l->endLine>=lineNUM){
					if(strcmp(l->commandName,"WORKING_END")==0){
						ppbuff(l);
					}

					if(strcmp(l->commandName,"WORKING_BEGIN")!=0 &&
					   strcmp(l->commandName,"WORKING_END") != 0){
						inbuff[0] = 'O';
						inbuff[1] = 'C';
						inbuff[2] = 'E';
						inbuff[3] = 'S';
						inbuff[4] = 'Q';
						inbuff[5] = 'L';
						inbuff[6] = '*';
					}

					outbuff = inbuff;
					len = strlen(outbuff);
					fwrite (outbuff ,len, 1 , outfile );

					if (EOFflg == 1){
						fputc('\n',outfile);

					}
				}
				else{
					if(lineNUM - l->endLine == 1){
						if(strcmp(l->commandName,"WORKING_END")){
							ppbuff(l);
						}
						if (l->next != NULL)
							l = l->next;

						if(l->startLine<= lineNUM && l->endLine>=lineNUM &&
						   (strcmp(l->commandName,"WORKING_BEGIN")!=0 &&
						    strcmp(l->commandName,"WORKING_END")!=0)){
							inbuff[0] = 'O';
							inbuff[1] = 'C';
							inbuff[2] = 'E';
							inbuff[3] = 'S';
							inbuff[4] = 'Q';
							inbuff[5] = 'L';
							inbuff[6] = '*';
						}
						outbuff = inbuff;
						len = strlen(outbuff);
						fwrite (outbuff ,len, 1 , outfile );
					}else{
						outbuff = inbuff;
						len = strlen(outbuff);
						fwrite (outbuff ,len, 1 , outfile );

					}
				}
			}else{
				outbuff = inbuff;
				len = strlen(outbuff);
				fwrite (outbuff ,len, 1 , outfile );
			}
		}
	}

	fclose(readfile);
	fclose(outfile);

	remove(ppin);
}

void ppoutput_incfile(char *ppin,char *ppout,struct cb_exec_list *head){
	FILE *readfile;
	size_t len;

	struct cb_exec_list *l;
	l = head;

 	readfile = fopen_or_die(ppin,"r");
 	outfile  = fopen_or_die(ppout,"w");

	EOFFLG = 0;
	if (readfile && outfile){
		for(;EOFflg != 1;){
			com_readline(readfile, inbuff, &lineNUM, &EOFflg);
			if(head){
				if (l->startLine<= lineNUM && l->endLine>=lineNUM){
					if (strcmp(l->commandName, "INCFILE") == 0){
						inbuff[0] = 'O';
						inbuff[1] = 'C';
						inbuff[2] = 'E';
						inbuff[3] = 'S';
						inbuff[4] = 'Q';
						inbuff[5] = 'L';
						inbuff[6] = '*';
					}

					outbuff = inbuff;
					len = strlen(outbuff);
					fwrite (outbuff ,len, 1 , outfile );

					if (EOFflg == 1){
						fputc('\n',outfile);
					}
				}
				else{
					if(lineNUM - l->endLine == 1){
						if(strcmp(l->commandName,"INCFILE")==0){
							ppbuff_incfile(l);
						}
						if (l->next != NULL)
							l = l->next;

						if(l->startLine<= lineNUM && l->endLine>=lineNUM &&
						   (strcmp(l->commandName, "INCFILE") == 0)){
							inbuff[0] = 'O';
							inbuff[1] = 'C';
							inbuff[2] = 'E';
							inbuff[3] = 'S';
							inbuff[4] = 'Q';
							inbuff[5] = 'L';
							inbuff[6] = '*';
						}
						outbuff = inbuff;
						len = strlen(outbuff);
						fwrite (outbuff ,len, 1 , outfile );
					}else{
						outbuff = inbuff;
						len = strlen(outbuff);
						fwrite (outbuff ,len, 1 , outfile );

					}
				}
			}else{
				outbuff = inbuff;
				len = strlen(outbuff);
				fwrite (outbuff ,len, 1 , outfile);
			}
		}
	}
	fclose(readfile);
	fclose(outfile);
}

int check_Dchar(char c){
	unsigned char uc = (unsigned char)c;
	if(uc >= 0x81 && uc <= 0x9f){
		return 1;
	}else if(uc >= 0xe0 && uc <= 0xef){
		return 1;
	}
	return 0;
}

int get_host_group_length(struct cb_field *field, int *length){
	if(field == NULL) return 0;


	if((field->pictype == PIC_NATIONAL) ||
			(field->pictype == PIC_NATIONAL_VARYING)){
		*length += field->picnsize * 2;
	}else{
		*length += field->picnsize;
	}

	return get_host_group_length(field->sister, length);
}

int get_host_group_table_info(struct cb_field *field, int *iteration, int *length){
	if(field == NULL) return 0;

	if(field->occurs){
		if(*iteration == -1 || field->occurs < *iteration){
			*iteration = field->occurs;
		}
	} else {
		*iteration = 0;
	}
	if((field->pictype == PIC_NATIONAL) ||
			(field->pictype == PIC_NATIONAL_VARYING)){
		*length += field->picnsize * 2;
	}else{
		*length += field->picnsize;
	}

	return get_host_group_table_info(field->sister, iteration, length);
}

void parameter_split(struct cb_field *vp_parent){
	struct cb_field *vp_len, *vp_arr;
	char *basename;
	int varlen;
	int vartype;

	vp_len = malloc(sizeof(struct cb_field));
	vp_arr = malloc(sizeof(struct cb_field));
	if( vp_len == NULL || vp_arr == NULL){
	     printmsg("parameter_split: memory allocation for cb_field failed.\n");
	     goto die_parameter_split;
	     return;
	}

	memset(vp_len, 0 ,sizeof(struct cb_field));
	memset(vp_arr, 0 ,sizeof(struct cb_field));

	basename = vp_parent->sname;
	varlen = vp_parent->picnsize;
	vartype = vp_parent->pictype;

	// vp_len
	vp_len->sname = (char *)malloc((strlen(basename) + strlen("-LEN") + TERMINAL_LENGTH) * sizeof(char));
	if(vp_len->sname == NULL){
	     printmsg("parameter_split: memory allocation for vp_len->sname failed.\n");
	     goto die_parameter_split;
	     return;
	}
	com_sprintf(vp_len->sname,sizeof(vp_len->sname), "%s-LEN", basename);
	vp_len->level = vp_parent->level + 1;
	vp_len->parent = vp_parent;
	vp_len->pictype = PIC_NUMERIC;
	vp_len->usage = USAGE_BINARY_NATIVE;
	vp_len->have_sign = 1;
	vp_len->picnsize = 10;
	vp_parent->children = vp_len;

	// vp_arr
	vp_arr->sname = (char *)malloc((strlen(basename) + strlen("-ARR") + TERMINAL_LENGTH) * sizeof(char));
	if(vp_arr->sname == NULL){
	     printmsg("parameter_split: memory allocation for vp_arr->sname failed.\n");
	     goto die_parameter_split;
	     return;
	}
	com_sprintf(vp_arr->sname,sizeof(vp_arr->sname), "%s-ARR", basename);
	vp_arr->level = vp_parent->level + 1;
	vp_arr->pictype = vartype;
	vp_arr->picnsize = varlen;
	vp_arr->parent = vp_parent;
	vp_len->sister = vp_arr;

	// vp_parent
	if(vp_parent->pictype == PIC_NATIONAL){
	     vp_parent->pictype = PIC_NATIONAL_VARYING;
	} else {
	     vp_parent->pictype = PIC_ALPHANUMERIC_VARYING;
	}

	return;

die_parameter_split:
	if(vp_len){
	     if(vp_len->sname) free(vp_len->sname);
	     free(vp_len);
	}
	if(vp_arr){
	     if(vp_arr->sname) free(vp_arr->sname);
	     free(vp_arr);
	}
	exit(-1);
	return;
}



FILE* fopen_or_die(char *filename, const char *mode){
	FILE* retval;
	com_fopen(&retval,filename, mode);

	if(retval == NULL){
		perror(filename);
		exit(-1);
	}

	return retval;
}

void _printlog(char *msg){
	printmsg("%s\n", msg);
}

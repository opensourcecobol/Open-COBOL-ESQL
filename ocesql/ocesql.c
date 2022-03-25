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
#include <sys/types.h>
#include <stdlib.h>

#include "ocesqlutil.h"
#include "ocesql.h"

int  processid = 0;
char  *cb_source_file = NULL;
int   cb_source_line = 0;
char *include_path = NULL;
int currenthostno = 0;
extern int hostlineno;

int flag_external = 0;

extern struct cb_hostreference_list *host_reference_list;
extern struct cb_res_hostreference_list *res_host_reference_list;
extern struct cb_sql_list *sql_list;
extern struct cb_exec_list *exec_list;
extern char dbname[BUFFSIZE];
extern char prepname[BUFFSIZE];
extern char cursorname[BUFFSIZE];
char *errorfilename = NULL;
char *filenameID = NULL;

char * gettmpname(const char *name)
{
	char buff[BUFFSIZE];
	memset(buff, 0, sizeof(buff));
	com_sprintf (buff, sizeof(buff), "%scob%d%s", "", processid, name);
	return com_strdup(buff);
}

void
file_basename (const char *filename, char *buff)
{
	const char	*startp;
	const char	*endp;
	size_t		len;

	/* Remove directory name */
	startp = strrchr (filename, '/');
	if (startp) {
		startp++;
	} else {
		startp = filename;
	}

	/* Remove extension */
	endp = strrchr (filename, '.');
	if (endp > startp) {
		len = endp - startp;
	} else {
		len = strlen (startp);
	}

	/* Copy base name */
	com_strncpy(buff, BUFFSIZE, startp, len);
	buff[len] = '\0';
}


char * gettranslatename(char *name)
{
	char buff[BUFFSIZE];
	char basename[BUFFSIZE];
	memset(basename, 0, sizeof(basename));
	memset(buff, 0, sizeof(buff));
	file_basename (name, basename);
	com_sprintf (buff, sizeof(buff), "preeql%s.cob", basename);
	return com_strdup(buff);
}

int translate (struct filename *fn)
{
	int ret;
	char *tmpfile;

	tmpfile = gettmpname("tmp");

	// 1st: LOAD INCLUDE FILE
	com_fopen (&yyin, fn->source, "rb");
	if (!yyin) {
		perror (fn->source);
		return -1;
	}
	ret =  yyparse ();
	fclose (yyin);
	if (ret) {
		return ret;
	}
	ppoutput_incfile(fn->source, tmpfile, exec_list);

	// reset parser
	exec_list = NULL;
	yylineno = 1;
	sqlnum = 0;

	// 2nd: PARSE
	com_fopen (&yyin, tmpfile, "rb");
	if (!yyin) {
		com_unlink(tmpfile);
		perror (tmpfile);
		return -1;
	}
	ret =  yyparse ();
	fclose (yyin);
	if (ret) {
		return ret;
	}

	ppoutput(tmpfile, fn->translate, exec_list);

	return 0;
}


struct cb_sql_list *
cb_text_list_add (struct cb_sql_list *list, char *text)
{
	struct cb_sql_list *p;
	struct cb_sql_list *l;

	p = malloc (sizeof (struct cb_sql_list));
	p->sqltext = com_strdup (text);

	p->next = NULL;

	if (!list) {
		return p;
	} else {
		for (l = list; l->next; l = l->next) ;
		l->next = p;

		return list;
	}
}

struct cb_sql_list *
cb_add_text_list (struct cb_sql_list *list, struct cb_sql_list *targetlist)
{
	struct cb_sql_list *t;
	struct cb_sql_list *l;
	struct cb_sql_list *p;

    p = list;
    l = list;
	t = targetlist;

	for(; l->next ; l = l->next);
	l->next = t;
	t = p;
	targetlist = t;

	sql_list = targetlist;

	return targetlist;

}


char *
cb_host_list_add (struct cb_hostreference_list *list, char *text)
{
	int hostno;
	char temps[BUFFSIZE];

	hostno = cb_search_list(text);

	com_sprintf(temps,sizeof(temps),"$%d",hostno);
	return com_strdup(temps);
}

void
cb_res_host_list_add (struct cb_res_hostreference_list *list, char *text)
{
	struct cb_res_hostreference_list *l;
	struct cb_res_hostreference_list *p;

	p = malloc (sizeof (struct cb_res_hostreference_list));
	p->hostreference = com_strdup (text);
	p->lineno = hostlineno;
	p->next = NULL;

	if ( res_host_reference_list == NULL ){
		res_host_reference_list = p;
	} else {
		l = res_host_reference_list;
		for(; l->next ; l = l->next);
		l->next = p;
	}
}

int
cb_search_list(char *text)
{
	struct cb_hostreference_list *l;
	struct cb_hostreference_list *p;
	l = host_reference_list;
	int i = 0;

	for(; l ; l = l->next) {
		i++;
	}

	p = malloc (sizeof (struct cb_hostreference_list));
	p->hostreference = com_strdup (text);
	p->hostno = i+1;
	p->lineno = hostlineno;
	p->next = NULL;

	if ( host_reference_list == NULL ){
		host_reference_list = p;
	} else {
		l = host_reference_list;
		for(; l->next ; l = l->next);
		l->next = p;
	}

	return i+1;
}

void
cb_set_cursorname(char *text)
{
	memset(cursorname,0,sizeof(cursorname));
	com_strcpy(cursorname,sizeof(cursorname),filenameID);
	com_strcat(cursorname,sizeof(cursorname),"_");
	com_strcat(cursorname,sizeof(cursorname),text);
}

void
cb_set_dbname(char *text)
{
	memset(dbname,0,sizeof(dbname));
	com_strcpy(dbname,sizeof(dbname),text);
}

void
cb_set_prepname(char *text)
{
	memset(prepname,0,sizeof(prepname));
	com_strcpy(prepname,sizeof(prepname),text);
}

char *
cb_get_env(char *filename, int num)
{
	char *path = NULL;

	char buff[BUFFSIZE];

	size_t len;

	switch(num)
	{
	case 1:
		com_dupenv(&path, &len, "OPNCBL_COBDIR");
		break;
	case 2:
		com_dupenv(&path, &len, "OPNCBL_SRCDIR");
		break;
	case 3:
		com_dupenv(&path, &len, "OPNCBL_EROUT");
		break;
	default:
		break;
	}


  	if(path == NULL)
		return com_strdup(filename);

  	memset(buff,0,strlen(buff));
	com_strcpy(buff,sizeof(buff),path);
	com_strcat(buff,sizeof(buff),filename);
	return com_strdup(buff);
}

void version(void){
	printf("Open Cobol ESQL (Ocesql)\n");
	printf("Version 1.3.0\n");
	printf("\n");
	printf("March 24, 2022\n");
	printf("\n");
	printf("Tokyo System House Co., Ltd. <opencobol@tsh-world.co.jp>\n");
}

void print_version(void){
	version();
	exit(-1);
}

void print_usage(void){
	version();
	printf("\n");
	printf("Usage: ocesql [options] SOURCE [DESTFILE] [LOGFILE]\n");
	printf("\n");
	printf("options\n");
	printf("      --inc=include_dir      set INCLUDE FILE directory path.\n");
	printf("\n");
	printf("usage\n");
	printf("  -v, --version              show version.\n");
	printf("  -h, --help                 show this usage.\n");
	exit(-1);
}

int main (int argc, char *argv[])
{
	struct filename transfile = {NULL, NULL};
	char *fileandpath = NULL;
	char *outputname = NULL;

	const char *prelopt = "--";
	int preloptlen = 2;
	const char *preopt = "-";
	int preoptlen = 1;
	char *opthead;
	char *opt;
	char *optval;

	char *env;
	char *tempid;
	int iret;

	int i;
	int optind;
	int optfile_idx = 1;
	
	for (optind=1; optind<argc; optind++){
		if(strncmp(prelopt, argv[optind], preloptlen) == 0){
			// long option
			char *p;
			opthead = argv[optind] + sizeof(char) * preloptlen;
			p = strchr(opthead, '=');
			if(p != NULL){
				optval = p + sizeof(char);
				*p = '\0';
			} else {
				optval = NULL;
			}
			if(strcmp("inc", opthead) == 0){
				if(optval != NULL){
					include_path = com_strdup(optval);
				} else {
					printf("invalid option: --inc is get directory path parameter.\n");
				}
			} else if(strcmp("version", opthead) == 0){
				print_version();
				break;
			} else if(strcmp("help", opthead) == 0){
				print_usage();
				break;
			} else {
				printf("invalid option: %s\n", argv[optind]);
				print_usage();
				break;
			}
		} else if(strncmp(preopt, argv[optind], preoptlen) == 0){
			print_usage();
			break;
		} else {
			if(transfile.source == NULL){
				fileandpath = cb_get_env(argv[optind], 1);
				transfile.source = com_strdup(fileandpath);
			} else if(outputname == NULL){
				fileandpath = cb_get_env(argv[optind], 2);
				outputname = com_strdup(fileandpath);
			} else if(errorfilename == NULL){
				fileandpath = cb_get_env(argv[optind], 3);
				errorfilename = com_strdup(fileandpath);
			} else {
				printf("too many arguments.\n");
				print_usage();
			}
		}
	}


	if(transfile.source == NULL){
		print_usage();
	} else {
#ifdef _WIN32
		tempid = strrchr(fileandpath,'\\');
#else
		tempid = strrchr(fileandpath,'/');
#endif
		if(!tempid){
			tempid = fileandpath;
		}else{
			tempid = tempid + 1;
		}
		for(i=0;i<strlen(tempid);i++){
			if(tempid[i] == '-'){
				tempid[i] = '_';
			}else if(tempid[i] == '.'){
				tempid[i] = '\0';
			}
		}
		filenameID = com_strdup(tempid);
	}
	openerrorfile(errorfilename);
	printmsg("precompile start: %s\n",transfile.source);
	printmsg("=======================================================\n");
	printmsg("              LIST OF CALLED DB Library API            \n");
	printmsg("=======================================================\n");

	processid = com_getpid();

	if( outputname != NULL)
		transfile.translate =  outputname;
	else
		transfile.translate =  gettranslatename(transfile.source);

	size_t len;
	com_dupenv(&env, &len, "OCESQL_EXT");
	if(env == NULL)
		flag_external=0;
	else if(strcmp(env,"Y")==0)
		flag_external=1;
	else
		flag_external=0;

	iret = translate(&transfile);
	printmsg("=======================================================\n");

	free(filenameID);


	if(iret != 0){
		printmsg("translate error\n");
	}
	closeerrorfile();
	return iret;

}

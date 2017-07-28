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

#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <getopt.h>

#include "define.h"

char *tmpdir = "/tmp/";
pid_t  processid = 0;
char  *cb_source_file = NULL;
int   cb_source_line = 0;
char *include_path = NULL;
int currenthostno = 0;
int hostlineno;

int flag_external = 0;

struct cb_hostreference_list *host_reference_list = NULL;
struct cb_res_hostreference_list *res_host_reference_list = NULL;
struct cb_sql_list *sql_list = NULL;
struct cb_exec_list *exec_list;
char cursorname[BUFFSIZE];
char *errorfilename = NULL;
char *filenameID = NULL;

static struct option longopts[] =
{
	{"inc", required_argument, NULL, 'i'},
	{0,0,0,0} // end of array
};

char * gettmpname(const char *name)
{
	char buff[BUFFSIZE];
	memset(buff, 0, sizeof(buff));
	sprintf (buff, "%scob%d%s", tmpdir, processid,name);
	return strdup(buff);
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
	strncpy (buff, startp, len);
	buff[len] = '\0';
}


char * gettranslatename(char *name)
{
	char buff[BUFFSIZE];
	char basename[BUFFSIZE];
	memset(basename, 0, sizeof(basename));
	memset(buff, 0, sizeof(buff));
	file_basename (name, basename);
	sprintf (buff, "preeql%s.cob", basename);
	return strdup(buff);

}

int translate (struct filename *fn)
{
	int ret;
	char *tmpfile;

	tmpfile = gettmpname("tmp");

	// 1st: LOAD INCLUDE FILE
	yyin = fopen (fn->source, "rb");
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
	yyin = fopen (tmpfile, "rb");
	if (!yyin) {
	 	unlink(tmpfile);
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
	p->sqltext = strdup (text);

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

	sprintf(temps,"$%d",hostno);
	return strdup(temps);

}

void
cb_res_host_list_add (struct cb_res_hostreference_list *list, char *text)
{
	struct cb_res_hostreference_list *l;
	struct cb_res_hostreference_list *p;

	p = malloc (sizeof (struct cb_res_hostreference_list));
	p->hostreference = strdup (text);
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
		if (strcmp(l->hostreference, text) == 0){
			return l->hostno;
		}
	}

	p = malloc (sizeof (struct cb_hostreference_list));
	p->hostreference = strdup (text);
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
	strcpy(cursorname,filenameID);
	strcat(cursorname,"_");
	strcat(cursorname,text);
}

char *
cb_get_env(char *filename, int num)
{
	char *path = NULL;

    char buff[BUFFSIZE];

    switch(num)
    {
	case 1:
		path = getenv("OPNCBL_COBDIR");
		break;
	case 2:
		path = getenv("OPNCBL_SRCDIR");
		break;
	case 3:
		path = getenv("OPNCBL_EROUT");
		break;
	default:
		break;
    }


  	if(path == NULL)
  		return strdup(filename);

  	memset(buff,0,strlen(buff));
  	strcpy(buff,path);
  	strcat(buff,filename);
  	return strdup(buff);
}

void usage(void){
	printf("\n");
	printf("Open Cobol ESQL (Ocesql)\n");
	printf("Version 1.0.0\n");
	printf("June 28, 2013\n");
	printf("Tokyo System House Co., Ltd. <opencobol@tsh-world.co.jp>\n");
	printf("\n");
	printf("Usage: ocesql [--inc=include_dir] SOURCE [DESTFILE] [LOGFILE]\n");
	exit(-1);
}

int main (int argc, char *argv[])
{
	struct filename transfile;
	char *outputname = NULL;

	char *fileandpath;
	char *env;
	char *tempid;
	int iret;

	int opt;
	int option_index;

	int i;

	while(1){
		opt = getopt_long(argc, argv, "i:", longopts, &option_index);
		if(opt == -1) break;

		switch(opt){
		case 'i':
			if(!optarg){
				usage();
			} else {
				include_path = strdup(optarg);
			}
			break;
		case '?':
			usage();
			break;
		}
	}

	if(optind >= argc) usage();
	fileandpath = cb_get_env(argv[optind++],1);
	transfile.source = strdup(fileandpath);

	tempid = strrchr(fileandpath,'/');
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
	filenameID = strdup(tempid);


	if(optind < argc){
		fileandpath = cb_get_env(argv[optind++], 2);
		outputname = strdup(fileandpath);
	}

	if(optind < argc){
		fileandpath = cb_get_env(argv[optind++],3);
		errorfilename = strdup(fileandpath);
	}

	if(optind < argc){
		printf("too many arguments.\n");
		usage();
	}

	printf("precompile start: %s\n",transfile.source);
	printf("=======================================================\n");
	printf("              LIST OF CALLED DB Library API            \n");
	printf("=======================================================\n");

	processid = getpid ();

	if( outputname != NULL)
		transfile.translate =  outputname;
	else
		transfile.translate =  gettranslatename(transfile.source);

	env = getenv("OCESQL_EXT");
	if(env == NULL)
		flag_external=0;
	else if(strcmp(env,"Y")==0)
		flag_external=1;
	else
		flag_external=0;

	iret = translate(&transfile);
	printf("=======================================================\n");

	free(filenameID);

	if(iret != 0){
		printf("translate error\n");
		return 1;
	}

	return 0;

}

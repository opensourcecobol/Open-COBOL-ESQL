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

#ifndef DEFINE_H_1234567890
#define DEFINE_H_1234567890

#include <string.h>
#include <errno.h>
#include <stdio.h>

#define BUFFSIZE 256
#define MAX_LINESIZE 128

#define PICTYPEX  1
#define PICTYPES  2

#define PIC_ALPHABETIC 		0x01
#define PIC_NUMERIC 		0x02
#define PIC_NATIONAL		0x04
#define PIC_ALPHANUMERIC	(PIC_ALPHABETIC | PIC_NUMERIC)

#define HVARTYPE_UNSIGNED_NUMERIC 1
#define HVARTYPE_SIGNED_TRAILING_SEPARATE 2
#define HVARTYPE_SIGNED_TRAILING_COMBINED 3
#define HVARTYPE_SIGNED_LEADING_SEPARATE 4
#define HVARTYPE_SIGNED_LEADING_COMBINED 5
#define HVARTYPE_UNSIGNED_PACKED 8
#define HVARTYPE_SIGNED_PACKED 9
#define HVARTYPE_ALPHABETIC 16
#define HVARTYPE_GROUP 22
#define HVARTYPE_FLOAT 23
#define HVARTYPE_NATIONAL 24

#define USAGECOMP1 1
#define USAGECOMP2 2
#define USAGECOMPOTHER 3

enum oc_usage{
	USAGE_NONE,
	USAGE_FLOAT,
	USAGE_DOUBLE,
	USAGE_PACKED
};

#define ERR_NOTDEF_WORKING 1
#define ERR_NOTDEF_CONVERSION 2
#define ERR_EXCEED_LIMIT_LINE_LENGTH 901

#define  SIGNLEADING 1
#define  FLAGVARYING 1

#define SIGN_SEPARATE 1

#define strcall       " CALL "
#define strusing      " USING"
#define strend        " END-CALL"
#define strsqlca      "  BY REFERENCE SQLCA"
#define strreference  "  BY REFERENCE "
#define strconnid     "  BY VALUE CONNID"
#define strbyvalue    "  BY VALUE "

#define incfolder     "INC"
#define copypath      "sqlca.cbl"

struct filename {
	char  *source;
	char  *translate;
};

struct cb_sql_list {
	char *sqltext;
	struct cb_sql_list *next;
};

struct cb_hostreference_list {
	char *hostreference;
	int hostno;
	int lineno;
	struct cb_hostreference_list *next;
};

struct cb_res_hostreference_list {
	char *hostreference;
	int lineno;
	struct cb_res_hostreference_list *next;
};

struct cb_exec_list {
	int startLine;
	int endLine;
	int period;
	struct cb_hostreference_list *host_list;
	int hostreferenceCount;
	struct cb_res_hostreference_list *res_host_list;
	int conn_use_other_db;
	struct cb_sql_list *sql_list;
	char *dbName;
	char *cursorName;
	char *commandName;
	int command_putother;
	char *sqlName;
	char *incfileName;
	struct cb_exec_list *next;
};

struct cb_field {
	char	*sname;
	int		level;
	int		usage;
//	int		sign_leading;
	int		occurs;
        int		flag_varying;
	struct cb_field *parent;
	struct cb_field *children;
	struct cb_field *sister;
//	char  *picname;

	int pictype;
	int picnsize;
	int scale;
	unsigned char have_sign;
	int sign_leading;
	int separate;
};


extern struct cb_exec_list *exec_list;
extern struct cb_hostreference_list *host_reference_list;
extern struct cb_res_hostreference_list *res_host_reference_list;
extern int command_putother;
extern struct cb_sql_list *sql_list;
extern int currenthostno;
extern char * cb_source_file;
extern int  cb_source_line;
extern char *include_path;

extern FILE *yyin;
extern FILE *yyout;
extern int yylex(void);
extern int yyparse (void);

extern char inbuff[256];
extern char out[256];
extern struct cb_exec_list *head;
extern char *outbuff;
extern FILE *outfile;
extern int EOFflg;
extern int EOFFLG;
extern int pointflg;
extern int lineNUM;

extern int yylineno;
extern int startlineno;
extern int endlineno;
extern int hostlineno;
extern int period;
extern char commandname[BUFFSIZE];
extern char cursorname[BUFFSIZE];
extern char sqlname[BUFFSIZE];
extern int sqlnum;
extern char *yytext;
extern int hostreferenceCount;
extern char incfilename[BUFFSIZE];
extern char *errorfilename;

extern int flag_external;

extern char *filenameID;

extern struct cb_sql_list *
cb_add_text_list (struct cb_sql_list *list, struct cb_sql_list *targetlist);
extern struct cb_sql_list *
cb_text_list_add (struct cb_sql_list *list, char *text);
char *
cb_host_list_add (struct cb_hostreference_list *list, char *text);
void
cb_res_host_list_add (struct cb_res_hostreference_list *list, char *text);
int
cb_search_list(char *text);
void
cb_set_cursorname(char *text);
void
cb_set_prepname(char *text);
extern struct cb_field *
getfieldbyname(char *name);
extern int
gethostvarianttype(char *name , int *type , int *len , int *scale);

void
outwrite();
FILE *
fopen_or_die(char *filename, const char *mode);
void
_printlog(char *msg);
void
readline(FILE *readfile);
char *
SQcount(int i);
char *
substring(int dexlen,char *wk_str, int flag_end);
void
sql_string(struct cb_exec_list *wk_text);
void
outsqlfiller(struct cb_exec_list *wk_head_p);
void
ppbuff(struct cb_exec_list *list);
extern int
ppoutputparam(struct cb_hostreference_list *host_list);
extern void
_ppoutputparam(char *varface, int type, int digits, int scale);
extern void
ppoutput(char *ppin,char *ppout,struct cb_exec_list *head);
extern void
ppoutput_incfile(char *ppin,char *ppout,struct cb_exec_list *head);
int
check_Dchar(char c);
int
get_host_group_length(struct cb_field *field, int *length);
int
get_host_group_table_info(struct cb_field *field, int *length);

extern int
printerrormsg(char *name, int line, char * code, char *filename);

#endif


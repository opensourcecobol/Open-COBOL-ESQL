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


%defines
%{


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
	  	printmsg("%06d:%s\n", yylineno,msg);
		return 0;
	}


%}

%union {
	char *s;
	long int ld;
	struct cb_sql_list	*l;
	struct cb_hostreference_list *h;
}

%token<s> SELECT
%token<s> SELECTFROM
%token<s> TOKEN
%token<s> CURNAME
%token<s> HOSTTOKEN
%token<s> WORD
%token<s> PICTURE
%token<s> INSERT
%token<s> UPDATE
%token<s> DISCONNECT
%token<s> DELETE
%token<s> EXECUTE
%token<s> OTHERFUNC
%token<s> INTO
%token<ld> NUMERIC
%token END_EXEC
%token EXECSQL
%token EXECSQL_INCLUDE
%token PREPARE
%token FROM
%token DECLARE
%token CURSOR
%token FOR
%token WORKINGBEGIN
%token WORKINGEND
%token HOSTVARIANTBEGIN
%token HOSTVARIANTEND
%token INCLUDE_FILE
%token INCLUDE_SQLCA
%token SQLCA
%token IDENTIFIED_BY
%token COMMIT_WORK
%token ROLLBACK_WORK
%token CONNECT
%token USING
%token OPEN
%token CLOSE
%token FETCH
%token TRAILING
%token COMP_1
%token COMP_2
%token COMP_3
%token COMP_5
%token USAGE
%token SIGN
%token LEADING
%token SEPARATE
%token AT
%token IS
%token ARE
%token VALUE
%token VARYING
%token ALL
%token OCCURS
%token EXTERNAL
%token TIMES
%token CONST
%token WHERECURRENTOF

%type <l> token_list declaresql includesql incfile preparesql execsql
%type <l> opensql selectintosql select insertsql insert updatesql
%type <l> update deletesql delete disconnect disconnectsql othersql
%type <s> host_reference expr dbid prepared_stname

%%
sqlstate_list:
| sqlstate_list sqlstate;
sqlstate: declaresql
| sqlvariantstates
| incfile
| connectsql
| preparesql
| execsql
| opensql
| closesql
| fetchsql
| commitsql
| rollbacksql
| selectintosql
| insertsql
| deletesql
| updatesql
| disconnectsql
| othersql
;

updatesql:
EXECSQL otherdb update
token_list END_EXEC
{
	$$ = cb_add_text_list ($3, $4);
	put_exec_list();
}


update:
UPDATE {$$ = cb_text_list_add (NULL, $1);}


disconnectsql:
EXECSQL otherdb disconnect
token_list END_EXEC
{
	$$ = cb_add_text_list ($3, $4);
	put_exec_list();
}

disconnect:
DISCONNECT {$$ = cb_text_list_add (NULL, $1);}

deletesql:
EXECSQL otherdb delete
token_list END_EXEC
{
	$$ = cb_add_text_list ($3, $4);
	put_exec_list();
}


delete:
DELETE {$$ = cb_text_list_add (NULL, $1);}

insertsql:
EXECSQL otherdb insert
token_list END_EXEC
{
	$$ = cb_add_text_list ($3, $4);
	put_exec_list();
}

insert:
INSERT {$$ = cb_text_list_add (NULL, $1);}
| insert INTO {$$ = cb_text_list_add ($1, $2);}



rollbacksql:
EXECSQL otherdb ROLLBACK_WORK END_EXEC {
	put_exec_list();
}

commitsql:
EXECSQL otherdb COMMIT_WORK END_EXEC {
	put_exec_list();
}



fetchsql:
EXECSQL otherdb fetch INTO res_host_references END_EXEC {
	put_exec_list();
}
fetch:
FETCH expr { cb_set_cursorname($2);}

host_references:
host_reference {cb_host_list_add (host_reference_list, $1);}
| host_references TOKEN
| host_references host_reference {cb_host_list_add (host_reference_list, $2);}

res_host_references:
host_reference {cb_res_host_list_add (res_host_reference_list, $1);}
| res_host_references TOKEN
| res_host_references host_reference {cb_res_host_list_add (res_host_reference_list, $2);}

closesql:
EXECSQL otherdb CLOSE expr END_EXEC {
	cb_set_cursorname($4);
	put_exec_list();
}

opensql:
EXECSQL otherdb OPEN expr END_EXEC {
	cb_set_cursorname($4);
	put_exec_list();
}
| EXECSQL otherdb OPEN expr USING host_references END_EXEC {
	cb_set_cursorname($4);
	put_exec_list();
}

otherdb:
| AT dbid { }

dbid:
HOSTTOKEN {
	cb_set_dbname($1);
}

connectsql:
EXECSQL CONNECT otherdb END_EXEC { put_exec_list(); }
| EXECSQL connect identified otherdb using END_EXEC { put_exec_list(); }
| EXECSQL AT dbid connect END_EXEC { put_exec_list(); }
| EXECSQL connect otherdb END_EXEC { put_exec_list(); }

othersql:
EXECSQL otherdb OTHERFUNC token_list END_EXEC {
	$$ = cb_add_text_list(cb_text_list_add(NULL, $3), $4);
	put_exec_list();
}

connect:
CONNECT host_reference {
	cb_host_list_add (host_reference_list, $2);
}

identified:
IDENTIFIED_BY host_reference {
	cb_host_list_add (host_reference_list, $2);
}

using:
USING host_reference {
	cb_host_list_add (host_reference_list, $2);
}

incfile:
EXECSQL_INCLUDE INCLUDE_FILE END_EXEC{
	put_exec_list();
}

includesql:
EXECSQL_INCLUDE INCLUDE_SQLCA END_EXEC{
	put_exec_list();
}

preparesql:
EXECSQL otherdb PREPARE prepared_stname FROM statement_id END_EXEC {
	put_exec_list();
}

execsql:
EXECSQL otherdb EXECUTE prepared_stname USING host_references END_EXEC {
	put_exec_list();
}
| EXECSQL otherdb EXECUTE prepared_stname END_EXEC {
	put_exec_list();
}

selectintosql:
EXECSQL otherdb SELECT token_list INTO res_host_references SELECTFROM token_list END_EXEC  {
	$$ = cb_add_text_list(cb_text_list_add(NULL, $3), $4);
	cb_add_text_list($$, cb_text_list_add(NULL, $7));
	cb_add_text_list($$, $8);
	put_exec_list();
}
| EXECSQL otherdb SELECT token_list INTO res_host_references END_EXEC  {
	$$ = cb_add_text_list(cb_text_list_add(NULL, $3), $4);
	put_exec_list();
}


declaresql:
EXECSQL otherdb declare_for select END_EXEC { put_exec_list(); }
| EXECSQL otherdb declare_for prepared_stname END_EXEC { put_exec_list(); }

prepared_stname:
TOKEN{ cb_set_prepname($1); }

statement_id:
HOSTTOKEN{ cb_host_list_add (host_reference_list, $1); }

select:
SELECT token_list{ $$ = cb_add_text_list (cb_text_list_add (NULL, $1), $2);}

declare_for:
DECLARE expr CURSOR FOR { cb_set_cursorname($2);}

token_list:
expr				{      $$ = cb_text_list_add (NULL, $1);}
| token_list expr	{      $$ = cb_text_list_add ($1, $2);}
| token_list host_reference   {
	$$ = cb_text_list_add ($1, cb_host_list_add (host_reference_list, $2));
}
| token_list WHERECURRENTOF CURNAME {
	     $$ = cb_text_list_add($1, "WHERE CURRENT OF");
	     cb_set_cursorname($3);
	     $$ = cb_text_list_add($1, cursorname);
}

host_reference:
HOSTTOKEN {}

expr: TOKEN {}
|SELECT{}
|FOR {}
|UPDATE {}

sqlvariantstates: WORKINGBEGIN {
	current_field = NULL;
	description_field = NULL;
	put_exec_list();
}

sqlvariantstate_list
WORKINGEND {
	// check host_variable
	put_exec_list();
}
;

sqlvariantstate_list:
|sqlvariantstate_list incfile
|sqlvariantstate_list includesql
|sqlvariantstate_list declare_for
|sqlvariantstate_list sqlvariantstate '.'
|sqlvariantstate_list HOSTVARIANTBEGIN { put_exec_list(); }
|sqlvariantstate_list HOSTVARIANTEND { put_exec_list(); }
;

sqlvariantstate:
NUMERIC WORD {
	struct cb_field *x;

	x =  cb_build_field_tree( $1, $2 , current_field);
	if( x != NULL)
	{
		if( x->level != 78)
			current_field = x;
	}
}
data_description_clause_sequence
{
	if (description_field == NULL)
		description_field = current_field;
}
|NUMERIC {
	struct cb_field *x;

	x =  cb_build_field_tree( $1, "" , current_field); // regist dummy name
	if( x != NULL){
		if( x->level != 78)
			current_field = x;
	}
}
data_description_clause_sequence
{
	if (description_field == NULL)
		description_field = current_field;
}
;


data_description_clause_sequence:
{}
| data_description_clause_sequence data_description_clause
{}
;

data_description_clause:
picture_clause
| usage_clause
| sign_clause
| occurs_clause
| value_clause
| external_clause
| varying_clause
;

picture_clause:
PICTURE         {  build_picture( $1,current_field); }
;

usage_clause:
usage
| USAGE _is usage
;

usage:
COMP_1			{ current_field->usage = USAGE_FLOAT;   }
| COMP_2			{ current_field->usage = USAGE_DOUBLE; }
| COMP_3			{ current_field->usage = USAGE_PACKED; }
| COMP_5                        { current_field->usage = USAGE_OTHER;  }
| WORD              { current_field->usage = USAGE_OTHER; }
;

varying_clause:
VARYING
{
	if(current_field->pictype != PIC_ALPHANUMERIC &&
		current_field->pictype != PIC_NATIONAL){
		printmsg("parse error: %s specified the data types are not available to VARYING\n",
		       current_field->sname);
		exit(-1);
	}

	var_varying = current_field;
	put_exec_list();
}
;

value_clause: VALUE _is_are _all const_clause {}

const_clause: NUMERIC {}
|WORD {}
|CONST {}

sign_clause:
_sign_is LEADING flag_separate
{
	current_field->sign_leading = SIGNLEADING;
}
| _sign_is TRAILING flag_separate
{

}
;

_sign_is:	 SIGN  {}
| SIGN IS {}
;
flag_separate:
| SEPARATE { current_field->separate = SIGN_SEPARATE; }
;

occurs_clause:
OCCURS NUMERIC _times
{
	current_field->occurs = (int)$2;
}
;

external_clause:
_is EXTERNAL {}
;

_is:		| IS;
_is_are:	| IS | ARE;
_all:           | ALL;
_times:		| TIMES;

%%


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
			printmsg("parse error: %s level should start from 01 or 66 or 77 or 88\n", name);
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


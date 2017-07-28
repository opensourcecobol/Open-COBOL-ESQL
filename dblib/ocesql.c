/*
 * Copyright (C) 2013 Tokyo System House Co.,Ltd.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1,
 * or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; see the file COPYING.LIB.  If
 * not, write to the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <stdbool.h>
#include <unistd.h>
#include <math.h>
#include <malloc.h>
#include "ocdblog.h"
#include "ocdbutil.h"
#include "ocesql.h"

typedef struct sql_var {
	int type; // set OCDB_TYPE_*
	int length; // size
	int power; // power
	void *addr; // address of variable
	void *data; // data(for SetSQLParams)
	char *realdata; // realdata
} SQLVAR;

typedef struct sql_var_list {
	SQLVAR sv;
	struct sql_var_list *next;
} SQLVARLIST;

struct query_info {
	char *pname;  // default
	char *query;
	int nParams;
};

typedef struct cursor_list {
	int connid; // connection id
	char *cname; // default NULL
	char *query; // default NULL
	int nParams; // params for query
	int isOpened; //open flag
	int tuples; //fetched row number
	SQLVARLIST *plist; // parameter list
	struct cursor_list *next;
} CURSORLIST;

static CURSORLIST _cursor_list = {0, NULL, NULL, 0, 0, 0, NULL, NULL};
static SQLVARLIST *_sql_var_lists = NULL;
static SQLVARLIST *_sql_res_var_lists = NULL;
static int _var_lists_length = 0;
static int _res_var_lists_length = 0;


static void sqlca_initialize(struct sqlca_t *);

/* sql var list */
static void init_sql_var_list(void);
static void reset_sql_var_list(void);
static SQLVARLIST * new_sql_var_list(void);
static SQLVARLIST * add_sql_var_list(int, int, int, void *);
static SQLVARLIST * add_sql_res_var_list(int, int, int, void *);
static void clear_sql_var_list(SQLVARLIST *);
static void show_sql_var_list(SQLVARLIST *);
void create_realdata(SQLVAR *);
void create_coboldata_lowvalue(SQLVAR *);
void create_coboldata(SQLVAR *, char *);

/* cursor list */
static CURSORLIST *new_cursor_list(void);
static int add_cursor_list(int, char *, char *, int);
static void clear_cursor_list(CURSORLIST *, int);
static void _clear_cursor_list(CURSORLIST *, int);
static void show_cursor_list(CURSORLIST *);
static CURSORLIST * get_cursor_from_list(char *);

static int _ocesqlExec(struct sqlca_t *, int , char *);
static int _ocesqlExecParams(struct sqlca_t *, int, char *, int);
static int _ocesqlCursorDeclare(struct sqlca_t *, int, char *, char *, int);
static int _ocesqlExecSelectIntoOne(struct sqlca_t *, int, char *, int, int);
static int _ocesqlConnectMain(struct sqlca_t *, char *, char *, char *, char *);
static int _ocesqlConnect(struct sqlca_t *, char *, int, char *, int, char *, int, char *);
static int _ocesqlDisconnect(struct sqlca_t *, int);

static int _ocesqlResolveCONNID(struct sqlca_t *, char *, int);

static void
sqlca_initialize(struct sqlca_t * sqlca){
	memcpy((char *) sqlca, (char *) &sqlca_init, sizeof(struct sqlca_t));
}

int
OCESQLConnect(struct sqlca_t *st, char *user, int userlen, char *passwd, int passwdlen, char *name, int namelen){
	LOG("OCESQLConnect start\n")
	return _ocesqlConnect(st, user, userlen, passwd, passwdlen, name, namelen, NULL);
}

int
_ocesqlConnect(struct sqlca_t *st, char *user, int userlen, char *passwd, int passwdlen, char *name, int namelen, char *atdb){
	char *dbuser, *dbpasswd, *dbname;
	char *tmpuser, *tmppasswd, *tmpname;
	int ret;

	tmpname = ocdb_getenv("OCDB_DB_NAME", NULL);
	tmpuser = ocdb_getenv("OCDB_DB_USER", NULL);
	tmppasswd =  ocdb_getenv("OCDB_DB_PASS", NULL);

	if(((dbname = get_str_without_after_space(oc_strndup(name, namelen))) == NULL)||
			(strlen(dbname)<=0)){
		if(tmpname) dbname = strdup(tmpname);
	}

	if(((dbuser = get_str_without_after_space(oc_strndup(user, userlen))) == NULL)||
			(strlen(dbuser)<=0)){
		if(tmpuser) dbuser = strdup(tmpuser);
	}

	if(((dbpasswd = get_str_without_after_space(oc_strndup(passwd, passwdlen))) == NULL)||
			(strlen(dbpasswd)<=0)){
		if(tmppasswd) dbpasswd = strdup(tmppasswd);
	}

	ret = _ocesqlConnectMain(st, dbname, dbuser, dbpasswd, OCESQL_DEFAULT_DBNAME);


	free(dbname);
	free(dbuser);
	free(dbpasswd);

	return ret;
}

int
_ocesqlConnectMain(struct sqlca_t *st, char *name, char *user, char *passwd, char *conndbname){
	int connectId;
	int autocommit;
	int dbtype;

	char *cencoding = "SJIS";

	dbtype = atoi(ocdb_getenv("OCDB_DB_TYPE", "1"));

	char *dbname = name ? _strdup(name) : NULL;
	char *dbhost = NULL;
	char *dbport = NULL;
	char *real_dbname = NULL;
	char *connstr = NULL;

	sqlca_initialize(st);

	// dbtype check
	if(OCDBCheckType(dbtype)){
		st->sqlcode = OCDB_CONN_FAIL_CONNECT;
		ERRLOG("dbtype invalid: %d.\n", dbtype);
		return 1;
	}

	int id = OCDBResolveCONNID(conndbname);
	if(id > 0){
		if(OCDBStatus(id) == OCDB_CONN_CONNECT_OK){
			LOG("connection cid %s is already connected.\n", conndbname);
			OCDBSetLibErrorStatus(st,OCDB_NO_ERROR);
			return 0;
		}else{
			OCDBFinish(id);
		}
	}

	if (dbname != NULL){
 		char *tmpstr;

		tmpstr = strrchr(dbname, ':');
		if (tmpstr != NULL){
			dbport = _strdup(tmpstr + 1);
			*tmpstr = '\0';
		}
		tmpstr = strrchr(dbname, '@');
		if (tmpstr != NULL){
			dbhost = _strdup(tmpstr + 1);
			*tmpstr = '\0';
		}

		if(strlen(dbname) > 0){
			real_dbname = _strdup(dbname);
		}
	}

	connstr = _alloc(strlen_or_null(real_dbname) + strlen_or_null(dbhost)
                         + strlen_or_null(dbport) + strlen_or_null(user)
			 + strlen_or_null(passwd) + sizeof(" host = port = dbname = user = password ="));

	sprintf(connstr, "%s%s %s%s %s%s %s%s %s%s",
			real_dbname ? "dbname=" : "", real_dbname ? real_dbname : "",
			dbhost ? "host=" : "", dbhost ? dbhost : "",
			dbport ? "port=" : "", dbport ? dbport : "",
			(user && strlen(user) > 0) ? "user=" : "", user ? user : "",
			(passwd && strlen(passwd) > 0) ? "password=" : "", passwd ? passwd : "");

	autocommit = OCDB_AUTOCOMMIT_ON;

	connectId = OCDBConnect(dbtype, connstr, conndbname, autocommit, cencoding);
	LOG("publish connectId: %d.\n", connectId);

	if (dbhost) free(dbhost);
	if (dbport) free(dbport);
	if (dbname) free(dbname);
	if (real_dbname) free(real_dbname);

	if (connectId == OCDB_CONN_FAIL_CONNECT){
		OCDBSetLibErrorStatus(st,OCDB_CONNECT);
		ERRLOG("connection failed. connect params is :%s\n",connstr);
		free(connstr);
		return 1;
	}else if(connectId == INVALID_CONN_ID){
		OCDBSetLibErrorStatus(st,OCDB_OUT_OF_MEMORY);
		ERRLOG("create connect data error\n");
		free(connstr);
		return 1;
	}

	free(connstr);

	OCDBExec(connectId, "BEGIN");
	if(OCDBSetResultStatus(connectId,st) != RESULT_SUCCESS){
		return 1;
	}

	LOG("connection success. connectId = %d, dbname = %s\n", connectId, conndbname);
	return 0;
}

int
OCESQLExec(struct sqlca_t *st, char *query){
	LOG("OCESQLExec start\n");
	int id;
	LOG("SQL:#%s#\n", query);
	id = _ocesqlResolveCONNID(st, OCESQL_DEFAULT_DBNAME, OCESQL_DEFAULT_DBLENG);
	if(id == RESULT_FAILED){
		ERRLOG("connection id is not found.\n");
		OCDBSetLibErrorStatus(st,OCDB_NO_CONN);
		return 1;
	}

	return _ocesqlExec(st, id, query);
}

static int
_ocesqlExec(struct sqlca_t *st, int id, char *query){
	sqlca_initialize(st);
	// check argument
	if(query == NULL || strlen(query) == 0){
		OCDBSetLibErrorStatus(st,OCDB_EMPTY);
		return 1;
	}

	OCDBExec(id, query);
	;
	if(OCDBSetResultStatus(id,st) != RESULT_SUCCESS){
		return 1;
	}

	if(strcmp(query, "COMMIT") == 0|| strcmp(query, "ROLLBACK") == 0){
		clear_cursor_list(&_cursor_list, id);
		OCDBExec(id, "BEGIN");
		if(OCDBSetResultStatus(id,st) != RESULT_SUCCESS){
			return 1;
		}
	}

	return 0;
}

int
OCESQLExecParams(struct sqlca_t *st, char *query, int nParams){
	LOG("OCESQLExecParams start\n");
	int id;
	LOG("SQL:#%s#\n", query);
	id = _ocesqlResolveCONNID(st, OCESQL_DEFAULT_DBNAME, OCESQL_DEFAULT_DBLENG);
	if(id == RESULT_FAILED){
		ERRLOG("connection id is not found.\n");
		OCDBSetLibErrorStatus(st,OCDB_NO_CONN);
		return 1;
	}
	return _ocesqlExecParams(st, id, query, nParams);
}

int
_ocesqlExecParams(struct sqlca_t *st, int id, char *query, int nParams){
	int i;
	char **arr;
	SQLVARLIST *p = _sql_var_lists;

	sqlca_initialize(st);

	// check argument
	if(query == NULL || strlen(query) == 0 || nParams == 0){
		OCDBSetLibErrorStatus(st,OCDB_EMPTY);
		return 1;
	}

	if(_var_lists_length > nParams){
		OCDBSetLibErrorStatus(st,OCDB_TOO_MANY_ARGUMENTS);
		return 1;
	}else if(_var_lists_length < nParams){
		OCDBSetLibErrorStatus(st,OCDB_TOO_FEW_ARGUMENTS);
		return 1;
	}

	if((arr = (char **)malloc(sizeof(char *) * nParams)) == NULL){
		OCDBSetLibErrorStatus(st,OCDB_OUT_OF_MEMORY);
		return 1;
	}


	// set parameters
	for(i=0; i<_var_lists_length; i++, p=p->next){
		arr[i] = p->sv.realdata;
	}

	OCDBExecParams(id, query, nParams, NULL,
				   (const char * const *)arr, NULL, NULL, 0);
	if(OCDBSetResultStatus(id,st) != RESULT_SUCCESS){
		return 1;
	}

	if(strcmp(query, "COMMIT") == 0|| strcmp(query, "ROLLBACK") == 0){
		clear_cursor_list(&_cursor_list, id);
		OCDBExec(id, "BEGIN");
		if(OCDBSetResultStatus(id,st) != RESULT_SUCCESS){
				return 1;
		}
	}
	return 0;
}

int
OCESQLCursorDeclare(struct sqlca_t *st, char *cname, char *query){
	LOG("OCESQLCursorDeclare start\n");
	int id;
	LOG("SQL:#%s#\n", query);
	id = _ocesqlResolveCONNID(st, OCESQL_DEFAULT_DBNAME, OCESQL_DEFAULT_DBLENG);
	if(id == RESULT_FAILED){
		ERRLOG("connection id is not found.\n");
		OCDBSetLibErrorStatus(st,OCDB_NO_CONN);
		return 1;
	}

	return _ocesqlCursorDeclare(st, id, cname, query, 0);
}

int
OCESQLCursorDeclareParams(struct sqlca_t *st, char *cname, char *query, int nParams){
	LOG("OCESQLCursorDeclareParams start\n");
	int id;
	LOG("SQL:#%s#\n", query);
	id = _ocesqlResolveCONNID(st, OCESQL_DEFAULT_DBNAME, OCESQL_DEFAULT_DBLENG);
	if(id == RESULT_FAILED){
		ERRLOG("connection id is not found.\n");
		OCDBSetLibErrorStatus(st,OCDB_NO_CONN);
		return 1;
	}
	// check argument
	if(nParams == 0){
		OCDBSetLibErrorStatus(st,OCDB_EMPTY);
		return 1;
	}

	if(_var_lists_length > nParams){
		OCDBSetLibErrorStatus(st,OCDB_TOO_MANY_ARGUMENTS);
		return 1;
	}else if(_var_lists_length < nParams){
		OCDBSetLibErrorStatus(st,OCDB_TOO_FEW_ARGUMENTS);
		return 1;
	}

	return _ocesqlCursorDeclare(st, id, cname, query, nParams);
}

int
_ocesqlCursorDeclare(struct sqlca_t *st, int id, char *cname, char *query, int nParams){
	int i;
	int res;
	char **arr;

	sqlca_initialize(st);

	// check argument
	if(cname == NULL || strlen(cname) == 0 ||
	   query == NULL || strlen(query) == 0 ){
		OCDBSetLibErrorStatus(st,OCDB_EMPTY);
		return 1;
	}
	res =add_cursor_list(id, cname, query, nParams);
	if(res == RESULT_FAILED){
		OCDBSetLibErrorStatus(st,OCDB_WARNING_PORTAL_EXISTS);
		return 1;
	}else if(res == RESULT_ERROR){
		OCDBSetLibErrorStatus(st,OCDB_OUT_OF_MEMORY);
		return 1;
	}
	return 0;
}

int
OCESQLCursorOpen(struct sqlca_t *st, char *cname){
	LOG("OCESQLCursorOpen start\n");
	CURSORLIST *cursor;

	sqlca_initialize(st);

	// check argument
	LOG("cname=#%s#\n", cname);
	if(cname == NULL || strlen(cname) == 0){
		OCDBSetLibErrorStatus(st,OCDB_EMPTY);
		return 1;
	}

	// search cursor
	cursor = get_cursor_from_list(cname);
	if(cursor == NULL){
		ERRLOG("cursor %s not registered.\n", cname);
		OCDBSetLibErrorStatus(st,OCDB_WARNING_UNKNOWN_PORTAL);
		return 1;
	}

	if(cursor->query == NULL){
		ERRLOG("cursor %s not registered.\n", cname);
		OCDBSetLibErrorStatus(st,OCDB_EMPTY);
		return 1;
	}

	if(cursor->isOpened){
		LOG("cursor %s alredy opened.\n", cname)
		OCDBCursorClose(cursor->connid, cname);
		if(OCDBSetResultStatus(cursor->connid,st) != RESULT_SUCCESS){
			ERRLOG("cursor %s close failed.\n", cname);
			return 1;
		}
		cursor->isOpened = 0;
	}

	// DECLARE CURSOR
	if(cursor->nParams > 0){
		int i;
		char **arr;
		SQLVARLIST *p = cursor->plist;

		if((arr = (char **)malloc(sizeof(char *) * cursor->nParams)) == NULL){
			ERRLOG("memory allocation failed.\n");
			OCDBSetLibErrorStatus(st,OCDB_OUT_OF_MEMORY);
			return 1;
		}

		// set parameters
		for(i=0; i<cursor->nParams; i++, p=p->next){
			create_realdata(&p->sv);
			arr[i] = p->sv.realdata;
			LOG("params[%d]:#%s#\n",i, p->sv.realdata)
		}

		OCDBCursorDeclareParams(cursor->connid, cursor->cname, cursor->query,
					cursor->nParams, NULL, (const char * const *)arr,
					NULL, NULL, 0, OCDB_CURSOR_WITH_HOLD_OFF);

		free(arr);
	}else {
		OCDBCursorDeclare(cursor->connid, cursor->cname,
				  cursor->query, OCDB_CURSOR_WITH_HOLD_OFF);
	}
	OCDBSetResultStatus(cursor->connid,st);
	if(OCDBSetResultStatus(cursor->connid,st) != RESULT_SUCCESS){
		return 1;
	}

	// OPEN CURSOR
	OCDBCursorOpen(cursor->connid, cursor->cname);
	if(OCDBSetResultStatus(cursor->connid,st) != RESULT_SUCCESS){
		return 1;
	}
	cursor->isOpened = 1;
	return 0;
}

int
OCESQLCursorFetchOne(struct sqlca_t *st, char *cname){
	LOG("OCESQLCursorFetchOne start\n");
	int i;
	int id;
	int ntuples;
	CURSORLIST *cursor;
	SQLVARLIST *p = _sql_res_var_lists;

	sqlca_initialize(st);

	// check argument
	if(cname == NULL || strlen(cname) == 0){
		OCDBSetLibErrorStatus(st,OCDB_EMPTY);
		return 1;
	}
	LOG("cname:%s\n",cname)
	cursor = get_cursor_from_list(cname);
	if(cursor == NULL){
		ERRLOG("cursor %s not registered.\n", cname);
		OCDBSetLibErrorStatus(st,OCDB_WARNING_UNKNOWN_PORTAL);
		return 1;
	}
	id = cursor->connid;


	// exec sql
	OCDBCursorFetchOne(id, cname, OCDB_READ_NEXT);
	if(OCDBSetResultStatus(id,st) != RESULT_SUCCESS){
		return 1;
	}
	// check numtuples
	ntuples = OCDBNtuples(id);
	if(ntuples < 1){
		st->sqlerrd[2] = cursor->tuples;
		LOG("TUPLES NODATA\n");
		return 0;
	} else if(ntuples > 1) {
		OCDBSetLibErrorStatus(st,OCDB_TOO_MANY_MATCHES);
		return 1;
	} else {
		// set params
		char *retstr;
		for(i=0; i< _res_var_lists_length; i++, p = p->next){
			retstr = OCDBGetvalue(id, 0, i);
			create_coboldata(&p->sv, retstr);
		}
	}

	cursor->tuples += st->sqlerrd[2];
	st->sqlerrd[2] = cursor->tuples;
	return 0;
}

int
OCESQLCursorClose(struct sqlca_t *st, char *cname){
	LOG("OCESQLCursorClose start\n")
	CURSORLIST *cursor;
	int id;

	sqlca_initialize(st);

	// check argument
	if(cname == NULL || strlen(cname) == 0){
		OCDBSetLibErrorStatus(st,OCDB_EMPTY);
		return 1;
	}

	LOG("id:%d, cname:%s\n",id,cname);

	cursor = get_cursor_from_list(cname);
	if(cursor == NULL){
		ERRLOG("cursor %s not registered.\n", cname);
		OCDBSetLibErrorStatus(st,OCDB_WARNING_UNKNOWN_PORTAL);
		return 1;
	}
	if(!cursor->isOpened){
		LOG("cursor %s not opened.\n", cname)
		return 0;
	}

	id = cursor->connid;

	OCDBCursorClose(id, cname);
	if(OCDBSetResultStatus(id,st) != RESULT_SUCCESS){
		return 1;
	}

	cursor->isOpened = 0;

	return 0;
}

int
OCESQLExecSelectIntoOne(struct sqlca_t *st, char *query, int nParams, int nResParams){
	LOG("OCESQLExecSelectIntoOne start\n");
	int id;
	LOG("SQL:#%s#\n", query);
	id = _ocesqlResolveCONNID(st, OCESQL_DEFAULT_DBNAME, OCESQL_DEFAULT_DBLENG);
	if(id == RESULT_FAILED){
		ERRLOG("connection id is not found.\n");
		OCDBSetLibErrorStatus(st,OCDB_NO_CONN);
		return 1;
	}

	return _ocesqlExecSelectIntoOne(st, id, query, nParams, nResParams);
}

int
_ocesqlExecSelectIntoOne(struct sqlca_t *st, int id, char *query, int nParams, int nResParams){

	int i;
	int fields = 0;
	SQLVARLIST *p = _sql_res_var_lists;

	// check argument
	if(query == NULL || strlen(query) == 0){
		ERRLOG("ARGUMENT ERROR\n")
		OCDBSetLibErrorStatus(st,OCDB_EMPTY);
		return 1;
	}

	if(nParams > 0){
		if(_ocesqlExecParams(st, id, query, nParams) != 0) return 1;
	}else{
		if(_ocesqlExec(st, id, query) != 0) return 1;
	}

	fields = OCDBNfields(id);
	if(fields != nResParams){
		ERRLOG("ResParams(%d) and fields(%d) are different\n",nResParams, fields)
		OCDBSetLibErrorStatus(st,OCDB_OUT_OF_MEMORY);
		return 1;
	}

	// check numtuples
	if(OCDBNtuples(id) < 1){
		LOG("TUPLES NODATA\n")
	} else {
		// set params
		char *retstr;
		for(i=0; i< _res_var_lists_length; i++, p = p->next){
			if(i>=fields)
				break;
			retstr = OCDBGetvalue(id, 0, i);
			create_coboldata(&p->sv, retstr);
		}
	}
	return 0;
}

int
OCESQLDisconnect(struct sqlca_t *st){
	LOG("OCESQLDisconnect start\n");
	int id;
	id = _ocesqlResolveCONNID(st, OCESQL_DEFAULT_DBNAME, OCESQL_DEFAULT_DBLENG);
	if(id == RESULT_FAILED){
		ERRLOG("connection id is not found.\n");
		OCDBSetLibErrorStatus(st,OCDB_NO_CONN);
		return 1;
	}

	return _ocesqlDisconnect(st, id);
}

static int
_ocesqlDisconnect(struct sqlca_t *st, int id){
	OCDBFinish(id);
	return 0;
}

int
OCESQLStartSQL(void){
	LOG("#begin\n");
	init_sql_var_list();
	LOG("#end\n");
	return 0;
}

int
OCESQLSetSQLParams(int type, int length, int scale, void *addr){
	if(type < OCDB_TYPE_MIN || type > OCDB_TYPE_MAX){
		ERRLOG("invalide arugument 'type': %d\n", type);
		return 1;
	}

	if(length < 0){
		ERRLOG("invalide argument 'length': %d\n", length);
		return 1;
	}

	if(!addr){
		ERRLOG("invalide argument addr is NULL\n");
		return 1;
	}

	if(add_sql_var_list(type ,length, scale, addr) == NULL){
		ERRLOG("fail to add SQLVARLIST\n");
		return 1;
	}
	return 0;
}

int
OCESQLSetResultParams(int type, int length, int scale, void *addr){
	if(type < OCDB_TYPE_MIN || type > OCDB_TYPE_MAX){
		ERRLOG("invalide arugument 'type': %d\n", type);
		return 1;
	}

	if(length < 0){
		ERRLOG("invalide argument 'length': %d\n", length);
		return 1;
	}

	if(!addr){
		ERRLOG("invalide argument addr is NULL\n");
		return 1;
	}

	if(add_sql_res_var_list(type ,length, scale, addr) == NULL){
		ERRLOG("fail to add SQLVARLIST\n");
		return 1;
	}
	return 0;
}

int
OCESQLEndSQL(void){
	LOG("#debug start dump var_list\n");
	show_sql_var_list(_sql_var_lists);
	LOG("#debug start dump res_list\n");
	show_sql_var_list(_sql_res_var_lists);
	LOG("#debug end dump list\n");

	clear_sql_var_list(_sql_var_lists);
	reset_sql_var_list();
	return 0;
}

static void
init_sql_var_list(void){
	if(_sql_var_lists != NULL){
		clear_sql_var_list(_sql_var_lists);
	}
	if(_sql_res_var_lists != NULL){
		clear_sql_var_list(_sql_res_var_lists);
	}
	reset_sql_var_list();

	if((_sql_var_lists = new_sql_var_list()) == NULL){
		ERRLOG("cannot initialize SQLVARLIST\n");
		return;
	}

	if((_sql_res_var_lists = new_sql_var_list()) == NULL){
		ERRLOG("cannot initialize SQLVARLIST\n");
		return;
	}

	return;
}

static void
reset_sql_var_list(void){
	_sql_var_lists = NULL;
	_sql_res_var_lists = NULL;
	_var_lists_length = 0;
	_res_var_lists_length = 0;
	return;
}

static SQLVARLIST *
new_sql_var_list(void){
	SQLVARLIST *p;
	p = (SQLVARLIST *)malloc(sizeof(SQLVARLIST));
	if(p != NULL){
		// initialize
		p->sv.type = 0;
		p->sv.length = 0;
		p->sv.power = 0;
		p->sv.addr = NULL;
		p->sv.data = NULL;
		p->sv.realdata = NULL;
		p->next = NULL;
	}

	return p;
}

static SQLVARLIST *
add_sql_var_list(int type , int length, int power, void *addr){

	SQLVARLIST *p = _sql_var_lists;

	if(_sql_var_lists == NULL){
		ERRLOG("_sql_var_lists has not been initialized\n");
		return NULL;
	}

	while(p->next != NULL){
		p = p->next;
	}

	if((p->next = new_sql_var_list()) == NULL){
		ERRLOG("cannot generate new SQLVARLIST\n");
		return NULL;
	}

	p->sv.type = type;
	p->sv.length = length;
	p->sv.power = power;
	p->sv.addr = addr;


	create_realdata(&p->sv);
	_var_lists_length++;

	return p;
}

static SQLVARLIST *
add_sql_res_var_list(int type , int length, int power, void *addr){
	SQLVARLIST *p = _sql_res_var_lists;

	if(_sql_res_var_lists == NULL){
		ERRLOG("_sql_var_lists has not been initialized\n");
		return NULL;
	}

	while(p->next != NULL){
		p = p->next;
	}

	if((p->next = new_sql_var_list()) == NULL){
		ERRLOG("cannot generate new SQLVARLIST\n");
		return NULL;
	}

	p->sv.type = type;
	p->sv.length = length;
	p->sv.power = power;
	p->sv.addr = addr;


	create_realdata(&p->sv);
	_res_var_lists_length++;

	return p;
}

void create_realdata(SQLVAR *sv){
	int type = sv->type;
	int length = sv->length;
	int power = sv->power;
	void *addr = sv->addr;

	SQLVAR sv_tmp;
	sv_tmp.type = type;
	sv_tmp.length = length;
	sv_tmp.power = power;
	sv_tmp.addr = addr;

	switch(sv_tmp.type){
	case OCDB_TYPE_UNSIGNED_NUMBER:
	{
		int realdata_length;

		sv_tmp.data = (char *)calloc(sv_tmp.length + TERMINAL_LENGTH, sizeof(char));
		memcpy(sv_tmp.data, sv_tmp.addr, sv_tmp.length);

		/* set real data */
		realdata_length = sv_tmp.length;
		// 小数点
		if(sv_tmp.power < 0){
			realdata_length++;
		}
		sv_tmp.realdata = (char *)calloc(realdata_length + TERMINAL_LENGTH, sizeof(char));
		memcpy(sv_tmp.realdata, sv_tmp.data, realdata_length);

		if(sv_tmp.power < 0){
			insert_decimal_point(sv_tmp.realdata, realdata_length, sv_tmp.power);
		}

		LOG("%d %d->%d#data:%s#realdata:%s\n", sv_tmp.type, length,
			sv_tmp.length, sv_tmp.data, sv_tmp.realdata);
		break;
	}
	case OCDB_TYPE_SIGNED_NUMBER_TC:
	{
		int realdata_length;

		sv_tmp.data = (char *)calloc(sv_tmp.length + TERMINAL_LENGTH, sizeof(char));
		memcpy(sv_tmp.data, sv_tmp.addr, sv_tmp.length);

		/* set real data */
		// 符号部分
		realdata_length = SIGN_LENGTH + sv_tmp.length;
		// 小数点
		if(sv_tmp.power < 0){
			realdata_length++;
		}

		sv_tmp.realdata = (char *)calloc(realdata_length + TERMINAL_LENGTH, sizeof(char));
		memcpy(sv_tmp.realdata + SIGN_LENGTH, sv_tmp.data, sv_tmp.length);

		// 符号は最後の1桁で判別
		if(type_tc_is_positive(sv_tmp.realdata + SIGN_LENGTH + sv_tmp.length - 1)){
			sv_tmp.realdata[0] = '+';
		} else {
			sv_tmp.realdata[0] = '-';
		}

		if(sv_tmp.power < 0){
			insert_decimal_point(sv_tmp.realdata, realdata_length, sv_tmp.power);
		}

		LOG("%d %d->%d#data:%s#realdata:%s\n", sv_tmp.type, length,
			sv_tmp.length, sv_tmp.data, sv_tmp.realdata);
		break;
	}
	case OCDB_TYPE_SIGNED_NUMBER_LS:
	{
		int realdata_length;

		sv_tmp.data = (char *)calloc(SIGN_LENGTH +  sv_tmp.length + TERMINAL_LENGTH, sizeof(char));
		memcpy(sv_tmp.data, sv_tmp.addr, sv_tmp.length + SIGN_LENGTH);

		/* set real data */
		// 符号部分
		realdata_length = SIGN_LENGTH + sv_tmp.length;
		// 小数点
		if(sv_tmp.power < 0){
			realdata_length++;
		}
		sv_tmp.realdata = (char *)calloc(realdata_length + TERMINAL_LENGTH, sizeof(char));
		memcpy(sv_tmp.realdata, sv_tmp.data, realdata_length);

		if(sv_tmp.power < 0){
			insert_decimal_point(sv_tmp.realdata, realdata_length, sv_tmp.power);
		}

		LOG("%d %d->%d#data:%s#realdata:%s\n", sv_tmp.type, length,
			sv_tmp.length, sv_tmp.data, sv_tmp.realdata);
		break;
	}
	case OCDB_TYPE_UNSIGNED_NUMBER_PD:
	{
		double dlength;
		int skip_first;
		int realdata_length;

		dlength = ceil(((double)sv_tmp.length + 1)/2);
		skip_first = (sv_tmp.length + 1) % 2; // 1 -> skip first 4 bits
		sv_tmp.data = (char *)calloc((int)dlength + TERMINAL_LENGTH, sizeof(char));
		memcpy(sv_tmp.data, addr, (int)dlength);

		/* set real data */
		int i;
		int index = 0;
		char *ptr;
		unsigned char tmp;
		unsigned char ubit = 0xF0;
		unsigned char lbit = 0x0F;

		realdata_length = sv_tmp.length;
		// 小数点
		if(sv_tmp.power < 0){
			realdata_length++;
		}

		sv_tmp.realdata = (char *)calloc(realdata_length + TERMINAL_LENGTH, sizeof(char));
		for(i=0; i<dlength; i++){
			char val[2];

			ptr = sv_tmp.data + i * sizeof(char);
			tmp = (unsigned char)*ptr;

			if(i!=0 || !skip_first){
				sprintf(val, "%d", (tmp & ubit) >> 4);
				sv_tmp.realdata[index] = val[0];
				index++;
			}
			if(i != dlength - 1){
				sprintf(val, "%d", tmp & lbit);
				sv_tmp.realdata[index] = val[0];
				index++;
			}
		}

		if(sv_tmp.power < 0){
			insert_decimal_point(sv_tmp.realdata, realdata_length, sv_tmp.power);
		}

		LOG("%d %d->%d#data:%s#realdata:%s#\n", sv_tmp.type, length,
			sv_tmp.length, sv_tmp.data, sv_tmp.realdata);
		break;
	}
	case OCDB_TYPE_SIGNED_NUMBER_PD:
	{
		double dlength, dlengthbuf;
		int skip_first;
		int realdata_length;

		dlength = ceil(((double)sv_tmp.length + 1)/2);
		skip_first = (sv_tmp.length + 1) % 2; // 1 -> skip first 4 bits
		sv_tmp.data = (char *)calloc((int)dlength + TERMINAL_LENGTH, sizeof(char));
		memcpy(sv_tmp.data, addr, (int)dlength);

		/* set real data */
		int i;
		int index = SIGN_LENGTH;
		char *ptr;
		unsigned char tmp;
		unsigned char ubit = 0xF0;
		unsigned char lbit = 0x0F;

		// 符号部分
		realdata_length = SIGN_LENGTH + sv_tmp.length;
		// 小数点
		if(sv_tmp.power < 0){
			realdata_length++;
		}

		sv_tmp.realdata = (char *)calloc(realdata_length + TERMINAL_LENGTH, sizeof(char));
		for(i=0; i<dlength; i++){
			char val[2];

			ptr = sv_tmp.data + i * sizeof(char);
			tmp = (unsigned char)*ptr;

			if(i!=0 || !skip_first){
				sprintf(val, "%d", (tmp & ubit) >> 4);
				sv_tmp.realdata[index] = val[0];
				index++;
			}
			if(i != dlength - 1){
				sprintf(val, "%d", tmp & lbit);
				sv_tmp.realdata[index] = val[0];
				index++;
			} else {
				if((tmp & lbit) == 0x0C){
					sv_tmp.realdata[0] = '+';
				} else {
					sv_tmp.realdata[0] = '-';
				}
			}
		}

		if(sv_tmp.power < 0){
			insert_decimal_point(sv_tmp.realdata, realdata_length, sv_tmp.power);
		}

		LOG("%d %d->%d#data:%s#realdata:%s\n", sv_tmp.type, length,
			sv_tmp.length, sv_tmp.data, sv_tmp.realdata);
		break;
	}
	case OCDB_TYPE_JAPANESE:
		length = length * 2;
		/* no break */
	case OCDB_TYPE_ALPHANUMERIC:
		sv_tmp.data = (char *)calloc(length + TERMINAL_LENGTH, sizeof(char));
		sv_tmp.realdata = (char *)calloc(length + TERMINAL_LENGTH, sizeof(char));
		memcpy(sv_tmp.data, (char *)addr, length);
		memcpy(sv_tmp.realdata, (char *)addr, length);
		LOG("%d %d->%d#%s#%s#\n", sv_tmp.type, length, sv_tmp.length, (char *)sv_tmp.data, (char *)sv_tmp.realdata);
		break;
	default:
		sv_tmp.data = (char *)calloc(length + TERMINAL_LENGTH, sizeof(char));
		sv_tmp.realdata = (char *)calloc(length + TERMINAL_LENGTH, sizeof(char));

		memcpy(sv_tmp.data, (char *)addr, length);
		memcpy(sv_tmp.realdata, (char *)addr, length);
		LOG("%d %d->%d#%s#%s#\n", sv_tmp.type, length, sv_tmp.length, (char *)sv_tmp.data, (char *)sv_tmp.realdata);
		break;
	}
	if(sv->realdata){
		free(sv->realdata);
		sv->realdata = NULL;
	}
	sv->realdata = strdup(sv_tmp.realdata);
	sv->data = strdup(sv_tmp.data);
}

void create_coboldata_lowvalue(SQLVAR *sv){
	void *addr = sv->addr;

	memset(addr,0,sv->length);

	return;
}

void create_coboldata(SQLVAR *sv, char *retstr){
	void *addr = sv->addr;

	switch(sv->type){
	case OCDB_TYPE_UNSIGNED_NUMBER:
	{
		char *ptr;

		int fillzero;
		int zcount;
		char *final;

		// fill zero
		final = (char *)calloc(sv->length + TERMINAL_LENGTH, sizeof(char));

		// before decimal point
		int beforedp = 0;
		for(ptr = retstr; *ptr != '\0'; ptr++){
			if(*ptr == '.'){
				break;
			} else {
				beforedp++;
			}
		}

		fillzero = sv->length - beforedp + sv->power;
		for(zcount = 0; zcount < fillzero; zcount++){
			strcat(final, "0");
		}
		memcpy(final + fillzero, retstr, beforedp);

		if(sv->power < 0){
			int afterdp = 0;

			if(*ptr != '\0'){
				ptr++;

				// after decimal point
				for(; *ptr != '\0'; ptr++){
					afterdp++;
				}

				// fill zero
				memcpy(final + fillzero + beforedp,
					   retstr + beforedp + DECIMAL_LENGTH, afterdp);
			}

			fillzero = - sv->power - afterdp;
			for(zcount = 0; zcount < fillzero; zcount++){
				strcat(final, "0");
			}
		}

		memcpy(addr, final, sv->length);
		free(final);
		break;
	}
	case OCDB_TYPE_SIGNED_NUMBER_TC:
	{
		char *value;
		char *ptr;
		int is_negative = false;

		int fillzero;
		int zcount;
		char *final;
		int final_length;

		// fill zero
		final = (char *)calloc(sv->length + TERMINAL_LENGTH, sizeof(char));

		if(retstr[0] == '-'){
			is_negative = true;
			value = retstr + 1;
		} else {
			value = retstr;
		}

		// before decimal point
		int beforedp = 0;
		for(ptr = value; *ptr != '\0'; ptr++){
			if(*ptr == '.'){
				break;
			} else {
				beforedp++;
			}
		}

		fillzero = sv->length - beforedp + sv->power;
		for(zcount = 0; zcount < fillzero; zcount++){
			strcat(final, "0");
		}
		memcpy(final + fillzero, value, beforedp);

		if(sv->power < 0){
			int afterdp = 0;

			if(*ptr != '\0'){
				ptr++;

				// after decimal point
				for(; *ptr != '\0'; ptr++){
					afterdp++;
				}
				memcpy(final + fillzero + beforedp, value +
					   beforedp + DECIMAL_LENGTH, afterdp);
			}

			// fill zero
			fillzero = - sv->power - afterdp;
			for(zcount = 0; zcount < fillzero; zcount++){
				strcat(final, "0");
			}
		}

		final_length = strlen(final);
		if(is_negative){
			int index = *(final + (final_length - 1)) - '0';
			final[final_length - 1] = type_tc_negative_final_number[index];
		}

		memcpy(addr, final, sv->length + SIGN_LENGTH);
		free(final);
		break;
	}
	case OCDB_TYPE_SIGNED_NUMBER_LS:
	{
		char *value;
		char *ptr;

		int fillzero;
		int zcount;
		char *final;

		// fill zero
		final = (char *)calloc(SIGN_LENGTH +  sv->length + TERMINAL_LENGTH, sizeof(char));

		if(retstr[0] == '-'){
			final[0] = '-';
			value = retstr + 1;
		} else {
			final[0] = '+';
			value = retstr;
		}

		// before decimal point
		int beforedp = 0;
		for(ptr = value; *ptr != '\0'; ptr++){
			if(*ptr == '.'){
				break;
			} else {
				beforedp++;
			}
		}

		fillzero = sv->length - beforedp + sv->power;
		for(zcount = 0; zcount < fillzero; zcount++){
			strcat(final, "0");
		}
		memcpy(final + SIGN_LENGTH + fillzero, value, beforedp);

		if(sv->power < 0){
			int afterdp = 0;

			if(*ptr != '\0'){
				ptr++;

				// after decimal point
				for(; *ptr != '\0'; ptr++){
					afterdp++;
				}

				// fill zero
				memcpy(final + SIGN_LENGTH + fillzero + beforedp,
					   value + beforedp + DECIMAL_LENGTH, afterdp);
			}

			fillzero = - sv->power - afterdp;
			for(zcount = 0; zcount < fillzero; zcount++){
				strcat(final, "0");
			}
		}

		memcpy(addr, final, sv->length + SIGN_LENGTH);
		free(final);
		break;
	}
	case OCDB_TYPE_UNSIGNED_NUMBER_PD:
	{
		char *value = retstr;
		char *ptr;
		int is_negative = false;

		int fillzero;
		int zcount;
		char *pre_final;
		char *final;

		double dlength;
		int skip_first;
		int i;
		unsigned char ubit = 0xF0;
		unsigned char lbit = 0x0F;

		dlength = ceil(((double)sv->length + 1)/2);
		skip_first = (sv->length + 1) % 2; // 1 -> skip first 4 bits

		pre_final = (char *)calloc((int)dlength + TERMINAL_LENGTH, sizeof(char));

		// before decimal point
		int beforedp = 0;
		for(ptr = value; *ptr != '\0'; ptr++){
			if(*ptr == '.'){
				break;
			} else {
				beforedp++;
			}
		}

		fillzero = sv->length - beforedp + sv->power;
		for(zcount = 0; zcount < fillzero; zcount++){
			strcat(pre_final, "0");
		}
		memcpy(pre_final + fillzero, value, beforedp);

		if(sv->power < 0){
			int afterdp = 0;

			if(*ptr != '\0'){
				ptr++;

				// after decimal point
				for(; *ptr != '\0'; ptr++){
					afterdp++;
				}
				memcpy(pre_final + fillzero + beforedp,
					   value + beforedp + DECIMAL_LENGTH, afterdp);
			}

			// fill zero
			fillzero = - sv->power - afterdp;
			for(zcount = 0; zcount < fillzero; zcount++){
				strcat(pre_final, "0");
			}
		}

		// format setting
		final = (char *)calloc((int)dlength + TERMINAL_LENGTH, sizeof(char));
		ptr = pre_final;
		for(i=0; i<dlength; i++){
			unsigned char vubit = 0x00;
			unsigned char vlbit = 0x00;

			if(i == 0 && skip_first){
				vubit = 0x00;
			} else {
				vubit = (*ptr) << 4;
				vubit = vubit & ubit;
				ptr++;
			}

			if(i != dlength - 1){
				vlbit = *ptr;
				vlbit = vlbit & lbit;
				ptr++;
			} else {
				vlbit = 0x0F;
			}

			final[i] = vubit | vlbit;
		}

		memcpy(addr, final, (int)dlength);
		free(pre_final);
		free(final);
		break;
	}
	case OCDB_TYPE_SIGNED_NUMBER_PD:
	{
		char *value;
		char *ptr;
		int is_negative = false;

		int fillzero;
		int zcount;
		char *pre_final;
		char *final;

		double dlength;
		int skip_first;
		int i;
		unsigned char ubit = 0xF0;
		unsigned char lbit = 0x0F;

		dlength = ceil((double)(sv->length + 1)/2);
		skip_first = (sv->length + 1) % 2; // 1 -> skip first 4 bits

		if(retstr[0] == '-'){
			is_negative = true;
			value = retstr + 1;
		} else {
			value = retstr;
		}

		pre_final = (char *)calloc((int)dlength + TERMINAL_LENGTH, sizeof(char));

		// before decimal point
		int beforedp = 0;
		for(ptr = value; *ptr != '\0'; ptr++){
			if(*ptr == '.'){
				break;
			} else {
				beforedp++;
			}
		}

		fillzero = sv->length - beforedp + sv->power;
		for(zcount = 0; zcount < fillzero; zcount++){
			strcat(pre_final, "0");
		}
		memcpy(pre_final + fillzero, value, beforedp);

		if(sv->power < 0){
			int afterdp = 0;

			if(*ptr != '\0'){
				ptr++;

				// after decimal point
				for(; *ptr != '\0'; ptr++){
					afterdp++;
				}
				memcpy(pre_final + fillzero + beforedp,
					   value + beforedp + DECIMAL_LENGTH, afterdp);
			}

			// fill zero
			fillzero = - sv->power - afterdp;
			for(zcount = 0; zcount < fillzero; zcount++){
				strcat(pre_final, "0");
			}
		}

		// format setting
		final = (char *)calloc((int)dlength + TERMINAL_LENGTH, sizeof(char));
		ptr = pre_final;
		for(i=0; i<dlength; i++){
			unsigned char vubit = 0x00;
			unsigned char vlbit = 0x00;

			if(i == 0 && skip_first){
				vubit = 0x00;
			} else {
				vubit = (*ptr) << 4;
				vubit = vubit & ubit;
				ptr++;
			}

			if(i != dlength - 1){
				vlbit = *ptr;
				vlbit = vlbit & lbit;
				ptr++;
			} else {
				if(is_negative){
					vlbit = 0x0D;
				} else {
					vlbit = 0x0C;
				}
			}

			final[i] = vubit | vlbit;
		}

		memcpy(addr, final, (int)dlength);
		free(pre_final);
		free(final);
		break;
	}
	case OCDB_TYPE_ALPHANUMERIC:
		// 文字の長さだけメモリコピー
		if(strlen(retstr) >= sv->length){
			memcpy(addr, retstr, sv->length);
		}else{
			memset(addr,' ',sv->length);
			memcpy(addr,retstr,strlen(retstr));
		}
		break;
	case OCDB_TYPE_JAPANESE:
		// 文字の長さだけメモリコピー
		if(strlen(retstr) >= sv->length*2){
			memcpy(addr, retstr, sv->length*2);
		}else{
			int i;
			char *tmp = (char *)addr;
			for(i=0;i+1<sv->length*2;i=i+2){
				tmp[i] = 0x81;
				tmp[i+1] = 0x40;
			}
			memcpy(addr,retstr,strlen(retstr));
		}
		break;
	default:
		break;
	}
#ifndef NDEBUG
	char *tmp;
	if(sv->type == OCDB_TYPE_JAPANESE){
		tmp = oc_strndup((char *)addr,sv->length*2);
	}else{
		tmp = oc_strndup((char *)addr,sv->length);
	}
	LOG("%d %d#%s#%s#\n", sv->type, sv->length, retstr, tmp);
	if(tmp) free(tmp);
#endif
}

static void show_sql_var_list(SQLVARLIST *p){
	if(p != NULL){
		LOG("%p %d %d %d %p %p\n", p, p->sv.type, p->sv.length, p->sv.power,
			p->sv.addr, p->next);
		show_sql_var_list(p->next);
	}
}


/*
 * <Function name>
 *   clear_sql_var_list
 *
 * <Outline>
 *   埋め込みSQLリストのメモリ解放
 *
 * <Input>
 *   @SQLVARLIST *
 */
static void
clear_sql_var_list(SQLVARLIST *p){
	if(p != NULL){
		clear_sql_var_list(p->next);
		if(p->sv.data)
			free(p->sv.data);
		if(p->sv.realdata)
			free(p->sv.realdata);
		free(p);
	}
}

static int
_ocesqlResolveCONNID(struct sqlca_t *st, char *atdb, int atdblen){
	char *cid = NULL;
	int id;

	cid = get_str_without_after_space(oc_strndup(atdb, atdblen));

	id = OCDBResolveCONNID(cid);
	if(id == RESULT_FAILED){
		ERRLOG("connection name %s is not found in connection list.\n", cid);
		return RESULT_FAILED;
	}
	free(cid);

	return id;
}

static CURSORLIST *
new_cursor_list(void){
	CURSORLIST *p;
	p = (CURSORLIST *)malloc(sizeof(CURSORLIST));
	if(p != NULL){
		// initialize
		p->connid = 0;
		p->cname = NULL;
		p->query = NULL;
		p->nParams = 0;
		p->next = NULL;
	}

	return p;
}

static int
add_cursor_list(int id, char *cname, char *query, int nParams){
	CURSORLIST *p = &_cursor_list;
	int isExist = 0;

	while(p->next != NULL){
		p = p->next;
		// duplication check
		if(strcmp(cname, p->cname) == 0){
			if(p->isOpened){
				ERRLOG("cursor name '%s' already registered and opened.\n", cname);
				return RESULT_FAILED;
			}
			isExist = 1;
			LOG("cursor name '%s' already registered.\n", cname);
			break;
		}
	}
	if(!isExist){
		if((p->next = new_cursor_list()) == NULL){
			ERRLOG("cannot generate new CURSORLIST\n");
			return RESULT_ERROR;
		}
		p = p->next;
	}
	p->connid = id;
	p->cname = _strdup(cname);
	p->query = _strdup(query);
	p->nParams = nParams;
	p->plist = NULL;
	p->isOpened = 0;
	p->tuples = 0;

	if(nParams > 0){
		// set parameter list
		SQLVARLIST *sp = _sql_var_lists;
		SQLVARLIST *current;
		if(_sql_var_lists == NULL){
			ERRLOG("_sql_var_lists has not been initialized\n");
			return RESULT_ERROR;
		}

		if((p->plist = new_sql_var_list()) == NULL){
			ERRLOG("cannot initialize SQLVARLIST\n");
			return RESULT_ERROR;
		}
		current = p->plist;

		while(sp->next != NULL){
			if((current->next = new_sql_var_list()) == NULL){
				ERRLOG("cannot initialize SQLVARLIST\n");
				return RESULT_ERROR;
			}

			// copy
			current->sv.type = sp->sv.type;
			LOG("current->sv.type=#%d#\n", current->sv.type);
			current->sv.length = sp->sv.length;
			current->sv.power = sp->sv.power;
			current->sv.addr = sp->sv.addr;

			current->sv.data = calloc(sizeof(sp->sv.data) + TERMINAL_LENGTH, sizeof(void));
			if(!current->sv.data){
				ERRLOG("current->sv.data allocation failed.\n");
				return RESULT_ERROR;
			}
			memcpy(current->sv.data, sp->sv.data, sizeof(sp->sv.data));
			LOG("current->sv.data=#%s#\n", current->sv.data);

			current->sv.realdata = _strdup(sp->sv.realdata);

			sp = sp->next;
			current = current->next;
		}
	}

	return RESULT_SUCCESS;
}

static void show_cursor_list(CURSORLIST *p){
	if(p != NULL){
		LOG("%d %s\n", p->connid, p->cname);
		show_cursor_list(p->next);
	}
}

static void
clear_cursor_list(CURSORLIST *list, int id){
	_clear_cursor_list(list->next, id);
}

static void
_clear_cursor_list(CURSORLIST *p, int id){
	if(p != NULL){
		_clear_cursor_list(p->next, id);
		if(p->connid == id)
			p->isOpened = 0;
	}
}

static CURSORLIST *
get_cursor_from_list(char *cname){
	CURSORLIST *p = &_cursor_list;
	CURSORLIST *ret = NULL;
	LOG("#target:%s#\n", cname)
	while(p->next != NULL){
		p = p->next;
		LOG("#search:%s#\n", p->cname)
		if(strcmp(cname, p->cname) == 0){
			ret = p;
			break;
		}
	}

	if(ret == NULL){
		ERRLOG("cursor name '%s' is not found in cursor list.\n", cname);
		return NULL;
	}
	LOG("#return:%s#\n", ret->cname)
	return ret;
}


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
#include <stdarg.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>
#include <malloc.h>
#include <ctype.h>
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

typedef struct prepare_list {
	struct query_info sq;
	struct prepare_list *next;
} PREPARELIST;

typedef struct cursor_list {
	int connid; // connection id
	char *cname; // default NULL
	struct prepare_list *sp;
	char *query; // default NULL
	int nParams; // params for query
	int isOpened; // open flag
	int tuples; // fetched rows
	SQLVARLIST *plist; // parameter list
	struct cursor_list *next;
} CURSORLIST;



static PREPARELIST _prepare_list = {{NULL, NULL, 0}, NULL};
static CURSORLIST _cursor_list = {0, NULL, NULL, NULL, 0, 0, 0, NULL, NULL};
static SQLVARLIST *_sql_var_lists = NULL;
static SQLVARLIST *_sql_res_var_lists = NULL;
static int _var_lists_length = 0;
static int _res_var_lists_length = 0;
static int _occurs_length = 0;
static int _occurs_iter = 0;
static int _occurs_is_parent = 0;


static void sqlca_initialize(struct sqlca_t *);

/* sql var list */
static void init_sql_var_list(void);
static void reset_sql_var_list(void);
static SQLVARLIST * new_sql_var_list(void);
static SQLVARLIST * add_sql_var_list(int, int, int, void *);
static SQLVARLIST * add_sql_res_var_list(int, int, int, void *);
static void clear_sql_var_list(SQLVARLIST *);
static void show_sql_var_list(SQLVARLIST *);
static PREPARELIST * new_prepare_list(void);
static PREPARELIST * add_prepare_list(char *, char *, int);

static void create_realdata(SQLVAR *, int);
static void create_coboldata_lowvalue(SQLVAR *, int);
static void create_coboldata(SQLVAR *, int, char *);
static int get_varchar_length(char *);
static void set_varchar_length(int, char *);

/* cursor list */
static CURSORLIST *new_cursor_list(void);
static int add_cursor_list(int, char *, char *, int);
static int add_cursor_list_with_prepare(int, char *, PREPARELIST *);
static void remove_cursor_list(char *);

static void clear_cursor_list(CURSORLIST *, int);
static void _clear_cursor_list(CURSORLIST *, int);
static void show_cursor_list(CURSORLIST *);
static CURSORLIST * get_cursor_from_list(char *);
static PREPARELIST * get_prepare_from_list(char *);


static void _ocesqlExec(struct sqlca_t *, int , char *);
static void _ocesqlExecParams(struct sqlca_t *, int, char *, int);
static void _ocesqlExecParamsOccurs(struct sqlca_t *, int, char *, int);
static void _ocesqlCursorDeclare(struct sqlca_t *, int, char *, char *, int);
static void _ocesqlPreparedCursorDeclare(struct sqlca_t *, int, char *, char *);
static int _ocesqlExecPrepare(struct sqlca_t *, int, char *, int);
static void _ocesqlExecSelectIntoOne(struct sqlca_t *, int, char *, int, int);
static void _ocesqlExecSelectIntoOccurs(struct sqlca_t *, int, char *, int, int);
static int _ocesqlConnectMain(struct sqlca_t *, char *, char *, char *, char *);
static int _ocesqlConnect(struct sqlca_t *, char *, int, char *, int, char *, int, char *);
static int _ocesqlConnectInformal(struct sqlca_t *, char *, int, char *);
static void _ocesqlDisconnect(struct sqlca_t *, int);
static void _ocesqlReleaseConnection(int, void *);

static int _ocesqlResolveCONNID(struct sqlca_t *, char *, int);


static void
sqlca_initialize(struct sqlca_t * sqlca){
	memcpy((char *) sqlca, (char *) &sqlca_init, sizeof(struct sqlca_t));
}

/*
 * <Function name>
 *   OCESQLConnect
 *
 * <Outline>
 *   データベース接続を試みる。成功したらコネクションIDを発行して返す
 *   return する前に st.sqlcode に値をセットすること。
 *
 * <Input>
 *   @st: SQLCA pointer
 *   //@name: database name
 *   //@user: user name
 *   //@passwd: password
 *
 * <Output>
 *   success : ConnectionId
 *   failure ; OCESQL_NO_CONNECTION
 */
int
OCESQLConnect(struct sqlca_t *st, char *user, int userlen, char *passwd, int passwdlen, char *name, int namelen){
	LOG("OCESQLConnect start\n");
	return _ocesqlConnect(st, user, userlen, passwd, passwdlen, name, namelen, NULL);
}

/*
 * <Function name>
 *   OCESQLIDConnect
 *
 * <Outline>
 *   データベース接続を試みる。引数に接続先情報を指定の書式でセットする。
 *   成功したらコネクションIDを発行して返す。
 *   return する前に st.sqlcode に値をセットすること。
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @atdb: Connection Identifier
 *   @atdblen: length of atdb
 *   //@name: database name
 *   //@user: user name
 *   //@passwd: password
 *
 * <Output>
 *   success : ConnectionId
 *   failure ; OCESQL_NO_CONNECTION
 */
int
OCESQLIDConnect(struct sqlca_t *st, char *atdb, int atdblen, char *user, int userlen, char *passwd, int passwdlen, char *name, int namelen){
	char *atdbbuf;
	LOG("OCESQLIDConnect start\n");
	atdbbuf = get_str_without_after_space(oc_strndup(atdb, atdblen));
	if((!atdbbuf) || (*atdbbuf == '\0')){
		OCDBSetLibErrorStatus(st,OCPG_VAR_NOT_CHAR);
		return 1;
	}

	_ocesqlConnect(st, user, userlen, passwd, passwdlen, name, namelen, atdbbuf);

	free(atdbbuf);

	return 0;
}

/*
 * <Function name>
 *   OCESQLConnectShort
 *
 * <Outline>
 *   データベース接続を試みる。成功したらコネクションIDを発行して返す
 *   return する前に st.sqlcode に値をセットすること。
 *
 * <Input>
 *   @st: SQLCA pointer
 *   //@name: database name
 *   //@user: user name
 *   //@passwd: password
 *
 * <Output>
 *   success : ConnectionId
 *   failure ; OCESQL_NO_CONNECTION
 */
int
OCESQLConnectShort(struct sqlca_t *st){
	char	user[256];
	int		userlen;
	char	passwd[256];
	int		passwdlen;
	char	name[256];
	int		namelen;

	LOG("OCESQLConnectShort start\n");
	(void)memset(user, 0x00, sizeof(user));
	userlen = 0;
	(void)memset(passwd, 0x00, sizeof(passwd));
	passwdlen = 0;
	(void)memset(name, 0x00, sizeof(name));
	namelen = 0;
	return _ocesqlConnect(st, user, userlen, passwd, passwdlen, name, namelen, NULL);
}

/*
 * <Function name>
 *   OCESQLIDConnectShort
 *
 * <Outline>
 *   データベース接続を試みる。成功したらコネクションIDを発行して返す
 *   return する前に st.sqlcode に値をセットすること。
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @atdb: Connection Identifier
 *   @atdblen: length of atdb
 *   //@name: database name
 *   //@user: user name
 *   //@passwd: password
 *
 * <Output>
 *   success : ConnectionId
 *   failure ; OCESQL_NO_CONNECTION
 */
int
OCESQLIDConnectShort(struct sqlca_t *st, char *atdb, int atdblen){
	char *atdbbuf;
	LOG("OCESQLIDConnect start\n");
	atdbbuf = get_str_without_after_space(oc_strndup(atdb, atdblen));
	if((!atdbbuf) || (*atdbbuf == '\0')){
		OCDBSetLibErrorStatus(st,OCPG_VAR_NOT_CHAR);
		return 1;
	}
	char	user[256];
	int		userlen;
	char	passwd[256];
	int		passwdlen;
	char	name[256];
	int		namelen;

	LOG("OCESQLConnectShort start\n");
	(void)memset(user, 0x00, sizeof(user));
	userlen = 0;
	(void)memset(passwd, 0x00, sizeof(passwd));
	passwdlen = 0;
	(void)memset(name, 0x00, sizeof(name));
	namelen = 0;
	return _ocesqlConnect(st, user, userlen, passwd, passwdlen, name, namelen, atdbbuf);
}

static int
_ocesqlConnect(struct sqlca_t *st, char *user, int userlen, char *passwd, int passwdlen, char *name, int namelen, char *atdb){
	char *dbuser, *dbpasswd, *dbname;
	char *tmpuser, *tmppasswd, *tmpname;
	int ret;

	tmpname   = get_str_without_after_space(oc_strndup(name, namelen));
	tmpuser   = get_str_without_after_space(oc_strndup(user, userlen));
	tmppasswd = get_str_without_after_space(oc_strndup(passwd, passwdlen));

	if(!tmpname){
		dbname = com_strdup(com_getenv("OCDB_DB_NAME", NULL));
	}else if(*tmpname == '\0'){
		free(tmpname);
		tmpname = com_getenv("OCDB_DB_NAME", NULL);
		if(tmpname)
			dbname = com_strdup(tmpname);
		else
			dbname = NULL;
	}else{
		dbname = tmpname;
	}

	if(!tmpuser){
		dbuser = com_strdup(com_getenv("OCDB_DB_USER", NULL));
	}else if(*tmpuser == '\0'){
		free(tmpuser);
		tmpuser = com_getenv("OCDB_DB_USER", NULL);
		if(tmpuser)
			dbuser = com_strdup(tmpuser);
		else
			dbuser = NULL;
	}else{
		dbuser = tmpuser;
	}

	if(!tmppasswd){
		dbpasswd = com_strdup(com_getenv("OCDB_DB_PASS", NULL));
	}else if(*tmppasswd == '\0'){
		free(tmppasswd);
		tmppasswd = com_getenv("OCDB_DB_PASS", NULL);
		if(tmppasswd)
			dbpasswd = com_strdup(tmppasswd);
		else
			dbpasswd = NULL;
	}else{
		dbpasswd = tmppasswd;
	}

	if(atdb){
		ret = _ocesqlConnectMain(st, dbname, dbuser, dbpasswd, atdb);
	}else{
		ret = _ocesqlConnectMain(st, dbname, dbuser, dbpasswd, OCESQL_DEFAULT_DBNAME);
	}

	if(dbname) free(dbname);
	if(dbuser) free(dbuser);
	if(dbpasswd) free(dbpasswd);

	return ret;
}

/*
 * <Function name>
 *   OCESQLConnectInformal
 *
 * <Outline>
 *   データベース接続を試みる。引数に接続先情報を指定の書式でセットする。
 *   成功したらコネクションIDを発行して返す
 *   return する前に st.sqlcode に値をセットすること。
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @conninfo: Connection Info (USER/PASSWD@DBNAME)
 *   @conninfolen: length of conninfo
 *
 * <Output>
 *   success : ConnectionId
 *   failure ; OCESQL_NO_CONNECTION
 */
int
OCESQLConnectInformal(struct sqlca_t *st, char *conninfo, int conninfolen){
	LOG("OCESQLConnectInformal start\n");
	return _ocesqlConnectInformal(st, conninfo, conninfolen, NULL);
}

/*
 * <Function name>
 *   OCESQLIDConnectInformal
 *
 * <Outline>
 *   データベース接続を試みる。引数に接続先情報を指定の書式でセットする。
 *   成功したら、ATDBNAMEとCONNECTION IDのセットを登録する。
 *   return する前に st.sqlcode に値をセットすること。
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @atdb: Connection Identifier
 *   @atdblen: length of atdb
 *   @conninfo: Connection Info (USER/PASSWD@DBNAME)
 *   @conninfolen: length of conninfo
 *
 * <Output>
 *   success : ConnectionId
 *   failure ; OCESQL_NO_CONNECTION
 */
int
OCESQLIDConnectInformal(struct sqlca_t *st, char *atdb, int atdblen, char *conninfo, int conninfolen){
	LOG("OCESQLIDConnectInformal start\n");
	char *atdbbuf;
	atdbbuf = get_str_without_after_space(oc_strndup(atdb, atdblen));
	if((!atdbbuf) || (*atdbbuf == '\0')){
		OCDBSetLibErrorStatus(st,OCPG_VAR_NOT_CHAR);
		return 1;
	}

	_ocesqlConnectInformal(st, conninfo, conninfolen, atdbbuf);

	free(atdbbuf);

	return 0;
}

static int
_ocesqlConnectInformal(struct sqlca_t *st, char *conninfo, int conninfolen, char *atdb){

	char *conninfobuf,*dsn,*passwd;
	char *dbname, *dbuser, *dbpasswd;
	char *tmpname, *tmpuser, *tmppasswd;
	int ret;

	conninfobuf = get_str_without_after_space(oc_strndup(conninfo,conninfolen));
	if(!conninfobuf){
		ERRLOG("Connection information is NULL\n",conninfobuf);
		return -1;
	}
	LOG("%s\n",conninfobuf);
	dsn = strchr(conninfobuf,'@');
	if(dsn){
		*dsn='\0';
		dsn++;
	}

	passwd = strchr(conninfobuf,'/');
	if(passwd){
		*passwd='\0';
		passwd++;
	}


	tmpname   = get_str_without_after_space(dsn);
	tmpuser   = get_str_without_after_space(conninfobuf);
	tmppasswd = get_str_without_after_space(passwd);

	if(!tmpname || *tmpname == '\0'){
		tmpname = com_getenv("OCDB_DB_NAME", NULL);
		if(tmpname)
			dbname = com_strdup(tmpname);
		else
			dbname = NULL;
	}else{
		dbname = com_strdup(tmpname);
	}

	if(!tmpuser || *tmpuser == '\0'){
		tmpuser = com_getenv("OCDB_DB_USER", NULL);
		if(tmpuser)
			dbuser = com_strdup(tmpuser);
		else
			dbuser = NULL;
	}else{
		dbuser = com_strdup(tmpuser);
	}

	if(!tmppasswd || *tmppasswd == '\0'){
		tmppasswd = com_getenv("OCDB_DB_PASS", NULL);
		if(tmppasswd)
			dbpasswd = com_strdup(tmppasswd);
		else
			dbpasswd = NULL;
	}else{
		dbpasswd = com_strdup(tmppasswd);
	}

	if(atdb){
		ret = _ocesqlConnectMain(st, dbname, dbuser, dbpasswd, atdb);
	}else{
		ret = _ocesqlConnectMain(st, dbname, dbuser, dbpasswd, OCESQL_DEFAULT_DBNAME);
	}

	if(conninfobuf) free(conninfobuf);
	if(dbname) free(dbname);
	if(dbuser) free(dbuser);
	if(dbpasswd) free(dbpasswd);

	return ret;
}

static int
_ocesqlConnectMain(struct sqlca_t *st, char *name, char *user, char *passwd, char *conndbname){
	int connectId;
	int autocommit;
	int dbtype = USE_PGSQL; // aiming at a specific target

	char *cencoding = com_strdup(com_getenv("OCDB_DB_CHAR", "SJIS"));

	char *dbname = name ? com_strdup(name) : NULL;
	char *dbhost = NULL;
	char *dbport = NULL;
	char *real_dbname = NULL;
	char *connstr = NULL;
	int connlen = 0;

	LOG("dbname   = %s\n",name);
	LOG("user     = %s\n",user);
	LOG("password = %s\n",passwd);
	LOG("connname = %s\n",conndbname);

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
			return 0;
		}else{
			OCDBFinish(id);
		}
	}

	if (dbname != NULL){
 		char *tmpstr;

		tmpstr = strrchr(dbname, ':');
		if (tmpstr != NULL){
			dbport = com_strdup(tmpstr + 1);
			*tmpstr = '\0';
		}
		tmpstr = strrchr(dbname, '@');
		if (tmpstr != NULL){
			dbhost = com_strdup(tmpstr + 1);
			*tmpstr = '\0';
		}

		if(strlen(dbname) > 0){
			real_dbname = com_strdup(dbname);
		}
	}

	connlen = strlen_or_null(real_dbname) + strlen_or_null(dbhost) +
		strlen_or_null(dbport) + strlen_or_null(user) +
		strlen_or_null(passwd) + sizeof(" host = port = dbname = user = password =");
	connstr = _alloc(connlen);

	com_sprintf(connstr, connlen, "%s%s %s%s %s%s %s%s %s%s",
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
	if (cencoding) free(cencoding);

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

/*
 * <Function name>
 *   OCESQLPrepare
 *
 * <Outline>
 *   SQL文を準備する
 *   実行結果(OCDB_RES_* -> ocdb.h 参照)はSQLCAに格納する
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @sname: SQL name
 *   @query: query
 *   @querylen: length of query
 */
int
OCESQLPrepare(struct sqlca_t *st, char *sname, char *query, int querylen){
	LOG("OCESQLPrepare start\n");
	char *querybuf, *pquery;
	int nParams;

	querybuf = oc_strndup(query, querylen);
	pquery = get_str_replace_hostvalue(querybuf, &nParams);

	if(querybuf)
		free(querybuf);
	LOG("Add prepare: sname:%s, nParams:%d, query:'%s'\n",sname,nParams,pquery);
	if(add_prepare_list(sname, pquery, nParams) ==NULL){
		OCDBSetLibErrorStatus(st,OCDB_OUT_OF_MEMORY);
	}

	return 0;
}

/*
 * <Function name>
 *   OCESQLExec
 *
 * <Outline>
 *   実処理は_ocesqlExecにて
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @id: ConnectionId
 *   @query: SQL query
 */
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
	_ocesqlExec(st, id, query);
	return 0;
}

/*
 * <Function name>
 *   OCESQLIDExec
 *
 * <Outline>
 *   ここでは接続ID取得のみを実施
 *   実処理は_ocesqlExecにて
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @atdb: Connection Identifier
 *   @atdblen: length of atdb
 *   @query: SQL query
 */
int
OCESQLIDExec(struct sqlca_t *st, char *atdb, int atdblen, char *query){
	LOG("OCESQLIDExec start\n");
	int id;
	LOG("SQL:#%s#\n", query);
	id = _ocesqlResolveCONNID(st, atdb, atdblen);
	if(id == RESULT_FAILED){
		ERRLOG("connection id is not found.\n");
		OCDBSetLibErrorStatus(st,OCDB_NO_CONN);
		return 1;
	}
	_ocesqlExec(st, id, query);
	return 0;
}

/*
 * <Function name>
 *   _ocesqlExec
 *
 * <Outline>
 *   SQLクエリの実行
 *   COMMIT, ROLLBACK の時は BEGIN も行う。
 *   実行結果(OCDB_RES_* -> ocdb.h 参照)はSQLCAに格納する
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @id: ConnectionId
 *   @query: SQL query
 */
static void
_ocesqlExec(struct sqlca_t *st, int id, char *query){
	sqlca_initialize(st);

	// check argument
	if(query == NULL || strlen(query) == 0){
		OCDBSetLibErrorStatus(st,OCDB_EMPTY);
		return;
	}

	OCDBExec(id, query);
	if(OCDBSetResultStatus(id,st) != RESULT_SUCCESS){
		return;
	}

	if(strcmp(query, "COMMIT") == 0|| strcmp(query, "ROLLBACK") == 0){
		clear_cursor_list(&_cursor_list, id);
		OCDBExec(id, "BEGIN");
		OCDBSetResultStatus(id,st);
	}

	return;
}

/*
 * <Function name>
 *   OCESQLExecParams
 *
 * <Outline>
 *   実処理は_ocesqlExecParamsにて
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @id: ConnectionId
 *   @query: SQL query
 *   @nParams: パラメータの個数
 */
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
	_ocesqlExecParams(st, id, query, nParams);
	return 0;
}

/*
 * <Function name>
 *   OCESQLIDExecParams
 *
 * <Outline>
 *   ここでは接続ID取得のみを実施
 *   実処理は_ocesqlExecParamsにて
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @atdb: Connection Identifier
 *   @atdblen: length of atdb
 *   @query: SQL query
 *   @nParams: パラメータの個数
 */
int
OCESQLIDExecParams(struct sqlca_t *st, char *atdb, int atdblen, char *query, int nParams){
	LOG("OCESQLIDExecParams start\n");
	int id;
	id = _ocesqlResolveCONNID(st, atdb, atdblen);
	LOG("id=%d\n", id);
	if(id == RESULT_FAILED){
		ERRLOG("connection id is not found.\n");
		OCDBSetLibErrorStatus(st,OCDB_NO_CONN);
		return 1;
	}
	_ocesqlExecParams(st, id, query, nParams);
	return 0;
}

/*
 * <Function name>
 *   _ocesqlExecParams
 *
 * <Outline>
 *   埋め込みパラメータ付きSQLクエリの実行
 *   COMMIT, ROLLBACK の時は BEGIN も行う。
 *   実行結果(OCDB_RES_* -> ocdb.h 参照)はSQLCAに格納する
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @id: ConnectionId
 *   @query: SQL query
 *   @nParams: パラメータの個数
 */
static void
_ocesqlExecParams(struct sqlca_t *st, int id, char *query, int nParams){
	int i;
	char **arr;
	SQLVARLIST *p = _sql_var_lists;

	sqlca_initialize(st);

	// check argument
	if(query == NULL || strlen(query) == 0 || nParams == 0){
		OCDBSetLibErrorStatus(st,OCDB_EMPTY);
		return;
	}

	if(_var_lists_length > nParams){
		OCDBSetLibErrorStatus(st,OCDB_TOO_MANY_ARGUMENTS);
		return;
	}else if(_var_lists_length < nParams){
		OCDBSetLibErrorStatus(st,OCDB_TOO_FEW_ARGUMENTS);
		return;
	}

	arr = NULL;
	if((arr = (char **)malloc(sizeof(char *) * nParams)) == NULL){
		OCDBSetLibErrorStatus(st,OCDB_OUT_OF_MEMORY);
		return;
	}

	// set parameters
	for(i=0; i<_var_lists_length; i++, p=p->next){
		arr[i] = p->sv.realdata;
	}

	OCDBExecParams(id, query, nParams, NULL,
				   (const char * const *)arr, NULL, NULL, 0);
	if(arr != NULL){
		free(arr);
	}

	if(OCDBSetResultStatus(id,st) != RESULT_SUCCESS){
		return;
	}

	if(strcmp(query, "COMMIT") == 0|| strcmp(query, "ROLLBACK") == 0){
		clear_cursor_list(&_cursor_list, id);
		OCDBExec(id, "BEGIN");
		OCDBSetResultStatus(id,st);
	}
	return;
}

/*
 * <Function name>
 *   OCESQLExecParamsOccurs
 *
 * <Outline>
 *   実処理は_ocesqlExecParamsOccursにて
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @id: ConnectionId
 *   @query: SQL query
 *   @nParams: パラメータの個数
 */
int
OCESQLExecParamsOccurs(struct sqlca_t *st, char *query, int nParams){
	LOG("OCESQLExecParamsOccurs start\n");
	int id;
	LOG("SQL:#%s#\n", query);
	id = _ocesqlResolveCONNID(st, OCESQL_DEFAULT_DBNAME, OCESQL_DEFAULT_DBLENG);
	if(id == RESULT_FAILED){
		ERRLOG("connection id is not found.\n");
		OCDBSetLibErrorStatus(st,OCDB_NO_CONN);
		return 1;
	}
	_ocesqlExecParamsOccurs(st, id, query, nParams);
	return 0;
}

/*
 * <Function name>
 *   OCESQLIDExecParamsOccurs
 *
 * <Outline>
 *   ここでは接続ID取得のみを実施
 *   実処理は_ocesqlExecParamsOccursにて
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @atdb: Connection Identifier
 *   @atdblen: length of atdb
 *   @query: SQL query
 *   @nParams: パラメータの個数
 */
int
OCESQLIDExecParamsOccurs(struct sqlca_t *st, char *atdb, int atdblen, char *query, int nParams){
	LOG("OCESQLIDExecParamsOccurs start\n");
	int id;
	id = _ocesqlResolveCONNID(st, atdb, atdblen);
	if(id == RESULT_FAILED){
		ERRLOG("connection id is not found.\n");
		OCDBSetLibErrorStatus(st,OCDB_NO_CONN);
		return 1;
	}
	_ocesqlExecParamsOccurs(st, id, query, nParams);
	return 0;
}

/*
 * <Function name>
 *   _ocesqlExecParamsOccurs
 *
 * <Outline>
 *   埋め込みパラメータ付きSQLクエリの実行
 *   パラメータははSetHostTableで定義した繰り返し回数と一周のバイト数に
 *   従いセットされる
 *   対応するSQLはINSERT文,UPDATE文,DELETE文のみ。
 *
 *   実行結果(OCDB_RES_* -> ocdb.h 参照)はSQLCAに格納する
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @id: ConnectionId
 *   @query: SQL query
 *   @nParams: パラメータの個数
 */
static void
_ocesqlExecParamsOccurs(struct sqlca_t *st, int id, char *query, int nParams){
	int i, index;
	char **arr;
	SQLVARLIST *p;

	sqlca_initialize(st);

	// check argument
	if(query == NULL || strlen(query) == 0 || nParams == 0 || _occurs_iter > 500){
		OCDBSetLibErrorStatus(st,OCDB_EMPTY);
		return;
	}
	arr = NULL;
	if((arr = (char **)malloc(sizeof(char *) * nParams)) == NULL){
		OCDBSetLibErrorStatus(st,OCDB_OUT_OF_MEMORY);
		return;
	}

	for(index=0; index<_occurs_iter; index++){
		// set parameters
		p = _sql_var_lists;
		for(i=0; i<_var_lists_length; i++, p=p->next){
			create_realdata(&p->sv, index);
			arr[i] = p->sv.realdata;
		}

		OCDBExecParams(id, query, nParams, NULL,
				(const char * const *)arr, NULL, NULL, 0);
		if(OCDBSetResultStatus(id,st) != RESULT_SUCCESS){
			break;
		}
	}
	if(arr != NULL){
		free(arr);
	}

	return;
}

/*
 * <Function name>
 *   OCESQLCursorDeclare
 *
 * <Outline>
 *   実処理は_ocesqlCursorDeclareにて
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @id: ConnectionId
 *   @cname: cursor name
 *   @query: SQL query
 */
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
	_ocesqlCursorDeclare(st, id, cname, query, 0);
	return 0;
}

/*
 * <Function name>
 *   OCESQLIDCursorDeclare
 *
 * <Outline>
 *   カーソル宣言
 *   CONNECTION IDの替わりに接続子を利用する。
 *   実行結果(OCDB_RES_* -> ocdb.h 参照)はSQLCAに格納する
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @atdb: Connection Identifier
 *   @atdblen: length of atdb
 *   @cname: cursor name
 *   @query: SQL query
 */
int
OCESQLIDCursorDeclare(struct sqlca_t *st, char *atdb, int atdblen, char *cname, char *query){
	LOG("OCESQLIDCursorDeclare start\n");
	int id;
	id = _ocesqlResolveCONNID(st, atdb, atdblen);
	if(id == RESULT_FAILED){
		ERRLOG("connection id is not found.\n");
		OCDBSetLibErrorStatus(st,OCDB_NO_CONN);
		return 1;
	}
	_ocesqlCursorDeclare(st, id, cname, query, 0);
	return 0;
}

/*
 * <Function name>
 *   OCESQLCursorDeclareParams
 *
 * <Outline>
 *   実処理は_ocesqlCursorDeclareParamsにて
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @id: ConnectionId
 *   @cname: cursor name
 *   @query: SQL query
 */
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

	_ocesqlCursorDeclare(st, id, cname, query, nParams);
	return 0;
}

/*
 * <Function name>
 *   OCESQLCursorDeclareParams
 *
 * <Outline>
 *   ここでは接続ID取得のみを実施
 *   実処理は_ocesqlCursorDeclareParamsにて
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @atdb: Connection Identifier
 *   @atdblen: length of atdb
 *   @query: SQL query
 *   @nParams: パラメータの個数
 */
int
OCESQLIDCursorDeclareParams(struct sqlca_t *st, char *atdb, int atdblen, char *cname, char *query, int nParams){
	LOG("OCESQLIDCursorDeclareParams start\n");
	int id;
	id = _ocesqlResolveCONNID(st, atdb, atdblen);
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
	_ocesqlCursorDeclare(st, id, cname, query, nParams);
	return 0;
}

/*
 * <Function name>
 *   _ocesqlCursorDeclareParams
 *
 * <Outline>
 *   埋め込みパラメータ付きカーソル宣言
 *   実行結果(OCDB_RES_* -> ocdb.h 参照)はSQLCAに格納する
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @id: ConnectionId
 *   @cname: Cursor name
 *   @query: SQL query
 *   @nParams: パラメータの個数
 */
static void
_ocesqlCursorDeclare(struct sqlca_t *st, int id, char *cname, char *query, int nParams){
	int res;

	sqlca_initialize(st);

	// check argument
	if(cname == NULL || strlen(cname) == 0 ||
	   query == NULL || strlen(query) == 0 ){
		OCDBSetLibErrorStatus(st,OCDB_EMPTY);
		return;
	}

	res = add_cursor_list(id, cname, query, nParams);

	if(res == RESULT_FAILED){
		OCDBSetLibErrorStatus(st,OCDB_WARNING_PORTAL_EXISTS);
	}else if(res == RESULT_ERROR){
		OCDBSetLibErrorStatus(st,OCDB_OUT_OF_MEMORY);
	}
	return;
}

/*
 * <Function name>
 *   OCESQLPreparedCursorDeclare
 *
 * <Outline>
 *   実処理は_ocesqlPreparedCursorDeclareにて
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @id: ConnectionId
 *   @cname: Cursor name
 *   @sname: Prepared SQLID
 */
int
OCESQLPreparedCursorDeclare(struct sqlca_t *st, char *cname, char *sname){
	LOG("OCESQLPreparedCursorDeclare start\n");
	int id;
	id = _ocesqlResolveCONNID(st, OCESQL_DEFAULT_DBNAME, OCESQL_DEFAULT_DBLENG);
	if(id == RESULT_FAILED){
		ERRLOG("connection id is not found.\n");
		OCDBSetLibErrorStatus(st,OCDB_NO_CONN);
		return 1;
	}
	_ocesqlPreparedCursorDeclare(st, id, cname, sname);
	return 0;
}


/*
 * <Function name>
 *   OCESQLIDPreparedCursorDeclare
 *
 * <Outline>
 *   実処理は_ocesqlPreparedCursorDeclareにて
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @atdb: Connection Identifier
 *   @atdblen: length of atdb
 *   @cname: Cursor name
 *   @sname: Prepared SQLID
 */
int
OCESQLIDPreparedCursorDeclare(struct sqlca_t *st, char *atdb, int atdblen, char *cname, char *sname){
	LOG("OCESQLIDPreparedCursorDeclare start\n");
	int id;
	id = _ocesqlResolveCONNID(st, atdb, atdblen);
	if(id == RESULT_FAILED){
		ERRLOG("connection id is not found.\n");
		OCDBSetLibErrorStatus(st,OCDB_NO_CONN);
		return 1;
	}
	_ocesqlPreparedCursorDeclare(st, id, cname, sname);
	return 0;
}

/*
 * <Function name>
 *   _ocesqlPreparedCursorDeclare
 *
 * <Outline>
 *   埋め込みパラメータ付きカーソル宣言
 *   SQL文にはOCESQLPrepareにて定義された識別子を使う。
 *   実行結果(OCDB_RES_* -> ocdb.h 参照)はSQLCAに格納する
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @id: ConnectionId
 *   @cname: Cursor name
 *   @sname: Prepared SQLID
 */
static void
_ocesqlPreparedCursorDeclare(struct sqlca_t *st, int id, char *cname, char *sname){
	int res;
	PREPARELIST *prepare;

	sqlca_initialize(st);

	// check argument
	if(cname == NULL || strlen(cname) == 0 ||
	   sname == NULL || strlen(sname) == 0 ){
		OCDBSetLibErrorStatus(st,OCDB_EMPTY);
		return;
	}

	LOG("Declare Cursor with prepare: id:%d, cname:%s, sname:%s\n",id,cname,sname);

	// search prepare
	prepare = get_prepare_from_list(sname);
	if(prepare == NULL){
		ERRLOG("prepare %s not registered.\n", sname);
		OCDBSetLibErrorStatus(st,OCDB_INVALID_STMT);
		return;
	}

	if((res = add_cursor_list_with_prepare(id, cname, prepare)) == RESULT_FAILED){
		OCDBSetLibErrorStatus(st,OCDB_WARNING_PORTAL_EXISTS);
	}else if((res = add_cursor_list_with_prepare(id, cname, prepare)) == RESULT_ERROR){
		OCDBSetLibErrorStatus(st,OCDB_OUT_OF_MEMORY);
	}
}

/*
 * <Function name>
 *   OCESQLExecPrepare
 *
 * <Outline>
 *   OCESQLPrepare文で準備されたSQLを実行する。
 *   実行結果(OCDB_RES_* -> ocdb.h 参照)はSQLCAに格納する
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @sname: Prepared SQLID
 *   @nParams: パラメータの個数
 */

int
OCESQLExecPrepare(struct sqlca_t *st, char *sname, int nParams){
	LOG("OCESQLExecPrepared start\n");
	int id;
	id = _ocesqlResolveCONNID(st, OCESQL_DEFAULT_DBNAME, strlen(OCESQL_DEFAULT_DBNAME));
	if(id == RESULT_FAILED){
		ERRLOG("connection id is not found.\n");
		OCDBSetLibErrorStatus(st,OCDB_NO_CONN);
		return 1;
	}

	return _ocesqlExecPrepare(st, id, sname, nParams);
}

/*
 * <Function name>
 *   OCESQLIDExecPrepare
 *
 * <Outline>
 *   OCESQLPrepare文で準備されたSQLを実行する。
 *   実行結果(OCDB_RES_* -> ocdb.h 参照)はSQLCAに格納する
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @atdb: Connection Identifier
 *   @atdblen: length of atdb
 *   @sname: Prepared SQLID
 *   @nParams: パラメータの個数
 */
int
OCESQLIDExecPrepare(struct sqlca_t *st, char *atdb, int atdblen, char *sname, int nParams){
	LOG("OCESQLExecPrepared start\n");
	int id;
	id = _ocesqlResolveCONNID(st, atdb, atdblen);
	if(id == RESULT_FAILED){
		ERRLOG("connection id is not found.\n");
		OCDBSetLibErrorStatus(st,OCDB_NO_CONN);
		return 1;
	}

	return _ocesqlExecPrepare(st, id, sname, nParams);
}


static int
_ocesqlExecPrepare(struct sqlca_t *st, int id, char *sname, int nParams){
	int i;
	char **arr;
	char *query;
	SQLVARLIST *p = _sql_var_lists;
	PREPARELIST *prepare;

	sqlca_initialize(st);

	// search prepare
	prepare = get_prepare_from_list(sname);
	if(prepare == NULL){
		ERRLOG("prepare %s not registered.\n", sname);
		OCDBSetLibErrorStatus(st,OCDB_INVALID_STMT);
		return 1;
	}

	query = prepare->sq.query;

	// check argument
	if(query == NULL || strlen(query) == 0 || nParams != prepare->sq.nParams){
		ERRLOG("prepare %s argument error.\n", sname);
		OCDBSetLibErrorStatus(st,OCDB_EMPTY);
		return 1;
	}

	if(nParams > 0){
		if(prepare->sq.nParams != nParams){
			ERRLOG("A number of parameters(%d) and prepared sql parameters(%d) is unmatch.\n",nParams,prepare->sq.nParams);
			OCDBSetLibErrorStatus(st,OCDB_EMPTY);
			com_strcpy(st->sqlerrm.sqlerrmc, SQLERRMC_LEN, "A number of parameters and prepared sql parameters is unmatch.");
			st->sqlerrm.sqlerrml = strlen("A number of parameters and prepared sql parameters is unmatch.");
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
		free(arr);
	}else{
		OCDBExec(id, query);
	}

	if(OCDBSetResultStatus(id,st) != RESULT_SUCCESS){
		return 1;
	}

	if(strcmp(query, "COMMIT") == 0|| strcmp(query, "ROLLBACK") == 0){
		clear_cursor_list(&_cursor_list, id);
		OCDBExec(id, "BEGIN");
	}
	return 0;
}

/*
 * <Function name>
 *   OCESQLCursorOpen
 *
 * <Outline>
 *   カーソルオープン
 *   実行結果(OCDB_RES_* -> ocdb.h 参照)はSQLCAに格納する
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @cname: cursor name
 */
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

	if(cursor->isOpened){
		LOG("cursor %s alredy opened.\n", cname);
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
			create_realdata(&p->sv,0);
			arr[i] = p->sv.realdata;
			LOG("params[%d]:#%s#\n",i, p->sv.realdata);
		}

		OCDBCursorDeclareParams(cursor->connid, cursor->cname, cursor->query,
					cursor->nParams, NULL, (const char * const *)arr,
					NULL, NULL, 0, OCDB_CURSOR_WITH_HOLD_OFF);
		free(arr);
	} else if(cursor->sp){
		LOG("with prepare: sname:%s, query:%s\n",cursor->sp->sq.pname, cursor->sp->sq.query);
		OCDBCursorDeclare(cursor->connid, cursor->cname,
				cursor->sp->sq.query, OCDB_CURSOR_WITH_HOLD_OFF);
	}else {
		OCDBCursorDeclare(cursor->connid, cursor->cname,
				  cursor->query, OCDB_CURSOR_WITH_HOLD_OFF);
	}

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

/*
 * <Function name>
 *   OCESQLCursorOpenParams -- 使わないかも
 *
 * <Outline>
 *   カーソルオープン
 *   実行結果(OCDB_RES_* -> ocdb.h 参照)はSQLCAに格納する
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @cname: cursor name
 *   @nParams: パラメータの個数
 */
int
OCESQLCursorOpenParams(struct sqlca_t *st, char *cname, int nParams){
	CURSORLIST *cursor;
	LOG("OCESQLCursorOpenParams start\n");
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
	if(cursor->sp == NULL){
		ERRLOG("prepare sql in cursor '%s' not registered.\n", cname);
		OCDBSetLibErrorStatus(st,OCDB_INVALID_STMT);
		return 1;
	}
	if(cursor->sp->sq.nParams != nParams){
		ERRLOG("A number of parameters(%d) and prepared sql parameters(%d) is unmatch.\n",nParams,cursor->sp->sq.nParams);
		OCDBSetLibErrorStatus(st,OCDB_EMPTY);
		com_strcpy(st->sqlerrm.sqlerrmc, SQLERRMC_LEN, "A number of parameters and prepared sql parameters is unmatch.");
		st->sqlerrm.sqlerrml = strlen("A number of parameters and prepared sql parameters is unmatch.");
		return 1;
	}
	if(cursor->isOpened){
		LOG("cursor %s alredy opened.\n", cname);
		OCDBCursorClose(cursor->connid, cname);
		if(OCDBSetResultStatus(cursor->connid,st) != RESULT_SUCCESS){
			ERRLOG("cursor %s close failed.\n", cname);
			com_strncpy(st->sqlerrm.sqlerrmc, SQLERRMC_LEN, OCDBResultErrorMessage(cursor->connid), SQLERRMC_LEN - 1);
			return 1;
		}
		cursor->isOpened = 0;
	}

	int i;
	char **arr;
	SQLVARLIST *p = _sql_var_lists;

	if((arr = (char **)malloc(sizeof(char *) * nParams)) == NULL){
		ERRLOG("memory allocation failed.\n");
		OCDBSetLibErrorStatus(st,OCDB_OUT_OF_MEMORY);
		return 1;
	}

	// set parameters
	for(i=0; i<_var_lists_length; i++, p=p->next){
		arr[i] = p->sv.realdata;
		LOG("params[%d]:#%s#\n",i, p->sv.realdata);
	}
	LOG("with prepare: sname:%s, query:%s\n",cursor->sp->sq.pname, cursor->sp->sq.query);
	OCDBCursorDeclareParams(cursor->connid, cursor->cname, cursor->sp->sq.query,
			cursor->sp->sq.nParams, NULL, (const char * const *)arr,
					NULL, NULL, 0, OCDB_CURSOR_WITH_HOLD_OFF);

	free(arr);
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

/*
 * <Function name>
 *   OCESQLCursorFetchOne
 *
 * <Outline>
 *   カーソルフェッチ
 *   実行結果(OCDB_RES_* -> ocdb.h 参照)はSQLCAに格納する
 *   OCDBGetValueで返される値は左詰めになっていることを前提としている。
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @id: ConnectionId
 *   @cname: cursor name
 */
int
OCESQLCursorFetchOne(struct sqlca_t *st, char *cname){
 	LOG("OCESQLCursorFetchOne start\n");
	int i;
	int id;
	CURSORLIST *cursor;
	SQLVARLIST *p = _sql_res_var_lists;

	sqlca_initialize(st);

	// check argument
	if(cname == NULL || strlen(cname) == 0){
		OCDBSetLibErrorStatus(st,OCDB_EMPTY);
		return 1;
	}
	LOG("cname:%s\n",cname);
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

	if(OCDBNfields(id) != _res_var_lists_length){
		ERRLOG("A number of parameters(%d) and results(%d) is unmatch.\n",_res_var_lists_length,OCDBNfields(id));
		OCDBSetLibErrorStatus(st,OCDB_EMPTY);
		com_strcpy(st->sqlerrm.sqlerrmc, SQLERRMC_LEN, "A number of parameters and results is unmatch.");
		st->sqlerrm.sqlerrml = strlen("A number of parameters and results is unmatch.");
		return 1;
	}

	// check numtuples
	if(OCDBNtuples(id) < 1){
		OCDBSetLibErrorStatus(st,OCDB_NOT_FOUND);
		st->sqlerrd[2] = cursor->tuples;
		LOG("TUPLES NODATA\n");
		return 1;
	} else {
		// set params
		char *retstr;
		for(i=0; i< _res_var_lists_length; i++, p = p->next){
			retstr = OCDBGetvalue(id, 0, i);
			create_coboldata(&p->sv, 0, retstr);
		}
	}
	cursor->tuples += st->sqlerrd[2];
	st->sqlerrd[2] = cursor->tuples;
	return 0;
}

/*
 * <Function name>
 *   OCESQLCursorFetchOccurs
 *
 * <Outline>
 *   カーソルフェッチ
 *   実行結果(OCDB_RES_* -> ocdb.h 参照)はSQLCAに格納する
 *   OCDBGetValueで返される値は左詰めになっていることを前提としている。
 *
 *   フェッチした値はSetHostTableで定義した繰り返し回数と一周のバイト数に
 *   従いセットされる
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @id: ConnectionId
 *   @cname: cursor name
 */
int
OCESQLCursorFetchOccurs(struct sqlca_t *st, char *cname){
	LOG("OCESQLCursorFetchOccurs start\n");
	int i,j;
	int id;
	int tuples = 0;
	int fields = 0;
	CURSORLIST *cursor;
	SQLVARLIST *p = _sql_res_var_lists;

	sqlca_initialize(st);

	// check argument
	if(cname == NULL || strlen(cname) == 0 || _occurs_iter > 500){
		OCDBSetLibErrorStatus(st,OCDB_EMPTY);
		return 1;
	}
	LOG("cname:%s\n",cname);
	cursor = get_cursor_from_list(cname);
	if(cursor == NULL){
		ERRLOG("cursor %s not registered.\n", cname);
		OCDBSetLibErrorStatus(st,OCDB_WARNING_UNKNOWN_PORTAL);
		return 1;
	}
	id = cursor->connid;

	// exec sql
	OCDBCursorFetchOccurs(id, cname, OCDB_READ_NEXT, _occurs_iter);

	if(st->sqlcode < 0){
		return 1;
	}

	fields = OCDBNfields(id);
	if(fields != _res_var_lists_length){
		ERRLOG("A number of parameters(%d) and results(%d) is unmatch.\n",_res_var_lists_length, fields);
		OCDBSetLibErrorStatus(st,OCDB_EMPTY);
		com_strcpy(st->sqlerrm.sqlerrmc, SQLERRMC_LEN, "A number of parameters and results is unmatch.");
		st->sqlerrm.sqlerrml = strlen("A number of parameters and results is unmatch.");
		return 1;
	}

	tuples = OCDBNtuples(id);
	if(tuples < 1){
		OCDBSetLibErrorStatus(st,OCDB_NOT_FOUND);
		LOG("TUPLES NODATA\n");
		return 0;
	}
	if(tuples > _occurs_iter){
		LOG("Data was taken more than a specified. OCCURS:%d, TUPLES:%d.\n",_occurs_iter,tuples);
		tuples = _occurs_iter;
	}
	for(j=0; j<tuples; j++){
		SQLVARLIST *p = _sql_res_var_lists;
		// set params
		char *retstr;
		for(i=0; i< _res_var_lists_length; i++, p = p->next){
			if(i>=fields)
				break;
			retstr = OCDBGetvalue(id, j, i);
			create_coboldata(&p->sv, j, retstr);

		}
		cursor->tuples += 1;
	}

	if(tuples < _occurs_iter){
		for(;j<_occurs_iter;j++){
			SQLVARLIST *p = _sql_res_var_lists;
			// set zero
			for(i=0; i< _res_var_lists_length; i++, p = p->next){
				create_coboldata_lowvalue(&p->sv, j);
			}
		}
	}

	st->sqlerrd[2] = cursor->tuples;
	return 0;
}

/*
 * <Function name>
 *   OCESQLCursorClose
 *
 * <Outline>
 *   カーソルクローズ
 *   実行結果(OCDB_RES_* -> ocdb.h 参照)はSQLCAに格納する
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @id: ConnectionId
 *   @cname: cursor name
 */
int
OCESQLCursorClose(struct sqlca_t *st, char *cname){
 	LOG("OCESQLCursorClose start\n");
	CURSORLIST *cursor;
	int id;

	sqlca_initialize(st);

	// check argument
	if(cname == NULL || strlen(cname) == 0){
		OCDBSetLibErrorStatus(st,OCDB_EMPTY);
		return 1;
	}
	LOG("Cursor Name:%s\n",cname);

	cursor = get_cursor_from_list(cname);
	if(cursor == NULL){
		ERRLOG("cursor %s not registered.\n", cname);
		OCDBSetLibErrorStatus(st,OCDB_WARNING_UNKNOWN_PORTAL);
		return 1;
	}
	if(!cursor->isOpened){
		LOG("cursor %s not opened.\n", cname);
		return 0;
	}

	id = cursor->connid;
	LOG("Connect ID:%d\n",id);

	OCDBCursorClose(id, cname);
	if(OCDBSetResultStatus(id,st) != RESULT_SUCCESS){
		return 1;
	}

	cursor->isOpened = 0;

	return 0;
}

/*
 * <Function name>
 *   OCESQLExecSelectIntoOne
 *
 * <Outline>
 *   実処理は_ocesqlExecSelectIntoOneで
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @id: ConnectionId
 *   @query: SQL query
 *   @nParams: パラメータの個数
 *   @nResParams: パラメータの個数
 */
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
	_ocesqlExecSelectIntoOne(st, id, query, nParams, nResParams);
	return 0;
}

/*
 * <Function name>
 *   OCESQLIDExecSelectIntoOne
 *
 * <Outline>
 *   ここでは接続ID取得のみを実施
 *   実処理は_ocesqlExecSelectIntoOneで
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @atdb: Connection Identifier
 *   @atdblen: length of atdb
 *   @query: SQL query
 *   @nParams: パラメータの個数
 *   @nResParams: パラメータの個数
 */
int
OCESQLIDExecSelectIntoOne(struct sqlca_t *st, char *atdb, int atdblen,
						  char *query, int nParams, int nResParams){
	LOG("OCESQLIDExecSelectIntoOne start\n");
	int id;
	id = _ocesqlResolveCONNID(st, atdb, atdblen);
	if(id == RESULT_FAILED){
		ERRLOG("connection id is not found.\n");
		OCDBSetLibErrorStatus(st,OCDB_NO_CONN);
		return 1;
	}
	_ocesqlExecSelectIntoOne(st, id, query, nParams, nResParams);
	return 0;
}

/*
 * <Function name>
 *   _ocesqlExecSelectIntoOne
 *
 * <Outline>
 *   SELECT INTO の実行
 *   実行結果(OCDB_RES_* -> ocdb.h 参照)はSQLCAに格納する
 *   OCDBGetValueで返される値は左詰めになっていることを前提としている。
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @id: ConnectionId
 *   @query: SQL query
 *   @nParams: パラメータの個数
 *   @nResParams: パラメータの個数
 */
static void
_ocesqlExecSelectIntoOne(struct sqlca_t *st, int id, char *query, int nParams, int nResParams){

	int i;
	int fields = 0;
	SQLVARLIST *p = _sql_res_var_lists;

	// check argument
	if(query == NULL || strlen(query) == 0){
		ERRLOG("ARGUMENT ERROR\n");
		OCDBSetLibErrorStatus(st,OCDB_EMPTY);
		return;
	}

	if(nParams > 0){
		_ocesqlExecParams(st, id, query, nParams);
	}else{
		_ocesqlExec(st, id, query);
	}

	if(OCDBSetResultStatus(id,st) != RESULT_SUCCESS){
		return;
	}

	fields = OCDBNfields(id);
	if(fields != nResParams){
		ERRLOG("ResParams(%d) and fields(%d) are different\n",nResParams, fields);
		OCDBSetLibErrorStatus(st,OCDB_EMPTY);
		return;
	}

	// check numtuples
	if(OCDBNtuples(id) < 1){
		OCDBSetLibErrorStatus(st,OCDB_NOT_FOUND);
		LOG("TUPLES NODATA\n");
	} else {
		// set params
		char *retstr;
		for(i=0; i< _res_var_lists_length; i++, p = p->next){
			if(i>=fields)
				break;
			retstr = OCDBGetvalue(id, 0, i);
			create_coboldata(&p->sv, 0, retstr);
		}
	}
	return;
}

/*
 * <Function name>
 *   OCESQLExecSelectIntoOccurs
 *
 * <Outline>
 *   実処理は_ocesqlExecSelectIntoOccursで
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @id: ConnectionId
 *   @query: SQL query
 *   @nParams: パラメータの個数
 *   @nResParams: パラメータの個数
 */
int
OCESQLExecSelectIntoOccurs(struct sqlca_t *st, char *query, int nParams, int nResParams){
	LOG("OCESQLExecSelectIntoOccurs start\n");
	int id;
	LOG("SQL:#%s#\n", query);
	id = _ocesqlResolveCONNID(st, OCESQL_DEFAULT_DBNAME, OCESQL_DEFAULT_DBLENG);
	if(id == RESULT_FAILED){
		ERRLOG("connection id is not found.\n");
		OCDBSetLibErrorStatus(st,OCDB_NO_CONN);
		return 1;
	}
	_ocesqlExecSelectIntoOccurs(st, id, query, nParams, nResParams);
	return 0;
}

/*
 * <Function name>
 *   OCESQLIDExecSelectIntoOccurs
 *
 * <Outline>
 *   ここでは接続ID取得のみを実施
 *   実処理は_ocesqlExecSelectIntoOccursで
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @id: ConnectionId
 *   @query: SQL query
 *   @nParams: パラメータの個数
 *   @nResParams: パラメータの個数
 */
int
OCESQLIDExecSelectIntoOccurs(struct sqlca_t *st, char *atdb, int atdblen, char *query, int nParams, int nResParams){
 	LOG("OCESQLIDExecSelectIntoOccurs start\n");
	int id;
	id = _ocesqlResolveCONNID(st, atdb, atdblen);
	if(id == RESULT_FAILED){
		ERRLOG("connection id is not found.\n");
		OCDBSetLibErrorStatus(st,OCDB_NO_CONN);
		return 1;
	}
	_ocesqlExecSelectIntoOccurs(st, id, query, nParams, nResParams);
	return 0;
}

/*
 * <Function name>
 *   _ocesqlExecSelectIntoOccurs
 *
 * <Outline>
 *   SELECT INTO の実行
 *   実行結果(OCDB_RES_* -> ocdb.h 参照)はSQLCAに格納する
 *   OCDBGetValueで返される値は左詰めになっていることを前提としている。
 *
 *   SELECTした値はSetHostTableで定義した繰り返し回数と一周のバイト数に
 *   従いセットされる
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @id: ConnectionId
 *   @query: SQL query
 *   @nParams: パラメータの個数
 *   @nResParams: パラメータの個数
 */
static void
_ocesqlExecSelectIntoOccurs(struct sqlca_t *st, int id, char *query, int nParams, int nResParams){

	int i, j;
	int tuples = 0;
	int fields = 0;
	SQLVARLIST *p = _sql_res_var_lists;


	// check argument
	if(query == NULL || strlen(query) == 0 || _occurs_iter > 500){
		OCDBSetLibErrorStatus(st,OCDB_EMPTY);
		return;
	}

	if(nParams > 0){
		_ocesqlExecParams(st, id, query, nParams);
	}else{
		_ocesqlExec(st, id, query);
	}

	if(OCDBSetResultStatus(id,st) != RESULT_SUCCESS){
		return;
	}

	fields = OCDBNfields(id);
	if(fields != nResParams){
		ERRLOG("ResParams(%d) and fields(%d) are different\n",nResParams, fields);
		OCDBSetLibErrorStatus(st,OCDB_EMPTY);
		return;
	}

	tuples = OCDBNtuples(id);
	if(tuples < 1){
		OCDBSetLibErrorStatus(st,OCDB_NOT_FOUND);
		LOG("TUPLES NODATA\n");
		return;
	}
	if(tuples > _occurs_iter){
		tuples = _occurs_iter;
	}
	for(j=0; j<tuples; j++){
		SQLVARLIST *p = _sql_res_var_lists;
		// set params
		char *retstr;
		for(i=0; i< _res_var_lists_length; i++, p = p->next){
			if(i>=fields)
				break;
			retstr = OCDBGetvalue(id, j, i);
			create_coboldata(&p->sv, j, retstr);
		}
	}

	if(tuples < _occurs_iter){
		for(;j<_occurs_iter;j++){
			SQLVARLIST *p = _sql_res_var_lists;
			// set zero
			for(i=0; i< _res_var_lists_length; i++, p = p->next){
				create_coboldata_lowvalue(&p->sv, j);
			}
		}

	}
	return;
}

/*
 * <Function name>
 *   OCESQLNtuples
 *
 * <Outline>
 *   結果リソースから行数を取得する
 *
 * <Input>
 *   @id: ConnectionId
 *
 * <Output>
 *   success: 行数
 *   failure: OCDB_INVALID_NUMBER
 */
int
OCESQLNtuples(int id){
	return OCDBNtuples(id);
}

/*
 * <Function name>
 *   OCESQLNfields
 *
 * <Outline>
 *   結果リソースからフィールド数を取得する
 *
 * <Input>
 *   @id: ConnectionId
 *
 * <Output>
 *   success: フィールド数
 *   failure: OCDB_INVALID_NUMBER
 */
int
OCESQLNfields(int id){
	return OCDBNfields(id);
}

/*
 * <Function name>
 *   OCESQLGetvalue
 *
 * <Outline>
 *   結果リソースの1行目の指定したフィールドからデータを文字列型で取得する
 *
 * <Input>
 *   @id: ConnectionId
 *   @index: フィールド番号(0からスタート)
 *
 * <Output>
 *   success: データ文字列
 *   failure: OCDB_INVALID_NUMBER
 */
char *
OCESQLGetvalue(int id, int index){
	return OCDBGetvalue(id, 0, index);
}

/*
 * <Function name>
 *   OCESQLResultErrorMessage
 *
 * <Outline>
 *   SQLCAからエラーメッセージを取得する
 *
 * <Input>
 *   @st: SQLCA
 *
 * <Output>
 *   success: エラーメッセージ
 *   failure: NULL
 */
char *
OCESQLResultErrorMessage(struct sqlca_t *st){
	return st->sqlerrm.sqlerrmc;
}

/*
 * <Function name>
 *   OCESQLDisconnect
 *
 * <Outline>
 *   実処理は_ocesqlDisconnect
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @atdb: Connection Identifier
 *   @atdblen: length of atdb
 */
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
	_ocesqlDisconnect(st, id);
	return 0;
}

/*
 * <Function name>
 *   OCESQLIDDisconnect
 *
 * <Outline>
 *   ここでは接続ID取得のみを実施
 *   実処理は_ocesqlDisconnect
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @atdb: Connection Identifier
 *   @atdblen: length of atdb
 */
int
OCESQLIDDisconnect(struct sqlca_t *st, char *atdb, int atdblen){
 	LOG("OCESQLIDDisconnect start\n");
	int id;
	id = _ocesqlResolveCONNID(st, atdb, atdblen);
	if(id == RESULT_FAILED){
		ERRLOG("connection id is not found.\n");
		OCDBSetLibErrorStatus(st,OCDB_NO_CONN);
		return 1;
	}
	_ocesqlDisconnect(st, id);
	return 0;
}

/*
 * <Function name>
 *   _ocesqlDisonnect
 *
 * <Outline>
 *   データベースから切断する
 *
 * <Input>
 *   @st: SQLCA pointer
 *   @id: ConnectionId
 *
 */
static void
_ocesqlDisconnect(struct sqlca_t *st, int id){
	OCDBFinish(id);
}

/*
 * <Function name>
 *   OCESQLStartSQL
 *
 * <Outline>
 *   SQLクエリ作成開始処理
 *   _sql_var_lists のクリアを行う
 *   _var_lists_length のクリアを行う
 *
 */
int
OCESQLStartSQL(void){
	LOG("#begin\n");
	init_sql_var_list();
	LOG("#end\n");
	return 0;
}

/*
 * <Function name>
 *   OCESQLSetSQLParams
 *
 * <Outline>
 *   指定されたパラメータを埋め込みSQLリストに追加する
 *
 * <Input>
 *   type : 埋込変数の型
 *   length : 埋込変数のsize
 *   scale : スケール(10のパワーとして表現されたフィールドのスケール)
 *   addr : 変数のアドレス
 *
 * < Output>
 *   何もしない
 *
 */
int
OCESQLSetSQLParams(int type, int length, int scale, void *addr){
	if(type < OCDB_TYPE_MIN || type > OCDB_TYPE_MAX){
		ERRLOG("invalide arugument 'type': %d\n", type);
		return 1;
	}

	if(length < 0){ // 長さがない場合
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

/*
 * <Function name>
 *   OCESQLSetSQLParams
 *
 * <Outline>
 *   指定されたパラメータをSQL結果格納リストに追加する
 *
 * <Input>
 *   type : 埋込変数の型
 *   length : 埋込変数のsize
 *   scale : スケール(10のパワーとして表現されたフィールドのスケール)
 *   addr : 変数のアドレス
 *
 * < Output>
 *   何もしない
 *
 */
int
OCESQLSetResultParams(int type, int length, int scale, void *addr){
	if(type < OCDB_TYPE_MIN || type > OCDB_TYPE_MAX){
		ERRLOG("invalide arugument 'type': %d\n", type);
		return 1;
	}

	if(length < 0){ // 長さがない場合
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

/*
 * <Function name>
 *   OCESQLSetHostTable
 *
 * <Outline>
 *   OCCURS変数がホスト変数に指定されている時に繰り返し回数と
 *   一周のバイト数をセットする
 *
 * <Input>
 *   iter: iteration count
 *   length: byte length of host variable
 *
 * < Output>
 *   何もしない
 *
 */
int
OCESQLSetHostTable(int iter, int length, int is_parent){
	if(iter < 0){
		ERRLOG("invalide arugument 'iter': %d\n", iter);
		return 1;
	}
	if(length < 0){
		ERRLOG("invalide arugument 'length': %d\n", length);
		return 1;
	}
	_occurs_iter = iter;
	_occurs_length = length;
	_occurs_is_parent = is_parent;
	return 0;
}

/*
 * <Function name>
 *   OCESQLEndSQL
 *
 * <Outline>
 *   SQLクエリ終了処理
 *   _sql_var_lists のクリアを行う
 *   _var_lists_length のクリアを行う
 */
int
OCESQLEndSQL(void){
	LOG("#debug start dump var_list\n");
	show_sql_var_list(_sql_var_lists);
	LOG("#debug start dump res_list\n");
	show_sql_var_list(_sql_res_var_lists);
	LOG("#debug end dump list\n");

	clear_sql_var_list(_sql_var_lists);
	clear_sql_var_list(_sql_res_var_lists);
	reset_sql_var_list();
	return 0;
}

/*
 * <Function name>
 *   init_sql_var_list
 *
 * <Outline>
 *   埋め込みSQLリスト初期化
 *   必要に応じてclear_sql_var_listも実行する
 */
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

/*
 * <Function name>
 *   reset_sql_var_list
 *
 * <Outline>
 *   グローバル変数初期化
 *   - _sql_var_lists
 *   - _var_lists_length
 */
static void
reset_sql_var_list(void){
	_sql_var_lists = NULL;
	_sql_res_var_lists = NULL;
	_var_lists_length = 0;
	_res_var_lists_length = 0;
	_occurs_length = 0;
	_occurs_iter = 0;
	_occurs_is_parent = 0;
	return;
}

/*
 * <Function name>
 *   new_sql_var_list
 *
 * <Outline>
 *   埋め込みSQLリスト生成
 */
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

/*
 * <Function name>
 *   add_sql_var_list
 *
 * <Outline>
 *   SQL埋込変数リストの要素を作成し追加する
 *   value には、length + 1の文字列を準備する
 *   addrのデータを型の長さに合わせてdataに格納する
 *   addrのデータと型情報をもとに整形したデータはrealdataに格納する
 *
 * <Input>
 *   type : 埋込変数の型
 *   length : 埋込変数のsize
 *   power : 埋込変数のpower
 *   addr : 変数のアドレス
 *
 */
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


	create_realdata(&p->sv, 0);
	_var_lists_length++;

	return p;
}

/*
 * <Function name>
 *   add_sql_var_list
 *
 * <Outline>
 *   SQL埋込変数リストの要素を作成し追加する
 *   value には、length + 1の文字列を準備する
 *   addrのデータを型の長さに合わせてdataに格納する
 *   addrのデータと型情報をもとに整形したデータはrealdataに格納する
 *
 * <Input>
 *   type : 埋込変数の型
 *   length : 埋込変数のsize
 *   power : 埋込変数のpower
 *   addr : 変数のアドレス
 *
 */
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

	_res_var_lists_length++;

	return p;
}

static void
create_realdata(SQLVAR *sv,int index){
	int type = sv->type;
	int length = sv->length;
	int power = sv->power;
	void *addr = sv->addr;
	char *caddr = addr;

	if(_occurs_is_parent){
		caddr += index * _occurs_length;
	}else{
		caddr += index * length;
	}
	addr = caddr;

	SQLVAR sv_tmp;
	sv_tmp.type = type;
	sv_tmp.length = length;
	sv_tmp.power = power;
	sv_tmp.addr = addr;
	sv_tmp.data = NULL;
	sv_tmp.realdata = NULL;

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

			ptr = (char *)sv_tmp.data + i * sizeof(char);
			tmp = (unsigned char)*ptr;
			int vallen = 2;

			if(i!=0 || !skip_first){
				com_sprintf(val, vallen, "%d", (tmp & ubit) >> 4);
				sv_tmp.realdata[index] = val[0];
				index++;
			}
			if(i != dlength - 1){
				com_sprintf(val, vallen, "%d", tmp & lbit);
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
		double dlength;
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
			int vallen = 2;

			ptr = (char *)sv_tmp.data + i * sizeof(char);
			tmp = (unsigned char)*ptr;

			if(i!=0 || !skip_first){
				com_sprintf(val, vallen, "%d", (tmp & ubit) >> 4);
				sv_tmp.realdata[index] = val[0];
				index++;
			}
			if(i != dlength - 1){
				com_sprintf(val, vallen, "%d", tmp & lbit);
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
	case OCDB_TYPE_ALPHANUMERIC_VARYING:
	{
		int lensize = 0;
		memcpy(&lensize,addr,OCDB_VARCHAR_HEADER_BYTE);
		LOG("VARYING-LEN:%d\n",lensize);

		sv_tmp.data = (char *)calloc(lensize + TERMINAL_LENGTH, sizeof(char));
		sv_tmp.realdata = (char *)calloc(lensize + TERMINAL_LENGTH, sizeof(char));

		memcpy(sv_tmp.data, (char *)((char *)addr + OCDB_VARCHAR_HEADER_BYTE), lensize);
		memcpy(sv_tmp.realdata, (char *)((char *)addr + OCDB_VARCHAR_HEADER_BYTE), lensize);
		LOG("%d %d->%d#%s#%s#\n", sv_tmp.type, lensize, sv_tmp.length, (char *)sv_tmp.data, (char *)sv_tmp.realdata);
		break;
	}
	case OCDB_TYPE_JAPANESE_VARYING:
	{
		int lensize = 0;
		memcpy(&lensize,addr,OCDB_VARCHAR_HEADER_BYTE);
		lensize = lensize * 2;
		LOG("VARYING-LEN:%d\n",lensize);

		sv_tmp.data = (char *)calloc(lensize + TERMINAL_LENGTH, sizeof(char));
		sv_tmp.realdata = (char *)calloc(lensize + TERMINAL_LENGTH, sizeof(char));

		memcpy(sv_tmp.data, (char *)((char *)addr + OCDB_VARCHAR_HEADER_BYTE), lensize);
		memcpy(sv_tmp.realdata, (char *)((char *)addr + OCDB_VARCHAR_HEADER_BYTE), lensize);
		LOG("%d %d->%d#%s#%s#\n", sv_tmp.type, lensize, sv_tmp.length, (char *)sv_tmp.data, (char *)sv_tmp.realdata);
		break;
	}
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
	if(sv->data){
			free(sv->data);
			sv->data = NULL;
		}
	if(sv_tmp.realdata != NULL){
		sv->realdata = com_strdup(sv_tmp.realdata);
		free(sv_tmp.realdata);
		sv_tmp.realdata = NULL;
	}
	if(sv_tmp.data != NULL){
		sv->data = com_strdup(sv_tmp.data);
		free(sv_tmp.data);
		sv_tmp.data = NULL;
	}
}

static void
create_coboldata_lowvalue(SQLVAR *sv, int index){
	void *addr = sv->addr;
	char *caddr = addr;
	if(_occurs_is_parent){
		caddr += index * _occurs_length;
	}else{
		caddr += index * sv->length;
	}
	addr = caddr;

	memset(addr,0,sv->length);

	return;
}
static void
create_coboldata(SQLVAR *sv, int index, char *retstr){
	void *addr = sv->addr;
	int tmp_len = 0;

	char *caddr = addr;
	if(_occurs_is_parent){
		caddr += index * _occurs_length;
	}else{
		caddr += index * sv->length;
	}
	addr = caddr;

	switch(sv->type){
	case OCDB_TYPE_UNSIGNED_NUMBER:
	{
		char *ptr;

		int fillzero;
		int zcount;
		char *final;
		int finalbuflen;

		// fill zero
		finalbuflen = sv->length + TERMINAL_LENGTH;
		final = (char *)calloc(finalbuflen, sizeof(char));

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
			final[zcount] = '0';
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
				final[zcount] = '0';
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
		int finalbuflen;
		int final_length;

		// fill zero
		finalbuflen = sv->length;
		final = (char *)calloc(finalbuflen, sizeof(char));

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
			final[zcount] = '0';
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
				final[zcount] = '0';
			}
		}

		final_length = strlen(final);
		if(is_negative){
			int index = *(final + (final_length - 1)) - '0';
			final[final_length - 1] = type_tc_negative_final_number[index];
		}

		memcpy(addr, final, sv->length);
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
		int finalbuflen;

		// fill zero
		finalbuflen = SIGN_LENGTH +  sv->length + TERMINAL_LENGTH;
		final = (char *)calloc(finalbuflen, sizeof(char));

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
			final[zcount] = '0';
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
				final[zcount] = '0';
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
		int pre_final_len;
		char *final;

		double dlength;
		int skip_first;
		int i;
		unsigned char ubit = 0xF0;
		unsigned char lbit = 0x0F;

		dlength = ceil(((double)sv->length + 1)/2);
		skip_first = (sv->length + 1) % 2; // 1 -> skip first 4 bits

		pre_final_len = sv->length + TERMINAL_LENGTH;
		pre_final = (char *)calloc(pre_final_len, sizeof(char));

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
			pre_final[zcount] = '0';
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
				pre_final[zcount] = '0';
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
		int pre_final_len;
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

		pre_final_len = (int)dlength + TERMINAL_LENGTH;
		pre_final = (char *)calloc(pre_final_len, sizeof(char));

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
			pre_final[zcount] = '0';
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
				pre_final[zcount] = '0';
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
	case OCDB_TYPE_ALPHANUMERIC_VARYING:
		if(strlen(retstr) >= sv->length){
			tmp_len = sv->length;
			memcpy(addr, &tmp_len, OCDB_VARCHAR_HEADER_BYTE);
			memcpy((char *)addr + OCDB_VARCHAR_HEADER_BYTE, retstr, sv->length);
		} else {
			tmp_len = strlen(retstr);
			memcpy(addr, &tmp_len, OCDB_VARCHAR_HEADER_BYTE);
			memset((char *)addr + OCDB_VARCHAR_HEADER_BYTE,' ',sv->length);
			memcpy((char *)addr + OCDB_VARCHAR_HEADER_BYTE,retstr,strlen(retstr));
		}
		LOG("VARYING-LEN:%d\n",tmp_len);
		break;
	case OCDB_TYPE_JAPANESE_VARYING:
		if(strlen(retstr) >= sv->length*2){
			tmp_len = sv->length;
			memcpy(addr, &tmp_len, OCDB_VARCHAR_HEADER_BYTE);
			memcpy(addr, retstr, sv->length*2);
		}else{
			int i;
			char *tmp = (char *)((char *)addr+OCDB_VARCHAR_HEADER_BYTE);
			for(i=0;i+1<sv->length*2;i=i+2){
				tmp[i] = 0x81;
				tmp[i+1] = 0x40;
			}
			tmp_len = strlen(retstr)/2;
			memcpy(addr, &tmp_len, OCDB_VARCHAR_HEADER_BYTE);
			memcpy((char *)addr + OCDB_VARCHAR_HEADER_BYTE,retstr,tmp_len*2);
		}
		LOG("VARYING-LEN:%d\n",tmp_len);
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

static int
get_varchar_length(char *hexval){
	int i, shift;
	int retval = 0;
	unsigned char tmp1, tmp2, tmp;
	if(get_endian() == LITTLEENDIAN){
		int is = 0;
		for(i=0; i<OCDB_VARCHAR_HEADER_BYTE; i+=2){
			tmp1 = tolower(hexval[i]);
			if(isdigit(tmp1)){
				tmp1 -= '0';
			} else {
				tmp1 -= ('a' - 10);
			}

			tmp2 = tolower(hexval[i+1]);
			if(isdigit(tmp2)){
				tmp2 -= '0';
			} else {
				tmp2 -= ('a' - 10);
			}
			tmp = (tmp1<<4) + tmp2;
			if(tmp == 0) is++;
			shift = i/2 - is;
			retval += (tmp<<(shift*8));
		}
	} else {
		for(i=0; i<OCDB_VARCHAR_HEADER_BYTE; i+=2){
			tmp1 = tolower(hexval[i]);
			if(isdigit(tmp1)){
				tmp1 -= '0';
			} else {
				tmp1 -= ('a' - 10);
			}

			tmp2 = tolower(hexval[i+1]);
			if(isdigit(tmp2)){
				tmp2 -= '0';
			} else {
				tmp2 -= ('a' - 10);
			}
			tmp = (tmp1<<4) + tmp2;
			shift = (OCDB_VARCHAR_HEADER_BYTE - i)/2 - 1;
			retval += (tmp<<(shift*8));
		}
	}

	return retval;
}

static void
set_varchar_length(int len, char *dest){
	int i;
	char c;

	if(get_endian() == LITTLEENDIAN){
		for(i=OCDB_VARCHAR_HEADER_BYTE-1 ; i>0; i-=2){
			c = 0xf & (len >> ((i-1)*4));
			dest[i] = (c>9) ? (c+'A'-10) : (c+'0');
			c = 0xf & (len >> (i*4));
			dest[i+1] = (c>9) ? (c+'A'-10) : (c+'0');
		}
	} else {
		for(i=0; i<OCDB_VARCHAR_HEADER_BYTE; i++){
			c = 0xf & (len >> (i*4));
			dest[OCDB_VARCHAR_HEADER_BYTE-i-1] =
			     (c>9) ? (c+'A'-10) : (c+'0');
		}
	}
	return;
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

static void
_ocesqlReleaseConnection(int status, void *arg){
	int connectId = *(int *)arg;
	OCDBFinish(connectId);
}

static int
_ocesqlResolveCONNID(struct sqlca_t *st, char *atdb, int atdblen){
	char *cid = NULL;
	int id;

	cid = get_str_without_after_space(oc_strndup(atdb, atdblen));
	if(cid == NULL){
		return RESULT_FAILED;
	}
	id = OCDBResolveCONNID(cid);
	if(id == RESULT_FAILED){
		ERRLOG("connection name %s is not found in connection list.\n", cid);
		OCDBSetLibErrorStatus(st,OCDB_EMPTY);
		free(cid);
		return RESULT_FAILED;
	}
	free(cid);

	return id;
}

/*
 * <Function name>
 *   new_cursor_list
 *
 * <Outline>
 *   CURSORリスト生成
 */
static CURSORLIST *
new_cursor_list(void){
	CURSORLIST *p;
	p = (CURSORLIST *)malloc(sizeof(CURSORLIST));
	if(p != NULL){
		// initialize
		p->connid = 0;
		p->cname = NULL;
		p->sp = NULL;
		p->query = NULL;
		p->nParams = 0;
		p->isOpened = 0;
		p->tuples = 0;
		p->next = NULL;
	}

	return p;
}

/*
 * <Function name>
 *   add_cursor_list
 *
 * <Outline>
 *   DeclareCursorで定義されたCURSORをリストに登録する
 *
 * <Input>
 *   id : 接続ID
 *   cname : CURSOR NAME
 *   query : query
 *   nParams : number of parameters
 *
 * <Output>
 *   success: RESULT_SUCCESS
 *   failure: RESULT_FAILED
 */
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
			if(p->query){
				free(p->query);
			}
			if(p->plist){
				clear_sql_var_list(p->plist);
			}
			break;
		}
	}
	if(!isExist){
		if((p->next = new_cursor_list()) == NULL){
			ERRLOG("cannot generate new CURSORLIST\n");
			return RESULT_ERROR;
		}
		p = p->next;
		p->cname = com_strdup(cname);
	}
	p->connid = id;
	p->query = com_strdup(query);
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
			current->sv.data = calloc(sp->sv.length + TERMINAL_LENGTH, sizeof(char));

			if (!current->sv.data){
				ERRLOG("current->sv.data allocation failed.\n");
				return RESULT_ERROR;
			}
			memcpy(current->sv.data, sp->sv.data, sp->sv.length);
			LOG("current->sv.data=#%s#\n", current->sv.data);

			current->sv.realdata = com_strdup(sp->sv.realdata);

			sp = sp->next;
			current = current->next;
		}
	}

	return RESULT_SUCCESS;
}


static int
add_cursor_list_with_prepare(int id, char *cname, PREPARELIST *prepare){
	CURSORLIST *p = &_cursor_list;
	int isExist=0;
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
		p->cname = com_strdup(cname);
	}
	p->connid = id;
	p->sp = prepare;
	p->isOpened = 0;
	p->tuples = 0;


	return RESULT_SUCCESS;
}

/*
 * <Function name>
 *   remove_cursor_list
 *
 * <Outline>
 *   DeclareCursorで定義されたCURSORをリストに登録する
 *
 * <Input>
 *   cname : CURSOR NAME
 */
static void
remove_cursor_list(char *cname){
	CURSORLIST *p, *prev;

	p = &_cursor_list;

	while(p->next != NULL){
		prev = p;
		p = p->next;
		if(strcmp(cname, p->cname) == 0){
			prev->next = p->next;
			if(p->cname)
				free(p->cname);
			if(p->query)
				free(p->query);
			free(p);
			LOG("remove cursor %s.\n", cname);
			return;
		}
	}
}

static void show_cursor_list(CURSORLIST *p){
	if(p != NULL){
		LOG("%d %s\n", p->connid, p->cname);
		show_cursor_list(p->next);
	}
}

/*
 * <Function name>
 *   clear_sql_var_list
 *
 * <Outline>
 *   CURSORリストのメモリ解放
 *
 * <Input>
 *   @CURSORLIST *
 */
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

/*
 * <Function name>
 *   get_cursor_list_connid
 *
 * <Outline>
 *   引数のcnameと一致するCURSOR情報を返す
 *
 * <Input>
 *   cname : CURSOR NAME
 *
 * <Output>
 *   success : CURSORLIST変数
 *   failure : RESULT_FAILED
 */
static CURSORLIST *
get_cursor_from_list(char *cname){
	CURSORLIST *p = &_cursor_list;
	CURSORLIST *ret = NULL;
	LOG("#target:%s#\n", cname);
	while(p->next != NULL){
		p = p->next;
		LOG("#search:%s#\n", p->cname);
		if(strcmp(cname, p->cname) == 0){
			ret = p;
			break;
		}
	}

	if(ret == NULL){
		ERRLOG("cursor name '%s' is not found in cursor list.\n", cname);
		return NULL;
	}
	LOG("#return:%s#\n", ret->cname);
	return ret;
}

static void
show_prepare_list(){
	PREPARELIST *p =&_prepare_list;
	while(p != NULL){
		if(p->sq.pname){
			LOG("#sname:%s#\n", p->sq.pname);
		}else{
			LOG("#sname:NULL#\n");
		}
		p = p->next;
	}
}

static PREPARELIST*
new_prepare_list(){
	PREPARELIST *p;
	p = (PREPARELIST *)malloc(sizeof(PREPARELIST));
	if(p != NULL){
		// initialize
		p->sq.pname = NULL;
		p->sq.query = NULL;
		p->sq.nParams = 0;
		p->next = NULL;
	}
	return p;

}

static PREPARELIST*
add_prepare_list(char *sname, char *query, int nParams){
	int isExist = 0;
	PREPARELIST *p =&_prepare_list;

	if(p == NULL){
		ERRLOG("_prepare_list has not been initialized\n");
		return NULL;
	}

	while(p->next != NULL){
		p = p->next;
		if(strcmp(p->sq.pname, sname)==0){
			LOG("prepare name '%s' already registered\n", sname);
			isExist =1;
			if(p->sq.query){
				free(p->sq.query);
				p->sq.query=NULL;
			}
			break;
		}
	}

	if(!isExist){
		if((p->next = new_prepare_list()) == NULL){
				ERRLOG("cannot generate new SQLVARLIST\n");
				return NULL;
		}
		p = p->next;
		p->sq.pname = com_strdup(sname);
	}
	p->sq.query = query;
	p->sq.nParams = nParams;

		show_prepare_list();

	return p;
}

static PREPARELIST *
get_prepare_from_list(char *sname){

	PREPARELIST *p =&_prepare_list;
	PREPARELIST *ret = NULL;

	if(p == NULL){
		ERRLOG("_prepare_list has not been initialized\n");
		return NULL;
	}
	LOG("#tartget:%s#\n", sname);
	while(p->next != NULL){
		p = p->next;
		if(p->sq.pname == NULL){
			break;
		}
		LOG("#search:%s#\n", p->sq.pname);
		if(strcmp(p->sq.pname, sname) == 0){
			ret = p;
			break;
		}
	}

	if(ret == NULL){
		ERRLOG("prepare name '%s' is not found in prepare list.\n", sname);
		show_prepare_list();
		return NULL;
	}
	LOG("#return:%s#\n", ret->sq.pname);
	return ret;
}





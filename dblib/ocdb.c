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
#include <malloc.h>
#include "ocdblog.h"
#include "ocdb.h"
#include "ocdbutil.h"

static struct conn_list _conn_lists = {{-1,NULL,0,0,0,0,NULL,NULL},NULL};
static int _next_conn_id = DEFAULT_NEXT_CONN_ID;

/* connection list */
static struct s_conn *look_up_conn_lists(int);
static struct conn_list *create_conn(int, char *, unsigned long);
static int add_conn_lists(int, char *, unsigned long);
static void free_conn_lists(int);

/*
 * OCDBCheckType
 * @dbtype
 * return value
 *   success: RESULT_SUCCESS
 *   failed: RESULT_FAILED
 */
int
OCDBCheckType(int dbtype){
	int retval = RESULT_FAILED;

	// check
	if(1)
	{
		retval = RESULT_SUCCESS;
	}

	return retval;
}

/*
 * OCDBConnect
 * @conninfo:  * failure: -1
 */
int
OCDBConnect(int dbtype, char *conninfo, char *connname, int autocommit, char *cencoding){
	int retval = OCDB_CONN_NOT_CONNECT;
	unsigned long connaddr;
#ifdef PGSQL_MODE_ON
	connaddr = OCDB_PGConnect(conninfo, autocommit, cencoding);
#endif

	if(connaddr == OCDB_CONN_FAIL_CONNECT){
		return OCDB_CONN_FAIL_CONNECT;
	}

	retval = add_conn_lists(dbtype, connname, connaddr);
	if(retval == INVALID_CONN_ID){
#ifdef PGSQL_MODE_ON
		OCDB_PGFinish(connaddr);
#endif
		return INVALID_CONN_ID;
	}

	LOG("connid=%d\n", retval);
	return retval;
}

/*
 * OCDBStatus
 * @id connction id
 * return
 *  success: OCDB_CONN_CONNECT_OK
 *  failure: OCDB_CONN_FAIL_CONNECT
 */
int
OCDBStatus(int id){
	struct s_conn *p_conn;
	int retval = OCDB_CONN_FAIL_CONNECT;

	p_conn = look_up_conn_lists(id);

	if(p_conn == NULL) return retval;

#ifdef PGSQL_MODE_ON
	retval = OCDB_PGstatus(p_conn->connaddr);
#endif

	return retval;
}

/*
 * OCDBErrorMessage
 * @id connction id
 * success: string
 * failure: NULL
 */
char *
OCDBErrorMessage(int id){
	struct s_conn *p_conn;
	char *retstr;

	p_conn = look_up_conn_lists(id);
	if(p_conn == NULL){
		return NULL;
	}

#ifdef PGSQL_MODE_ON
	retstr = OCDB_PGErrorMessage(p_conn->connaddr);
#endif

	return retstr;
}

/*
 * OCDBExec
 * @id connction id
 * @query SQL query
 */
void
OCDBExec(int id, char *query){
	struct s_conn *p_conn;

	p_conn = look_up_conn_lists(id);
	if(p_conn == NULL){
		return;
	}
	// initialize
#ifdef PGSQL_MODE_ON
	if(p_conn->resaddr != OCDB_RES_DEFAULT_ADDRESS){
		// release resource
		OCDB_PGClear(p_conn->resaddr);
	}
#endif
	p_conn->resaddr = OCDB_RES_DEFAULT_ADDRESS;
	p_conn->result = RESULT_SUCCESS;

#ifdef PGSQL_MODE_ON
	p_conn->resaddr = OCDB_PGExec(p_conn->connaddr, query);
	if(!p_conn->resaddr){
		ERRLOG("PostgreSQL Result is NULL");
	}
#endif
return;
}

/*
 * OCDBExecParams
 * @id connction id
 * @query SQL query
 */
void
OCDBExecParams(int id, char *query, int nParams,
		const int *paramsTypes, const char * const *paramValues,
		const int *paramLengths, const int *paramFormats,
		int resultFormat){
	struct s_conn *p_conn;

	p_conn = look_up_conn_lists(id);
	LOG("id=%d,p_conn=%d\n", id, p_conn);
	if(p_conn == NULL){
		return;
	}


	// initialize
#ifdef PGSQL_MODE_ON
	if(p_conn->resaddr != OCDB_RES_DEFAULT_ADDRESS){
		// release resource
		OCDB_PGClear(p_conn->resaddr);
	}
#endif
	p_conn->resaddr = OCDB_RES_DEFAULT_ADDRESS;
	p_conn->result = RESULT_SUCCESS;

#ifdef PGSQL_MODE_ON
	p_conn->resaddr = OCDB_PGExecParams(p_conn->connaddr, query, nParams, paramsTypes, paramValues,
			paramLengths, paramFormats, resultFormat);
	if(!p_conn->resaddr){
		ERRLOG("PostgreSQL Result is NULL");
	}
#endif
return;
}

/*
 * OCDBCursorDeclare
 * @id connction id
 * @cname cursor name
 * @query SQL query
 * @with_hold flag for Cursor Definition "WITH HOLD"
 */
void
OCDBCursorDeclare(int id, char *cname, char *query, int with_hold){
	struct s_conn *p_conn;

	p_conn = look_up_conn_lists(id);
	if(p_conn == NULL){
		return;
	}

	// initialize
#ifdef PGSQL_MODE_ON
	if(p_conn->resaddr != OCDB_RES_DEFAULT_ADDRESS){
		// release resource
		OCDB_PGClear(p_conn->resaddr);
	}
#endif
	p_conn->resaddr = OCDB_RES_DEFAULT_ADDRESS;
	p_conn->result = RESULT_SUCCESS;

#ifdef PGSQL_MODE_ON
	p_conn->resaddr = OCDB_PGCursorDeclare(p_conn->connaddr, cname, query, with_hold);
	if(!p_conn->resaddr){
		ERRLOG("PostgreSQL Result is NULL");
	}
#endif
return;
}

/*
 * OCDBCursorDeclareParams
 * @id connction id
 * @query SQL query
 * @with_hold flag for Cursor Definition "WITH HOLD"
 */
void
OCDBCursorDeclareParams(int id, char *cname, char *query, int nParams,
		const int *paramsTypes, const char * const *paramValues,
		const int *paramLengths, const int *paramFormats,
		int resultFormat, int with_hold){
	struct s_conn *p_conn;

	p_conn = look_up_conn_lists(id);
	if(p_conn == NULL){
		return;
	}

	// initialize
#ifdef PGSQL_MODE_ON
	if(p_conn->resaddr != OCDB_RES_DEFAULT_ADDRESS){
		// release resource
		OCDB_PGClear(p_conn->resaddr);
	}
#endif
	p_conn->resaddr = OCDB_RES_DEFAULT_ADDRESS;
	p_conn->result = RESULT_SUCCESS;

#ifdef PGSQL_MODE_ON
	p_conn->resaddr = OCDB_PGCursorDeclareParams(p_conn->connaddr, cname, query, nParams,
			paramsTypes, paramValues, paramLengths,
			paramFormats, resultFormat, with_hold);
	if(!p_conn->resaddr){
		ERRLOG("PostgreSQL Result is NULL");
	}
#endif
return;
}

/*
 * OCDBCursorOpen
 * @id connction id
 * @cname cursor name
 */
void
OCDBCursorOpen(int id, char *cname){
	struct s_conn *p_conn;

	p_conn = look_up_conn_lists(id);
	if(p_conn == NULL){
		return;
	}

	// initialize
#ifdef PGSQL_MODE_ON
	if(p_conn->resaddr != OCDB_RES_DEFAULT_ADDRESS){
		// release resource
		OCDB_PGClear(p_conn->resaddr);
	}
#endif
	p_conn->resaddr = OCDB_RES_DEFAULT_ADDRESS;
	p_conn->result = RESULT_SUCCESS;

#ifdef PGSQL_MODE_ON
	// dummy
	p_conn->result = RESULT_FLAG1_PGSQL_DUMMYOPEN;
	return;
#endif

	return;
}

/*
 * OCDBCursorFetchOne
 * @id connction id
 * @cname cursor name
 */
void
OCDBCursorFetchOne(int id, char *cname, int fetchmode){
	struct s_conn *p_conn;

	p_conn = look_up_conn_lists(id);
	if(p_conn == NULL){
		return;
	}

	// initialize
#ifdef PGSQL_MODE_ON
	if(p_conn->resaddr != OCDB_RES_DEFAULT_ADDRESS){
		// release resource
		OCDB_PGClear(p_conn->resaddr);
	}
#endif
	p_conn->resaddr = OCDB_RES_DEFAULT_ADDRESS;
	p_conn->result = RESULT_SUCCESS;

#ifdef PGSQL_MODE_ON
	LOG("addr:%d, cname:%s, mode:%d\n",p_conn->connaddr, cname, fetchmode);
	p_conn->resaddr = OCDB_PGCursorFetchOne(p_conn->connaddr, cname, fetchmode);
	if(!p_conn->resaddr){
		ERRLOG("PostgreSQL Result is NULL");
	}
#endif
return;
}

/*
 * OCDBCursorFetchOccurs
 * @id connction id
 * @cname cursor name
 * @count fetch data count
 */
void
OCDBCursorFetchOccurs(int id, char *cname, int fetchmode, int count){
	struct s_conn *p_conn;

	p_conn = look_up_conn_lists(id);
	if(p_conn == NULL){
		return;
	}

	// initialize
#ifdef PGSQL_MODE_ON
	if(p_conn->resaddr != OCDB_RES_DEFAULT_ADDRESS){
		// release resource
		OCDB_PGClear(p_conn->resaddr);
	}
#endif
	p_conn->resaddr = OCDB_RES_DEFAULT_ADDRESS;
	p_conn->result = RESULT_SUCCESS;

#ifdef PGSQL_MODE_ON
	LOG("addr:%d, cname:%s, mode:%d, count:%d\n",p_conn->connaddr, cname, fetchmode, count);
	p_conn->resaddr = OCDB_PGCursorFetchOccurs(p_conn->connaddr, cname, fetchmode, count);
	if(!p_conn->resaddr){
		ERRLOG("PostgreSQL Result is NULL");
	}
#endif
return;
}

/*
 * OCDBCursorClose
 * @id connction id
 * @cname cursor name
 */
void
OCDBCursorClose(int id, char *cname){
	struct s_conn *p_conn;

	p_conn = look_up_conn_lists(id);
	if(p_conn == NULL){
		return;
	}

	// initialize
#ifdef PGSQL_MODE_ON
	if(p_conn->resaddr != OCDB_RES_DEFAULT_ADDRESS){
		// release resource
		OCDB_PGClear(p_conn->resaddr);
	}
#endif
	p_conn->resaddr = OCDB_RES_DEFAULT_ADDRESS;
	p_conn->result = RESULT_SUCCESS;

#ifdef PGSQL_MODE_ON
	p_conn->resaddr = OCDB_PGCursorClose(p_conn->connaddr, cname);
	if(!p_conn->resaddr){
		ERRLOG("PostgreSQL Result is NULL");
	}
#endif
return;
}

int
OCDBSetResultStatus(int id, struct sqlca_t *st){
	struct s_conn *p_conn;
	int retval;

	p_conn = look_up_conn_lists(id);
	if(p_conn == NULL){
	  //return OCDB_RES_FATAL_ERROR;
		return RESULT_ERROR;
	}

	if(p_conn->resaddr == OCDB_RES_DEFAULT_ADDRESS &&
		p_conn->result <= RESULT_FLAGBASE){
		// 結果リソースが無いため成功で返す
	  //return OCDB_RES_COMMAND_OK;
		return RESULT_ERROR;
	}

#ifdef PGSQL_MODE_ON
	if(p_conn->result == RESULT_FLAG1_PGSQL_DUMMYOPEN){
		// DUMMY OPENの対応
		retval = RESULT_SUCCESS;
	} else {
		retval = OCDB_PGSetResultStatus(p_conn->resaddr,st);
	}
#endif
	return retval;
}

/*
 * OCDBResultErrorMessage
 * @id connction id
 * return value
 *   errorMessage(or NULL)
 */
char *
OCDBResultErrorMessage(int id){
	struct s_conn *p_conn;

	LOG("id=%d\n", id);
	p_conn = look_up_conn_lists(id);
	if(p_conn == NULL){
		return OCDB_RES_NOCONNECT_ERRORMSG;
	}

	// initialize
	p_conn->errorMessage = NULL;
#ifdef PGSQL_MODE_ON
	p_conn->errorMessage = OCDB_PGResultErrorMessage(p_conn->resaddr);
#endif
	return p_conn->errorMessage;
}


char *
OCDBResultErrorField(int id){
	struct s_conn *p_conn;
	char *errfield;

	LOG("id=%d\n", id);
	p_conn = look_up_conn_lists(id);
	if(p_conn == NULL){
		return OCDB_RES_NOCONNECT_ERRORMSG;
	}

	// initialize
	errfield = NULL;
#ifdef PGSQL_MODE_ON
	errfield = OCDB_PGResultErrorField(p_conn->resaddr);
#endif
	return errfield;
}

int
OCDBCmdTuples(int id){
	struct s_conn *p_con;
	int retval;

	p_con = look_up_conn_lists(id);
	if(p_con == NULL){
		return OCDB_INVALID_NUMBER;
	}

#ifdef PGSQL_MODE_ON
	retval = OCDB_PGcmdtuples(p_con->resaddr);
#endif

	return retval;
}


int
OCDBNtuples(int id){
	struct s_conn *p_con;
	int retval;

	p_con = look_up_conn_lists(id);
	if(p_con == NULL){
		return OCDB_INVALID_NUMBER;
	}

#ifdef PGSQL_MODE_ON
	retval = OCDB_PGntuples(p_con->resaddr);
#endif

	return retval;
}

int
OCDBNfields(int id){
	struct s_conn *p_conn;
	int retval;

	p_conn = look_up_conn_lists(id);
	if(p_conn == NULL){
		return OCDB_INVALID_NUMBER;
	}

#ifdef PGSQL_MODE_ON
	retval = OCDB_PGnfields(p_conn->resaddr);
#endif

	return retval;
}

char *
OCDBFname(int id, int index){
	struct s_conn *p_conn;
	char *retstr;

	p_conn = look_up_conn_lists(id);
	if(p_conn == NULL){
		return OCDB_INVALID_STRING;
	}

#ifdef PGSQL_MODE_ON
	retstr = OCDB_PGfname(p_conn->resaddr, index);
#endif

	return retstr;
}

int
OCDBFnumber(int id, const char *fname){
	struct s_conn *p_conn;
	int retval;

	p_conn = look_up_conn_lists(id);
	if(p_conn == NULL){
		return OCDB_INVALID_NUMBER;
	}

#ifdef PGSQL_MODE_ON
	retval = OCDB_PGfnumber(p_conn->resaddr, fname);
#endif

	return retval;
}

char *
OCDBGetvalue(int id, int row, int cnum){
	struct s_conn *p_conn;
	char *retstr;

	p_conn = look_up_conn_lists(id);
	if(p_conn == NULL){
		return NULL;
	}

#ifdef PGSQL_MODE_ON
	retstr = OCDB_PGgetvalue(p_conn->resaddr, row, cnum);
	LOG("retstr:%s\n",retstr);
#endif

	return retstr;
}


void
OCDBDropTable(int id, char *tname){
	struct s_conn *p_conn;

	p_conn = look_up_conn_lists(id);
	if(p_conn == NULL){
		return;
	}

#ifdef PGSQL_MODE_ON
	p_conn->resaddr = OCDB_PGDropTable(p_conn->connaddr, tname);
	if(!p_conn->resaddr){
		ERRLOG("PostgreSQL Result is NULL");
	}
#endif
return;
}

void
OCDBDeleteTable(int id, char *tname){
	struct s_conn *p_conn;

	p_conn = look_up_conn_lists(id);
	if(p_conn == NULL){
		return;
	}

#ifdef PGSQL_MODE_ON
	p_conn->resaddr = OCDB_PGDeleteTable(p_conn->connaddr, tname);
	if(!p_conn->resaddr){
		ERRLOG("PostgreSQL Result is NULL");
	}
#endif
return;
}

void
OCDBFinish(int id){
	struct s_conn *p_conn;

	p_conn = look_up_conn_lists(id);
	if(p_conn == NULL){
		LOG("no connection id %d found.\n", id);
		return;
	}

#ifdef PGSQL_MODE_ON
	if(p_conn->resaddr){
		OCDB_PGClear(p_conn->resaddr);
	}
	OCDB_PGFinish(p_conn->connaddr);
#endif
	LOG("connection id %d released.\n", id);
	free_conn_lists(id);
	return;
}

int
OCDBSetLibErrorStatus(struct sqlca_t * st, int errorno){
	int retval;
#ifdef PGSQL_MODE_ON
	retval = OCDB_PGSetLibErrorStatus(st, errorno);
#endif
	return retval;
}

/*
 * <Function name>
 *   OCDBResolveCONNID
 *
 * <Outline>
 *   接続識別子から接続IDを探し出す
 *
 * <Input>
 *  cid : 接続識別子
 *
 * <Output>
 *   success : 接続ID
 *   failure : -1(RESULT_FAILED)
 */
int
OCDBResolveCONNID(char *cid){
	struct conn_list *tmp = &_conn_lists;

	while(tmp->next != NULL){
		tmp = tmp->next;
		LOG("#%d#%s#search:%s#\n", tmp->sc.id, tmp->sc.cid, cid);
		if(strcmp(cid, tmp->sc.cid) == 0){
			LOG("return connid=%d\n", tmp->sc.id);
			return tmp->sc.id;
			break;
		}
	}

	return RESULT_FAILED;
}

/*
 * <Function name>
 *   look_up_conn_lists
 *
 * <Outline>
 *   接続IDの情報を接続リストから捜し出す
 *
 * <Input>
 *  id : 接続ID
 *
 * <Output>
 *   success : 接続リソース構造体(struct s_conn *)
 *   failure : NULL
 */
static struct s_conn *
look_up_conn_lists(int id){
	struct s_conn *retval = NULL;
	struct conn_list *tmp = &_conn_lists;

	while(tmp->next != NULL){
		tmp = tmp->next;
		if(tmp->sc.id == id){
			retval = &(tmp->sc);
			break;
		}
	}
	return retval;
}

/*
 * <Function name>
 *   create_conn
 *
 * <Outline>
 *   接続用リストの要素を新規に作成する
 *
 * <Input>
 *   dbtype : データベースのタイプ
 *   connname : 接続時に指定した識別子(指定が無い場合はNULL)
 *   connaddr : コネクションリソースのアドレス(PostgresならPGconn *と同等)
 *
 * <Output>
 *   success : struct conn_list *
 *   failure ; NULL
 */
static struct conn_list *
create_conn(int dbtype, char *connname, unsigned long connaddr){
	// initialize
	int newId;
	struct conn_list *p;

	p = (struct conn_list *)malloc(sizeof(struct conn_list));
	if(p != NULL){
		newId = _next_conn_id;
		_next_conn_id++;

		p->next = NULL;
		p->sc.id = newId;
		p->sc.cid = com_strdup(connname);
		p->sc.dbtype = dbtype;
		p->sc.connaddr = connaddr;
		p->sc.resaddr = OCDB_RES_DEFAULT_ADDRESS;
		p->sc.result = RESULT_SUCCESS;;
		p->sc.errorMessage = NULL;
		p->sc.pid = NULL;
	}

	return p;
}

/*
 * <Function name>
 *   create_conn
 *
 * <Outline>
 *   接続用リストの要素をコネクションリストに追加する
 *
 * <Input>
 *   dbtype : データベースのタイプ
 *   connname : 接続時に指定した識別子(指定が無い場合はNULL)
 *   connaddr : コネクションリソースのアドレス(PostgresならPGconn *と同等)
 *
 * <Output>
 *   success : craete_connで発行された接続ID
 *   failure : INVALID_CONN_ID
 */
static int
add_conn_lists(int dbtype, char *connname, unsigned long connaddr){
	// add list and return unique ID(start: 1)
	struct conn_list *index = &_conn_lists;

	while(index->next != NULL){
		index = index->next;
		if(index->sc.cid != NULL &&
				strcmp(index->sc.cid, connname) == 0){
			ERRLOG("connection id %s is already registered.\n", connname);
			return INVALID_CONN_ID;
		}
		if(index->sc.cid != NULL &&
				strcmp(index->sc.cid, connname) == 0){
			return index->sc.id;
		}
		/*--------------------------*/
	}

	// insert
	index->next = create_conn(dbtype, connname, connaddr);
	if(index->next == NULL){
		return INVALID_CONN_ID;
	}

	return index->next->sc.id;
}

/*
 * <Function name>
 *   free_conn_lists
 *
 * <Outline>
 *   接続IDに対応する接続を終了し、リストから消去する
 *
 * <Input>
 *  id : 接続ID
 *
 * <Output>
 *  なし
 */
static void
free_conn_lists(int id){
	struct conn_list *tmp = &_conn_lists;
	struct conn_list *tmp_prev = NULL;

	while(tmp->next != NULL){
		tmp_prev = tmp;
		tmp = tmp->next;
		if(tmp->sc.id == id){
			tmp_prev->next = tmp->next;
			if(tmp->sc.cid) free(tmp->sc.cid);
			if(tmp->sc.pid) free(tmp->sc.pid);
			free(tmp);
			break;
		}
	}
}

/*
 * common APIs
 */
char *
_alloc(long size)
{
	char *new = (char *) calloc(1L, size);
	return (new);
}

/*
 *	last_dir_separator
 *
 * Find the location of the last directory separator, return
 * NULL if not found.
 */
char *
last_dir_separator(const char *filename)
{
	const char *p,
	*ret = NULL;

	for (p = skip_drive(filename); *p; p++)
		if (IS_DIR_SEP(*p))
			ret = p;
	return (char *) ret;
}

int
strlen_or_null(const char *string)
{
	if (!string)
		return 0;
	return (strlen(string));
}


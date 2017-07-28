/*
 * Copyright (C) 2015 Tokyo System House Co.,Ltd.
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
#include <string.h>
#include <malloc.h>

#include "ocpgsql.h"
#include "ocdblog.h"
#include "ocdbutil.h"

#define SET_SQLSTATE(ststate,sqlstate) memcpy((ststate),(sqlstate), SQLSTATE_LEN)

static char sql_savepoint[]          = "SAVEPOINT oc_save";
static char sql_release_savepoint[]  = "RELEASE SAVEPOINT oc_save";
static char sql_rollback_savepoint[] = "ROLLBACK TO oc_save";

static unsigned long PGLockConnId = OCDB_CONN_FAIL_CONNECT;

//単文ロールバックフラグ
static int rollback_one_mode = 0;

unsigned long
OCDB_PGConnect(char *conninfo, int autocommit, char *cencoding){
	PGconn *conn;
	PGresult *res = NULL;
	char *env_rollback_mode;

	conn = PQconnectdb(conninfo);

	if(conn == NULL){
		return OCDB_CONN_FAIL_CONNECT;
	} else if(PQstatus(conn) != CONNECTION_OK){
		ERRLOG("%s\n",PQerrorMessage(conn));
		PQfinish(conn);
		return OCDB_CONN_FAIL_CONNECT;
	}

	PQsetClientEncoding(conn, cencoding);

	if(autocommit == OCDB_AUTOCOMMIT_ON){
		res = PQexec(conn, "SET AUTOCOMMIT TO ON");
	} else {
		res = PQexec(conn, "SET AUTOCOMMIT TO OFF");
	}
	if(res != NULL){
		PQclear(res);
	}
	env_rollback_mode = com_getenv("OCDB_PG_IGNORE_ERROR", NULL);
	if(env_rollback_mode != NULL && strcmp(env_rollback_mode, "Y")==0){
		rollback_one_mode = 1;
	}else{
		rollback_one_mode = 0;
	}

	return (unsigned long)conn;
}

int
OCDB_PGstatus(unsigned long connadr){
	ConnStatusType stat;

	stat = PQstatus((PGconn *)connadr);

	if(stat == CONNECTION_OK){
		return OCDB_CONN_CONNECT_OK;
	}

	return OCDB_CONN_FAIL_CONNECT;
}

char *
OCDB_PGErrorMessage(unsigned long connadr){
	PGconn *conn;
	conn = (PGconn *)connadr;
	return  PQerrorMessage(conn);
}

void
OCDB_PGClear(unsigned long resaddr){
	PGresult *res;

	res = (PGresult *)resaddr;
	PQclear(res);
	return;
}

unsigned long
OCDB_PGExec(unsigned long connadr, char *query){
	PGconn *conn;
	PGresult *res;
	int status;
	int tmp_rollback_one_mode=0;

	LOG("CONNADDR: %d, EXEC SQL: %s\n", connadr, query);
	conn = (PGconn *)connadr;
	if(rollback_one_mode){
		if(strcmp(query, "BEGIN") == 0 ||
				strcmp(query, "COMMIT") == 0 ||
				strcmp(query, "ROLLBACK") == 0){
			tmp_rollback_one_mode = 0;
		}
		if(tmp_rollback_one_mode){
			PQexec(conn, sql_savepoint);
		}
	}
	res = PQexec(conn, query);
	if(tmp_rollback_one_mode){
		status = PQresultStatus(res);
		if(status == PGRES_COMMAND_OK || status == PGRES_TUPLES_OK ||
				status == PGRES_COPY_OUT || status == PGRES_COPY_IN){
			PQexec(conn, sql_release_savepoint);
		} else {
			PQexec(conn, sql_rollback_savepoint);
			PQexec(conn, sql_release_savepoint);
		}
	}
	return (unsigned long)res;
}

unsigned long
OCDB_PGExecParams(unsigned long connadr, char *query, int nParams,
		const int *paramsTypes, const char * const *paramValues,
		const int *paramLengths, const int *paramFormats,
		int resultFormat){
	PGconn *conn;
	PGresult *res;
	int status;

	LOG("CONNADDR: %d, EXEC SQL: %s\n", connadr, query);
	conn = (PGconn *)connadr;
	if(rollback_one_mode){
		PQexec(conn, sql_savepoint);
	}
	res = PQexecParams(conn, query, nParams, (const Oid *)paramsTypes, paramValues,
			paramLengths, paramFormats, resultFormat);
	conn = (PGconn *)connadr;
	if(rollback_one_mode){
		status = PQresultStatus(res);
		if(status == PGRES_COMMAND_OK || status == PGRES_TUPLES_OK ||
				status == PGRES_COPY_OUT || status == PGRES_COPY_IN){
			PQexec(conn, sql_release_savepoint);
		} else {
			PQexec(conn, sql_rollback_savepoint);
			PQexec(conn, sql_release_savepoint);
		}
	}
	return (unsigned long)res;
}

unsigned long
OCDB_PGCursorDeclare(unsigned long connadr, char *cname, char *query, int with_hold){
	const char *query_part_with_hold_on[] = {"DECLARE ", " CURSOR WITH HOLD FOR "};
	const char *query_part[] = {"DECLARE ", " CURSOR FOR "};
	int true_query_size;
	char *true_query;
	unsigned long res;

	if(with_hold == OCDB_CURSOR_WITH_HOLD_ON){
		true_query_size = strlen(query_part_with_hold_on[0]) + strlen(cname) +
			strlen(query_part_with_hold_on[1]) + strlen(query) + 1;
		true_query = (char *)malloc(true_query_size * sizeof(char));
		if(true_query == NULL){
			return OCDB_RES_DEFAULT_ADDRESS;
		}

		// build query
		com_sprintf(true_query, true_query_size, "%s%s%s%s",  query_part_with_hold_on[0], cname,
				query_part_with_hold_on[1], query);
	} else {
		true_query_size = strlen(query_part[0]) + strlen(cname) +
			strlen(query_part[1]) + strlen(query) + 1;
		true_query = (char *)malloc(true_query_size * sizeof(char));
		if(true_query == NULL){
			return OCDB_RES_DEFAULT_ADDRESS;
		}

		// build query
		com_sprintf(true_query, true_query_size, "%s%s%s%s",  query_part[0], cname, query_part[1], query);
	}

	res = (unsigned long)OCDB_PGExec(connadr, true_query);
	free(true_query);
	return res;
}

unsigned long
OCDB_PGCursorDeclareParams(unsigned long connadr, char *cname, char *query, int nParams,
		const int *paramsTypes, const char * const *paramValues,
		const int *paramLengths, const int *paramFormats,
		int resultFormat, int with_hold){
	const char *query_part_with_hold_on[] = {"DECLARE ", " CURSOR WITH HOLD FOR "};
	const char *query_part[] = {"DECLARE ", " CURSOR FOR "};
	char *true_query;
	int true_query_size;
	unsigned long res;

	if(with_hold == OCDB_CURSOR_WITH_HOLD_ON){
		true_query_size = strlen(query_part_with_hold_on[0]) + strlen(cname) +
			strlen(query_part_with_hold_on[1]) + strlen(query) + 1;
		true_query = (char *)malloc(true_query_size * sizeof(char));
		if(true_query == NULL){
			return OCDB_RES_DEFAULT_ADDRESS;
		}

		// build query
		com_sprintf(true_query, true_query_size, "%s%s%s%s",  query_part_with_hold_on[0], cname,
				query_part_with_hold_on[1], query);
	} else {
		true_query_size = strlen(query_part[0]) + strlen(cname) +
			strlen(query_part[1]) + strlen(query) + 1;
		true_query = (char *)malloc(true_query_size * sizeof(char));
		if(true_query == NULL){
			return OCDB_RES_DEFAULT_ADDRESS;
		}

		// build query
		com_sprintf(true_query, true_query_size, "%s%s%s%s",  query_part[0], cname, query_part[1], query);
	}

	res = (unsigned long)OCDB_PGExecParams(connadr, true_query, nParams,
			paramsTypes, paramValues,
			paramLengths, paramFormats, resultFormat);
	free(true_query);
	return res;
}

unsigned long
OCDB_PGCursorFetchOne(unsigned long connadr, char *cname, int fetchmode){
	const char *query_part[] = {"FETCH ", " RELATIVE ", " FROM "};
	const char next[] = "1";
	const char current[] = "0";
	const char previous[] = "-1";
	char *true_query;
	int true_query_size;
	unsigned long res;

	true_query_size = strlen(query_part[0]) + strlen(query_part[1]) + 1 +
		strlen(query_part[2]) + strlen(cname) + 1;
	true_query = (char *)malloc(true_query_size * sizeof(char));
	if(true_query == NULL){
		return OCDB_RES_DEFAULT_ADDRESS;
	}
	// build query
	if(fetchmode == OCDB_READ_CURRENT){
		com_sprintf(true_query, true_query_size, "%s%s%s%s%s",
				query_part[0], query_part[1], current, query_part[2], cname);
	} else if(fetchmode == OCDB_READ_PREVIOUS){
		com_sprintf(true_query, true_query_size, "%s%s%s%s%s",
				query_part[0], query_part[1], previous, query_part[2], cname);
	} else { // NEXT
		com_sprintf(true_query, true_query_size, "%s%s%s%s%s",
				query_part[0], query_part[1], next, query_part[2], cname);
	}
	res = (unsigned long)OCDB_PGExec(connadr, true_query);
	free(true_query);
	return res;
}

unsigned long
OCDB_PGCursorFetchOccurs(unsigned long connadr, char *cname, int fetchmode, int fetchcount){

	const char *query_part[] = {"FETCH ", " FORWARD ", " BACKWARD " , " FROM "};
	char *true_query;
	int true_query_size;
	char *str_fetchcount;
	int str_fetchcount_size;
	const char *str_read_mode;
	unsigned long res;

	//カレントはエラーとする
	if(fetchmode == OCDB_READ_CURRENT){
		return OCDB_RES_DEFAULT_ADDRESS;
	}

	//読み込み件数文字列を作成
	str_fetchcount_size = (int)log10(fetchcount) + 1 + 1;
	str_fetchcount = (char *)calloc(str_fetchcount_size, sizeof(char));
	if(str_fetchcount == NULL){
		return OCDB_RES_DEFAULT_ADDRESS;
	}
	com_sprintf(str_fetchcount, str_fetchcount_size * sizeof(char),
		    "%d",fetchcount); 

	if(fetchmode == OCDB_READ_PREVIOUS){
		str_read_mode = query_part[2];
	}else{
		str_read_mode = query_part[1];
	}

	true_query_size = strlen(query_part[0]) + strlen(str_read_mode) + strlen(str_fetchcount) +
		strlen(query_part[3]) + strlen(cname) + 1;
	true_query = (char *)malloc(true_query_size * sizeof(char));
	if(true_query == NULL){
		return OCDB_RES_DEFAULT_ADDRESS;
	}
	// build query
	com_sprintf(true_query, true_query_size * sizeof(char), "%s%s%s%s%s",
			query_part[0], str_read_mode, str_fetchcount, query_part[3], cname);

	res = (unsigned long)OCDB_PGExec(connadr, true_query);
	free(true_query);
	free(str_fetchcount);
	return res;
}

unsigned long
OCDB_PGCursorClose(unsigned long connadr, char *cname){
	const char *query_part[] = {"CLOSE "};
	char *true_query;
	int true_query_size;
	unsigned long res;

	true_query_size = strlen(query_part[0]) + strlen(cname) + 1;
	true_query = (char *)malloc(true_query_size * sizeof(char));
	if(true_query == NULL){
		return OCDB_RES_DEFAULT_ADDRESS;
	}

	// build query
	com_sprintf(true_query, true_query_size, "%s%s",  query_part[0], cname);
	res = (unsigned long)OCDB_PGExec(connadr, true_query);
	free(true_query);
	return res;
}

char *
OCDB_PGResultErrorField(unsigned long connres){
	PGresult *res = (PGresult *)connres;
	return  PQresultErrorField(res,PG_DIAG_SQLSTATE);
}

char *
OCDB_PGResultErrorMessage(unsigned long connres){
	PGresult *res = (PGresult *)connres;
	return PQresultErrorMessage(res);
}

int
OCDB_PGcmdtuples(unsigned long connres){
	PGresult *res = (PGresult *)connres;
	char *result = PQcmdTuples(res);
	if((result != NULL) && (strlen(result) > 0)){
		return  atoi(result);
	}
	return 0;
}

int
OCDB_PGntuples(unsigned long connres){
	PGresult *res = (PGresult *)connres;
	return  PQntuples(res);
}

int
OCDB_PGnfields(unsigned long connres){
	PGresult *res = (PGresult *)connres;
	return  PQnfields(res);
}

char *
OCDB_PGfname(unsigned long connres, int index){
	PGresult *res = (PGresult *)connres;
	return  PQfname(res, index);
}

int
OCDB_PGfnumber(unsigned long connres, const char *fname){
	PGresult *res = (PGresult *)connres;
	return  PQfnumber(res, fname);
}

char *
OCDB_PGgetvalue(unsigned long connres, int row, int cnum){
	PGresult *res = (PGresult *)connres;
	return  PQgetvalue(res, row, cnum);
}

unsigned long
OCDB_PGDropTable(unsigned long connaddr, char *tname){
	PGconn *conn;
	char *query;
	int query_size;
	unsigned long retval;
	const char *constr[] = {"DROP TABLE IF EXISTS "};

	query_size = strlen(constr[0]) + strlen(tname) + TERMINAL_LENGTH;
	query = (char *)calloc(query_size, sizeof(char));
	if(query == NULL){
		ERRLOG("memory allocation failed.\n");
		return OCDB_RES_DEFAULT_ADDRESS;
	}
	com_sprintf(query, query_size, "%s%s", constr[0], tname);
	conn = (PGconn *)connaddr;
	retval = (unsigned long)OCDB_PGExec(connaddr, query);
	free(query);
	return retval;
};

unsigned long
OCDB_PGDeleteTable(unsigned long connaddr, char *tname){
	PGconn *conn;
	char *query;
	int query_size;
	unsigned long retval;
	const char *constr[] = {"DELETE FROM "};

	query_size = strlen(constr[0]) + strlen(tname) + TERMINAL_LENGTH;
	query = (char *)calloc(query_size, sizeof(char));
	if(query == NULL){
		ERRLOG("memory allocation failed.\n");
		return OCDB_RES_DEFAULT_ADDRESS;
	}
	com_sprintf(query, query_size, "%s%s", constr[0], tname);
	conn = (PGconn *)connaddr;
	retval = (unsigned long)OCDB_PGExec(connaddr, query);
	free(query);
	return retval;
};

void
OCDB_PGFinish(unsigned long connaddr){
	PGconn *conn;

	conn = (PGconn *)connaddr;
	PQfinish(conn);
	if(PGLockConnId != OCDB_CONN_FAIL_CONNECT){
		PQfinish((PGconn *)PGLockConnId);
		PGLockConnId = OCDB_CONN_FAIL_CONNECT;
	}
};

int
OCDB_PGSetResultStatus(unsigned long connaddr, struct sqlca_t *st){
	PGresult *res = (PGresult *)connaddr;
	char* state;
	char* errmsg;
	int resultstatus;
	int errlen;

	resultstatus = PQresultStatus(res);

	char *cmdTresult = PQcmdTuples(res);
	if((cmdTresult != NULL) && (strlen(cmdTresult) > 0)){
		st->sqlerrd[2] = atoi(cmdTresult);
	}else{
		st->sqlerrd[2] = 0;
	}

	state = NULL;
	switch(resultstatus){
	case PGRES_COMMAND_OK:
		st->sqlcode = OCPG_NO_ERROR;
		SET_SQLSTATE(st->sqlstate,"00000");
		break;
	case PGRES_TUPLES_OK:
		if(PQntuples(res) <= 0){
			st->sqlcode = OCPG_NOT_FOUND;
			SET_SQLSTATE(st->sqlstate,"02000");
		}else{
			st->sqlcode = OCPG_NO_ERROR;
			SET_SQLSTATE(st->sqlstate,"00000");
		}
		break;
	case PGRES_EMPTY_QUERY:
		st->sqlcode = OCPG_EMPTY;
		SET_SQLSTATE(st->sqlstate,"YE002");
		break;
	case PGRES_NONFATAL_ERROR:
	case PGRES_FATAL_ERROR:
		state = PQresultErrorField(res,PG_DIAG_SQLSTATE);
		if(state){
			SET_SQLSTATE(st->sqlstate,state);
			if(strcmp(state,"08001")==0){
				st->sqlcode = OCPG_CONNECT;
			}else if(strcmp(state,"08003")==0){
				st->sqlcode = OCPG_NO_CONN;
			}else if(strcmp(state,"08007")==0){
				st->sqlcode = OCPG_TRANS;
			}else if(strcmp(state,"21000")==0){
				st->sqlcode = OCPG_SUBSELECT_NOT_ONE;
			}else if(strcmp(state,"23505")==0){
				st->sqlcode = OCPG_DUPLICATE_KEY;
			}else if(strcmp(state,"25001")==0){
				st->sqlcode = OCPG_WARNING_IN_TRANSACTION;
			}else if(strcmp(state,"25P01")==0){
				st->sqlcode = OCPG_WARNING_NO_TRANSACTION;
			}else if(strcmp(state,"34000")==0){
				st->sqlcode = OCPG_WARNING_UNKNOWN_PORTAL;
			}else if(strcmp(state,"42804")==0){
				st->sqlcode = OCPG_DATA_FORMAT_ERROR;
			}else if(strcmp(state,"42P03")==0){
				st->sqlcode = OCPG_WARNING_PORTAL_EXISTS;
			}else if(strcmp(state,"55P03")==0){
				st->sqlcode = OCPG_PGSQL;
			}else{
				st->sqlcode = OCPG_PGSQL;
			}
		}else{
			st->sqlcode = OCDB_UNDEFINED_ERROR;
			SET_SQLSTATE(st->sqlstate,"     ");
		}
		break;
	case PGRES_BAD_RESPONSE:
	default:
		st->sqlcode = OCDB_UNDEFINED_ERROR;
		SET_SQLSTATE(st->sqlstate,"     ");
		break;
	}

	if(st->sqlcode < 0){
		errmsg = PQresultErrorMessage(res);
		if(errmsg){
			LOG("MESSAGE:%s\n",errmsg);
			errlen = strlen(errmsg);
			if(errlen > SQLERRMC_LEN) errlen = SQLERRMC_LEN;
			com_strncpy(st->sqlerrm.sqlerrmc, SQLERRMC_LEN, errmsg, errlen);
			st->sqlerrm.sqlerrml = errlen;
		}

		ERRLOG("%d:%5s:%-70s\n",st->sqlcode,st->sqlstate,st->sqlerrm.sqlerrmc);
	}

	if(st->sqlcode < 0){
		return RESULT_ERROR;
	}
	return RESULT_SUCCESS;
}

int
OCDB_PGSetLibErrorStatus(struct sqlca_t *st, int errno){
	switch(errno){
	case OCDB_NO_ERROR:
		st->sqlcode = OCPG_NO_ERROR;
		SET_SQLSTATE(st->sqlstate,"00000");
		break;
	case OCDB_NOT_FOUND:
		st->sqlcode = OCPG_NOT_FOUND;
		SET_SQLSTATE(st->sqlstate,"02000");
		break;
	case OCDB_OUT_OF_MEMORY:
		st->sqlcode = OCPG_OUT_OF_MEMORY;
		SET_SQLSTATE(st->sqlstate,"YE001");
		break;
	case OCDB_UNSUPPORTED:
		st->sqlcode = OCPG_UNSUPPORTED;
		SET_SQLSTATE(st->sqlstate,"YE002");
		break;
	case OCDB_TOO_MANY_ARGUMENTS:
		st->sqlcode = OCPG_TOO_MANY_ARGUMENTS;
		SET_SQLSTATE(st->sqlstate,"07001");
		break;
	case OCDB_TOO_FEW_ARGUMENTS:
		st->sqlcode = OCPG_TOO_FEW_ARGUMENTS;
		SET_SQLSTATE(st->sqlstate,"07002");
		break;
	case OCDB_TOO_MANY_MATCHES:
		st->sqlcode = OCPG_TOO_MANY_MATCHES;
		SET_SQLSTATE(st->sqlstate,"21000");
		break;
	case OCDB_DATA_FORMAT_ERROR:
		st->sqlcode = OCPG_DATA_FORMAT_ERROR;
		SET_SQLSTATE(st->sqlstate,"42804");
		break;
	case OCDB_EMPTY:
		st->sqlcode = OCPG_EMPTY;
		SET_SQLSTATE(st->sqlstate,"YE002");
		break;
	case OCDB_MISSING_INDICATOR:
		st->sqlcode = OCPG_MISSING_INDICATOR;
		SET_SQLSTATE(st->sqlstate,"22002");
		break;
	case OCDB_NO_CONN:
		st->sqlcode = OCPG_NO_CONN;
		SET_SQLSTATE(st->sqlstate,"08003");
		break;
	case OCDB_NOT_CONN:
		st->sqlcode = OCPG_NOT_CONN;
		SET_SQLSTATE(st->sqlstate,"YE002");
		break;
	case OCDB_INVALID_STMT:
		st->sqlcode = OCPG_INVALID_STMT;
		SET_SQLSTATE(st->sqlstate,"26000");
		break;
	case OCDB_INFORMIX_DUPLICATE_KEY:
		st->sqlcode = OCPG_INFORMIX_DUPLICATE_KEY;
		SET_SQLSTATE(st->sqlstate,"23505");
		break;
	case OCDB_UNKNOWN_DESCRIPTOR:
		st->sqlcode = OCPG_UNKNOWN_DESCRIPTOR;
		SET_SQLSTATE(st->sqlstate,"33000");
		break;
	case OCDB_INVALID_DESCRIPTOR_INDEX:
		st->sqlcode = OCPG_INVALID_DESCRIPTOR_INDEX;
		SET_SQLSTATE(st->sqlstate,"07009");
		break;
	case OCDB_UNKNOWN_DESCRIPTOR_ITEM:
		st->sqlcode = OCPG_UNKNOWN_DESCRIPTOR_ITEM;
		SET_SQLSTATE(st->sqlstate,"YE002");
		break;
	case OCDB_VAR_NOT_NUMERIC:
		st->sqlcode = OCPG_VAR_NOT_NUMERIC;
		SET_SQLSTATE(st->sqlstate,"07006");
		break;
	case OCDB_VAR_NOT_CHAR:
		st->sqlcode = OCPG_VAR_NOT_CHAR;
		SET_SQLSTATE(st->sqlstate,"07006");
		break;
	case OCDB_INFORMIX_SUBSELECT_NOT_ONE:
		st->sqlcode = OCPG_INFORMIX_SUBSELECT_NOT_ONE;
		SET_SQLSTATE(st->sqlstate,"21000");
		break;
	case OCDB_PGSQL:
		st->sqlcode = OCPG_PGSQL;
		break;
	case OCDB_TRANS:
		st->sqlcode = OCPG_TRANS;
		SET_SQLSTATE(st->sqlstate,"08007");
		break;
	case OCDB_CONNECT:
		st->sqlcode = OCPG_CONNECT;
		SET_SQLSTATE(st->sqlstate,"08001");
		break;
	case OCDB_DUPLICATE_KEY:
		st->sqlcode = OCPG_DUPLICATE_KEY;
		SET_SQLSTATE(st->sqlstate,"23505");
		break;
	case OCDB_SUBSELECT_NOT_ONE:
		st->sqlcode = OCPG_SUBSELECT_NOT_ONE;
		SET_SQLSTATE(st->sqlstate,"21000");
		break;
	case OCDB_WARNING_UNKNOWN_PORTAL:
		st->sqlcode = OCPG_WARNING_UNKNOWN_PORTAL;
		SET_SQLSTATE(st->sqlstate,"34000");
		break;
	case OCDB_WARNING_IN_TRANSACTION:
		st->sqlcode = OCPG_WARNING_IN_TRANSACTION;
		SET_SQLSTATE(st->sqlstate,"25001");
		break;
	case OCDB_WARNING_NO_TRANSACTION:
		st->sqlcode = OCPG_WARNING_NO_TRANSACTION;
		SET_SQLSTATE(st->sqlstate,"25P01");
		break;
	case OCDB_WARNING_PORTAL_EXISTS:
		st->sqlcode = OCPG_WARNING_PORTAL_EXISTS;
		SET_SQLSTATE(st->sqlstate,"42P03");
		break;
	case OCDB_LOCK_ERROR:
		st->sqlcode = OCPG_LOCK_ERROR;
		SET_SQLSTATE(st->sqlstate,"57033");
		break;
	case OCDB_JDD_ERROR:
		st->sqlcode = OCPG_JDD_ERROR;
		SET_SQLSTATE(st->sqlstate,"     ");
		break;
	default:
		st->sqlcode = OCDB_UNDEFINED_ERROR;
		SET_SQLSTATE(st->sqlstate,"     ");
		break;
	}

	if(st->sqlcode < 0){
		return RESULT_ERROR;
	}
	return RESULT_SUCCESS;
}


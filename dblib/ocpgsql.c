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
#include <string.h>
#include <malloc.h>
#include <unistd.h>
#include <libpq-fe.h>

#include "ocpgsql.h"
#include "ocdblog.h"
#include "ocdb.h"
#include "ocdbutil.h"

#define SET_SQLSTATE(ststate,sqlstate) strncpy((ststate), (sqlstate), SQLSTATE_LEN)

unsigned long
OCDB_PGConnect(char *conninfo, int autocommit, char *cencoding){
	PGconn *conn;
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
		PQexec(conn, "SET AUTOCOMMIT TO ON");
	} else {
		PQexec(conn, "SET AUTOCOMMIT TO OFF");
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

	LOG("CONNADDR: %d, EXEC SQL: %s\n", connadr, query);
	conn = (PGconn *)connadr;
	res = PQexec(conn, query);

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
	res = PQexecParams(conn, query, nParams, (const Oid *)paramsTypes, paramValues,
			paramLengths, paramFormats, resultFormat);

	return (unsigned long)res;
}

unsigned long
OCDB_PGCursorDeclare(unsigned long connadr, char *cname, char *query, int with_hold){
	const char *query_part_with_hold_on[] = {"DECLARE ", " CURSOR WITH HOLD FOR "};
	const char *query_part[] = {"DECLARE ", " CURSOR FOR "};
	char *true_query;
	unsigned long res;

	if(with_hold == OCDB_CURSOR_WITH_HOLD_ON){
		true_query = (char *)malloc((strlen(query_part_with_hold_on[0]) +
				strlen(cname) +
				strlen(query_part_with_hold_on[1]) +
				strlen(query) + 1) * sizeof(char));
		if(true_query == NULL){
			return OCDB_RES_DEFAULT_ADDRESS;
		}

		// build query
		sprintf(true_query, "%s%s%s%s",  query_part_with_hold_on[0], cname,
				query_part_with_hold_on[1], query);
	} else {
		true_query = (char *)malloc((strlen(query_part[0]) +
				strlen(cname) +
				strlen(query_part[1]) +
				strlen(query) + 1) * sizeof(char));
		if(true_query == NULL){
			return OCDB_RES_DEFAULT_ADDRESS;
		}

		// build query
		sprintf(true_query, "%s%s%s%s",  query_part[0], cname, query_part[1], query);
	}

	res = (unsigned long)OCDB_PGExec(connadr, true_query);
	if(res){
		// カーソルを確定させるためにCOMMITを入れる
		//OCDB_PGExec(connadr, "COMMIT");
		//OCDB_PGExec(connadr, "BEGIN");
	}

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
	unsigned long res;

	if(with_hold == OCDB_CURSOR_WITH_HOLD_ON){
		true_query = (char *)malloc((strlen(query_part_with_hold_on[0]) +
				strlen(cname) +
				strlen(query_part_with_hold_on[1]) +
				strlen(query) + 1) * sizeof(char));
		if(true_query == NULL){
			return OCDB_RES_DEFAULT_ADDRESS;
		}

		// build query
		sprintf(true_query, "%s%s%s%s",  query_part_with_hold_on[0], cname,
				query_part_with_hold_on[1], query);
	} else {
		true_query = (char *)malloc((strlen(query_part[0]) +
				strlen(cname) +
				strlen(query_part[1]) +
				strlen(query) + 1) * sizeof(char));
		if(true_query == NULL){
			return OCDB_RES_DEFAULT_ADDRESS;
		}

		// build query
		sprintf(true_query, "%s%s%s%s",  query_part[0], cname, query_part[1], query);
	}

	res = (unsigned long)OCDB_PGExecParams(connadr, true_query, nParams,
			paramsTypes, paramValues,
			paramLengths, paramFormats, resultFormat);
	if(res){
	}

	return res;
}

unsigned long
OCDB_PGCursorFetchOne(unsigned long connadr, char *cname, int fetchmode){
	const char *query_part[] = {"FETCH ", " RELATIVE ", " FROM "};
	const char next[] = "1";
	const char current[] = "0";
	const char previous[] = "-1";
	char *true_query;

	true_query = (char *)malloc((strlen(query_part[0]) + strlen(query_part[1]) + 1 +
			strlen(query_part[2]) + strlen(cname) + 1) * sizeof(char));
	if(true_query == NULL){
		return OCDB_RES_DEFAULT_ADDRESS;
	}
	// build query
	if(fetchmode == OCDB_READ_CURRENT){
		sprintf(true_query, "%s%s%s%s%s",
				query_part[0], query_part[1], current, query_part[2], cname);
	} else if(fetchmode == OCDB_READ_PREVIOUS){
		sprintf(true_query, "%s%s%s%s%s",
				query_part[0], query_part[1], previous, query_part[2], cname);
	} else { // NEXT
		sprintf(true_query, "%s%s%s%s%s",
				query_part[0], query_part[1], next, query_part[2], cname);
	}

	return (unsigned long)OCDB_PGExec(connadr, true_query);
}

unsigned long
OCDB_PGCursorClose(unsigned long connadr, char *cname){
	const char *query_part[] = {"CLOSE "};
	char *true_query;

	true_query = (char *)malloc((strlen(query_part[0]) +
			strlen(cname) + 1) * sizeof(char));
	if(true_query == NULL){
		return OCDB_RES_DEFAULT_ADDRESS;
	}

	// build query
	sprintf(true_query, "%s%s",  query_part[0], cname);

	return (unsigned long)OCDB_PGExec(connadr, true_query);
}

int
OCDB_PGResultStatus(unsigned long connres){
	PGresult *res = (PGresult *)connres;
	return PQresultStatus(res);
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

void
OCDB_PGFinish(unsigned long connaddr){
	PGconn *conn;

	conn = (PGconn *)connaddr;
	PQfinish(conn);
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
			strncpy(st->sqlerrm.sqlerrmc, errmsg, errlen);
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

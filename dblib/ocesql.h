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


#ifndef OCESQL_H
#define OCESQL_H

#include "ocdb.h"

#define OCESQL_CONN_CONNECT_OK 0
#define OCESQL_CONN_FAIL_CONNECT (-1)

#define OCESQL_NO_CONNECTION (-1)

#define OCESQL_TYPE_PD_POSITIVE 'C'
#define OCESQL_TYPE_PD_NEGATIVE 'D'

#define OCESQL_OK 0
#define OCESQL_NORECORD 1
#define OCESQL_TIMEOUT 2

#define OCESQL_DEFAULT_DBNAME "OCDB_DEFAULT_DBNAME"
#define OCESQL_DEFAULT_DBLENG strlen(OCESQL_DEFAULT_DBNAME)

#ifdef _WIN32
__declspec(dllexport) int OCESQLConnect(struct sqlca_t *, char *, int, char *, int, char *, int);
__declspec(dllexport) int OCESQLConnectShort(struct sqlca_t *);
__declspec(dllexport) int OCESQLConnectInformal(struct sqlca_t *, char *, int);
__declspec(dllexport) int OCESQLIDConnect(struct sqlca_t *, char *, int, char *, int, char *, int, char *, int);
__declspec(dllexport) int OCESQLIDConnectShort(struct sqlca_t *, char *, int);
__declspec(dllexport) int OCESQLIDConnectInformal(struct sqlca_t *, char *, int, char *, int);
__declspec(dllexport) int OCESQLDisconnect(struct sqlca_t *);
__declspec(dllexport) int OCESQLIDDisconnect(struct sqlca_t *, char*, int);

__declspec(dllexport) int OCESQLPrepare(struct sqlca_t *, char *, char *, int);
__declspec(dllexport) int OCESQLExec(struct sqlca_t *, char *);
__declspec(dllexport) int OCESQLIDExec(struct sqlca_t *, char *, int, char *);
__declspec(dllexport) int OCESQLExecParams(struct sqlca_t *, char *, int);
__declspec(dllexport) int OCESQLIDExecParams(struct sqlca_t *, char *, int, char *, int);
__declspec(dllexport) int OCESQLExecParamsOccurs(struct sqlca_t *, char *, int);
__declspec(dllexport) int OCESQLIDExecParamsOccurs(struct sqlca_t *, char *, int, char *, int);
__declspec(dllexport) int OCESQLCursorDeclare(struct sqlca_t *, char *, char *);
__declspec(dllexport) int OCESQLIDCursorDeclare(struct sqlca_t *, char *, int, char *, char *);
__declspec(dllexport) int OCESQLCursorDeclareParams(struct sqlca_t *, char *, char *, int);
__declspec(dllexport) int OCESQLIDCursorDeclareParams(struct sqlca_t *, char *, int, char *, char *, int);
__declspec(dllexport) int OCESQLPreparedCursorDeclare(struct sqlca_t *, char *, char *);
__declspec(dllexport) int OCESQLIDPreparedCursorDeclare(struct sqlca_t *, char *, int, char *, char *);
__declspec(dllexport) int OCESQLExecPrepare(struct sqlca_t *, char *, int);
__declspec(dllexport) int OCESQLIDExecPrepare(struct sqlca_t *, char *, int, char *, int);
__declspec(dllexport) int OCESQLCursorOpen(struct sqlca_t *, char *);
__declspec(dllexport) int OCESQLCursorOpenParams(struct sqlca_t *, char *, int);
__declspec(dllexport) int OCESQLCursorFetchOne(struct sqlca_t *, char *);
__declspec(dllexport) int OCESQLCursorFetchOccurs(struct sqlca_t *, char *);
__declspec(dllexport) int OCESQLCursorClose(struct sqlca_t *, char *);
__declspec(dllexport) int OCESQLExecSelectIntoOne(struct sqlca_t *, char *, int, int);
__declspec(dllexport) int OCESQLIDExecSelectIntoOne(struct sqlca_t *, char *, int, char *, int, int);
__declspec(dllexport) int OCESQLExecSelectIntoOccurs(struct sqlca_t *, char *, int, int);
__declspec(dllexport) int OCESQLIDExecSelectIntoOccurs(struct sqlca_t *, char *, int, char *, int, int);
__declspec(dllexport) int OCESQLNtuples(int);
__declspec(dllexport) int OCESQLNfields(int);
__declspec(dllexport) char *OCESQLGetvalue(int, int);
__declspec(dllexport) char *OCESQLResultErrorMessage(struct sqlca_t *);

__declspec(dllexport) int OCESQLStartSQL(void);
__declspec(dllexport) int OCESQLSetSQLParams(int, int, int, void *);
__declspec(dllexport) int OCESQLSetResultParams(int, int, int, void *);
__declspec(dllexport) int OCESQLSetHostTable(int, int, int);
__declspec(dllexport) int OCESQLEndSQL(void);
#else
int OCESQLConnect(struct sqlca_t *, char *, int, char *, int, char *, int);
int OCESQLConnectInformal(struct sqlca_t *, char *, int);
int OCESQLIDConnect(struct sqlca_t *, char *, int, char *, int, char *, int, char *, int);
int OCESQLIDConnectInformal(struct sqlca_t *, char *, int, char *, int);
int OCESQLDisconnect(struct sqlca_t *);
int OCESQLIDDisconnect(struct sqlca_t *, char*, int);

int OCESQLPrepare(struct sqlca_t *, char *, char *, int);
int OCESQLExec(struct sqlca_t *, char *);
int OCESQLIDExec(struct sqlca_t *, char *, int, char *);
int OCESQLExecParams(struct sqlca_t *, char *, int);
int OCESQLIDExecParams(struct sqlca_t *, char *, int, char *, int);
int OCESQLExecParamsOccurs(struct sqlca_t *, char *, int);
int OCESQLIDExecParamsOccurs(struct sqlca_t *, char *, int, char *, int);
int OCESQLCursorDeclare(struct sqlca_t *, char *, char *);
int OCESQLIDCursorDeclare(struct sqlca_t *, char *, int, char *, char *);
int OCESQLCursorDeclareParams(struct sqlca_t *, char *, char *, int);
int OCESQLIDCursorDeclareParams(struct sqlca_t *, char *, int, char *, char *, int);
int OCESQLPreparedCursorDeclare(struct sqlca_t *, char *, char *);
int OCESQLIDPreparedCursorDeclare(struct sqlca_t *, char *, int, char *, char *);
int OCESQLExecPrepare(struct sqlca_t *, char *, int);
int OCESQLIDExecPrepare(struct sqlca_t *, char *, int, char *, int);
int OCESQLCursorOpen(struct sqlca_t *, char *);
int OCESQLCursorOpenParams(struct sqlca_t *, char *, int);
int OCESQLCursorFetchOne(struct sqlca_t *, char *);
int OCESQLCursorFetchOccurs(struct sqlca_t *, char *);
int OCESQLCursorClose(struct sqlca_t *, char *);
int OCESQLExecSelectIntoOne(struct sqlca_t *, char *, int, int);
int OCESQLIDExecSelectIntoOne(struct sqlca_t *, char *, int, char *, int, int);
int OCESQLExecSelectIntoOccurs(struct sqlca_t *, char *, int, int);
int OCESQLIDExecSelectIntoOccurs(struct sqlca_t *, char *, int, char *, int, int);
int OCESQLNtuples(int);
int OCESQLNfields(int);
char *OCESQLGetvalue(int, int);
char *OCESQLResultErrorMessage(struct sqlca_t *);

int OCESQLStartSQL(void);
int OCESQLSetSQLParams(int, int, int, void *);
int OCESQLSetResultParams(int, int, int, void *);
int OCESQLSetHostTable(int, int, int);
int OCESQLEndSQL(void);
#endif

#endif

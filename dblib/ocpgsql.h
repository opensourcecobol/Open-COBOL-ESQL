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


#ifndef OCPGSQL_H
#define OCPGSQL_H
#include <libpq-fe.h>

/* define sqlca for PostgreSQL */
#define SQLERRMC_LEN	70
#define SQLSTATE_LEN	5

struct sqlca_t
{
	char		sqlcaid[8];
	int		sqlabc;
	int		sqlcode; // error code
	struct
	{
		short		sqlerrml;
		char		sqlerrmc[SQLERRMC_LEN];
	}			sqlerrm; // error message
	char		sqlerrp[8];
	int		sqlerrd[6];
	char		sqlwarn[8];
	char		sqlstate[SQLSTATE_LEN]; //err status
};

#include "ocdb.h"

static struct sqlca_t sqlca_init =
{
	{
		'S', 'Q', 'L', 'C', 'A', ' ', ' ', ' '
	},
	sizeof(struct sqlca_t),
	0,
	{
		0,
		{
			0
		}
	},
	{
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '
	},
	{
		0, 0, 0, 0, 0, 0
	},
	{
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '
	},
	{
		' ', ' ', ' ', ' ', ' '
	}
};

/* define ERRORCODE for PostgreSQL */
#define OCPG_NO_ERROR 0
#define OCPG_NOT_FOUND 100
#define OCPG_OUT_OF_MEMORY -12
#define OCPG_UNSUPPORTED -200
#define OCPG_TOO_MANY_ARGUMENTS -201
#define OCPG_TOO_FEW_ARGUMENTS -202
#define OCPG_TOO_MANY_MATCHES -203
#define OCPG_DATA_FORMAT_ERROR -204
#define OCPG_INT_FORMAT -204
#define OCPG_UINT_FORMAT -205
#define OCPG_FLOAT_FORMAT -206
#define OCPG_NUMERIC_FORMAT -207
#define OCPG_INTERVAL_FORMAT -208
#define OCPG_DATE_FORMAT -209
#define OCPG_TIMESTAMP_FORMAT -210
#define OCPG_CONVERT_BOOL -211
#define OCPG_EMPTY -212
#define OCPG_MISSING_INDICATOR -213
#define OCPG_NO_ARRAY -214
#define OCPG_DATA_NOT_ARRAY -215
#define OCPG_NO_CONN -220
#define OCPG_NOT_CONN -221
#define OCPG_INVALID_STMT -230
#define OCPG_INFORMIX_DUPLICATE_KEY -239
#define OCPG_UNKNOWN_DESCRIPTOR -240
#define OCPG_INVALID_DESCRIPTOR_INDEX -241
#define OCPG_UNKNOWN_DESCRIPTOR_ITEM -242
#define OCPG_VAR_NOT_NUMERIC -243
#define OCPG_VAR_NOT_CHAR -244
#define OCPG_INFORMIX_SUBSELECT_NOT_ONE -284
#define OCPG_PGSQL -400
#define OCPG_TRANS -401
#define OCPG_CONNECT -402
#define OCPG_DUPLICATE_KEY -403
#define OCPG_SUBSELECT_NOT_ONE -404
#define OCPG_WARNING_UNKNOWN_PORTAL -602
#define OCPG_WARNING_IN_TRANSACTION -603
#define OCPG_WARNING_NO_TRANSACTION -604
#define OCPG_WARNING_PORTAL_EXISTS -605
#define OCPG_LOCK_ERROR -606
#define OCPG_JDD_ERROR -607

/* method */
unsigned long OCDB_PGConnect(char *, int, char *);
int OCDB_PGstatus(unsigned long);
char *OCDB_PGErrorMessage(unsigned long);

void OCDB_PGClear(unsigned long);
unsigned long OCDB_PGExec(unsigned long, char *);
unsigned long OCDB_PGExecParams(unsigned long, char *, int,
                    const int *, const char * const *,
                    const int *, const int *, int);
unsigned long OCDB_PGCursorDeclare(unsigned long, char *, char *, int);
unsigned long OCDB_PGCursorDeclareParams(unsigned long, char *, char *, int,
                    const int *, const char * const *,
     	  	    const int *, const int *, int, int);
unsigned long OCDB_PGCursorFetchOne(unsigned long, char *, int);
unsigned long OCDB_PGCursorFetchOccurs(unsigned long, char *, int, int);
unsigned long OCDB_PGCursorClose(unsigned long, char *);
char *OCDB_PGResultErrorMessage(unsigned long);
char *OCDB_PGResultErrorField(unsigned long);
int OCDB_PGcmdtuples(unsigned long);
int OCDB_PGntuples(unsigned long);
int OCDB_PGnfields(unsigned long);
char *OCDB_PGfname(unsigned long, int);
int OCDB_PGfnumber(unsigned long, const char *);
char *OCDB_PGgetvalue(unsigned long, int, int);
unsigned long OCDB_PGDropTable(unsigned long, char *);
void OCDB_PGFinish(unsigned long);

unsigned long OCDB_PGDeleteTable(unsigned long, char *);
//void OCDB_PGsqlca_initialize(struct sqlca_t * sqlca);

static char errmsg_buf[SQLERRMC_LEN];
static char state_buf[] = "     ";

int OCDB_PGSetResultStatus(unsigned long, struct sqlca_t *);
int OCDB_PGSetLibErrorStatus(struct sqlca_t *, int);

#endif

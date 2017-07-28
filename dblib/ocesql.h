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

#ifndef OCESQL_H
#define OCESQL_H


#include "ocdb.h"

#define OCESQL_CONN_CONNECT_OK 0
#define OCESQL_CONN_FAIL_CONNECT (-1)

#define OCESQL_NO_CONNECTION (-1)

/*
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
	char		sqlstate[5]; //err status
} sqlca;

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

*/

#define OCESQL_TYPE_PD_POSITIVE 'C'
#define OCESQL_TYPE_PD_NEGATIVE 'D'

#define OCESQL_OK 0
#define OCESQL_NORECORD 1
#define OCESQL_TIMEOUT 2

#define OCESQL_DEFAULT_DBNAME "OCDB_DEFAULT_DBNAME"
#define OCESQL_DEFAULT_DBLENG strlen(OCESQL_DEFAULT_DBNAME)

int OCESQLConnect(struct sqlca_t *, char *, int, char *, int, char *, int);
int OCESQLDisconnect(struct sqlca_t *);

int OCESQLExec(struct sqlca_t *, char *);
int OCESQLExecParams(struct sqlca_t *, char *, int);
int OCESQLExecSelectIntoOne(struct sqlca_t *, char *, int, int);

int OCESQLCursorDeclare(struct sqlca_t *, char *, char *);
int OCESQLCursorDeclareParams(struct sqlca_t *, char *, char *, int);
int OCESQLCursorOpen(struct sqlca_t *, char *);
int OCESQLCursorFetchOne(struct sqlca_t *, char *);
int OCESQLCursorClose(struct sqlca_t *, char *);

int OCESQLStartSQL(void);
int OCESQLSetSQLParams(int, int, int, void *);
int OCESQLSetResultParams(int, int, int, void *);
int OCESQLEndSQL(void);

#endif

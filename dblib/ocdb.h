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


#ifndef OCDB_H
#define OCDB_H

#define PGSQL_MODE_ON

typedef struct lock_conn {
	char *db;
	char *schema;
	char *table;
	int lockconnid;
	char *pid;
}LOCK_CONN;

#ifdef PGSQL_MODE_ON
#include "ocpgsql.h"
#define OCDB_TUPLES_NODATA OCPG_NOT_FOUND
#endif

#define RESULT_FAILED -1
#define RESULT_SUCCESS 0
#define RESULT_ERROR -2
#define RESULT_FLAGBASE 10
#define RESULT_FLAG1_PGSQL_DUMMYOPEN 11

// default resource address
#define OCDB_RES_DEFAULT_ADDRESS 0

#define DEFAULT_NEXT_CONN_ID 1
#define INVALID_CONN_ID -1
#define OCDB_INVALID_NUMBER -1
#define OCDB_INVALID_STRING (NULL)
#define OCDB_CONN_CONNECT_OK 1
#define OCDB_CONN_NOT_CONNECT 0
#define OCDB_CONN_FAIL_CONNECT (-1)

//file mode
#define OCDB_FILEMODE_INPUT	1
#define OCDB_FILEMODE_OUTPUT	2
#define OCDB_FILEMODE_I_O	3
#define OCDB_FILEMODE_EXTEND	4

//lock mode
#define OCDB_LOCK_EXCLUSIVE	1
#define OCDB_LOCK_MANUAL	2
#define OCDB_LOCK_AUTOMATIC	4
#define OCDB_LOCK_MULTIPLE	8
#define OCDB_LOCK_MASK	0x7

//wait for locks
#define OCDB_LOCK_WAIT		1
#define OCDB_LOCK_NOWAIT	0

// autocommit
#define OCDB_AUTOCOMMIT_OFF 0
#define OCDB_AUTOCOMMIT_ON 1

// cursor with hold
#define OCDB_CURSOR_WITH_HOLD_OFF 0
#define OCDB_CURSOR_WITH_HOLD_ON 1

// Lcok table response
#define OCDB_RESLOCK_COMMAND_OK	0
#define OCDB_RESLOCK_TABLE_NOLOCK	10
#define OCDB_RESLOCK_TABLE_LOCKED	11
#define OCDB_RESLOCK_TABLE_LOCKED_OWN	12
#define OCDB_RESLOCK_RECORD_NOLOCK	20
#define OCDB_RESLOCK_RECORD_LOCKED	21
#define OCDB_RESLOCK_RECORD_LOCKED_OWN	22
#define OCDB_RESLOCK_ERROR_NOTCONNECT	90
#define OCDB_RESLOCK_ERROR_NODATABASE	91
#define OCDB_RESLOCK_ERROR_NOTABLE	92
#define OCDB_RESLOCK_ERROR_SEGFAULT	98
#define OCDB_RESLOCK_ERROR_FATAL	99

// direction of FetchOne
enum {
	OCDB_READ_NEXT = 1,     // READ NEXT
	OCDB_READ_PREVIOUS,     // READ PREVIOUS
	OCDB_READ_CURRENT       // READ CURRENT CURSOR
};

// 型情報
//#define OCDB_TYPE_FORMATTED_NUMBER 0        // 数字編集
#define OCDB_TYPE_UNSIGNED_NUMBER 1         // 符号無数字
//#define OCDB_TYPE_SIGNED_NUMBER_TS 2        // 符号付数字(trailing separate)
#define OCDB_TYPE_SIGNED_NUMBER_TC 3        // 符号付数字(trailing combined)
#define OCDB_TYPE_SIGNED_NUMBER_LS 4        // 符号付数字(leading separete)
//#define OCDB_TYPE_SIGNED_NUMBER_LC 5        // 符号付数字(leading combined)
//#define OCDB_TYPE_SIGNED_CALC 6             // 符号付計算
//#define OCDB_TYPE_UNSIGNED_CALC 7           // 符号無計算
#define OCDB_TYPE_UNSIGNED_NUMBER_PD 8        // 正のパック10進数
#define OCDB_TYPE_SIGNED_NUMBER_PD 9      // 符号付パック10進数
//#define OCDB_TYPE_COMP6 10                  // COMP-6
//#define OCDB_TYPE_SINGED_BINARY 11          // 符号付2進数
//#define OCDB_TYPE_UNSINGED_BINARY 12        // 符号無2進数
#define OCDB_TYPE_SINGED_BINARY_NATIVE 13   // 符号付2進数(native-order)
#define OCDB_TYPE_UNSINGED_BINARY_NATIVE 14 // 符号無2進数(native-order)
#define OCDB_TYPE_ALPHANUMERIC 16           // 英数字
//#define OCDB_TYPE_ALPHANUMERIC_J 17         // 英数字(桁寄せ)
//#define OCDB_TYPE_ALPHABETICAL 18           // 英字
//#define OCDB_TYPE_ALPHABETICAL_J 19         // 英字(桁寄せ)
//#define OCDB_TYPE_FORMATTED_ALPHANUMERIC 20 // 英数字編集
#define OCDB_TYPE_GROUP 22                  // 集団
//#define OCDB_TYPE_FLOATING_NUMBER 23        // Float or Double
#define OCDB_TYPE_JAPANESE 24               // 日本語
//#define OCDB_TYPE_JAPANESE_J 25             // 日本語(桁寄せ)
//#define OCDB_TYPE_FORMATTED_JAPANESE 26     // 日本語編集
//#define OCDB_TYPE_WIDE 27                   // wide
//#define OCDB_TYPE_WIDE_J 28                 // wide(桁寄せ)
//#define OCDB_TYPE_FORMATTED_WIDE 29         // wide
#define OCDB_TYPE_ALPHANUMERIC_VARYING 30     // VARYING(PIC X)
#define OCDB_TYPE_JAPANESE_VARYING 31         // VARYING(PIC N)
#define OCDB_TYPE_MIN 0                       // 型下限
#define OCDB_TYPE_MAX 32                      // 型上限

#define OCDB_TYPE_OPTION_DATE 1
#define OCDB_TYPE_OPTION_BINARY 2
#define OCDB_TYPE_OPTION_VARCHAR 3

#define OCDB_VARCHAR_HEADER_BYTE 4 // size of PIC S9(8) COMP-5

//SQL exec status(SQLCODE)
// over 0: NO ERROR
// OCDB_RES_COMMAND_OK(0):
//   - INSERT, UPDATEなど行を返さないSQLの成功
//   - SELECT, FETCHの結果が1行以上ある場合
// OCDB_RES_TUPLES_NODATA:
//   - SELECT, FETCHの結果が0行の場合
// OCDB_RES_TUPLES_NODATA:
//   - KEY指定時にSELECT, FETCHの結果が0行の場合
// (-10)-(-19): WARNING
// (-20)-(-29): ERROR
// (-30)-(-39): FATAL ERROR
// (-40)-(-49): LOCK STATUS
#define OCDB_RES_EMPTY_QUERY (-10)
#define OCDB_RES_COMMAND_OK 0
#define OCDB_RES_TUPLES_NODATA 10
#define OCDB_RES_INVALID_KEY 11
#define OCDB_RES_COPY_OUT 2
#define OCDB_RES_COPY_IN 3
#define OCDB_RES_BAD_RESPONSE (-20)
#define OCDB_RES_NONFATAL_ERROR (-21)
#define OCDB_RES_ARGUMENT_ERROR (-22)
#define OCDB_RES_ALLOCATE_ERROR (-23)
#define OCDB_RES_UNDEFINED_CONNECTION (-24)
#define OCDB_RES_UNDEFINED_PREPARE (-25)
#define OCDB_RES_UNDEFINED_CURSOR (-26)
#define OCDB_RES_OVERWRITE_OPENED_CURSOR (-27)
#define OCDB_RES_FATAL_ERROR (-30)
#define OCDB_RES_LOCKED_RECORD (-40)
#define OCDB_RES_LOCKED_TABLE (-41)

/* SQLCODE LIST */
enum{
	OCDB_NO_ERROR = 1,
	OCDB_NOT_FOUND,
	OCDB_OUT_OF_MEMORY,
	OCDB_UNSUPPORTED,
	OCDB_TOO_MANY_ARGUMENTS,
	OCDB_TOO_FEW_ARGUMENTS,
	OCDB_TOO_MANY_MATCHES,
	OCDB_DATA_FORMAT_ERROR,
	OCDB_INT_FORMAT,
	OCDB_UINT_FORMAT,
	OCDB_FLOAT_FORMAT,
	OCDB_NUMERIC_FORMAT,
	OCDB_INTERVAL_FORMAT,
	OCDB_DATE_FORMAT,
	OCDB_TIMESTAMP_FORMAT,
	OCDB_CONVERT_BOOL,
	OCDB_EMPTY,
	OCDB_MISSING_INDICATOR,
	OCDB_NO_ARRAY,
	OCDB_DATA_NOT_ARRAY,
	OCDB_NO_CONN,
	OCDB_NOT_CONN,
	OCDB_INVALID_STMT,
	OCDB_INFORMIX_DUPLICATE_KEY,
	OCDB_UNKNOWN_DESCRIPTOR,
	OCDB_INVALID_DESCRIPTOR_INDEX,
	OCDB_UNKNOWN_DESCRIPTOR_ITEM,
	OCDB_VAR_NOT_NUMERIC,
	OCDB_VAR_NOT_CHAR,
	OCDB_INFORMIX_SUBSELECT_NOT_ONE,
	OCDB_PGSQL,
	OCDB_TRANS,
	OCDB_CONNECT,
	OCDB_DUPLICATE_KEY,
	OCDB_SUBSELECT_NOT_ONE,
	OCDB_WARNING_UNKNOWN_PORTAL,
	OCDB_WARNING_IN_TRANSACTION,
	OCDB_WARNING_NO_TRANSACTION,
	OCDB_WARNING_PORTAL_EXISTS,
	OCDB_LOCK_ERROR,
	OCDB_JDD_ERROR
};
#define OCDB_UNDEFINED_ERROR -9999

#define OCDB_RES_NOCONNECT_ERRORMSG "Connection is not opened."

struct s_conn {
	int id;
	char *cid;
	int dbtype;
	unsigned long connaddr;
	unsigned long resaddr;
	int result;
	char *errorMessage;
	char *pid;
};

struct conn_list {
	struct s_conn sc;
	struct conn_list *next;
};

enum DBTYPE{
#ifdef PGSQL_MODE_ON
	USE_PGSQL = 0,
#endif
#ifdef MYSQL_MODE_ON
	USE_MYSQL = 1,
#endif
#ifdef ORACLE_MODE_ON
	USE_ORACLE = 2,
#endif
	DUMMY = -1,
};

/* method */
int OCDBCheckType(int);
int OCDBConnect(int, char *, char *, int, char *);
int OCDBStatus(int);
char *OCDBErrorMessage(int);

void OCDBExec(int, char *);
void OCDBExecParams(int, char *, int, const int *, const char * const *,
		const int *, const int *, int);
void OCDBCursorDeclare(int, char *, char *, int);
void OCDBCursorDeclareParams(int, char *, char *, int, const int *,
		const char * const *, const int *, const int *, int, int);
void OCDBCursorOpen(int, char *);
void OCDBCursorFetchOne(int, char *, int);
void OCDBCursorFetchOccurs(int, char *, int, int);
void OCDBCursorClose(int, char *);
char *OCDBResultErrorMessage(int);
char *OCDBResultErrorField(int);
int OCDBCmdTuples(int);
int OCDBNtuples(int);
int OCDBNfields(int);
char *OCDBFname(int, int);
int OCDBFnumber(int, const char *);
char *OCDBGetvalue(int, int, int);
void OCDBDropTable(int, char *);
void OCDBDeleteTable(int, char *);
void OCDBFinish(int);

int OCDBResolveCONNID(char *);

int OCDBSetResultStatus(int id, struct sqlca_t *);
int OCDBSetLibErrorStatus(struct sqlca_t *, int);

/* common APIs */
char *_alloc(long);
char *last_dir_separator(const char *);
int strlen_or_null(const char *);

#define skip_drive(path)	(path)

#ifndef WIN32
#define IS_DIR_SEP(ch)	((ch) == '/')

#define is_absolute_path(filename)		\
		(						\
				IS_DIR_SEP((filename)[0])		\
		)
#else
#define IS_DIR_SEP(ch)	((ch) == '/' || (ch) == '\\')

/* See path_is_relative_and_below_cwd() for how we handle 'E:abc'. */
#define is_absolute_path(filename)					\
		(									\
				IS_DIR_SEP((filename)[0]) ||					\
				(isalpha((unsigned char) ((filename)[0])) && (filename)[1] == ':' && \
						IS_DIR_SEP((filename)[2]))					\
		)
#endif

#endif

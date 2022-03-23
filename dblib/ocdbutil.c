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
#include <malloc.h>
#include <math.h>
#include "ocdbutil.h"
#include "ocdblog.h"
#include "ocdb.h"

#ifdef _WIN32
#include <windows.h>
#include <locale.h>
#else
#include <unistd.h>
#include <iconv.h>
#include <errno.h>
#endif

char type_tc_negative_final_number[] =
{
	'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y'
//     '{', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R'
};
static int type_tc_negative_final_number_len =
	sizeof(type_tc_negative_final_number)/sizeof(type_tc_negative_final_number[0]);

/*
 * <Function name>
 *   insert_decimal_point
 *
 * <Outline>
 *   powerで指定した位置に小数点を挿入する
 *
 * <Input>
 *   @data: 挿入対象
 *   @data_size: dataに割り当てられたサイズ(バイト単位)
 *   @power: 小数点以下の桁数(負の値)
 */
void insert_decimal_point(char *data, int data_size, int power){
	int before_length, after_length;
	before_length = strlen(data);
	after_length = strlen(data) + 1;

	int n_decimal_places = -power;

	// check size of data
	if(data_size < after_length){
		return;
	} else if(n_decimal_places <= 0 || n_decimal_places >= before_length){
		return;
	}

	memmove(data + (after_length-n_decimal_places), data + (before_length-n_decimal_places),
			n_decimal_places * sizeof(char));
	data[before_length - n_decimal_places] = '.';
}

/*
 * <Function name>
 *   type_tc_is_positive
 *
 * <Outline>
 *   OCDB_TYPE_SIGNED_NUMBER_TCのデータが正負であるかを判別し、
 *   負の値の場合は符号を取り除いた数値で引数を上書きする
 *   もし該当する数値が存在しない場合は、0をセットした上でtrueを返す
 *
 * <Input>
 *   @lastchar: 判別対象の文字
 *
 * <Output>
 *   判別対象が正 : true
 *   判別対象が負 : false
 *
 */
int type_tc_is_positive(char *lastchar){
	int i;

	if(*lastchar >= '0' &&  *lastchar <= '9')
		return true;

	for(i=0; i<type_tc_negative_final_number_len; i++){
		if(*lastchar == type_tc_negative_final_number[i]){
			char tmp[2];
			int maxlen = 2;
			com_sprintf(tmp, maxlen, "%d", i);
			*lastchar = tmp[0];
			return false;
		}
	}

	LOG("no final_number found: %c\n", *lastchar);
	*lastchar = 0;
	return true;
}



char *
uint_to_str(int i){
	int tmp = i;
	int dig = 0;
	char *ret;
	int len;

	if(i < 0) return NULL;
	do{
		dig++;
		tmp = tmp/10;
	}while(tmp > 0);

	len = dig+TERMINAL_LENGTH;
	if((ret = (char *)calloc(len,sizeof(char))) ==NULL){
		return NULL;
	}
	com_sprintf(ret, len,"%d",i);
	return ret;
}

char *
oc_strndup(char *src, int n){
	char *ret;

	if(n < 0){
		return NULL;
	}
	ret = (char *)malloc(sizeof(char) * (n + 1));
	if(!src){
		return NULL;
	}

	memcpy(ret,src,n);
	ret[n] = '\0';

	return ret;
}

char *
get_str_without_after_space(char *target){
	char *pos;

	if(!target){
		return NULL;
	}

	for(pos = target; *pos; pos++){
		if(*pos == ' '){
			*pos = '\0';
 			break;
		}
 	}

	return target;
}

typedef struct sql_str_list{
	char *tok;
	struct sql_str_list *next;
} SQLSTRLIST;

SQLSTRLIST *
new_sql_list(){
	SQLSTRLIST *p;
	p = (SQLSTRLIST *)malloc(sizeof(SQLSTRLIST));
	if(!p){
		return NULL;
	}
	p->tok = NULL;
	p->next = NULL;

	return p;
}

void
add_sql_list(SQLSTRLIST *list, char *tok){
	SQLSTRLIST *p = list;

	while(p->next != NULL){
		p = p->next;
	}

	p->next = new_sql_list();
	p->tok = tok;
}

void
clear_sql_list(SQLSTRLIST *list){
	SQLSTRLIST *p = list;
	SQLSTRLIST *p_next;

	while(p != NULL){
		p_next = p->next;
		if(p->tok) free(p->tok);
		free(p);
		p = p_next;
	}
}

char *
get_str_replace_hostvalue(char *target, int *nParams){
	char *tmptarget, *start, *now, *rep;
	int length;
	int hoststat;
	char *retstr;
	char buff[BUFFSIZE];

	LOG("src:'%s'\n",target);

	SQLSTRLIST *slist = new_sql_list();
	SQLSTRLIST *p;

	*nParams = 0;
	length = 0;

	if(slist == NULL){
		return NULL;
	}

	tmptarget = com_strdup(target);

	now = tmptarget;
	start = now;
	hoststat = 0;
	while(*now != '\0'){
		if(hoststat){
			if((*now == ',') ||
				(*now == ')') ||
				(*now == ' ')
			){
				*nParams = *nParams + 1;
				com_sprintf(buff, BUFFSIZE,"$%d\0",*nParams);
				rep = com_strdup(buff);
				length += strlen(rep);
				add_sql_list(slist, rep);
				start = now;
				hoststat = 0;
			}
		}else{
			if(*now == ':'){
				rep = oc_strndup(start,now - start);
				length += strlen(rep);
				add_sql_list(slist, rep);
				start = now;
				hoststat = 1;
			}
		}
		now++;
	}
	if(now != start){
		if(hoststat){
			*nParams = *nParams + 1;
			com_sprintf(buff, BUFFSIZE,"$%d\0",*nParams);
			rep = com_strdup(buff);
		}else{
			rep = com_strdup(start);
		}
		length += strlen(rep);
		add_sql_list(slist, rep);
	}


	retstr = (char *)calloc(length + 1,sizeof(char));

	p = slist;
	while(p->next != NULL){
		com_strcat(retstr, length + 1, p->tok);
		p = p->next;
	}

	clear_sql_list(slist);
	free(tmptarget);
	LOG("dest:'%s'\n",retstr);
	return retstr;
}

int get_endian(){
	int x=1;
	int retval = BIGENDIAN;

	if(*(char *)&x == 1) retval = LITTLEENDIAN;

	return retval;
}

char *
com_strdup(const char *string){
#ifdef _WIN32
	return _strdup(string);
#else
	char *new;

	if(string == NULL){
		return NULL;
	}

	new = strdup(string);
	return (new);
#endif
}

int
com_sprintf(char *buf, size_t bufs, const char * ccf, ...){
	int rcd;
	va_list list;
	va_start(list, ccf);

#ifdef _WIN32
	rcd = vsprintf_s(buf, bufs, ccf, list);
#else
	rcd = vsprintf(buf, ccf, list);
#endif
	va_end(list);
	return rcd;
}

char *
com_strcat(char *ob, size_t obs, const char *ib){
#ifdef _WIN32
	strcat_s(ob, obs, ib);
	return ob;
#else
	return strcat(ob, ib);
#endif
}

char *
com_strncat(char *rc, size_t rcs, const char *pc, size_t len){
#ifdef _WIN32
	strncat_s(rc, rcs, pc, len);
	return rc;
#else
	return strncat(rc, pc, len);
#endif
}

void
com_sleep(unsigned int t){
#ifdef _WIN32
	unsigned int st;
	st = t * 1000;
	Sleep(st);
#else
	sleep(t);
#endif
}

char *
com_strtok(char *st, const char *sd, char **ct){
#ifdef _WIN32
	return strtok_s(st, sd, ct);
#else
	return strtok(st, sd);
#endif
}

void
com_fopen(FILE** fp, const char *fn, const char *md ){
#ifdef _WIN32
	fopen_s(fp, fn, md);
#else
	*fp = fopen(fn, md);
#endif
}

char *
com_strcpy(char *st1, size_t stb, const char *st2){
#ifdef _WIN32
	strcpy_s(st1, stb, st2);
	return st1;
#else
	return strcpy(st1, st2);
#endif

}

char *
com_ctime(char *buf, size_t bufs, const time_t *tc){
#ifdef _WIN32
	ctime_s(buf, bufs, tc);
	return buf;
#else
	return ctime(tc);
#endif
}

char *
com_strncpy(char *st1, size_t stb1, const char *st2, size_t stb2){
#ifdef _WIN32
	strncpy_s(st1, stb1, st2, stb2);
	return st1;
#else
	return strncpy(st1, st2, stb2);
#endif
}

int
com_get_loglevel(){
	char *strenv;
	int retval =  LOG_OUTPUT_NOTHING;
#ifdef _WIN32
	size_t len;
	errno_t err = _dupenv_s(&strenv, &len, "OCDB_LOGLEVEL");

	if (err || (strenv == NULL)){
		retval = LOG_OUTPUT_NOTHING;
	} else {
		if (!strcmp(strenv, "NOLOG") || !strcmp(strenv, "nolog")){
			retval = LOG_OUTPUT_NOTHING;
		}
		else if (!strcmp(strenv, "ERR") || !strcmp(strenv, "err")){
			retval = LOG_OUTPUT_ERR;
		}
		else if (!strcmp(strenv, "DEBUG") || !strcmp(strenv, "debug")){
			retval = LOG_OUTPUT_DEBUG;
		}
		else{
			retval = atoi(strenv);
		}

		free(strenv);
	}
#else
	char *tmp = getenv("OCDB_LOGLEVEL");
	if(tmp != NULL){
		strenv = strdup(tmp);
		if(strenv != NULL){
			if(!strcmp(strenv, "NOLOG") || !strcmp(strenv, "nolog")){
				retval = LOG_OUTPUT_NOTHING;
			} else if(!strcmp(strenv, "ERR") || !strcmp(strenv, "err")){
				retval = LOG_OUTPUT_ERR;
			} else if(!strcmp(strenv, "DEBUG") || !strcmp(strenv, "debug")){
				retval = LOG_OUTPUT_DEBUG;
			}
			free(strenv);
		}else{
			retval = LOG_OUTPUT_NOTHING;
		}
	} else {
		retval = LOG_OUTPUT_NOTHING;
	}
#endif
	return retval;
}

char *
com_get_logfile(){
	char *strenv;

	if(logfile != NULL){
		return logfile;
	}
#ifdef _WIN32
	size_t len = 0;
	errno_t err;

	const char defpath[] = "D:\\develop\\ocesql.log";

	err = _dupenv_s(&strenv, &len, "OCDB_LOGFILE");

	if (!len){
		return com_strdup(defpath);
	} else {
		return strenv;
	}
#else
	const char defpath[] = "/tmp/ocesql.log";

	char *tmp = getenv("OCDB_LOGFILE");
	if(tmp != NULL){
		strenv = strdup(tmp);
	} else {
		strenv = strdup(defpath);
	}
	return strenv;
#endif
}

/*
 * <Function name>
 *   com_getenv
 *
 * <Outline>
 *   環境変数から値を取得する。ない場合はエラーログを残した上でNULLを返す
 *
 * <Input>
 *   @param: パラメータ名
 *   @def  : default value
 *
 * <Output>
 *   success: パラメータの値
 *   failure: default value
 */
char *com_getenv(char *param, char *def){
	char *env;

	if(param == NULL){
		ERRLOG("parameter is NULL\n");
		return def;
	}
#ifdef _WIN32
	size_t len = 0;
	errno_t err;

	err = _dupenv_s(&env, &len, param);
	if (!len){
		LOG("param '%s' is not set. set default value. \n", param);
		return def;
	}
#else
	env = getenv(param);
	if(env == NULL){
		LOG("param '%s' is not set. set default value. \n", param);
		return def;
	}
#endif
	LOG("param '%s' is %s. \n", param, env);
	return env;
}

void
com_fprint_log(FILE *fp, const char *filename, const char *funcname){
	time_t clock;
	time(&clock);
#ifdef _WIN32
	char logbuf[LOGBUFSIZE];
	char *ctx = NULL;

	com_ctime(logbuf, sizeof(logbuf), &clock);
	fprintf(fp, "[%s]#DEBUG# %s:%s(): ", com_strtok(logbuf, "\r\n", &ctx), filename, funcname);
#else
	fprintf(fp, "[%s]#DEBUG# %s:%s(): ", strtok(ctime(&clock), "\r\n"), filename, funcname);
#endif
}

void
com_fprint_elog(FILE *fp, const char *filename, const char *funcname){
	time_t clock;
	time(&clock);
#ifdef _WIN32
	char logbuf[LOGBUFSIZE];
	char *ctx = NULL;

	com_ctime(logbuf, sizeof(logbuf), &clock);
	fprintf(fp, "[%s]#ERROR# %s:%s(): ", com_strtok(logbuf, "\r\n", &ctx), filename, funcname);
#else
	fprintf(fp, "[%s]#ERROR# %s:%s(): ", strtok(ctime(&clock), "\r\n"), filename, funcname);
#endif
}

/*
* <Function name>
*   com_putenv
*
* <Outline>
*   環境変数を更新する。
*
* <Input>
*   @param: パラメータ名
*   @value: 更新値
*
* <Output>
*   success: RESULT_SUCCESS
*   failure: RESULT_FAILED
*/
int
com_putenv(const char *param, const char *value){
#ifdef _WIN32
	int res = _putenv_s(param, value);
#else
	int res = setenv(param, value, 1);
#endif
	if(res){
		int err = errno;
		ERRLOG("failed to exec com_putenv by errno: %d\n", err);
		return RESULT_FAILED;
	}
	return RESULT_SUCCESS;
}

char *
com_replace(char* src, const char* before, const char* after) {
	char *pos;

	if (src == NULL || before == NULL || after == NULL) {
		return src;
	}

	pos = strstr(src, before);

	if (pos == NULL) {
		return src;
	}

	while (pos != NULL){
		if (strlen(before) < strlen(after)) {
			src = realloc(src, strlen(src) + strlen(after) + 1 - strlen(before));
			memmove(pos + strlen(after), pos + strlen(before), strlen(src) - strlen(before) - (pos - src) + 1);
			memcpy(pos, after, strlen(after));
		}
		else {
			memcpy(pos, after, strlen(after));
			if (strlen(before) > strlen(after)) {
				memmove(pos + strlen(after), pos + strlen(before), strlen(src) - strlen(before) - (pos - src) + 1);
			}
		}
		pos = strstr(src, before);
	}

	return src;
}

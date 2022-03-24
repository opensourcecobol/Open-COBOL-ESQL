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
#include "ocesqlutil.h"

#ifdef _WIN32
#include <process.h>
#else
#include <unistd.h>
#include <iconv.h>
#endif

char *
com_strdup(const char *cid){
#ifdef _WIN32
	return _strdup(cid);
#else
	return strdup(cid);
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

void
com_fopen(FILE** fp, const char *fn, const char *md ){
#ifdef _WIN32
	fopen_s(fp, fn, md);
#else
	*fp = fopen(fn, md);
#endif
}

void
com_dupenv(char **buf, size_t *bufs, const char *nm){
#ifdef _WIN32
	_dupenv_s(buf, bufs, nm);
#else
	*buf = getenv(nm);
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
com_strncpy(char *st1, size_t stb1, const char *st2, size_t stb2){
#ifdef _WIN32
	strncpy_s(st1, stb1, st2, stb2);
	return st1;
#else
	return strncpy(st1, st2, stb2);
#endif
}

int
com_getpid(){
#ifdef _WIN32
	return _getpid();
#else
	return getpid();
#endif
}

int
com_unlink(const char *df){
#ifdef _WIN32
	return _unlink(df);
#else
	return unlink(df);
#endif
}

void
com_readline(FILE *readfile, char *inbuff, int *lineno, int *eofflg){
#ifdef _WIN32
	char ipchar;
	int n;

	if (readfile){
		ipchar = ' ';

		for (n = 0;ipchar != '\n';n++) {
			ipchar = fgetc(readfile);
			if (ipchar==EOF){
				*eofflg = 1;
				break;
			}
			inbuff[n] = ipchar;

		}
		inbuff[n] = '\0';
		*lineno = *lineno + 1;
	}
#else
	if(fgets(inbuff, 256, readfile) != NULL){
		int len = strlen(inbuff);
		if((inbuff[len-2] == '\r') && (inbuff[len-1] == '\n')){
			inbuff[len-2] = '\n';
			inbuff[len-1] = '\0';
		}
	} else if(feof(readfile)){
		*eofflg = 1;
	} else {
		printmsg("com_readline: although EOF wasn't detected, fgets() returned NULL ");
		*eofflg = 1;
	}
	*lineno = *lineno + 1;
#endif
}

int
com_stricmp(const char *str1, const char *str2){
#ifdef _WIN32
	return _stricmp(str1, str2);
#else
	return strcasecmp(str1, str2);
#endif
}


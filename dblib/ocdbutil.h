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


#ifndef OCDBUTIL_H
#define OCDBUTIL_H

#include <time.h>

#define SIGN_LENGTH 1
#define TERMINAL_LENGTH 1
#define DECIMAL_LENGTH 1

#define BIGENDIAN 1
#define LITTLEENDIAN 2

#define MEM_FREE(x) {free(x); x=NULL;}

#define BUFFSIZE 256

extern char type_tc_negative_final_number[];

void insert_decimal_point(char *, int, int);
int type_tc_is_positive(char *);
char *uint_to_str(int);
char *oc_strndup(char *, int);
char *get_str_without_after_space(char *);
char *get_str_replace_hostvalue(char *, int *);
int get_endian();

char *com_strdup(const char *);
int com_sprintf(char *, size_t, const char *, ...);
char *com_strcat(char *, size_t, const char *);
char *com_strncat(char *, size_t, const char *, size_t);
void com_sleep(unsigned int);
char *com_strtok(char *, const char *, char **);
void com_fopen(FILE**, const char *,const char *);
char *com_strcpy(char *, size_t, const char*);
char *com_ctime(char *, size_t, const time_t *);
char *com_strncpy(char *, size_t, const char*, size_t);
int com_get_loglevel();
char *com_get_logfile();
char *com_getenv(char *, char *);
void com_fprint_log(FILE *, const char *, const char *);
void com_fprint_elog(FILE *, const char *, const char *);
int com_putenv(const char *, const char *);
char *com_replace(char *, const char *, const char *); 


#endif

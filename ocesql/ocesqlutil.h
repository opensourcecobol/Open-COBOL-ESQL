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

#ifndef OCESQLUTIL_H
#define OCESQLUTIL_H

char *com_strdup(const char *);
int com_sprintf(char *, size_t, const char *, ...);
char *com_strcat(char *, size_t, const char *);
void com_fopen(FILE**, const char *,const char *);
char *com_strcpy(char *, size_t, const char*);
char *com_strncpy(char *, size_t, const char*, size_t);
void com_dupenv(char **, size_t *,const char *);
int com_getpid(void);
int com_unlink(const char *);
void com_readline(FILE *, char *, int *, int *);
int com_stricmp(const char *, const char *);

#endif

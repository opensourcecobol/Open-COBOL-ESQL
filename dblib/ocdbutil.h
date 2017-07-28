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

#ifndef OCDBUTIL_H
#define OCDBUTIL_H

#define SIGN_LENGTH 1
#define TERMINAL_LENGTH 1
#define DECIMAL_LENGTH 1

#define MEM_FREE(x) {free(x); x=NULL;}

#define BUFFSIZE 256

extern char type_tc_negative_final_number[];

void insert_decimal_point(char *, int, int);
int type_tc_is_positive(char *);
char *ocdb_getenv(char *, char *);
char *uint_to_str(int);
char *oc_strndup(char *, int);
char *get_str_without_after_space(char *);

#endif

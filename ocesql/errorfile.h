/* 
Copyright (C) 2022 Tokyo System House Co.,Ltd.
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
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include "ocesqlutil.h"



int openerrorfile(char *filename);
int closeerrorfile();
int spreadchar(char * code , char* msg, char *ret);
int geterrormsg(char *code , char *msg, int len);
int printerrormsg(char *name, int line, char * code);
int printmsg(char *format, ...);

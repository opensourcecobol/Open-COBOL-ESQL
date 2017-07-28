/*
 * Copyright (C) 2015 Tokyo System House Co.,Ltd.
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

#ifndef OCDBLOG_H /* OCDBLOG_H */
#define OCDBLOG_H

#define LOG_OUTPUT_NOTSET 0
#define LOG_OUTPUT_NOTHING 1
#define LOG_OUTPUT_ERR 2
#define LOG_OUTPUT_DEBUG 3

#define LOGBUFSIZE 26

static int loglevel = LOG_OUTPUT_NOTSET;
static char* logfile = NULL;

#define LOG(...) {OCLOG(__FILE__, __FUNCTION__,__VA_ARGS__);}
#define ERRLOG(...) {OCERRLOG(__FILE__, __FUNCTION__,__VA_ARGS__);}

void OCLOG(const char *, const char *, const char *, ...);
void OCERRLOG(const char *, const char *, const char *, ...);

#endif

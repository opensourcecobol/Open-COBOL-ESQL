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

#ifndef OCDBLOG_H /* OCDBLOG_H */
#define OCDBLOG_H

#include <stdio.h>
#include <string.h>
#include <time.h>

#define NDEBUG
#define LOG_OUTPUTFILE

#ifndef LOG_OUTPUTFILE /* LOG_OUTPUTFILE */
#ifdef NDEBUG /* NODEBUG */
#define LOG(args...) /*none*/
#else /* NODEBUG */
#define LOG(args...) {time_t clock; time(&clock); fprintf(stderr, "[%s]#DEBUG# %s:%s(): ", strtok(ctime(&clock), "\r\n"), __FILE__,  __func__); fprintf(stderr, args);}
#endif /* NODEBUG */
#define ERRLOG(args...) {time_t clock; time(&clock); fprintf(stderr, "[%s] #ERROR# %s:%s(): ", strtok(ctime(&clock), "\r\n"), __FILE__, __func__); fprintf(stderr, args);}
#else /* LOG_OUTPUTFILE */
#ifdef NDEBUG /* NODEBUG */
#define LOG(args...) /*none*/
#else /* NODEBUG */
#define LOG(args...) {time_t clock; FILE *fp; time(&clock); fp = fopen("/tmp/ocesql.log", "a"); if(fp != NULL){fprintf(fp, "[%s]#DEBUG# %s:%s(): ", strtok(ctime(&clock), "\r\n"), __FILE__, __func__); fprintf(fp, args); fclose(fp);}}
#endif /* NODEBUG */
#define ERRLOG(args...) {time_t clock; FILE *fp; time(&clock); fp = fopen("/tmp/ocesql.log", "a"); if(fp != NULL){fprintf(fp, "[%s]#ERROR# %s:%s(): ", strtok(ctime(&clock), "\r\n"), __FILE__, __func__); fprintf(fp, args); fclose(fp);}}
#endif /* LOG_OUTPUTFILE */

#endif /* OCDBLOG_H */

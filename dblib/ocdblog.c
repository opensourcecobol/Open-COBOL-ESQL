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

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>
#include "ocdbutil.h"
#include "ocdblog.h"

void OCLOG(const char *file, const char *func, const char *format, ...){
	if(loglevel == LOG_OUTPUT_NOTSET){
		loglevel = com_get_loglevel();
		logfile = com_get_logfile();
	}

	if(loglevel == LOG_OUTPUT_DEBUG){
		// output log
		FILE *fp;
		va_list args;

		com_fopen(&fp, logfile, "a");
		if(fp != NULL){
			com_fprint_log(fp, file, func);
			va_start(args, format);
			vfprintf(fp, format, args);
			va_end(args);
			fclose(fp);
		}
	}
}

void OCERRLOG(const char *file, const char *func, const char *format, ...){
	if(loglevel == LOG_OUTPUT_NOTSET){
		loglevel = com_get_loglevel();
		logfile = com_get_logfile();
	}

	if(loglevel >= LOG_OUTPUT_ERR){
		FILE *fp;
		va_list args;

		com_fopen(&fp, logfile, "a");
		if(fp != NULL){
			com_fprint_elog(fp, file, func);
			va_start(args, format);
			vfprintf(fp, format, args);
			va_end(args);
			fclose(fp);
		}
	}
}

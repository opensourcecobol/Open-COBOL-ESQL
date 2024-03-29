﻿/*
 * Copyright (C) 2015, 2022 Tokyo System House Co.,Ltd.
 * Copyright (C) 2022 Simon Sobisch
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

#ifndef OCDBLOG_H /* OCDBLOG_H */
#define OCDBLOG_H

enum ocloglevel {
    LOG_OUTPUT_NOTSET = 0,
    LOG_OUTPUT_NOTHING = 1,
    LOG_OUTPUT_ERR = 2,
    LOG_OUTPUT_DEBUG = 3
};

#define LOGBUFSIZE 26

extern enum ocloglevel loglevel;
extern char* logfile;

#define LOG(...) {if (loglevel == LOG_OUTPUT_NOTSET || loglevel == LOG_OUTPUT_DEBUG) OCLOG(__FILE__, __FUNCTION__,__VA_ARGS__);}
#define ERRLOG(...) {if (loglevel != LOG_OUTPUT_NOTHING) OCERRLOG(__FILE__, __FUNCTION__,__VA_ARGS__);}

void OCLOG(const char *, const char *, const char *, ...);
void OCERRLOG(const char *, const char *, const char *, ...);

/* actually defined in ocdbutil.h */
enum ocloglevel com_get_loglevel();
char *com_get_logfile();

#endif

#
# Makefile open-cobol-esql
#
# Copyright (C) 2015 Tokyo System House Co.,Ltd.
# Copyright (C) 2021 Simon Sobisch
#
# This file is part of Open Cobol ESQL.
#
# Open Cobol ESQL is free software: you can redistribute it
# and/or modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# Open Cobol ESQL is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Open Cobol ESQL.  If not, see <https://www.gnu.org/licenses/>.

if DISABLE_OCESQL
  OCESQL = 
else
  OCESQL = ocesql copy sample
endif

SUBDIRS = dblib $(OCESQL)

dist_noinst_SCRIPTS = autogen.sh build_aux/bootstrap

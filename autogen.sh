#!/bin/sh
# autogen.sh Open-COBOL-ESQL
# Bootstrap Open Cobol ESQL package from checked-out sources
# Note:  call as ./autogen.sh if you don't have readlink -f
#
# Copyright (C) 2019 Free Software Foundation, Inc.
# Copyright (C) 2021 Simon Sobisch
# Written by Simon Sobisch
#
# This file was part of GnuCOBOL and is incorporated into Open Cobol ESQL.
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

me=autogen.sh

# get path to Open Cobol ESQL main directory
if test "$0" = "./$me"; then
  MAINPATH=.
  GCMAINPATH=".."
else
  MAINPATH=$(dirname $(readlink -f "$0"))
  GCMAINPATH="$MAINPATH"
fi
if test ! -f $MAINPATH/$me; then
  echo; echo "ERROR - cannot set main directory [checked $MAINPATH/build_aux/$me] - aborting $me" && exit 1
fi

olddir_autogen=`pwd`
cd $MAINPATH/build_aux && (chmod -f u+x ./bootstrap; ./bootstrap); ret=$?
cd $olddir_autogen

if test $ret -ne 0; then
  exit $ret
fi

#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([Open COBOL ESQL], [1.3.0b], [opencobol@tsh-world.co.jp])
AC_CONFIG_SRCDIR([config.h.in])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_AUX_DIR([build_aux])
AM_INIT_AUTOMAKE

# Checks for programs.
AC_PROG_CC
AC_PROG_CPP

AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

AC_PROG_AWK

AM_PROG_LEX
AC_PROG_YACC

AC_PROG_LIBTOOL

LT_INIT([dlopen win32-dll])

# Configure options.
GEN_DEBUG_OPT="no"
AC_ARG_ENABLE(debug,
  [  --enable-debug          (OCESQL) enable -g C compiler debug option],
  [if test "$enable_debug" = "yes"
   then
        GEN_DEBUG_OPT="yes"
   fi])

if test "$GEN_DEBUG_OPT" = "no"
then
        CFLAGS=`echo "$CFLAGS" | sed -e 's/^-g//' -e 's/-g$//' -e 's/-g / /'`
fi

DISABLE_COMPILER=no
AC_ARG_ENABLE(precompiler,
  [  --disable-precompiler   (OCESQL) disable OCESQL precompiler install],
  [if test "$enable_precompiler" = "no"
   then
        DISABLE_COMPILER=yes
   fi])

AM_CONDITIONAL(DISABLE_OCESQL, test $DISABLE_COMPILER = yes)

# set PKG_CONFIG to use (cross-compile aware)
PKG_PROG_PKG_CONFIG

curr_libs="$LIBS"; curr_cppflags="$CPPFLAGS"
PKG_CHECK_MODULES([LIBPQ], [libpq >= 8.4.0], [], [#
])
if test -n "$LIBPQ_CFLAGS"; then
  CPPFLAGS="$CPPFLAGS $LIBPQ_CFLAGS"
fi
if test -z "${LIBPQ_LIBS+x}"; then
  LIBPQ_LIBS="-lpq"
fi
LIBS="$LIBS $LIBPQ_LIBS"

# Checks for PostgreSQL.
AC_CHECK_HEADERS([libpq-fe.h], [
	#AC_CHECK_LIB([pq], [PQconnectdb])
	#AC_MSG_CHECKING([if linking against libgmp with "$GMP_LIBS" works])
	#AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <gmp.h>]],
	#  [[mpz_t integ; mpz_init (integ);]])],
	#  [AC_MSG_RESULT([yes])
	#   curr_cppflags=$CPPFLAGS   # for the include in cobc generated modules
	#   LIBCOB_LIBS="$LIBCOB_LIBS $GMP_LIBS"
	#   USE_MATH=gmp],
	#  [AC_MSG_RESULT([no])
	#   if test "$USE_MATH" = "gmp"; then
	#     AC_MSG_ERROR([[GMP is required for --with-math=gmp, you may adjust GMP_LIBS]])
	#   fi
	#   LIBS="$curr_libs"; CPPFLAGS="$curr_cppflags"]
	#)
], [AC_MSG_ERROR([libpq-fe.h is required, you may adjust LIBPQ_CFLAGS])
])

LIBESQL_LIBS=$LIBPQ_LIBS
LIBESQL_CPPFLAGS=$LIBPQ_CFLAGS

LIBS=$curr_libs
CPPFLAGS=$curr_cppflags

# Checks for header files.
AC_FUNC_ALLOCA
AC_CHECK_HEADERS([inttypes.h libintl.h locale.h malloc.h stddef.h stdlib.h string.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_HEADER_STDBOOL
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT8_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT8_T

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_CHECK_FUNCS([memmove memset setenv strcasecmp strchr strdup strrchr strstr])

AC_CONFIG_FILES([Makefile dblib/Makefile ocesql/Makefile
                 copy/Makefile sample/Makefile])


AC_SUBST([LIBESQL_LIBS])
AC_SUBST([LIBESQL_CPPFLAGS])

AC_OUTPUT

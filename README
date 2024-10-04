
                   Open Cobol ESQL (Ocesql)
                       Version 1.4.0

                       October 4, 2024

                 Tokyo System House Co., Ltd.
                  opencobol@tsh-world.co.jp

1. Introduction
===============

Open Cobol ESQL (ocesql) is an open-source Embedded SQL pre-compiler
and run-time library designed for COBOL applications which access 
an open-source database.  Ocesql translates each EXEC SQL statements
into the standard COBOL CALL statements to the run-time library.

2. Package overview
===================

This package includes the following subdirectories:

    - copy          Copy books for pre-compiled COBOL programs
    - dblib         DB access library (Run-time library)
    - ocesql        ESQL pre-compiler
    - sample        Sample cobol programs

A build of this package typically installs an ocesql executable and 
run-time library as follows:

    - ocesql        ESQL pre-compiler
    - libocesql.so  Run-time library

3. License
==========

All programs except those in dblib subdirectories are distributed
under the GNU General Public License v3 or later.  See COPYING for details.

Programs in dblib subdirectories are distributed under the GNU
Lesser General Public License v3 or later.  See COPYING.LESSER for details.

See also AUTHORS for the author and contributors.

4. Requirements
===============

  1) Open-source database

    Ocesql currently supports PostgreSQL database only.

    o PostgreSQL C Library (libpq) 8.4 or later
      https://www.postgresql.org/
      https://www.postgresql.jp/

  2) COBOL Development System

    Ocesql is tested with the following COBOL compilers:

    o OpenCOBOL 1.1
      https://sourceforge.net/projects/open-cobol/

    o opensource COBOL 1.5.2J
      https://www.osscons.jp/osscobol/download/

    o Micro Focus Server Express 5.1
      Micro Focus Visual COBOL 2.1 for x64/x86 Linux
      https://www.microfocus.com/
      https://www.microfocus.co.jp/

    o GnuCOBOL 2.2
      https://www.gnu.org/software/gnucobol/

5. Installation
===============

To install Ocesql (PostgreSQL 9.0 or above):  
Before running "configure", you'll need to tell ocesql where your include PostgreSQL include files such as "libpq-fe.h" are, 
and also possibly the library location.  These vary by PostgreSQL version and distribution, but on a recent Ubuntu with PostgreSQL 14 this will be something like:

    export CPPFLAGS="-I/usr/include/postgresql"

An earlier version (PostgreSQL 9) might be:

    export CPPFLAGS="-I/usr/pgsql-9.x/include"
    export LDFLAGS="-L/usr/pgsql-9.x/lib"
    
Then run:

    ./configure
    make
    make install
    
If you want to install Ocesql on PostgreSQL < 9, just:

    ./configure
    make
    make install



The "make install" will default to "/usr/local" as the install path.
You may override this by specifying "--prefix=<your install path>" 
to the "./configure" command.

6. Sample programs
==================

Ocesql comes with two sample programs that demonstrate embedded 
SQL in a COBOL application.  The "sample" directory contains
following programs:

  o INSERTTBL.cbl ---- CONNECT, DROP TABLE, CREATE TABLE, 
                       INSERT, COMMIT, ROLLBACK, DISCONNECT

  o FETCHTBL.cbl ----- CONNECT, SELECT COUNT(*), DECLARE cursor, 
                       FETCH cursor, COMMIT, ROLLBACK, DISCONNECT

* Note that these sample programs includes Japanese Shift-JIS characters.
  If you wish to run on non-Japanese environment, modify them first so that the commented-out lines containing non-Japanese text are used in place of the Japanese ones.

To run the sample programs: 

  1) Create a sample database named "testdb"

     When PostgreSQL (non-Japanese):

       createdb -T template0 testdb

     Or, if working in a Japanese environment, encoding should be UTF-8.

       createdb -E UTF8 -T template0 --lc-collate=ja_JP.UTF-8 --lc-ctype=ja_JP.UTF-8 testdb

  2) Pre-compile the sample programs

       cd <your install path>/Open-COBOL-ESQL/sample
       ocesql INSERTTBL.cbl INSERTTBL.cob
       ocesql FETCHTBL.cbl FETCHTBL.cob

   * Usage: ocesql [--inc=<include path>] SOURCE [DESTFILE] [LOGFILE]

  3) Compile the sample programs with COBOL compiler and 
     generate executable files:

     Working with Micro Focus Server Express or Visual COBOL:

       export COBCPY=<your install path>/Open-COBOL-ESQL/copy
       cob -ze "" /usr/local/lib/libocesql.so -o mfocesql.so
       cob -x INSERTTBL.cob -C"INITCALL(mfocesql)"
       cob -x FETCHTBL.cob -C"INITCALL(mfocesql)"

     Working with OpenCOBOL or opensource COBOL:

       export COBCPY=<your install path>/Open-COBOL-ESQL/copy
       cobc -x -locesql INSERTTBL.cob
       cobc -x -locesql FETCHTBL.cob

  4) Run the sample programs:
  
       ./INSERTTBL
       ./FETCHTBL

   * To create a sample table, run INSERTTBL first.
     Database credentials are hardcoded in each sample program and by default access is as the "postgres" user.

7. Working with PostgreSQL
==========================

7-1. SQL Verbs
--------------
Ocesql accepts most SQL verbs supported by PostgreSQL's libpq.

The pre-compiler translates each embedded SQL statement into a standard
COBOL CALL statement to the run-time library.  When the CALL statement
is executed, the original SQL statement is passed to libpq's functions
like PQexecParams or PQexec, which in turn submits it to the database.

There are some exceptions to this rule.  When the following SQL verbs
are found, the pre-compiler performs syntax checking, parses these verbs
and generates a series of special SQL commands:

  o CONNECT username IDENTIFIED BY passwd USING dbname
  o DISCONNECT
  o SELECT ... INTO :host-variable ...
  o DECLARE CURSOR cursor FOR SELECT ...
  o OPEN cursor
  o CLOSE cursor
  o FETCH cursor INTO :host-variable

The sample programs demonstrate the use of these special verbs.

7-2. Error handling
-------------------
If SQLCODE or SQLSTATE of SQLCA is set to non-zero value, 
this means that a PostgreSQL error has happened internally.  
SQLSTATE is set to one of the PostgreSQL error codes listed in 
the following URL:

  https://www.postgresql.org/docs/8.4/static/errcodes-appendix.html
  https://www.postgresql.jp/document/8.4/html/errcodes-appendix.html

The sample programs demonstrate the use of SQLSTATE.

8. Working with other databases
===============================
Ocesql is currently supports PostgreSQL only, although it is designed 
to work with several open-source databases, 

----------------------------------------------------------------------
Copyright 2019 Tokyo System House Co., Ltd.

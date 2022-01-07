
       IDENTIFICATION              DIVISION.
      ******************************************************************
       PROGRAM-ID.                 prog.
      ******************************************************************
       DATA                        DIVISION.
      ******************************************************************
       WORKING-STORAGE             SECTION.
       01 TEST-DATA.
         03 FILLER PIC X(9) VALUE "0001____1".
         03 FILLER PIC X(9) VALUE "0002____2".
         03 FILLER PIC X(9) VALUE "0003____3".
         03 FILLER PIC X(9) VALUE "0004____4".
         03 FILLER PIC X(9) VALUE "0005____5".

       01 TEST-DATA-R REDEFINES TEST-DATA.
         03 TEST-TBL OCCURS 5.
           05 TEST-ID PIC 9(4).
           05 TEST-V  PIC X(5).

       01 IDX PIC 9.

       01 READ-DATA.
         03  READ-TBL    OCCURS  1.
           05  READ-V PIC X(5).

       01 SQL-COMMAND.
         03 SQL-COMMAND-LEN PIC 9(9) VALUE 50.
         03 SQL-COMMAND-ARR PIC X(50)
            VALUE "DELETE FROM TESTTABLE WHERE ID > ?".
       01 SQL-COMMAND-ARG PIC 99 VALUE 2.

       01 INVALID-COMMAND PIC X(50)
         VALUE "DELETE FROM TESTTABLEERROR WHERE ID > ?".

OCESQL*EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01 DATA-ID PIC 9(4).
       01 DATA-V PIC X(5).
       01  DBNAME                  PIC  X(30) VALUE SPACE.
       01  USERNAME                PIC  X(30) VALUE SPACE.
       01  PASSWD                  PIC  X(10) VALUE SPACE.
OCESQL*EXEC SQL END DECLARE SECTION END-EXEC.

OCESQL*EXEC SQL INCLUDE SQLCA END-EXEC.
OCESQL     copy "sqlca.cbl".

      ******************************************************************
OCESQL*
OCESQL 01  SQ0001.
OCESQL     02  FILLER PIC X(030) VALUE "DROP TABLE IF EXISTS TESTTABLE".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0002.
OCESQL     02  FILLER PIC X(048) VALUE "CREATE TABLE TESTTABLE ( ID in"
OCESQL  &  "teger, V char(5) )".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0003.
OCESQL     02  FILLER PIC X(039) VALUE "INSERT INTO TESTTABLE VALUES ("
OCESQL  &  " $1, $2 )".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0004.
OCESQL     02  FILLER PIC X(014) VALUE "DISCONNECT ALL".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
       PROCEDURE                   DIVISION.
      ******************************************************************
       MAIN-RTN.

         PERFORM SETUP-DB.

OCESQL*  EXEC SQL
OCESQL*      PREPARE st FROM :SQL-COMMAND
OCESQL*  END-EXEC.
OCESQL     CALL "OCESQLPrepare" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "st" & x"00"
OCESQL          BY REFERENCE SQL-COMMAND-ARR
OCESQL          BY VALUE SQL-COMMAND-LEN
OCESQL     END-CALL.
         PERFORM SHOW-STATUS.

OCESQL*  EXEC SQL
OCESQL*      EXECUTE st USING :SQL-COMMAND-ARG
OCESQL*  END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 2
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-COMMAND-ARG
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecPrepare" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "st" & x"00"
OCESQL          BY VALUE 1
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.
         PERFORM SHOW-STATUS.

         MOVE INVALID-COMMAND TO SQL-COMMAND-ARR.

OCESQL*  EXEC SQL
OCESQL*      PREPARE st FROM :SQL-COMMAND
OCESQL*  END-EXEC.
OCESQL     CALL "OCESQLPrepare" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "st" & x"00"
OCESQL          BY REFERENCE SQL-COMMAND-ARR
OCESQL          BY VALUE SQL-COMMAND-LEN
OCESQL     END-CALL.
         PERFORM SHOW-STATUS.

OCESQL*  EXEC SQL
OCESQL*      EXECUTE st USING :SQL-COMMAND-ARG
OCESQL*  END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 2
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-COMMAND-ARG
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecPrepare" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "st" & x"00"
OCESQL          BY VALUE 1
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.
         PERFORM SHOW-STATUS.

         PERFORM CLEANUP-DB.

         STOP RUN.

      ******************************************************************
       SETUP-DB.
      ******************************************************************

         MOVE  "testdb@db_postgres:5432"
           TO DBNAME.
         MOVE  "main_user"
           TO USERNAME.
         MOVE  "password"
           TO PASSWD.

OCESQL*  EXEC SQL
OCESQL*      CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
OCESQL*  END-EXEC.
OCESQL     CALL "OCESQLConnect" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE USERNAME
OCESQL          BY VALUE 30
OCESQL          BY REFERENCE PASSWD
OCESQL          BY VALUE 10
OCESQL          BY REFERENCE DBNAME
OCESQL          BY VALUE 30
OCESQL     END-CALL.

OCESQL*  EXEC SQL
OCESQL*      DROP TABLE IF EXISTS TESTTABLE
OCESQL*  END-EXEC.
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0001
OCESQL     END-CALL.

OCESQL*  EXEC SQL
OCESQL*      CREATE TABLE TESTTABLE
OCESQL*      (
OCESQL*        ID integer,
OCESQL*        V  char(5)
OCESQL*      )
OCESQL*  END-EXEC.
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0002
OCESQL     END-CALL.


         PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 5
           MOVE TEST-ID(IDX) TO DATA-ID
           MOVE TEST-V(IDX) TO DATA-V
OCESQL*    EXEC SQL
OCESQL*      INSERT INTO TESTTABLE VALUES
OCESQL*        (:DATA-ID, :DATA-V)
OCESQL*    END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 4
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE DATA-ID
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 5
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE DATA-V
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecParams" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0003
OCESQL          BY VALUE 2
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL
         END-PERFORM.

      ******************************************************************
       CLEANUP-DB.
      ******************************************************************

OCESQL*  EXEC SQL
OCESQL*      DISCONNECT ALL
OCESQL*  END-EXEC.
OCESQL     CALL "OCESQLDisconnect" USING
OCESQL          BY REFERENCE SQLCA
OCESQL     END-CALL.

      ******************************************************************
       SHOW-STATUS.
      ******************************************************************
         DISPLAY SQLCODE.
         DISPLAY SQLSTATE.




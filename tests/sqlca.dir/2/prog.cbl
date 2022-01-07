
       IDENTIFICATION              DIVISION.
      ******************************************************************
       PROGRAM-ID.                 prog.
      ******************************************************************
       DATA                        DIVISION.
      ******************************************************************
       WORKING-STORAGE             SECTION.
       01 READ-DATA.
         03  READ-TBL    OCCURS  1.
           05  READ-V PIC X(5).

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01 DATA-ID PIC 9(4).
       01 DATA-V PIC X(5).
       01  DBNAME                  PIC  X(30) VALUE SPACE.
       01  USERNAME                PIC  X(30) VALUE SPACE.
       01  PASSWD                  PIC  X(10) VALUE SPACE.
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.

      ******************************************************************
       PROCEDURE                   DIVISION.
      ******************************************************************
       MAIN-RTN.

       PERFORM SETUP-DB.

       EXEC SQL
         INSERT INTO TESTTABLE VALUES (1, 'hello')
       END-EXEC.
       PERFORM SHOW-STATUS.

       EXEC SQL
         INSERT INTO TESTTABLE VALUES ('invalid', 'invalid')
       END-EXEC.
       PERFORM SHOW-STATUS.

       EXEC SQL
         SELECT V INTO :READ-TBL FROM TESTTABLE
       END-EXEC.
       PERFORM SHOW-STATUS.

       EXEC SQL
         SELECT V INTO :READ-TBL FROM TESTTABLEERROR
       END-EXEC.
       PERFORM SHOW-STATUS.

       EXEC SQL
         UPDATE TESTTABLE SET V = 'world' WHERE ID = 1
       END-EXEC.
       PERFORM SHOW-STATUS.

       EXEC SQL
         UPDATE TESTTABLE SET V = 'world' WHERE ID = 3
       END-EXEC.
       PERFORM SHOW-STATUS.

       EXEC SQL
         UPDATE TESTTABLEERROR SET V = 'world' WHERE ID = 1
       END-EXEC.
       PERFORM SHOW-STATUS.

       EXEC SQL
         DELETE FROM TESTTABLE WHERE ID = 1
       END-EXEC.
       PERFORM SHOW-STATUS.

       EXEC SQL
         DELETE FROM TESTTABLE WHERE ID = 3
       END-EXEC.
       PERFORM SHOW-STATUS.

       EXEC SQL
         DELETE FROM TESTTABLEERROR WHERE ID = 3
       END-EXEC.
       PERFORM SHOW-STATUS.

       PERFORM CLEANUP-DB.

           STOP RUN.

      ******************************************************************
       SETUP-DB.
      ******************************************************************

           MOVE  "<|DB_NAME|>@<|DB_HOST|>:<|DB_PORT|>"
             TO DBNAME.
           MOVE  "<|DB_USER|>"
             TO USERNAME.
           MOVE  "<|DB_PASSWORD|>"
             TO PASSWD.

           EXEC SQL
               CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
           END-EXEC.

           EXEC SQL
               DROP TABLE IF EXISTS TESTTABLE
           END-EXEC.

           EXEC SQL
               CREATE TABLE TESTTABLE
               (
                 ID integer,
                 V  char(5)
               )
           END-EXEC.


      ******************************************************************
       CLEANUP-DB.
      ******************************************************************

           EXEC SQL
               DISCONNECT ALL
           END-EXEC.

      ******************************************************************
       SHOW-STATUS.
      ******************************************************************
           DISPLAY SQLCODE.
           DISPLAY SQLSTATE.


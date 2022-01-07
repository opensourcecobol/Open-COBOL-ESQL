
       IDENTIFICATION              DIVISION.
      ******************************************************************
       PROGRAM-ID.                 prog.
      ******************************************************************
       DATA                        DIVISION.
      ******************************************************************
       WORKING-STORAGE             SECTION.
       01 VV PIC S9(4)V9(2).
       01 VP PIC S9(4)PP.
       01 V PIC S9(4).

       01  TEST-DATA-V.
         03 FILLER       PIC S9(4)V9(2) VALUE 0.
         03 FILLER       PIC S9(4)V9(2) VALUE 0.01.
         03 FILLER       PIC S9(4)V9(2) VALUE -0.01.
         03 FILLER       PIC S9(4)V9(2) VALUE 1.01.
         03 FILLER       PIC S9(4)V9(2) VALUE -1.01.
         03 FILLER       PIC S9(4)V9(2) VALUE 10.1.
         03 FILLER       PIC S9(4)V9(2) VALUE -10.1.
         03 FILLER       PIC S9(4)V9(2) VALUE 1234.56.
         03 FILLER       PIC S9(4)V9(2) VALUE 9999.99.
         03 FILLER       PIC S9(4)V9(2) VALUE -9999.99.

       01  TEST-DATA-R-V   REDEFINES TEST-DATA-V.
         03  TEST-TBL-V    OCCURS  10.
           05  D-V             PIC S9(4)V9(2).

       01  TEST-DATA-P.
         03 FILLER       PIC S9(4)PP VALUE 0.
         03 FILLER       PIC S9(4)PP VALUE 100.
         03 FILLER       PIC S9(4)PP VALUE -100.
         03 FILLER       PIC S9(4)PP VALUE 110000.
         03 FILLER       PIC S9(4)PP VALUE -110000.
         03 FILLER       PIC S9(4)PP VALUE 123000.
         03 FILLER       PIC S9(4)PP VALUE -12300.
         03 FILLER       PIC S9(4)PP VALUE 100000.
         03 FILLER       PIC S9(4)PP VALUE 999900.
         03 FILLER       PIC S9(4)PP VALUE -999900.

       01  TEST-DATA-R-P   REDEFINES TEST-DATA-P.
         03  TEST-TBL-P    OCCURS  10.
           05  D-P             PIC S9(4)PP.

       01  TEST-DATA.
         03 FILLER       PIC S9(4) VALUE 0.
         03 FILLER       PIC S9(4) VALUE 1.
         03 FILLER       PIC S9(4) VALUE -1.
         03 FILLER       PIC S9(4) VALUE 10.
         03 FILLER       PIC S9(4) VALUE -10.
         03 FILLER       PIC S9(4) VALUE 100.
         03 FILLER       PIC S9(4) VALUE -100.
         03 FILLER       PIC S9(4) VALUE 1000.
         03 FILLER       PIC S9(4) VALUE 9999.
         03 FILLER       PIC S9(4) VALUE -9999.

       01  TEST-DATA-R   REDEFINES TEST-DATA.
         03  TEST-TBL    OCCURS  10.
           05  D             PIC S9(4).

       01  IDX                     PIC  S9(02).
       01 LOG-COUNT PIC 9999 VALUE 1.

       01 READ-DATA-TBL-V.
         03  READ-TBL-V    OCCURS  10.
           05  READ-DATA-V             PIC S9(4)V9(2).

       01 READ-DATA-TBL-P.
         03  READ-TBL-P    OCCURS  10.
           05  READ-DATA-P             PIC S9(4)PP.

       01 READ-DATA-TBL.
         03  READ-TBL    OCCURS  10.
           05  READ-DATA             PIC S9(4).

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
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

      *    SHOW RESULT
           EXEC SQL
               SELECT FIELD INTO :READ-TBL-V FROM TESTTABLEV ORDER BY N
           END-EXEC.

           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 10
               DISPLAY READ-DATA-V(IDX)
           END-PERFORM.

      *    SHOW RESULT
           EXEC SQL
               SELECT FIELD INTO :READ-TBL-P FROM TESTTABLEP ORDER BY N
           END-EXEC.

           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 10
               DISPLAY READ-DATA-P(IDX)
           END-PERFORM.

      *    SHOW RESULT
           EXEC SQL
               SELECT FIELD INTO :READ-TBL FROM TESTTABLE ORDER BY N
           END-EXEC.

           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 10
               DISPLAY READ-DATA(IDX)
           END-PERFORM.

       PERFORM CLEANUP-DB.

      *    END
           STOP RUN.

      ******************************************************************
       SETUP-DB.
      ******************************************************************

      *    SERVER
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
               DROP TABLE IF EXISTS TESTTABLEV
           END-EXEC.

           EXEC SQL
               DROP TABLE IF EXISTS TESTTABLEP
           END-EXEC.

           EXEC SQL
               DROP TABLE IF EXISTS TESTTABLE
           END-EXEC.

           EXEC SQL
                CREATE TABLE TESTTABLEV
                (
                    N         NUMERIC(2,0) NOT NULL,
                    FIELD     DECIMAL(6,2) NOT NULL
                )
           END-EXEC.

           EXEC SQL
                CREATE TABLE TESTTABLEP
                (
                    N         NUMERIC(2,0) NOT NULL,
                    FIELD     NUMERIC(6,0) NOT NULL
                )
           END-EXEC.

           EXEC SQL
                CREATE TABLE TESTTABLE
                (
                    N         NUMERIC(2,0) NOT NULL,
                    FIELD     NUMERIC(4,0) NOT NULL
                )
           END-EXEC.

      *    INSERT ROWS USING HOST VARIABLE
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 10
              MOVE D-V(IDX)     TO  VV
              EXEC SQL
                 INSERT INTO TESTTABLEV VALUES (:IDX, :VV)
              END-EXEC
           END-PERFORM.

      *    INSERT ROWS USING HOST VARIABLE
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 10
              MOVE D-P(IDX)     TO  VP
              EXEC SQL
                 INSERT INTO TESTTABLEP VALUES (:IDX, :VP)
              END-EXEC
           END-PERFORM.

      *    INSERT ROWS USING HOST VARIABLE
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 10
              MOVE D(IDX)     TO  V
              EXEC SQL
                 INSERT INTO TESTTABLE VALUES (:IDX, :V)
              END-EXEC
           END-PERFORM.

      *    COMMIT
           EXEC SQL
               COMMIT WORK
           END-EXEC.

      ******************************************************************
       CLEANUP-DB.
      ******************************************************************
           EXEC SQL
               DROP TABLE IF EXISTS TESTTABLEV
           END-EXEC.

           EXEC SQL
               DROP TABLE IF EXISTS TESTTABLEP
           END-EXEC.

           EXEC SQL
               DROP TABLE IF EXISTS TESTTABLE
           END-EXEC.

           EXEC SQL
               DISCONNECT ALL
           END-EXEC.


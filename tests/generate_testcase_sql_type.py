test_data = [
    {'sql_type': 'bigint', 'cobol_type': '9(11)', 'input_value': '12345678901', 'expected': '12345678901'},
    {'sql_type': 'bigserial', 'cobol_type': '9(11)', 'input_value': '12345678901', 'expected': '12345678901'},
    #{'sql_type': 'bit(3)', 'cobol_type': '9(3)', 'input_value': '5', 'expected': '101'},
    #{'sql_type': 'boolean', 'cobol_type': 'X(4)', 'input_value': '"TRUE"', 'expected': 'TRUE'},
    #{'sql_type': 'boolean', 'cobol_type': 'X(5)', 'input_value': '"FALSE"', 'expected': 'FALSE'},
    {'sql_type': 'char(5)', 'cobol_type': 'X(5)', 'input_value': '"hello"', 'expected': 'hello'},
    #{'sql_type': 'cidr', 'cobol_type': 'X(9)', 'input_value': '"127.0.0.1"', 'expected': '127.0.0.1'},
    {'sql_type': 'date', 'cobol_type': 'X(10)', 'input_value': '"2021-01-23"', 'expected': '2021-01-23'},
    {'sql_type': 'double precision', 'cobol_type': '999V99', 'input_value': '123.45', 'expected': '123.45'},
    #{'sql_type': 'inet', 'cobol_type': 'X(9)', 'input_value': '"127.0.0.1"', 'expected': '127.0.0.1'},
    {'sql_type': 'integer', 'cobol_type': '9(3)', 'input_value': '123', 'expected': '123'},
    #{'sql_type': 'macaddr', 'cobol_type': 'X(17)', 'input_value': '"08-00-2b-01-02-03"', 'expected': '08-00-2b-01-02-03'},
    {'sql_type': 'numeric(6,2)', 'cobol_type': '9(4)V9(2)', 'input_value': '1234.56', 'expected': '1234.56'},
    {'sql_type': 'real', 'cobol_type': '9(4)V9(2)', 'input_value': '1234.56', 'expected': '1234.56'},
    {'sql_type': 'smallint', 'cobol_type': '9(5)', 'input_value': '12345', 'expected': '12345'},
    {'sql_type': 'smallserial', 'cobol_type': '9(5)', 'input_value': '12345', 'expected': '12345'},
    {'sql_type': 'serial', 'cobol_type': '9(10)', 'input_value': '1234567890', 'expected': '1234567890'},
    {'sql_type': 'text', 'cobol_type': 'X(5)', 'input_value': '"hello"', 'expected': 'hello'},
    {'sql_type': 'time', 'cobol_type': 'X(8)', 'input_value': '"12:34:56"', 'expected': '12:34:56'},
    {'sql_type': 'timestamp', 'cobol_type': 'X(19)', 'input_value': '"2021-01-23 12:34:56"', 'expected': '2021-01-23 12:34:56'},
]

template_main = '''
AT_SETUP([sql data test])

AT_DATA([prog.cbl], [
       IDENTIFICATION              DIVISION.
      ******************************************************************
       PROGRAM-ID.                 prog.
      ******************************************************************
       DATA                        DIVISION.
      ******************************************************************
       WORKING-STORAGE             SECTION.
%s

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
%s
       *    END
       PERFORM CLEANUP-DB.
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

      ******************************************************************
       CLEANUP-DB.
      ******************************************************************
           EXEC SQL
               DROP TABLE IF EXISTS TESTTABLE
           END-EXEC.

           EXEC SQL
               DISCONNECT ALL
           END-EXEC.

])

AT_CHECK([ocesql prog.cbl prog.cob > /dev/null])
AT_CHECK([${EMBED_DB_INFO} prog.cob])
AT_CHECK([${COMPILE_MODULE} prog.cob])
AT_CHECK([${RUN_MODULE} prog], [0],
[%s])

AT_CLEANUP
'''

template_working_storage = '''       01 INPUT-DATA-{0} PIC {1} VALUE {2}.
       01 OUTPUT-DATA-{0}-TBL.
         03 OUTPUT-DATA-{0}-ARR OCCURS 1.
           05 OUTPUT-DATA-{0} PIC {1}.
'''

template_procedure_division = '''       EXEC SQL
           DROP TABLE IF EXISTS TESTTABLE
       END-EXEC.

       EXEC SQL
            CREATE TABLE TESTTABLE
            (
                FIELD {0}
            )
       END-EXEC.

       EXEC SQL
            INSERT INTO TESTTABLE VALUES (:INPUT-DATA-{1})
       END-EXEC

       EXEC SQL
           COMMIT WORK
       END-EXEC.

       EXEC SQL
           SELECT FIELD INTO :OUTPUT-DATA-{1}-ARR FROM TESTTABLE
       END-EXEC.

       DISPLAY OUTPUT-DATA-{1}(1).
      ******************************************************************

'''

working_storage = ''
procedure_division = ''
expected_result = ''
for i, t in enumerate(test_data):
    working_storage += template_working_storage.format(i, t['cobol_type'], t['input_value'])
    procedure_division += template_procedure_division.format(t['sql_type'], i)
    expected_result += t['expected'] + '\n'

test_script = template_main % (working_storage, procedure_division, expected_result)

with open('sql_data.src/sql_type.at', 'w') as f:
    f.write(test_script)

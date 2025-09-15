       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTMGMT.
      ******************************************************************
      * PROGRAM: ACCTMGMT - ACCOUNT MANAGEMENT SYSTEM
      * VERSION: 1.0
      * DATE: 2024-09-15
      * PURPOSE: MAIN ACCOUNT MANAGEMENT PROGRAM FOR BROKER-DEALER
      * FUNCTIONS: CREATE, READ, UPDATE, DELETE ACCOUNTS
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO "ACCOUNT.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACCOUNT-ID
               FILE STATUS IS WS-FILE-STATUS.

           SELECT TRANSACTION-LOG ASSIGN TO "TRANSLOG.DAT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-LOG-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       COPY ACCOUNT.

       FD  TRANSACTION-LOG.
       01  LOG-RECORD                    PIC X(200).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS                PIC XX.
       01  WS-LOG-STATUS                 PIC XX.
       
       01  WS-OPERATION                  PIC X(06).
           88  CREATE-OPERATION          VALUE 'CREATE'.
           88  READ-OPERATION            VALUE 'READ  '.
           88  UPDATE-OPERATION          VALUE 'UPDATE'.
           88  DELETE-OPERATION          VALUE 'DELETE'.
           88  INQUIRY-OPERATION         VALUE 'INQUIR'.

       01  WS-RETURN-CODE                PIC 9(02).
           88  SUCCESS                   VALUE 00.
           88  RECORD-NOT-FOUND          VALUE 01.
           88  DUPLICATE-KEY             VALUE 02.
           88  FILE-ERROR                VALUE 03.
           88  INVALID-DATA              VALUE 04.

       01  WS-CURRENT-DATE.
           05  WS-CURRENT-YEAR           PIC 9(04).
           05  WS-CURRENT-MONTH          PIC 9(02).
           05  WS-CURRENT-DAY            PIC 9(02).

       01  WS-CURRENT-TIME.
           05  WS-CURRENT-HOUR           PIC 9(02).
           05  WS-CURRENT-MINUTE         PIC 9(02).
           05  WS-CURRENT-SECOND         PIC 9(02).
           05  WS-CURRENT-HUNDREDTH      PIC 9(02).

       01  WS-TIMESTAMP                  PIC X(26).

       LINKAGE SECTION.
       01  LS-OPERATION-CODE             PIC X(06).
       01  LS-ACCOUNT-DATA.
           COPY ACCOUNT.
       01  LS-RETURN-CODE                PIC 9(02).

       PROCEDURE DIVISION USING LS-OPERATION-CODE
                               LS-ACCOUNT-DATA
                               LS-RETURN-CODE.

       MAIN-PROCESS.
           MOVE LS-OPERATION-CODE TO WS-OPERATION
           
           EVALUATE TRUE
               WHEN CREATE-OPERATION
                   PERFORM CREATE-ACCOUNT-PROCESS
               WHEN READ-OPERATION
                   PERFORM READ-ACCOUNT-PROCESS
               WHEN UPDATE-OPERATION
                   PERFORM UPDATE-ACCOUNT-PROCESS
               WHEN DELETE-OPERATION
                   PERFORM DELETE-ACCOUNT-PROCESS
               WHEN INQUIRY-OPERATION
                   PERFORM INQUIRY-ACCOUNT-PROCESS
               WHEN OTHER
                   MOVE 04 TO WS-RETURN-CODE
           END-EVALUATE
           
           MOVE WS-RETURN-CODE TO LS-RETURN-CODE
           GOBACK.

       CREATE-ACCOUNT-PROCESS.
           OPEN I-O ACCOUNT-FILE
           IF WS-FILE-STATUS NOT = '00'
               MOVE 03 TO WS-RETURN-CODE
               GO TO CREATE-ACCOUNT-EXIT
           END-IF
           
           MOVE CORRESPONDING LS-ACCOUNT-DATA TO ACCOUNT-RECORD
           PERFORM GET-CURRENT-TIMESTAMP
           MOVE WS-TIMESTAMP TO CREATED-TIMESTAMP
           MOVE WS-TIMESTAMP TO UPDATED-TIMESTAMP
           
           WRITE ACCOUNT-RECORD
           EVALUATE WS-FILE-STATUS
               WHEN '00'
                   MOVE 00 TO WS-RETURN-CODE
                   PERFORM LOG-TRANSACTION
               WHEN '22'
                   MOVE 02 TO WS-RETURN-CODE
               WHEN OTHER
                   MOVE 03 TO WS-RETURN-CODE
           END-EVALUATE
           
           CLOSE ACCOUNT-FILE
           
       CREATE-ACCOUNT-EXIT.
           EXIT.

       READ-ACCOUNT-PROCESS.
           OPEN INPUT ACCOUNT-FILE
           IF WS-FILE-STATUS NOT = '00'
               MOVE 03 TO WS-RETURN-CODE
               GO TO READ-ACCOUNT-EXIT
           END-IF
           
           MOVE LS-ACCOUNT-DATA TO ACCOUNT-RECORD
           READ ACCOUNT-FILE
           EVALUATE WS-FILE-STATUS
               WHEN '00'
                   MOVE 00 TO WS-RETURN-CODE
                   MOVE CORRESPONDING ACCOUNT-RECORD TO LS-ACCOUNT-DATA
               WHEN '23'
                   MOVE 01 TO WS-RETURN-CODE
               WHEN OTHER
                   MOVE 03 TO WS-RETURN-CODE
           END-EVALUATE
           
           CLOSE ACCOUNT-FILE
           
       READ-ACCOUNT-EXIT.
           EXIT.

       UPDATE-ACCOUNT-PROCESS.
           OPEN I-O ACCOUNT-FILE
           IF WS-FILE-STATUS NOT = '00'
               MOVE 03 TO WS-RETURN-CODE
               GO TO UPDATE-ACCOUNT-EXIT
           END-IF
           
           MOVE LS-ACCOUNT-DATA TO ACCOUNT-RECORD
           READ ACCOUNT-FILE
           IF WS-FILE-STATUS = '00'
               MOVE CORRESPONDING LS-ACCOUNT-DATA TO ACCOUNT-RECORD
               PERFORM GET-CURRENT-TIMESTAMP
               MOVE WS-TIMESTAMP TO UPDATED-TIMESTAMP
               REWRITE ACCOUNT-RECORD
               IF WS-FILE-STATUS = '00'
                   MOVE 00 TO WS-RETURN-CODE
                   PERFORM LOG-TRANSACTION
               ELSE
                   MOVE 03 TO WS-RETURN-CODE
               END-IF
           ELSE
               MOVE 01 TO WS-RETURN-CODE
           END-IF
           
           CLOSE ACCOUNT-FILE
           
       UPDATE-ACCOUNT-EXIT.
           EXIT.

       DELETE-ACCOUNT-PROCESS.
           OPEN I-O ACCOUNT-FILE
           IF WS-FILE-STATUS NOT = '00'
               MOVE 03 TO WS-RETURN-CODE
               GO TO DELETE-ACCOUNT-EXIT
           END-IF
           
           MOVE LS-ACCOUNT-DATA TO ACCOUNT-RECORD
           READ ACCOUNT-FILE
           IF WS-FILE-STATUS = '00'
               DELETE ACCOUNT-FILE
               IF WS-FILE-STATUS = '00'
                   MOVE 00 TO WS-RETURN-CODE
                   PERFORM LOG-TRANSACTION
               ELSE
                   MOVE 03 TO WS-RETURN-CODE
               END-IF
           ELSE
               MOVE 01 TO WS-RETURN-CODE
           END-IF
           
           CLOSE ACCOUNT-FILE
           
       DELETE-ACCOUNT-EXIT.
           EXIT.

       INQUIRY-ACCOUNT-PROCESS.
           PERFORM READ-ACCOUNT-PROCESS.

       GET-CURRENT-TIMESTAMP.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CURRENT-TIME FROM TIME
           
           STRING WS-CURRENT-YEAR '-'
                  WS-CURRENT-MONTH '-'
                  WS-CURRENT-DAY 'T'
                  WS-CURRENT-HOUR ':'
                  WS-CURRENT-MINUTE ':'
                  WS-CURRENT-SECOND '.'
                  WS-CURRENT-HUNDREDTH
                  'Z'
                  DELIMITED BY SIZE
                  INTO WS-TIMESTAMP
           END-STRING.

       LOG-TRANSACTION.
           OPEN EXTEND TRANSACTION-LOG
           IF WS-LOG-STATUS = '00'
               STRING WS-TIMESTAMP ' '
                      WS-OPERATION ' '
                      ACCOUNT-ID ' '
                      'SUCCESS'
                      DELIMITED BY SIZE
                      INTO LOG-RECORD
               WRITE LOG-RECORD
               CLOSE TRANSACTION-LOG
           END-IF.

       END PROGRAM ACCTMGMT.
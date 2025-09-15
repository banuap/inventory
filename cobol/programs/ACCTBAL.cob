       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTBAL.
      ******************************************************************
      * PROGRAM: ACCTBAL - ACCOUNT BALANCE MANAGEMENT
      * VERSION: 1.0
      * DATE: 2024-09-15
      * PURPOSE: ACCOUNT BALANCE CALCULATION AND VALIDATION
      * FUNCTIONS: CALCULATE BALANCES, VALIDATE TRANSACTIONS
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO "ACCOUNT.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACCOUNT-ID
               FILE STATUS IS WS-FILE-STATUS.

           SELECT TRANSACTION-FILE ASSIGN TO "TRANS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS TRANSACTION-ID
               ALTERNATE RECORD KEY IS ACCOUNT-ID
               FILE STATUS IS WS-TRANS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       COPY ACCOUNT.

       FD  TRANSACTION-FILE.
       COPY TRANSACTION.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS                PIC XX.
       01  WS-TRANS-STATUS               PIC XX.
       
       01  WS-OPERATION                  PIC X(10).
           88  UPDATE-BALANCE            VALUE 'UPDBAL'.
           88  VALIDATE-TRANSACTION      VALUE 'VALIDATE'.
           88  CALCULATE-AVAILABLE       VALUE 'CALCAVAIL'.

       01  WS-RETURN-CODE                PIC 9(02).
           88  SUCCESS                   VALUE 00.
           88  INSUFFICIENT-FUNDS        VALUE 10.
           88  ACCOUNT-NOT-FOUND         VALUE 01.
           88  INVALID-TRANSACTION       VALUE 11.
           88  FILE-ERROR                VALUE 03.

       01  WS-CALCULATED-BALANCE         PIC S9(13)V99 COMP-3.
       01  WS-CALCULATED-AVAILABLE       PIC S9(13)V99 COMP-3.
       01  WS-TEMP-AMOUNT                PIC S9(13)V99 COMP-3.

       LINKAGE SECTION.
       01  LS-OPERATION-CODE             PIC X(10).
       01  LS-ACCOUNT-ID                 PIC X(12).
       01  LS-TRANSACTION-AMOUNT         PIC S9(13)V99 COMP-3.
       01  LS-TRANSACTION-TYPE           PIC X(03).
       01  LS-CALCULATED-BALANCE         PIC S9(13)V99 COMP-3.
       01  LS-RETURN-CODE                PIC 9(02).

       PROCEDURE DIVISION USING LS-OPERATION-CODE
                               LS-ACCOUNT-ID
                               LS-TRANSACTION-AMOUNT
                               LS-TRANSACTION-TYPE
                               LS-CALCULATED-BALANCE
                               LS-RETURN-CODE.

       MAIN-PROCESS.
           MOVE LS-OPERATION-CODE TO WS-OPERATION
           
           EVALUATE TRUE
               WHEN UPDATE-BALANCE
                   PERFORM UPDATE-ACCOUNT-BALANCE
               WHEN VALIDATE-TRANSACTION
                   PERFORM VALIDATE-TRANSACTION-FUNDS
               WHEN CALCULATE-AVAILABLE
                   PERFORM CALCULATE-AVAILABLE-BALANCE
               WHEN OTHER
                   MOVE 04 TO WS-RETURN-CODE
           END-EVALUATE
           
           MOVE WS-RETURN-CODE TO LS-RETURN-CODE
           MOVE WS-CALCULATED-BALANCE TO LS-CALCULATED-BALANCE
           GOBACK.

       UPDATE-ACCOUNT-BALANCE.
           OPEN I-O ACCOUNT-FILE
           IF WS-FILE-STATUS NOT = '00'
               MOVE 03 TO WS-RETURN-CODE
               GO TO UPDATE-BALANCE-EXIT
           END-IF
           
           MOVE LS-ACCOUNT-ID TO ACCOUNT-ID
           READ ACCOUNT-FILE
           IF WS-FILE-STATUS = '00'
               EVALUATE LS-TRANSACTION-TYPE
                   WHEN 'DEP'
                   WHEN 'DIV'
                   WHEN 'INT'
                       ADD LS-TRANSACTION-AMOUNT TO ACCOUNT-BALANCE
                       ADD LS-TRANSACTION-AMOUNT TO AVAILABLE-BALANCE
                   WHEN 'WTH'
                   WHEN 'FEE'
                       SUBTRACT LS-TRANSACTION-AMOUNT FROM ACCOUNT-BALANCE
                       SUBTRACT LS-TRANSACTION-AMOUNT FROM AVAILABLE-BALANCE
                   WHEN 'BUY'
                       SUBTRACT LS-TRANSACTION-AMOUNT FROM AVAILABLE-BALANCE
                   WHEN 'SEL'
                       ADD LS-TRANSACTION-AMOUNT TO AVAILABLE-BALANCE
                   WHEN OTHER
                       MOVE 11 TO WS-RETURN-CODE
                       GO TO UPDATE-BALANCE-EXIT
               END-EVALUATE
               
               MOVE ACCOUNT-BALANCE TO WS-CALCULATED-BALANCE
               REWRITE ACCOUNT-RECORD
               IF WS-FILE-STATUS = '00'
                   MOVE 00 TO WS-RETURN-CODE
               ELSE
                   MOVE 03 TO WS-RETURN-CODE
               END-IF
           ELSE
               MOVE 01 TO WS-RETURN-CODE
           END-IF
           
           CLOSE ACCOUNT-FILE
           
       UPDATE-BALANCE-EXIT.
           EXIT.

       VALIDATE-TRANSACTION-FUNDS.
           OPEN INPUT ACCOUNT-FILE
           IF WS-FILE-STATUS NOT = '00'
               MOVE 03 TO WS-RETURN-CODE
               GO TO VALIDATE-EXIT
           END-IF
           
           MOVE LS-ACCOUNT-ID TO ACCOUNT-ID
           READ ACCOUNT-FILE
           IF WS-FILE-STATUS = '00'
               EVALUATE LS-TRANSACTION-TYPE
                   WHEN 'WTH'
                   WHEN 'BUY'
                   WHEN 'FEE'
                       IF AVAILABLE-BALANCE < LS-TRANSACTION-AMOUNT
                           MOVE 10 TO WS-RETURN-CODE
                       ELSE
                           MOVE 00 TO WS-RETURN-CODE
                       END-IF
                   WHEN 'DEP'
                   WHEN 'SEL'
                   WHEN 'DIV'
                   WHEN 'INT'
                       MOVE 00 TO WS-RETURN-CODE
                   WHEN OTHER
                       MOVE 11 TO WS-RETURN-CODE
               END-EVALUATE
               MOVE AVAILABLE-BALANCE TO WS-CALCULATED-BALANCE
           ELSE
               MOVE 01 TO WS-RETURN-CODE
           END-IF
           
           CLOSE ACCOUNT-FILE
           
       VALIDATE-EXIT.
           EXIT.

       CALCULATE-AVAILABLE-BALANCE.
           OPEN INPUT ACCOUNT-FILE
           OPEN INPUT TRANSACTION-FILE
           
           IF WS-FILE-STATUS NOT = '00' OR WS-TRANS-STATUS NOT = '00'
               MOVE 03 TO WS-RETURN-CODE
               GO TO CALC-AVAILABLE-EXIT
           END-IF
           
           MOVE LS-ACCOUNT-ID TO ACCOUNT-ID
           READ ACCOUNT-FILE
           IF WS-FILE-STATUS = '00'
               MOVE ACCOUNT-BALANCE TO WS-CALCULATED-AVAILABLE
               
      *        SUBTRACT PENDING TRANSACTIONS
               MOVE LS-ACCOUNT-ID TO ACCOUNT-ID OF TRANSACTION-RECORD
               START TRANSACTION-FILE KEY >= ACCOUNT-ID OF TRANSACTION-RECORD
               
               PERFORM UNTIL WS-TRANS-STATUS NOT = '00'
                   READ TRANSACTION-FILE NEXT RECORD
                   IF WS-TRANS-STATUS = '00'
                       IF ACCOUNT-ID OF TRANSACTION-RECORD = LS-ACCOUNT-ID
                           IF PENDING-STATUS
                               EVALUATE TRANSACTION-TYPE
                                   WHEN 'BUY'
                                   WHEN 'WTH'
                                   WHEN 'FEE'
                                       SUBTRACT TRANSACTION-AMOUNT 
                                         FROM WS-CALCULATED-AVAILABLE
                               END-EVALUATE
                           END-IF
                       ELSE
                           MOVE '10' TO WS-TRANS-STATUS
                       END-IF
                   END-IF
               END-PERFORM
               
               MOVE WS-CALCULATED-AVAILABLE TO WS-CALCULATED-BALANCE
               MOVE 00 TO WS-RETURN-CODE
           ELSE
               MOVE 01 TO WS-RETURN-CODE
           END-IF
           
           CLOSE ACCOUNT-FILE
           CLOSE TRANSACTION-FILE
           
       CALC-AVAILABLE-EXIT.
           EXIT.

       END PROGRAM ACCTBAL.
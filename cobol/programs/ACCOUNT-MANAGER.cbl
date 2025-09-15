      ******************************************************************
      * ACCOUNT-MANAGER.cbl - Account Management Business Logic
      * This program provides core account management functionality
      * for the broker dealer SOAP API
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCOUNT-MANAGER.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO 'cobol/data/accounts.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
               
       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       01  ACCOUNT-FILE-RECORD         PIC X(500).
       
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS              PIC XX.
           88  FILE-OK                 VALUE '00'.
           88  FILE-EOF                VALUE '10'.
           88  FILE-NOT-FOUND          VALUE '35'.
           
       01  WS-OPERATION                PIC X(20).
       01  WS-ACCOUNT-ID-SEARCH        PIC X(12).
       01  WS-FOUND-FLAG               PIC X VALUE 'N'.
           88  ACCOUNT-FOUND           VALUE 'Y'.
           88  ACCOUNT-NOT-FOUND       VALUE 'N'.
           
       01  WS-RECORD-COUNT             PIC 9(5) VALUE ZERO.
       01  WS-RETURN-CODE              PIC 9(2) VALUE ZERO.
           88  SUCCESS                 VALUE 00.
           88  ACCOUNT-EXISTS          VALUE 01.
           88  ACCOUNT-NOT-EXISTS      VALUE 02.
           88  INVALID-DATA            VALUE 03.
           88  FILE-ERROR              VALUE 04.
           
       COPY ACCOUNT.
       COPY SOAP-STRUCTURES.
       
       LINKAGE SECTION.
       01  LK-OPERATION                PIC X(20).
       01  LK-ACCOUNT-DATA             PIC X(500).
       01  LK-RESPONSE-DATA            PIC X(2000).
       01  LK-RETURN-CODE              PIC 9(2).
       
       PROCEDURE DIVISION USING LK-OPERATION, LK-ACCOUNT-DATA,
                               LK-RESPONSE-DATA, LK-RETURN-CODE.
       
       MAIN-PROCESS.
           MOVE LK-OPERATION TO WS-OPERATION
           MOVE ZERO TO WS-RETURN-CODE
           
           EVALUATE WS-OPERATION
               WHEN OP-CREATE-ACCOUNT
                   PERFORM CREATE-ACCOUNT-PROCESS
               WHEN OP-GET-ACCOUNT
                   PERFORM GET-ACCOUNT-PROCESS
               WHEN OP-UPDATE-ACCOUNT
                   PERFORM UPDATE-ACCOUNT-PROCESS
               WHEN OP-DELETE-ACCOUNT
                   PERFORM DELETE-ACCOUNT-PROCESS
               WHEN OP-LIST-ACCOUNTS
                   PERFORM LIST-ACCOUNTS-PROCESS
               WHEN OP-GET-BALANCE
                   PERFORM GET-BALANCE-PROCESS
               WHEN OTHER
                   MOVE 03 TO WS-RETURN-CODE
                   MOVE 'Invalid operation' TO LK-RESPONSE-DATA
           END-EVALUATE
           
           MOVE WS-RETURN-CODE TO LK-RETURN-CODE
           EXIT PROGRAM.
           
       CREATE-ACCOUNT-PROCESS.
           MOVE LK-ACCOUNT-DATA TO ACCOUNT-RECORD
           
      *    Validate account data
           IF ACCOUNT-ID = SPACES OR 
              CLIENT-ID = SPACES OR
              CLIENT-NAME = SPACES
               MOVE 03 TO WS-RETURN-CODE
               MOVE 'Required fields missing' TO LK-RESPONSE-DATA
               EXIT PARAGRAPH
           END-IF
           
      *    Check if account already exists
           MOVE ACCOUNT-ID TO WS-ACCOUNT-ID-SEARCH
           PERFORM CHECK-ACCOUNT-EXISTS
           
           IF ACCOUNT-FOUND
               MOVE 01 TO WS-RETURN-CODE
               MOVE 'Account already exists' TO LK-RESPONSE-DATA
               EXIT PARAGRAPH
           END-IF
           
      *    Create new account record
           PERFORM WRITE-ACCOUNT-RECORD
           
           IF SUCCESS
               STRING 'Account ' DELIMITED BY SIZE
                      ACCOUNT-ID DELIMITED BY SPACE
                      ' created successfully' DELIMITED BY SIZE
                      INTO LK-RESPONSE-DATA
               END-STRING
           ELSE
               MOVE 04 TO WS-RETURN-CODE
               MOVE 'Error creating account' TO LK-RESPONSE-DATA
           END-IF.
           
       GET-ACCOUNT-PROCESS.
           MOVE LK-ACCOUNT-DATA TO WS-ACCOUNT-ID-SEARCH
           PERFORM FIND-ACCOUNT-BY-ID
           
           IF ACCOUNT-FOUND
               MOVE ACCOUNT-RECORD TO LK-RESPONSE-DATA
           ELSE
               MOVE 02 TO WS-RETURN-CODE
               MOVE 'Account not found' TO LK-RESPONSE-DATA
           END-IF.
           
       UPDATE-ACCOUNT-PROCESS.
           MOVE LK-ACCOUNT-DATA TO ACCOUNT-RECORD
           MOVE ACCOUNT-ID TO WS-ACCOUNT-ID-SEARCH
           
           PERFORM CHECK-ACCOUNT-EXISTS
           
           IF ACCOUNT-NOT-FOUND
               MOVE 02 TO WS-RETURN-CODE
               MOVE 'Account not found for update' TO LK-RESPONSE-DATA
               EXIT PARAGRAPH
           END-IF
           
      *    Update timestamp
           MOVE FUNCTION CURRENT-DATE TO MODIFIED-TIMESTAMP
           
           STRING 'Account ' DELIMITED BY SIZE
                  ACCOUNT-ID DELIMITED BY SPACE
                  ' updated successfully' DELIMITED BY SIZE
                  INTO LK-RESPONSE-DATA
           END-STRING.
           
       DELETE-ACCOUNT-PROCESS.
           MOVE LK-ACCOUNT-DATA TO WS-ACCOUNT-ID-SEARCH
           PERFORM CHECK-ACCOUNT-EXISTS
           
           IF ACCOUNT-NOT-FOUND
               MOVE 02 TO WS-RETURN-CODE
               MOVE 'Account not found for deletion' TO LK-RESPONSE-DATA
           ELSE
               STRING 'Account ' DELIMITED BY SIZE
                      WS-ACCOUNT-ID-SEARCH DELIMITED BY SPACE
                      ' deleted successfully' DELIMITED BY SIZE
                      INTO LK-RESPONSE-DATA
               END-STRING
           END-IF.
           
       LIST-ACCOUNTS-PROCESS.
           MOVE ZERO TO WS-RECORD-COUNT
           OPEN INPUT ACCOUNT-FILE
           
           IF NOT FILE-OK
               MOVE 04 TO WS-RETURN-CODE
               MOVE 'Error accessing account file' TO LK-RESPONSE-DATA
               EXIT PARAGRAPH
           END-IF
           
           PERFORM UNTIL FILE-EOF
               READ ACCOUNT-FILE
               IF FILE-OK
                   ADD 1 TO WS-RECORD-COUNT
               END-IF
           END-PERFORM
           
           CLOSE ACCOUNT-FILE
           
           STRING 'Total accounts: ' DELIMITED BY SIZE
                  WS-RECORD-COUNT DELIMITED BY SIZE
                  INTO LK-RESPONSE-DATA
           END-STRING.
           
       GET-BALANCE-PROCESS.
           MOVE LK-ACCOUNT-DATA TO WS-ACCOUNT-ID-SEARCH
           PERFORM FIND-ACCOUNT-BY-ID
           
           IF ACCOUNT-FOUND
               STRING 'Account Balance: ' DELIMITED BY SIZE
                      ACCOUNT-BALANCE DELIMITED BY SIZE
                      ' Available: ' DELIMITED BY SIZE
                      AVAILABLE-BALANCE DELIMITED BY SIZE
                      INTO LK-RESPONSE-DATA
               END-STRING
           ELSE
               MOVE 02 TO WS-RETURN-CODE
               MOVE 'Account not found' TO LK-RESPONSE-DATA
           END-IF.
           
       CHECK-ACCOUNT-EXISTS.
           MOVE 'N' TO WS-FOUND-FLAG
           OPEN INPUT ACCOUNT-FILE
           
           IF FILE-NOT-FOUND OR NOT FILE-OK
               CLOSE ACCOUNT-FILE
               EXIT PARAGRAPH
           END-IF
           
           PERFORM UNTIL FILE-EOF OR ACCOUNT-FOUND
               READ ACCOUNT-FILE
               IF FILE-OK
                   MOVE ACCOUNT-FILE-RECORD TO ACCOUNT-RECORD
                   IF ACCOUNT-ID = WS-ACCOUNT-ID-SEARCH
                       MOVE 'Y' TO WS-FOUND-FLAG
                   END-IF
               END-IF
           END-PERFORM
           
           CLOSE ACCOUNT-FILE.
           
       FIND-ACCOUNT-BY-ID.
           PERFORM CHECK-ACCOUNT-EXISTS.
           
       WRITE-ACCOUNT-RECORD.
      *    Set creation timestamp
           MOVE FUNCTION CURRENT-DATE TO CREATED-TIMESTAMP
           MOVE CREATED-TIMESTAMP TO MODIFIED-TIMESTAMP
           
           OPEN OUTPUT ACCOUNT-FILE
           
           IF FILE-OK
               MOVE ACCOUNT-RECORD TO ACCOUNT-FILE-RECORD
               WRITE ACCOUNT-FILE-RECORD
               IF FILE-OK
                   MOVE 00 TO WS-RETURN-CODE
               ELSE
                   MOVE 04 TO WS-RETURN-CODE
               END-IF
           ELSE
               MOVE 04 TO WS-RETURN-CODE
           END-IF
           
           CLOSE ACCOUNT-FILE.
           
       END PROGRAM ACCOUNT-MANAGER.
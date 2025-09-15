      ******************************************************************
      * SIMPLE-ACCOUNT-DEMO.cbl - Simple Account Demo Program
      * This program demonstrates basic account management functionality
      * for the broker dealer SOAP API
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLE-ACCOUNT-DEMO.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ACCOUNT-ID               PIC X(12) VALUE 'ACC001'.
       01  WS-CLIENT-NAME              PIC X(50) VALUE 'John Doe'.
       01  WS-ACCOUNT-TYPE             PIC X(10) VALUE 'INDIVIDUAL'.
       01  WS-BALANCE                  PIC 9(7)V99 VALUE 10000.00.
       01  WS-STATUS                   PIC X(8) VALUE 'ACTIVE'.
       
       COPY ACCOUNT.
       
       PROCEDURE DIVISION.
       
       MAIN-PROCESS.
           DISPLAY 'COBOL Account Management Demo'
           DISPLAY '============================='
           
           PERFORM INIT-ACCOUNT-RECORD
           PERFORM DISPLAY-ACCOUNT-INFO
           
           DISPLAY 'Demo completed successfully!'
           STOP RUN.
           
       INIT-ACCOUNT-RECORD.
           MOVE WS-ACCOUNT-ID TO ACCOUNT-ID
           MOVE WS-CLIENT-NAME TO CLIENT-NAME
           MOVE WS-ACCOUNT-TYPE TO ACCOUNT-TYPE
           MOVE WS-BALANCE TO ACCOUNT-BALANCE
           MOVE WS-STATUS TO ACCOUNT-STATUS
           MOVE 'USD' TO ACCOUNT-CURRENCY
           MOVE '2024-01-15' TO OPEN-DATE
           MOVE 'John Smith' TO ACCOUNT-MANAGER
           MOVE 'ADMIN' TO CREATED-BY
           MOVE FUNCTION CURRENT-DATE TO CREATED-TIMESTAMP.
           
       DISPLAY-ACCOUNT-INFO.
           DISPLAY 'Account Information:'
           DISPLAY '  Account ID: ' ACCOUNT-ID
           DISPLAY '  Client Name: ' CLIENT-NAME
           DISPLAY '  Account Type: ' ACCOUNT-TYPE
           DISPLAY '  Balance: ' ACCOUNT-BALANCE
           DISPLAY '  Currency: ' ACCOUNT-CURRENCY
           DISPLAY '  Status: ' ACCOUNT-STATUS
           DISPLAY '  Account Manager: ' ACCOUNT-MANAGER
           DISPLAY '  Created By: ' CREATED-BY
           DISPLAY '  Created: ' CREATED-TIMESTAMP.
           
       END PROGRAM SIMPLE-ACCOUNT-DEMO.
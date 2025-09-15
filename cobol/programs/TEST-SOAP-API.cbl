      ******************************************************************
      * TEST-SOAP-API.cbl - Test Program for COBOL SOAP API
      * This program demonstrates the account management SOAP API
      * functionality for broker dealer operations
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-SOAP-API.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TEST-COUNTER             PIC 9(2) VALUE 1.
       01  WS-PASSED-TESTS             PIC 9(2) VALUE ZERO.
       01  WS-FAILED-TESTS             PIC 9(2) VALUE ZERO.
       01  WS-TEST-RESULT              PIC X(10).
       
       01  WS-SOAP-REQUEST             PIC X(4000).
       01  WS-SOAP-RESPONSE            PIC X(4000).
       
       01  WS-CREATE-ACCOUNT-REQUEST.
           05  FILLER                  PIC X(39) VALUE
               '<?xml version="1.0" encoding="UTF-8"?>'.
           05  FILLER                  PIC X(83) VALUE
               '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">'.
           05  FILLER                  PIC X(11) VALUE
               '<soap:Body>'.
           05  FILLER                  PIC X(25) VALUE
               '<AccountServiceRequest>'.
           05  FILLER                  PIC X(28) VALUE
               '<operation>CREATE_ACCOUNT'.
           05  FILLER                  PIC X(11) VALUE
               '</operation>'.
           05  FILLER                  PIC X(20) VALUE
               '<accountId>ACC001'.
           05  FILLER                  PIC X(12) VALUE
               '</accountId>'.
           05  FILLER                  PIC X(19) VALUE
               '<clientId>CLI001'.
           05  FILLER                  PIC X(11) VALUE
               '</clientId>'.
           05  FILLER                  PIC X(27) VALUE
               '<clientName>John Doe'.
           05  FILLER                  PIC X(13) VALUE
               '</clientName>'.
           05  FILLER                  PIC X(26) VALUE
               '<accountType>INDIVIDUAL'.
           05  FILLER                  PIC X(14) VALUE
               '</accountType>'.
           05  FILLER                  PIC X(26) VALUE
               '</AccountServiceRequest>'.
           05  FILLER                  PIC X(12) VALUE
               '</soap:Body>'.
           05  FILLER                  PIC X(16) VALUE
               '</soap:Envelope>'.
               
       01  WS-GET-ACCOUNT-REQUEST.
           05  FILLER                  PIC X(39) VALUE
               '<?xml version="1.0" encoding="UTF-8"?>'.
           05  FILLER                  PIC X(83) VALUE
               '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">'.
           05  FILLER                  PIC X(11) VALUE
               '<soap:Body>'.
           05  FILLER                  PIC X(25) VALUE
               '<AccountServiceRequest>'.
           05  FILLER                  PIC X(25) VALUE
               '<operation>GET_ACCOUNT'.
           05  FILLER                  PIC X(11) VALUE
               '</operation>'.
           05  FILLER                  PIC X(20) VALUE
               '<accountId>ACC001'.
           05  FILLER                  PIC X(12) VALUE
               '</accountId>'.
           05  FILLER                  PIC X(26) VALUE
               '</AccountServiceRequest>'.
           05  FILLER                  PIC X(12) VALUE
               '</soap:Body>'.
           05  FILLER                  PIC X(16) VALUE
               '</soap:Envelope>'.
       
       PROCEDURE DIVISION.
       
       MAIN-PROCESS.
           DISPLAY 'Starting COBOL SOAP API Tests...'
           DISPLAY '================================='
           
           PERFORM TEST-CREATE-ACCOUNT
           PERFORM TEST-GET-ACCOUNT
           PERFORM TEST-LIST-ACCOUNTS
           
           PERFORM DISPLAY-TEST-SUMMARY
           STOP RUN.
           
       TEST-CREATE-ACCOUNT.
           DISPLAY 'Test ' WS-TEST-COUNTER ': Create Account'
           
           MOVE WS-CREATE-ACCOUNT-REQUEST TO WS-SOAP-REQUEST
           
           CALL 'SOAP-SERVER' USING WS-SOAP-REQUEST, WS-SOAP-RESPONSE
           
           IF WS-SOAP-RESPONSE CONTAINS 'SUCCESS'
               MOVE 'PASSED' TO WS-TEST-RESULT
               ADD 1 TO WS-PASSED-TESTS
           ELSE
               MOVE 'FAILED' TO WS-TEST-RESULT
               ADD 1 TO WS-FAILED-TESTS
           END-IF
           
           DISPLAY '  Result: ' WS-TEST-RESULT
           DISPLAY '  Response: ' WS-SOAP-RESPONSE(1:100)
           ADD 1 TO WS-TEST-COUNTER.
           
       TEST-GET-ACCOUNT.
           DISPLAY 'Test ' WS-TEST-COUNTER ': Get Account'
           
           MOVE WS-GET-ACCOUNT-REQUEST TO WS-SOAP-REQUEST
           
           CALL 'SOAP-SERVER' USING WS-SOAP-REQUEST, WS-SOAP-RESPONSE
           
           IF WS-SOAP-RESPONSE CONTAINS 'SUCCESS' OR
              WS-SOAP-RESPONSE CONTAINS 'Account not found'
               MOVE 'PASSED' TO WS-TEST-RESULT
               ADD 1 TO WS-PASSED-TESTS
           ELSE
               MOVE 'FAILED' TO WS-TEST-RESULT
               ADD 1 TO WS-FAILED-TESTS
           END-IF
           
           DISPLAY '  Result: ' WS-TEST-RESULT
           DISPLAY '  Response: ' WS-SOAP-RESPONSE(1:100)
           ADD 1 TO WS-TEST-COUNTER.
           
       TEST-LIST-ACCOUNTS.
           DISPLAY 'Test ' WS-TEST-COUNTER ': List Accounts'
           
           STRING '<?xml version="1.0" encoding="UTF-8"?>'
                  '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">'
                  '<soap:Body>'
                  '<AccountServiceRequest>'
                  '<operation>LIST_ACCOUNTS</operation>'
                  '</AccountServiceRequest>'
                  '</soap:Body>'
                  '</soap:Envelope>'
                  DELIMITED BY SIZE
                  INTO WS-SOAP-REQUEST
           END-STRING
           
           CALL 'SOAP-SERVER' USING WS-SOAP-REQUEST, WS-SOAP-RESPONSE
           
           IF WS-SOAP-RESPONSE CONTAINS 'SUCCESS' OR
              WS-SOAP-RESPONSE CONTAINS 'Total accounts'
               MOVE 'PASSED' TO WS-TEST-RESULT
               ADD 1 TO WS-PASSED-TESTS
           ELSE
               MOVE 'FAILED' TO WS-TEST-RESULT
               ADD 1 TO WS-FAILED-TESTS
           END-IF
           
           DISPLAY '  Result: ' WS-TEST-RESULT
           DISPLAY '  Response: ' WS-SOAP-RESPONSE(1:100)
           ADD 1 TO WS-TEST-COUNTER.
           
       DISPLAY-TEST-SUMMARY.
           DISPLAY '================================='
           DISPLAY 'Test Summary:'
           DISPLAY '  Passed: ' WS-PASSED-TESTS
           DISPLAY '  Failed: ' WS-FAILED-TESTS
           COMPUTE WS-TEST-COUNTER = WS-PASSED-TESTS + WS-FAILED-TESTS
           DISPLAY '  Total:  ' WS-TEST-COUNTER
           
           IF WS-FAILED-TESTS = ZERO
               DISPLAY 'All tests passed successfully!'
           ELSE
               DISPLAY 'Some tests failed. Check the output above.'
           END-IF.
           
       END PROGRAM TEST-SOAP-API.
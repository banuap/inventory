      ******************************************************************
      * SOAP-STRUCTURES.cpy - SOAP Message Structures
      * This copybook defines SOAP request and response structures
      * for the account management web service.
      ******************************************************************
       01  SOAP-REQUEST.
           05  SOAP-ENVELOPE.
               10  SOAP-HEADER           PIC X(200).
               10  SOAP-BODY.
                   15  OPERATION-TYPE    PIC X(20).
                   15  REQUEST-DATA      PIC X(2000).
           
       01  SOAP-RESPONSE.
           05  SOAP-ENVELOPE-RESP.
               10  SOAP-HEADER-RESP      PIC X(200).
               10  SOAP-BODY-RESP.
                   15  RESPONSE-STATUS   PIC X(10).
                   15  RESPONSE-MESSAGE  PIC X(100).
                   15  RESPONSE-DATA     PIC X(2000).
                   15  ERROR-CODE        PIC X(10).
                   15  ERROR-DESCRIPTION PIC X(200).

       01  ACCOUNT-OPERATIONS.
           05  OP-CREATE-ACCOUNT         PIC X(20) VALUE 
               'CREATE_ACCOUNT'.
           05  OP-GET-ACCOUNT           PIC X(20) VALUE 
               'GET_ACCOUNT'.
           05  OP-UPDATE-ACCOUNT        PIC X(20) VALUE 
               'UPDATE_ACCOUNT'.
           05  OP-DELETE-ACCOUNT        PIC X(20) VALUE 
               'DELETE_ACCOUNT'.
           05  OP-LIST-ACCOUNTS         PIC X(20) VALUE 
               'LIST_ACCOUNTS'.
           05  OP-GET-BALANCE           PIC X(20) VALUE 
               'GET_BALANCE'.
           
       01  STATUS-CODES.
           05  STATUS-SUCCESS           PIC X(10) VALUE 
               'SUCCESS'.
           05  STATUS-ERROR             PIC X(10) VALUE 
               'ERROR'.
           05  STATUS-NOT-FOUND         PIC X(10) VALUE 
               'NOT_FOUND'.
           05  STATUS-INVALID           PIC X(10) VALUE 
               'INVALID'.
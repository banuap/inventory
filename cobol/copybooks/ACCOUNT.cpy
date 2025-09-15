      ******************************************************************
      * ACCOUNT.cpy - Account Record Structure for Broker Dealer
      * This copybook defines the account data structure used in 
      * the SOAP API for account management operations.
      ******************************************************************
       01  ACCOUNT-RECORD.
           05  ACCOUNT-ID                PIC X(12).
           05  ACCOUNT-TYPE              PIC X(10).
           05  ACCOUNT-STATUS            PIC X(8).
           05  CLIENT-ID                 PIC X(12).
           05  CLIENT-NAME               PIC X(50).
           05  ACCOUNT-BALANCE           PIC S9(13)V99 COMP-3.
           05  AVAILABLE-BALANCE         PIC S9(13)V99 COMP-3.
           05  MARGIN-BALANCE            PIC S9(13)V99 COMP-3.
           05  ACCOUNT-CURRENCY          PIC X(3).
           05  OPEN-DATE                 PIC X(10).
           05  LAST-TRANSACTION-DATE     PIC X(10).
           05  RISK-RATING               PIC X(5).
           05  ACCOUNT-MANAGER           PIC X(30).
           05  COMMISSION-RATE           PIC 9(3)V99 COMP-3.
           05  INTEREST-RATE             PIC 9(3)V999 COMP-3.
           05  CREATED-TIMESTAMP         PIC X(19).
           05  MODIFIED-TIMESTAMP        PIC X(19).
           05  CREATED-BY                PIC X(20).
           05  MODIFIED-BY               PIC X(20).
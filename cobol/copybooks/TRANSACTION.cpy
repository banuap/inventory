      ******************************************************************
      * TRANSACTION COPYBOOK - BROKER-DEALER TRANSACTION MANAGEMENT
      * VERSION: 1.0
      * DATE: 2024-09-15
      * PURPOSE: DEFINE TRANSACTION RECORD STRUCTURE
      ******************************************************************
       01  TRANSACTION-RECORD.
           05  TRANSACTION-ID            PIC X(16).
           05  ACCOUNT-ID                PIC X(12).
           05  TRANSACTION-TYPE          PIC X(03).
               88  DEPOSIT-TRANS         VALUE 'DEP'.
               88  WITHDRAWAL-TRANS      VALUE 'WTH'.
               88  TRANSFER-TRANS        VALUE 'TRF'.
               88  BUY-TRANS             VALUE 'BUY'.
               88  SELL-TRANS            VALUE 'SEL'.
               88  DIVIDEND-TRANS        VALUE 'DIV'.
               88  INTEREST-TRANS        VALUE 'INT'.
               88  FEE-TRANS             VALUE 'FEE'.
           05  TRANSACTION-AMOUNT        PIC S9(13)V99 COMP-3.
           05  TRANSACTION-DATE          PIC X(08).
           05  SETTLEMENT-DATE           PIC X(08).
           05  SECURITY-SYMBOL           PIC X(12).
           05  QUANTITY                  PIC S9(09)V999 COMP-3.
           05  PRICE                     PIC S9(09)V9999 COMP-3.
           05  COMMISSION                PIC S9(07)V99 COMP-3.
           05  FEES                      PIC S9(07)V99 COMP-3.
           05  NET-AMOUNT                PIC S9(13)V99 COMP-3.
           05  COUNTERPARTY-ID           PIC X(10).
           05  TRADE-REF-NUMBER          PIC X(20).
           05  EXECUTION-TIME            PIC X(08).
           05  ORDER-ID                  PIC X(16).
           05  BROKER-ID                 PIC X(08).
           05  CLEARING-FIRM             PIC X(08).
           05  TRANSACTION-STATUS        PIC X(01).
               88  PENDING-STATUS        VALUE 'P'.
               88  SETTLED-STATUS        VALUE 'S'.
               88  FAILED-STATUS         VALUE 'F'.
               88  CANCELLED-STATUS      VALUE 'C'.
           05  CREATED-TIMESTAMP         PIC X(26).
           05  UPDATED-TIMESTAMP         PIC X(26).
           05  CREATED-BY                PIC X(08).
           05  UPDATED-BY                PIC X(08).
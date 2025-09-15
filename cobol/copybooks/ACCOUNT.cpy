      ******************************************************************
      * ACCOUNT COPYBOOK - BROKER-DEALER ACCOUNT MANAGEMENT
      * VERSION: 1.0
      * DATE: 2024-09-15
      * PURPOSE: DEFINE ACCOUNT RECORD STRUCTURE FOR BROKER-DEALER
      ******************************************************************
       01  ACCOUNT-RECORD.
           05  ACCOUNT-ID                PIC X(12).
           05  ACCOUNT-TYPE              PIC X(02).
               88  CASH-ACCOUNT          VALUE 'CA'.
               88  MARGIN-ACCOUNT        VALUE 'MA'.
               88  IRA-ACCOUNT           VALUE 'IR'.
               88  TRUST-ACCOUNT         VALUE 'TR'.
           05  CUSTOMER-ID               PIC X(10).
           05  ACCOUNT-NAME              PIC X(50).
           05  ACCOUNT-STATUS            PIC X(01).
               88  ACTIVE-ACCOUNT        VALUE 'A'.
               88  INACTIVE-ACCOUNT      VALUE 'I'.
               88  CLOSED-ACCOUNT        VALUE 'C'.
               88  SUSPENDED-ACCOUNT     VALUE 'S'.
           05  ACCOUNT-BALANCE           PIC S9(13)V99 COMP-3.
           05  AVAILABLE-BALANCE         PIC S9(13)V99 COMP-3.
           05  MARGIN-BALANCE            PIC S9(13)V99 COMP-3.
           05  OPEN-DATE                 PIC X(08).
           05  CLOSE-DATE                PIC X(08).
           05  LAST-ACTIVITY-DATE        PIC X(08).
           05  BRANCH-CODE               PIC X(04).
           05  ACCOUNT-OFFICER           PIC X(08).
           05  INTEREST-RATE             PIC 9(03)V99 COMP-3.
           05  MAINTENANCE-FEE           PIC 9(07)V99 COMP-3.
           05  TAX-ID                    PIC X(11).
           05  REGULATORY-FLAGS          PIC X(10).
           05  CREATED-TIMESTAMP         PIC X(26).
           05  UPDATED-TIMESTAMP         PIC X(26).
           05  CREATED-BY                PIC X(08).
           05  UPDATED-BY                PIC X(08).
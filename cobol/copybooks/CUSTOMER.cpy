      ******************************************************************
      * CUSTOMER COPYBOOK - BROKER-DEALER CUSTOMER MANAGEMENT
      * VERSION: 1.0
      * DATE: 2024-09-15
      * PURPOSE: DEFINE CUSTOMER RECORD STRUCTURE
      ******************************************************************
       01  CUSTOMER-RECORD.
           05  CUSTOMER-ID               PIC X(10).
           05  CUSTOMER-TYPE             PIC X(01).
               88  INDIVIDUAL-CUSTOMER   VALUE 'I'.
               88  CORPORATE-CUSTOMER    VALUE 'C'.
               88  INSTITUTIONAL-CUSTOMER VALUE 'N'.
           05  FIRST-NAME                PIC X(25).
           05  MIDDLE-NAME               PIC X(25).
           05  LAST-NAME                 PIC X(35).
           05  COMPANY-NAME              PIC X(50).
           05  DATE-OF-BIRTH             PIC X(08).
           05  SSN-EIN                   PIC X(11).
           05  ADDRESS-LINE1             PIC X(40).
           05  ADDRESS-LINE2             PIC X(40).
           05  CITY                      PIC X(30).
           05  STATE                     PIC X(02).
           05  ZIP-CODE                  PIC X(10).
           05  COUNTRY                   PIC X(03).
           05  PHONE-PRIMARY             PIC X(15).
           05  PHONE-SECONDARY           PIC X(15).
           05  EMAIL-ADDRESS             PIC X(100).
           05  EMPLOYMENT-STATUS         PIC X(01).
               88  EMPLOYED              VALUE 'E'.
               88  UNEMPLOYED            VALUE 'U'.
               88  RETIRED               VALUE 'R'.
               88  STUDENT               VALUE 'S'.
           05  ANNUAL-INCOME             PIC S9(11)V99 COMP-3.
           05  NET-WORTH                 PIC S9(13)V99 COMP-3.
           05  INVESTMENT-OBJECTIVE      PIC X(02).
               88  GROWTH-OBJECTIVE      VALUE 'GR'.
               88  INCOME-OBJECTIVE      VALUE 'IN'.
               88  BALANCED-OBJECTIVE    VALUE 'BA'.
               88  SPECULATION-OBJECTIVE VALUE 'SP'.
           05  RISK-TOLERANCE            PIC X(01).
               88  LOW-RISK              VALUE 'L'.
               88  MEDIUM-RISK           VALUE 'M'.
               88  HIGH-RISK             VALUE 'H'.
           05  CUSTOMER-STATUS           PIC X(01).
               88  ACTIVE-CUSTOMER       VALUE 'A'.
               88  INACTIVE-CUSTOMER     VALUE 'I'.
               88  CLOSED-CUSTOMER       VALUE 'C'.
           05  KYC-STATUS                PIC X(01).
               88  KYC-COMPLETE          VALUE 'C'.
               88  KYC-PENDING           VALUE 'P'.
               88  KYC-EXPIRED           VALUE 'E'.
           05  AML-RISK-RATING           PIC X(01).
               88  LOW-AML-RISK          VALUE 'L'.
               88  MEDIUM-AML-RISK       VALUE 'M'.
               88  HIGH-AML-RISK         VALUE 'H'.
           05  CREATED-TIMESTAMP         PIC X(26).
           05  UPDATED-TIMESTAMP         PIC X(26).
           05  CREATED-BY                PIC X(08).
           05  UPDATED-BY                PIC X(08).
# COBOL Broker-Dealer Account Management System

## Overview

This COBOL-based account management system provides comprehensive functionality for broker-dealer operations, including account creation, balance management, transaction processing, and regulatory compliance features.

## Architecture

The system consists of four main components:

1. **COBOL Copybooks** - Data structure definitions
2. **COBOL Programs** - Core business logic
3. **JCL Scripts** - Batch processing jobs
4. **SOAP API** - Web service interface

## Components

### 1. Copybooks (`cobol/copybooks/`)

#### ACCOUNT.cpy
Defines the account record structure with the following key fields:
- Account ID (12 characters)
- Account Type (Cash, Margin, IRA, Trust)
- Customer ID
- Account Name
- Account Status (Active, Inactive, Closed, Suspended)
- Balance information (Account, Available, Margin)
- Dates (Open, Close, Last Activity)
- Administrative fields (Branch, Officer, Rates, Fees)
- Audit fields (Created/Updated timestamps and users)

#### TRANSACTION.cpy
Defines the transaction record structure for:
- Transaction identification and linking
- Transaction types (Deposit, Withdrawal, Buy, Sell, etc.)
- Financial details (Amount, Price, Commission, Fees)
- Settlement information
- Trade execution details
- Status tracking

#### CUSTOMER.cpy
Defines the customer record structure including:
- Customer identification and classification
- Personal/Corporate information
- Contact details
- Financial profile (Income, Net Worth)
- Investment preferences and risk tolerance
- Regulatory compliance (KYC, AML)

### 2. COBOL Programs (`cobol/programs/`)

#### ACCTMGMT.cob
Main account management program providing:
- **CREATE**: Create new accounts with validation
- **READ**: Retrieve account information
- **UPDATE**: Modify account details
- **DELETE**: Remove accounts (with audit trail)
- **INQUIRY**: Query account status

Features:
- Indexed file access for performance
- Comprehensive error handling
- Transaction logging
- Timestamp management

#### ACCTBAL.cob
Account balance management program providing:
- **UPDATE-BALANCE**: Update account balances based on transactions
- **VALIDATE-TRANSACTION**: Check sufficient funds before processing
- **CALCULATE-AVAILABLE**: Calculate available balance considering pending transactions

Features:
- Real-time balance calculations
- Pending transaction consideration
- Margin account support
- Funds validation logic

### 3. JCL Scripts (`cobol/jcl/`)

#### ACCTDLY.jcl
Daily account processing job that performs:
1. **Backup**: Create daily backup of account files
2. **Balance Update**: Process end-of-day balance calculations
3. **Regulatory Reports**: Generate required compliance reports
4. **Reconciliation**: Reconcile accounts with clearing positions
5. **Cleanup**: Remove temporary files

#### ACCTCRE.jcl
New account creation job that performs:
1. **Validation**: Verify customer information and KYC/AML status
2. **Creation**: Create new account record
3. **Initialization**: Set up initial balances
4. **Documentation**: Generate account opening forms
5. **Notification**: Send confirmation to customer

### 4. SOAP API (`cobol/soap-api/`)

#### account_soap_service.py
Python-based SOAP web service that provides:
- RESTful interface to COBOL programs
- XML/SOAP protocol support
- Complex data type definitions
- Error handling and fault management

**Available Operations:**
- `get_account(account_id)` - Retrieve account information
- `create_account(account_data)` - Create new account
- `update_account(account_data)` - Update existing account
- `delete_account(account_id)` - Delete account
- `get_account_balance(account_id)` - Get current balance
- `validate_transaction(account_id, amount, type)` - Validate transaction

#### client_example.py
Sample SOAP client demonstrating:
- Service connection and authentication
- Request/response handling
- Error management
- Usage examples for all operations

## Data Flow

1. **Account Creation**:
   - Customer validation (KYC/AML)
   - Account record creation via ACCTMGMT
   - Balance initialization via ACCTBAL
   - Documentation generation
   - Notification processing

2. **Transaction Processing**:
   - Transaction validation via ACCTBAL
   - Balance updates
   - Audit trail creation
   - Regulatory reporting

3. **Daily Processing**:
   - End-of-day balance calculations
   - Position reconciliation
   - Report generation
   - File maintenance

## Security Features

- Audit trail for all operations
- User authentication and authorization
- Data encryption in transit (SOAP/TLS)
- Regulatory compliance tracking
- Access logging

## Regulatory Compliance

- **KYC (Know Your Customer)** validation
- **AML (Anti-Money Laundering)** risk assessment
- **Audit trail** for all account changes
- **Regulatory reporting** capabilities
- **Data retention** policies

## Installation and Setup

### Prerequisites
- COBOL compiler (IBM Enterprise COBOL or GnuCOBOL)
- Python 3.8+ for SOAP API
- Required Python packages (see requirements.txt)

### Setup Steps

1. **Compile COBOL Programs**:
   ```bash
   # Example with GnuCOBOL
   cobc -x -o ACCTMGMT cobol/programs/ACCTMGMT.cob
   cobc -x -o ACCTBAL cobol/programs/ACCTBAL.cob
   ```

2. **Setup Data Files**:
   ```bash
   # Create indexed account file
   dd if=/dev/null of=ACCOUNT.DAT bs=1 count=0
   # Create transaction log
   touch TRANSLOG.DAT
   ```

3. **Install SOAP API Dependencies**:
   ```bash
   cd cobol/soap-api
   pip install -r requirements.txt
   ```

4. **Start SOAP Service**:
   ```bash
   python account_soap_service.py
   ```

## Usage Examples

### COBOL Program Calls
```cobol
CALL 'ACCTMGMT' USING 'CREATE', ACCOUNT-RECORD, RETURN-CODE
CALL 'ACCTBAL' USING 'VALIDATE', ACCOUNT-ID, AMOUNT, TRANS-TYPE, BALANCE, RETURN-CODE
```

### SOAP API Usage
```python
client = AccountSOAPClient()
result = client.create_account({
    'account_id': 'ACC123456789',
    'account_type': 'CA',
    'customer_id': 'CUST12345',
    'account_name': 'John Doe Trading'
})
```

### JCL Submission
```bash
# Submit daily processing job
submit ACCTDLY.jcl

# Submit account creation job with parameters
submit ACCTCRE.jcl,CUSTID=CUST12345,ACCTID=ACC123456789
```

## Error Codes

- **00** - Success
- **01** - Record not found
- **02** - Duplicate key/record exists
- **03** - File I/O error
- **04** - Invalid data/parameters
- **10** - Insufficient funds
- **11** - Invalid transaction type

## Performance Considerations

- Indexed file organization for fast lookups
- Batch processing for high-volume operations
- Efficient balance calculation algorithms
- Minimal file I/O operations
- Optimized COBOL program structure

## Monitoring and Maintenance

- Daily backup procedures
- Log file monitoring
- Performance metrics collection
- Error rate tracking
- Regulatory compliance verification

## Future Enhancements

- Real-time streaming interfaces
- Enhanced fraud detection
- Mobile API support
- Advanced analytics
- Machine learning integration
- Cloud deployment options

## Support and Contact

For technical support or questions about this system, please refer to the system documentation or contact the development team.
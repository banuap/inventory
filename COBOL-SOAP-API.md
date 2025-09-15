# COBOL SOAP API for Account Management

This document describes the COBOL SOAP API implementation for broker dealer account management operations integrated into the inventory management system.

## Overview

The COBOL SOAP API provides a web service interface for managing broker dealer accounts. It combines traditional COBOL business logic with modern web service technologies to deliver enterprise-grade account management functionality.

## Architecture

The implementation consists of several key components:

### 1. COBOL Business Logic
- **ACCOUNT-MANAGER.cbl**: Core business logic for account operations
- **ACCOUNT.cpy**: Account data structure copybook
- **SOAP-STRUCTURES.cpy**: SOAP-specific data structures

### 2. SOAP Web Service Layer
- **SOAP-SERVER.cbl**: SOAP XML processing and service interface
- **cobol_soap_integration.py**: Python integration layer with Flask

### 3. Data Management
- File-based account storage in `cobol/data/accounts.dat`
- Structured logging in `cobol/data/soap-server.log`

## API Endpoints

### SOAP Service Endpoint
```
POST /soap/accounts
Content-Type: text/xml; charset=utf-8
SOAPAction: http://inventory.broker/accounts/ManageAccount
```

### WSDL Definition
```
GET /soap/wsdl
```

### Test Endpoint
```
GET /soap/test
```

## Supported Operations

### 1. CREATE_ACCOUNT
Creates a new broker dealer account.

**Request Example:**
```xml
<?xml version="1.0" encoding="UTF-8"?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
    <soap:Body>
        <AccountServiceRequest>
            <operation>CREATE_ACCOUNT</operation>
            <accountId>ACC001</accountId>
            <clientId>CLI001</clientId>
            <clientName>John Doe</clientName>
            <accountType>INDIVIDUAL</accountType>
        </AccountServiceRequest>
    </soap:Body>
</soap:Envelope>
```

**Response Example:**
```xml
<?xml version="1.0" encoding="UTF-8"?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
    <soap:Body>
        <AccountServiceResponse>
            <status>SUCCESS</status>
            <message>Account ACC001 created successfully</message>
        </AccountServiceResponse>
    </soap:Body>
</soap:Envelope>
```

### 2. GET_ACCOUNT
Retrieves account details by account ID.

**Request Example:**
```xml
<?xml version="1.0" encoding="UTF-8"?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
    <soap:Body>
        <AccountServiceRequest>
            <operation>GET_ACCOUNT</operation>
            <accountId>ACC001</accountId>
        </AccountServiceRequest>
    </soap:Body>
</soap:Envelope>
```

**Response Example:**
```xml
<?xml version="1.0" encoding="UTF-8"?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
    <soap:Body>
        <AccountServiceResponse>
            <status>SUCCESS</status>
            <account>
                <accountId>ACC001</accountId>
                <clientName>John Doe</clientName>
                <accountType>INDIVIDUAL</accountType>
                <balance>10000.00</balance>
                <availableBalance>9500.00</availableBalance>
                <currency>USD</currency>
                <status>ACTIVE</status>
            </account>
        </AccountServiceResponse>
    </soap:Body>
</soap:Envelope>
```

### 3. GET_BALANCE
Retrieves account balance information.

**Request Example:**
```xml
<?xml version="1.0" encoding="UTF-8"?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
    <soap:Body>
        <AccountServiceRequest>
            <operation>GET_BALANCE</operation>
            <accountId>ACC001</accountId>
        </AccountServiceRequest>
    </soap:Body>
</soap:Envelope>
```

### 4. UPDATE_ACCOUNT
Updates account information.

### 5. DELETE_ACCOUNT
Removes an account from the system.

### 6. LIST_ACCOUNTS
Lists all accounts in the system.

## Account Data Structure

The account record contains the following fields:

- **ACCOUNT-ID**: Unique account identifier (12 characters)
- **ACCOUNT-TYPE**: Type of account (INDIVIDUAL, CORPORATE, etc.)
- **ACCOUNT-STATUS**: Current status (ACTIVE, INACTIVE, etc.)
- **CLIENT-ID**: Client identifier (12 characters)
- **CLIENT-NAME**: Client full name (50 characters)
- **ACCOUNT-BALANCE**: Current account balance (decimal)
- **AVAILABLE-BALANCE**: Available balance for trading (decimal)
- **MARGIN-BALANCE**: Margin account balance (decimal)
- **ACCOUNT-CURRENCY**: Currency code (3 characters)
- **OPEN-DATE**: Account opening date
- **RISK-RATING**: Risk assessment rating
- **ACCOUNT-MANAGER**: Assigned account manager
- **COMMISSION-RATE**: Commission rate percentage
- **INTEREST-RATE**: Interest rate percentage

## Error Handling

The API returns standardized error responses:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
    <soap:Body>
        <AccountServiceResponse>
            <status>ERROR</status>
            <errorCode>404</errorCode>
            <message>Account not found</message>
        </AccountServiceResponse>
    </soap:Body>
</soap:Envelope>
```

### Error Codes

- **00**: Success
- **01**: Account already exists
- **02**: Account not found
- **03**: Invalid data
- **04**: File/system error

## Building and Running

### Prerequisites
- GnuCOBOL compiler (`sudo apt install gnucobol`)
- Python 3.12+ with Flask

### Compilation
```bash
cd cobol
make setup-data
cobc -x -I copybooks -o bin/simple-account-demo programs/SIMPLE-ACCOUNT-DEMO.cbl
```

### Running the Demo
```bash
./cobol/bin/simple-account-demo
```

### Starting the Web Service
```bash
python app.py
```

## Testing

Test the SOAP API using curl:

```bash
# Test services endpoint
curl http://localhost:5000/api/services

# Test SOAP API
curl http://localhost:5000/soap/test

# Get WSDL
curl http://localhost:5000/soap/wsdl

# Create account via SOAP
curl -X POST http://localhost:5000/soap/accounts \
  -H "Content-Type: text/xml; charset=utf-8" \
  -H "SOAPAction: http://inventory.broker/accounts/ManageAccount" \
  -d '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
        <soap:Body>
          <AccountServiceRequest>
            <operation>CREATE_ACCOUNT</operation>
            <accountId>ACC001</accountId>
            <clientName>John Doe</clientName>
            <accountType>INDIVIDUAL</accountType>
          </AccountServiceRequest>
        </soap:Body>
      </soap:Envelope>'
```

## Integration with Existing System

The COBOL SOAP API seamlessly integrates with the existing Python Flask inventory management system:

- Sensor management continues to work through REST API
- Account management is available through SOAP API
- Both services are accessible from the same Flask application
- Unified service discovery through `/api/services` endpoint

## Production Considerations

For production deployment:

1. **Security**: Implement authentication and authorization
2. **Performance**: Use compiled COBOL modules for better performance
3. **Scalability**: Consider using a COBOL application server
4. **Monitoring**: Implement comprehensive logging and monitoring
5. **Data Storage**: Migrate from file-based to database storage
6. **Error Handling**: Enhance error handling and validation
7. **Documentation**: Generate comprehensive API documentation

## File Structure

```
cobol/
├── copybooks/
│   ├── ACCOUNT.cpy                 # Account data structure
│   └── SOAP-STRUCTURES.cpy        # SOAP message structures
├── programs/
│   ├── ACCOUNT-MANAGER.cbl         # Business logic module
│   ├── SIMPLE-ACCOUNT-DEMO.cbl     # Demo program
│   └── TEST-SOAP-API.cbl          # Test program
├── soap/
│   └── SOAP-SERVER.cbl             # SOAP service handler
├── data/
│   ├── accounts.dat                # Account data file
│   └── soap-server.log             # Service logs
├── bin/                            # Compiled executables
└── Makefile                        # Build configuration

cobol_soap_integration.py              # Python Flask integration
```

This implementation demonstrates how traditional COBOL business logic can be modernized with web service interfaces while maintaining the reliability and performance characteristics that make COBOL suitable for financial applications.
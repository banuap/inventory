# Inventory Sensor Management System

A comprehensive Flask-based API system for managing sensor inventory with location tracking, status monitoring, and historical data management. Now includes a COBOL SOAP API for broker dealer account management.

## Features

### Sensor Management (REST API)
- **Sensor CRUD Operations**: Create, read, update, and delete sensors
- **Location Tracking**: Track sensor locations with complete history
- **Status Management**: Monitor sensor status (Active, Inactive, Maintenance, Retired) with change history
- **Search & Filtering**: Advanced search and filtering capabilities
- **Statistics & Reporting**: Real-time statistics and reporting
- **REST API**: Complete RESTful API for integration
- **Command Line Interface**: Easy-to-use CLI for system interaction

### Account Management (COBOL SOAP API)
- **COBOL Business Logic**: Traditional COBOL for reliable financial operations
- **SOAP Web Service**: Standards-based SOAP interface
- **Broker Dealer Accounts**: Specialized account management for broker dealers
- **Account Operations**: Create, read, update, delete, and list accounts
- **Balance Management**: Real-time balance and available funds tracking
- **WSDL Support**: Full WSDL definition for service discovery

## Quick Start

### 1. Install Dependencies

```bash
# Install Python dependencies
pip install -r requirements.txt

# Install COBOL compiler (Ubuntu/Debian)
sudo apt install gnucobol
```

### 2. Start the Server

```bash
python app.py
```

The server will start on `http://localhost:5000`

### 3. Populate with Sample Data

```bash
python demo.py
```

### 4. Use the CLI Tool (Sensor Management)

```bash
# List all sensors
python cli.py list

# Show statistics
python cli.py stats

# Search sensors
python cli.py search temperature

# Add a new sensor
python cli.py add SENS-NEW "New Sensor Model" SN123456 "Manufacturer" "Type" "Location"

# Get sensor details
python cli.py get 1

# Update sensor location
python cli.py update 1 --location "New Location"

# Update sensor status
python cli.py update 1 --status "Maintenance" --reason "Scheduled maintenance"
```

### 5. Test the COBOL SOAP API

```bash
# Run COBOL demo
cd cobol
cobc -x -I copybooks -o bin/simple-account-demo programs/SIMPLE-ACCOUNT-DEMO.cbl
./bin/simple-account-demo

# Test SOAP API
python test_cobol_soap.py
```

## API Endpoints

### Service Discovery

- `GET /api/services` - List all available services (REST and SOAP)

### Sensor Management (REST API)

- `GET /api/sensors` - List all sensors (with optional filtering)
- `POST /api/sensors` - Create a new sensor
- `GET /api/sensors/{id}` - Get sensor details
- `PUT /api/sensors/{id}` - Update sensor
- `DELETE /api/sensors/{id}` - Delete sensor

### Search and Statistics

- `GET /api/sensors/search?q={query}` - Search sensors
- `GET /api/sensors/stats` - Get system statistics

### History Tracking

- `GET /api/sensors/{id}/location-history` - Get location history
- `GET /api/sensors/{id}/status-history` - Get status history

### Account Management (COBOL SOAP API)

- `POST /soap/accounts` - SOAP operations for account management
- `GET /soap/wsdl` - Get WSDL definition
- `GET /soap/test` - Test SOAP API functionality

#### SOAP Operations

- **CREATE_ACCOUNT** - Create new broker account
- **GET_ACCOUNT** - Retrieve account details
- **UPDATE_ACCOUNT** - Update account information
- **DELETE_ACCOUNT** - Delete account
- **LIST_ACCOUNTS** - List all accounts
- **GET_BALANCE** - Get account balance

### Filtering Parameters

- `status` - Filter by sensor status
- `type` - Filter by sensor type
- `manufacturer` - Filter by manufacturer
- `location` - Filter by location

## API Usage Examples

### Create a Sensor

```bash
curl -X POST http://localhost:5000/api/sensors \
  -H "Content-Type: application/json" \
  -d '{
    "inventory_id": "TEMP-001",
    "model": "ThermoMax Pro",
    "serial_number": "TM123456",
    "manufacturer": "ThermoTech",
    "sensor_type": "Temperature",
    "location": "Warehouse A",
    "status": "Active"
  }'
```

### Get All Sensors

```bash
curl http://localhost:5000/api/sensors
```

### Search Sensors

```bash
curl "http://localhost:5000/api/sensors/search?q=temperature"
```

### Filter by Status

```bash
curl "http://localhost:5000/api/sensors?status=Active"
```

### Update Sensor Location

```bash
curl -X PUT http://localhost:5000/api/sensors/1 \
  -H "Content-Type: application/json" \
  -d '{
    "location": "Warehouse B - Zone 2",
    "changed_by": "admin"
  }'
```

### Update Sensor Status

```bash
curl -X PUT http://localhost:5000/api/sensors/1 \
  -H "Content-Type: application/json" \
  -d '{
    "status": "Maintenance",
    "status_reason": "Scheduled calibration",
    "changed_by": "technician"
  }'
```

## COBOL SOAP API Usage Examples

### Check Available Services

```bash
curl http://localhost:5000/api/services
```

### Get WSDL Definition

```bash
curl http://localhost:5000/soap/wsdl
```

### Create Account

```bash
curl -X POST http://localhost:5000/soap/accounts \
  -H "Content-Type: text/xml; charset=utf-8" \
  -H "SOAPAction: http://inventory.broker/accounts/ManageAccount" \
  -d '<?xml version="1.0" encoding="UTF-8"?>
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
</soap:Envelope>'
```

### Get Account Balance

```bash
curl -X POST http://localhost:5000/soap/accounts \
  -H "Content-Type: text/xml; charset=utf-8" \
  -H "SOAPAction: http://inventory.broker/accounts/ManageAccount" \
  -d '<?xml version="1.0" encoding="UTF-8"?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
    <soap:Body>
        <AccountServiceRequest>
            <operation>GET_BALANCE</operation>
            <accountId>ACC001</accountId>
        </AccountServiceRequest>
    </soap:Body>
</soap:Envelope>'
```

### Test SOAP API

```bash
curl http://localhost:5000/soap/test
```

## Data Model

### Sensor Fields

- `id` - Auto-generated primary key
- `inventory_id` - Unique inventory identifier
- `model` - Sensor model name
- `serial_number` - Unique serial number
- `manufacturer` - Manufacturer name
- `sensor_type` - Type of sensor (Temperature, Humidity, Pressure, etc.)
- `location` - Current location
- `status` - Current status (Active, Inactive, Maintenance, Retired)
- `created_at` - Creation timestamp
- `updated_at` - Last update timestamp

### Status Values

- **Active** - Sensor is operational and in use
- **Inactive** - Sensor is not currently in use but operational
- **Maintenance** - Sensor is undergoing maintenance or calibration
- **Retired** - Sensor has been permanently removed from service

## Testing

Run the test suite:

```bash
pytest test_app.py -v
```

## Architecture

The system is built with:

- **Flask** - Web framework
- **SQLAlchemy** - Database ORM
- **SQLite** - Database (easily replaceable with PostgreSQL, MySQL, etc.)
- **Marshmallow** - Data serialization (for future enhancements)

## File Structure

```
inventory/
├── app.py              # Main Flask application
├── cli.py              # Command line interface
├── demo.py             # Demo script with sample data
├── test_app.py         # Test suite for REST API
├── test_cobol_soap.py  # Test suite for COBOL SOAP API
├── cobol_soap_integration.py  # COBOL SOAP integration module
├── requirements.txt    # Python dependencies
├── README.md           # This file
├── COBOL-SOAP-API.md   # COBOL SOAP API documentation
├── user-stories-sensor-management.md  # User stories and requirements
└── cobol/              # COBOL SOAP API implementation
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
```

## Future Enhancements

Based on the user stories, potential future enhancements include:

- User authentication and authorization
- Maintenance scheduling and alerts
- Data reading collection and analysis
- Report generation (PDF, Excel)
- Email/SMS notifications
- Integration with external systems
- Web UI dashboard
- Mobile app support
- Advanced analytics and insights

## Contributing

1. Ensure all tests pass: `pytest test_app.py`
2. Follow the existing code style
3. Add tests for new features
4. Update documentation as needed
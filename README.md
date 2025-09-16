# Inventory Management System

A comprehensive system for managing inventory and customer accounts with multiple microservices.

## Services

### 1. Sensor Management (Flask/Python)
A Flask-based API system for managing sensor inventory with location tracking, status monitoring, and historical data management.

### 2. Account Opening Service (Spring Boot/Java)
A Spring Boot microservice for customer account opening with support for different account types.

## Features

### Sensor Management Service
- **Sensor CRUD Operations**: Create, read, update, and delete sensors
- **Location Tracking**: Track sensor locations with complete history
- **Status Management**: Monitor sensor status (Active, Inactive, Maintenance, Retired) with change history
- **Search & Filtering**: Advanced search and filtering capabilities
- **Statistics & Reporting**: Real-time statistics and reporting
- **REST API**: Complete RESTful API for integration
- **Command Line Interface**: Easy-to-use CLI for system interaction

### Account Opening Service
- **Account Management**: Create, read, update, and delete customer accounts
- **Account Types**: Support for SAVINGS, CHECKING, BUSINESS, INVESTMENT, and CRYPTO accounts
- **Validation**: Input validation for all required fields and email format
- **Unique Email**: Ensures no duplicate accounts with the same email address
- **REST API**: Complete RESTful API for integration
- **JPA/Hibernate**: Database operations with Spring Data JPA

## Quick Start

### Sensor Management Service (Flask/Python)

#### 1. Install Python Dependencies

```bash
pip install -r requirements.txt
```

#### 2. Start the Sensor Management Server

```bash
python app.py
```

The server will start on `http://localhost:5000`

#### 3. Populate with Sample Data

```bash
python demo.py
```

#### 4. Use the CLI Tool

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

### Account Opening Service (Spring Boot/Java)

#### Prerequisites
- Java 17 or higher
- Maven 3.6 or higher

#### 1. Navigate to Account Service

```bash
cd account-service
```

#### 2. Build and Test

```bash
mvn clean test
```

#### 3. Start the Account Service

```bash
mvn spring-boot:run
```

The service will start on `http://localhost:8080`

#### 4. Test the Account API

```bash
# Get service info
curl http://localhost:8080/api/accounts/info

# Create an account
curl -X POST http://localhost:8080/api/accounts \
  -H "Content-Type: application/json" \
  -d '{
    "name": "John Doe",
    "address": "123 Main St, City, State",
    "email": "john@example.com",
    "accountType": "SAVINGS"
  }'

# Get all accounts
curl http://localhost:8080/api/accounts
```

## API Endpoints

### Sensor Management API (Flask - Port 5000)

#### Core Sensor Operations

- `GET /api/sensors` - List all sensors (with optional filtering)
- `POST /api/sensors` - Create a new sensor
- `GET /api/sensors/{id}` - Get sensor details
- `PUT /api/sensors/{id}` - Update sensor
- `DELETE /api/sensors/{id}` - Delete sensor

#### Search and Statistics

- `GET /api/sensors/search?q={query}` - Search sensors
- `GET /api/sensors/stats` - Get system statistics

#### History Tracking

- `GET /api/sensors/{id}/location-history` - Get location history
- `GET /api/sensors/{id}/status-history` - Get status history

### Account Opening API (Spring Boot - Port 8080)

#### Account Operations

- `GET /api/accounts/info` - Get service information
- `GET /api/accounts` - Get all accounts
- `POST /api/accounts` - Create a new account
- `GET /api/accounts/{id}` - Get account by ID
- `PUT /api/accounts/{id}` - Update account
- `DELETE /api/accounts/{id}` - Delete account

#### Search and Filter

- `GET /api/accounts/email/{email}` - Get account by email
- `GET /api/accounts/type/{accountType}` - Get accounts by type
- `GET /api/accounts/search?name={name}` - Search accounts by name

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
├── app.py                  # Main Flask application (Sensor Management)
├── cli.py                  # Command line interface for sensors
├── demo.py                 # Demo script with sample sensor data
├── test_app.py             # Test suite for sensor management
├── requirements.txt        # Python dependencies
├── user-stories-sensor-management.md  # User stories and requirements
├── account-service/        # Spring Boot Account Opening Microservice
│   ├── src/
│   │   ├── main/
│   │   │   ├── java/com/inventory/accountservice/
│   │   │   │   ├── AccountServiceApplication.java
│   │   │   │   ├── controller/AccountController.java
│   │   │   │   ├── service/AccountService.java
│   │   │   │   ├── repository/AccountRepository.java
│   │   │   │   ├── entity/Account.java
│   │   │   │   ├── entity/AccountType.java
│   │   │   │   └── dto/AccountDto.java
│   │   │   └── resources/
│   │   │       └── application.properties
│   │   └── test/
│   │       └── java/com/inventory/accountservice/
│   ├── pom.xml             # Maven configuration
│   └── README.md           # Account service documentation
└── README.md               # This file
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
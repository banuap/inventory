# Inventory Management System

A comprehensive microservices-based API system for managing inventory with sensor tracking and dealer management capabilities.

## Services

### 1. Sensor Management Service (Python/Flask)
A Flask-based API system for managing sensor inventory with location tracking, status monitoring, and historical data management.

### 2. Dealer Management Service (C#/ASP.NET Core)
A C# ASP.NET Core Web API microservice for managing dealer information including account numbers and addresses.

## Features

### Sensor Service Features
- **Sensor CRUD Operations**: Create, read, update, and delete sensors
- **Location Tracking**: Track sensor locations with complete history
- **Status Management**: Monitor sensor status (Active, Inactive, Maintenance, Retired) with change history
- **Search & Filtering**: Advanced search and filtering capabilities
- **Statistics & Reporting**: Real-time statistics and reporting
- **REST API**: Complete RESTful API for integration
- **Command Line Interface**: Easy-to-use CLI for system interaction

### Dealer Service Features
- **Dealer CRUD Operations**: Create, read, update, and delete dealers
- **Account Number Validation**: Ensures unique dealer account numbers
- **Search Functionality**: Search dealers by account number or address
- **REST API**: Complete RESTful API with proper HTTP status codes
- **Data Validation**: Comprehensive input validation and error handling

## Quick Start

### Sensor Service (Python)

#### 1. Install Dependencies

```bash
pip install -r requirements.txt
```

#### 2. Start the Server

```bash
python app.py
```

The server will start on `http://localhost:5000`

### Dealer Service (C#)

#### 1. Build and Run

```bash
cd DealerService
dotnet build
dotnet run
```

The service will start on `http://localhost:5143`

### 3. Populate with Sample Data

```bash
python demo.py
```

### 4. Use the CLI Tool

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

## API Endpoints

### Core Sensor Operations

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

### Filtering Parameters

- `status` - Filter by sensor status
- `type` - Filter by sensor type
- `manufacturer` - Filter by manufacturer
- `location` - Filter by location

## Dealer Service API Endpoints

### Core Dealer Operations

- `GET /api/dealers` - List all dealers
- `POST /api/dealers` - Create a new dealer
- `GET /api/dealers/{id}` - Get dealer by ID
- `PUT /api/dealers/{id}` - Update dealer
- `DELETE /api/dealers/{id}` - Delete dealer

### Search

- `GET /api/dealers/search?q={query}` - Search dealers by account number or address

## API Usage Examples

### Sensor Service Examples

#### Create a Sensor

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

#### Get All Sensors

```bash
curl http://localhost:5000/api/sensors
```

#### Search Sensors

```bash
curl "http://localhost:5000/api/sensors/search?q=temperature"
```

#### Filter by Status

```bash
curl "http://localhost:5000/api/sensors?status=Active"
```

### Dealer Service Examples

#### Create a Dealer

```bash
curl -X POST http://localhost:5143/api/dealers \
  -H "Content-Type: application/json" \
  -d '{
    "accountNumber": "ACC-001",
    "address": "123 Main Street, Anytown, ST 12345"
  }'
```

#### Get All Dealers

```bash
curl http://localhost:5143/api/dealers
```

#### Search Dealers

```bash
curl "http://localhost:5143/api/dealers/search?q=Main"
```

#### Update Sensor Location

```bash
curl -X PUT http://localhost:5000/api/sensors/1 \
  -H "Content-Type: application/json" \
  -d '{
    "location": "Warehouse B - Zone 2",
    "changed_by": "admin"
  }'
```

#### Update Sensor Status

```bash
curl -X PUT http://localhost:5000/api/sensors/1 \
  -H "Content-Type: application/json" \
  -d '{
    "status": "Maintenance",
    "status_reason": "Scheduled calibration",
    "changed_by": "technician"
  }'
```

## Data Models

### Sensor Model

#### Sensor Fields

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

#### Status Values

- **Active** - Sensor is operational and in use
- **Inactive** - Sensor is not currently in use but operational
- **Maintenance** - Sensor is undergoing maintenance or calibration
- **Retired** - Sensor has been permanently removed from service

### Dealer Model

#### Dealer Fields

- `id` - Auto-generated primary key (integer)
- `accountNumber` - Unique dealer account number (string, required, max 50 chars)
- `address` - Dealer address (string, required, max 500 chars)
- `createdAt` - Creation timestamp (DateTime, UTC)
- `updatedAt` - Last update timestamp (DateTime, UTC)

## Testing

### Sensor Service Tests

Run the Python test suite:

```bash
pytest test_app.py -v
```

### Dealer Service Tests

Run the C# test suite:

```bash
cd DealerService
dotnet test
```

## Architecture

The system is built with a microservices architecture:

### Sensor Service (Python)
- **Flask** - Web framework
- **SQLAlchemy** - Database ORM
- **SQLite** - Database (easily replaceable with PostgreSQL, MySQL, etc.)
- **Marshmallow** - Data serialization (for future enhancements)

### Dealer Service (C#)
- **ASP.NET Core 8.0** - Web framework
- **Entity Framework Core** - Database ORM
- **SQLite** - Database
- **Swagger/OpenAPI** - API documentation
- **xUnit** - Testing framework

## File Structure

```
inventory/
├── app.py              # Main Flask application (Sensor Service)
├── cli.py              # Command line interface
├── demo.py             # Demo script with sample data
├── test_app.py         # Test suite for Sensor Service
├── requirements.txt    # Python dependencies
├── README.md           # This file
├── user-stories-sensor-management.md  # User stories and requirements
└── DealerService/      # C# Dealer Management Microservice
    ├── Controllers/    # API Controllers
    ├── Data/          # Database context
    ├── Models/        # Data models
    ├── Tests/         # Unit tests
    ├── Program.cs     # Application entry point
    ├── DealerService.csproj  # Project file
    ├── appsettings.json      # Configuration
    └── README.md      # Dealer service documentation
```

## Service URLs

When running both services:

- **Sensor Service**: `http://localhost:5000`
- **Dealer Service**: `http://localhost:5143`

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
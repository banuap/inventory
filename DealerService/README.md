# Dealer Management Microservice

A C# ASP.NET Core Web API microservice for managing dealer information in the inventory system.

## Features

- **Full CRUD Operations**: Create, read, update, and delete dealers
- **Account Number Validation**: Ensures unique dealer account numbers
- **Search Functionality**: Search dealers by account number or address
- **REST API**: Complete RESTful API with proper HTTP status codes
- **Data Validation**: Comprehensive input validation and error handling
- **SQLite Database**: Lightweight database with Entity Framework Core
- **Swagger Documentation**: Auto-generated API documentation

## Requirements

- **Dealer Account Number**: Required, unique, max 50 characters
- **Dealer Address**: Required, max 500 characters

## Quick Start

### 1. Build and Run

```bash
cd DealerService
dotnet build
dotnet run
```

The service will start on `http://localhost:5143`

### 2. Access Swagger Documentation

Open `http://localhost:5143/swagger` in your browser to view the interactive API documentation.

## API Endpoints

### Core Dealer Operations

- `GET /api/dealers` - List all dealers
- `POST /api/dealers` - Create a new dealer
- `GET /api/dealers/{id}` - Get dealer by ID
- `PUT /api/dealers/{id}` - Update dealer
- `DELETE /api/dealers/{id}` - Delete dealer

### Search

- `GET /api/dealers/search?q={query}` - Search dealers by account number or address

### Info

- `GET /` - Welcome endpoint with service information
- `GET /swagger` - Interactive API documentation

## API Usage Examples

### Create a Dealer

```bash
curl -X POST http://localhost:5143/api/dealers \
  -H "Content-Type: application/json" \
  -d '{
    "accountNumber": "ACC-001", 
    "address": "123 Main Street, Anytown, ST 12345"
  }'
```

### Get All Dealers

```bash
curl http://localhost:5143/api/dealers
```

### Search Dealers

```bash
curl "http://localhost:5143/api/dealers/search?q=Main"
```

### Get Dealer by ID

```bash
curl http://localhost:5143/api/dealers/1
```

### Update Dealer

```bash
curl -X PUT http://localhost:5143/api/dealers/1 \
  -H "Content-Type: application/json" \
  -d '{
    "id": 1,
    "accountNumber": "ACC-001",
    "address": "456 Updated Street, Newtown, ST 54321"
  }'
```

### Delete Dealer

```bash
curl -X DELETE http://localhost:5143/api/dealers/1
```

## Data Model

### Dealer Fields

- `id` - Auto-generated primary key (integer)
- `accountNumber` - Unique dealer account number (string, required, max 50 chars)
- `address` - Dealer address (string, required, max 500 chars)
- `createdAt` - Creation timestamp (DateTime, UTC)
- `updatedAt` - Last update timestamp (DateTime, UTC)

## Database

The service uses SQLite with Entity Framework Core for data persistence. The database file (`dealers.db`) is created automatically when the service starts.

## Error Handling

The API provides comprehensive error handling with appropriate HTTP status codes:

- `200 OK` - Success
- `201 Created` - Resource created successfully
- `400 Bad Request` - Invalid input data
- `404 Not Found` - Resource not found
- `409 Conflict` - Duplicate account number
- `500 Internal Server Error` - Server error

## Architecture

- **ASP.NET Core 8.0** - Web framework
- **Entity Framework Core** - Database ORM
- **SQLite** - Database
- **Swagger/OpenAPI** - API documentation
- **xUnit** - Testing framework

## Running with Python Service

This C# microservice is designed to complement the existing Python sensor inventory service. Both services can run simultaneously:

- **Python Sensor Service**: `http://localhost:5000`
- **C# Dealer Service**: `http://localhost:5143`

## Configuration

Database connection and other settings can be configured in `appsettings.json`:

```json
{
  "ConnectionStrings": {
    "DefaultConnection": "Data Source=dealers.db"
  }
}
```

## Development

### Build

```bash
dotnet build
```

### Run Tests

```bash
dotnet test
```

### Database Migrations

The service uses `EnsureCreated()` for simplicity. For production, consider using Entity Framework migrations:

```bash
dotnet ef migrations add InitialCreate
dotnet ef database update
```
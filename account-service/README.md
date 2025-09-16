# Account Opening Microservice

A Spring Boot microservice for account opening functionality, providing REST API endpoints to manage customer accounts with the required fields: Name, Address, Email, and Type of account.

## Features

- **Account Management**: Create, read, update, and delete customer accounts
- **Account Types**: Support for SAVINGS, CHECKING, BUSINESS, and INVESTMENT accounts
- **Validation**: Input validation for all required fields and email format
- **Unique Email**: Ensures no duplicate accounts with the same email address
- **REST API**: Complete RESTful API for integration
- **H2 Database**: In-memory database for development and testing
- **JPA/Hibernate**: Database operations with Spring Data JPA

## Technologies Used

- **Java 17**: Programming language
- **Spring Boot 3.2.0**: Framework
- **Spring Data JPA**: Database operations
- **Spring Validation**: Input validation
- **H2 Database**: In-memory database
- **Maven**: Dependency management and build tool
- **JUnit 5**: Testing framework

## Quick Start

### Prerequisites

- Java 17 or higher
- Maven 3.6 or higher

### Build and Run

1. Navigate to the microservice directory:
```bash
cd account-service
```

2. Build the application:
```bash
mvn clean compile
```

3. Run tests:
```bash
mvn test
```

4. Start the application:
```bash
mvn spring-boot:run
```

The service will start on `http://localhost:8080`

## API Endpoints

### Service Information
- `GET /api/accounts/info` - Get service information and available endpoints

### Account Operations
- `GET /api/accounts` - Get all accounts
- `POST /api/accounts` - Create a new account
- `GET /api/accounts/{id}` - Get account by ID
- `PUT /api/accounts/{id}` - Update account
- `DELETE /api/accounts/{id}` - Delete account

### Search and Filter
- `GET /api/accounts/email/{email}` - Get account by email
- `GET /api/accounts/type/{accountType}` - Get accounts by type
- `GET /api/accounts/search?name={name}` - Search accounts by name

## Data Model

### Account Entity
- `id` - Auto-generated unique identifier
- `name` - Customer name (required)
- `address` - Customer address (required)
- `email` - Customer email (required, unique, validated format)
- `accountType` - Type of account (required: SAVINGS, CHECKING, BUSINESS, INVESTMENT)
- `createdAt` - Creation timestamp
- `updatedAt` - Last update timestamp

### Account Types
- **SAVINGS**: Personal savings account
- **CHECKING**: Personal checking account
- **BUSINESS**: Business account
- **INVESTMENT**: Investment account

## API Usage Examples

### Get Service Information
```bash
curl http://localhost:8080/api/accounts/info
```

### Create a New Account
```bash
curl -X POST http://localhost:8080/api/accounts \
  -H "Content-Type: application/json" \
  -d '{
    "name": "John Doe",
    "address": "123 Main Street, New York, NY 10001",
    "email": "john.doe@example.com",
    "accountType": "SAVINGS"
  }'
```

### Get All Accounts
```bash
curl http://localhost:8080/api/accounts
```

### Get Account by ID
```bash
curl http://localhost:8080/api/accounts/1
```

### Get Accounts by Type
```bash
curl http://localhost:8080/api/accounts/type/SAVINGS
```

### Search Accounts by Name
```bash
curl "http://localhost:8080/api/accounts/search?name=John"
```

### Update Account
```bash
curl -X PUT http://localhost:8080/api/accounts/1 \
  -H "Content-Type: application/json" \
  -d '{
    "name": "John Smith",
    "address": "456 Oak Avenue, Boston, MA 02101",
    "email": "john.smith@example.com",
    "accountType": "CHECKING"
  }'
```

### Delete Account
```bash
curl -X DELETE http://localhost:8080/api/accounts/1
```

## Validation Rules

- **Name**: Required, cannot be blank
- **Address**: Required, cannot be blank, max 500 characters
- **Email**: Required, must be valid email format, must be unique
- **Account Type**: Required, must be one of: SAVINGS, CHECKING, BUSINESS, INVESTMENT

## Error Handling

The API returns appropriate HTTP status codes:
- `200 OK`: Successful GET requests
- `201 Created`: Successful account creation
- `204 No Content`: Successful deletion
- `400 Bad Request`: Validation errors
- `404 Not Found`: Resource not found
- `409 Conflict`: Duplicate email address

## Database

The microservice uses H2 in-memory database for simplicity:
- **URL**: `jdbc:h2:mem:accountdb`
- **Console**: Available at `http://localhost:8080/h2-console` (username: `sa`, password: `password`)
- **Schema**: Auto-created on startup

## Testing

Run the complete test suite:
```bash
mvn test
```

The tests include:
- Unit tests for all REST endpoints
- Validation testing
- Error handling testing
- Service layer testing

## Configuration

Key configuration in `application.properties`:
- Server port: 8080
- Database: H2 in-memory
- JPA: Hibernate with SQL logging
- H2 Console: Enabled for development

## Development

### Project Structure
```
account-service/
├── src/
│   ├── main/
│   │   ├── java/com/inventory/accountservice/
│   │   │   ├── AccountServiceApplication.java
│   │   │   ├── controller/
│   │   │   │   └── AccountController.java
│   │   │   ├── service/
│   │   │   │   └── AccountService.java
│   │   │   ├── repository/
│   │   │   │   └── AccountRepository.java
│   │   │   ├── entity/
│   │   │   │   ├── Account.java
│   │   │   │   └── AccountType.java
│   │   │   └── dto/
│   │   │       └── AccountDto.java
│   │   └── resources/
│   │       └── application.properties
│   └── test/
│       └── java/com/inventory/accountservice/
│           ├── AccountServiceApplicationTests.java
│           └── controller/
│               └── AccountControllerTest.java
└── pom.xml
```

### Building for Production

For production deployment:
1. Update `application.properties` with production database settings
2. Build the application: `mvn clean package`
3. Run the JAR file: `java -jar target/account-service-1.0.0.jar`

## Future Enhancements

Potential improvements:
- User authentication and authorization
- Account balance management
- Transaction history
- Account statement generation
- Integration with external banking systems
- Audit logging
- Metrics and monitoring
- Docker containerization
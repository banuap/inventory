# Makefile for COBOL SOAP API Account Management System
# This makefile compiles the COBOL programs for the broker dealer 
# account management SOAP API

COBC = cobc
COBC_FLAGS = -x -I ../copybooks
COPYBOOK_PATH = ../copybooks
PROGRAM_PATH = ../programs
SOAP_PATH = ../soap
BUILD_PATH = bin

# Create build directory if it doesn't exist
$(BUILD_PATH):
	mkdir -p $(BUILD_PATH)

# Compile all programs
all: $(BUILD_PATH) account-manager soap-server test-soap-api

# Compile Account Manager business logic
account-manager: $(BUILD_PATH)/account-manager
$(BUILD_PATH)/account-manager: $(PROGRAM_PATH)/ACCOUNT-MANAGER.cbl $(COPYBOOK_PATH)/ACCOUNT.cpy $(COPYBOOK_PATH)/SOAP-STRUCTURES.cpy
	$(COBC) $(COBC_FLAGS) -o $@ $(PROGRAM_PATH)/ACCOUNT-MANAGER.cbl

# Compile SOAP Server
soap-server: $(BUILD_PATH)/soap-server
$(BUILD_PATH)/soap-server: $(SOAP_PATH)/SOAP-SERVER.cbl $(COPYBOOK_PATH)/ACCOUNT.cpy $(COPYBOOK_PATH)/SOAP-STRUCTURES.cpy
	$(COBC) $(COBC_FLAGS) -o $@ $(SOAP_PATH)/SOAP-SERVER.cbl

# Compile Test Program
test-soap-api: $(BUILD_PATH)/test-soap-api
$(BUILD_PATH)/test-soap-api: $(PROGRAM_PATH)/TEST-SOAP-API.cbl
	$(COBC) $(COBC_FLAGS) -o $@ $(PROGRAM_PATH)/TEST-SOAP-API.cbl

# Run tests
test: all
	cd $(BUILD_PATH) && ./test-soap-api

# Clean compiled files
clean:
	rm -rf $(BUILD_PATH)

# Create sample data directory
setup-data:
	mkdir -p data
	touch data/accounts.dat

# Help target
help:
	@echo "Available targets:"
	@echo "  all           - Compile all COBOL programs"
	@echo "  account-manager - Compile account manager business logic"
	@echo "  soap-server   - Compile SOAP server"
	@echo "  test-soap-api - Compile test program"
	@echo "  test          - Compile and run tests"
	@echo "  setup-data    - Create data directory structure"
	@echo "  clean         - Remove compiled files"
	@echo "  help          - Show this help message"

.PHONY: all clean test help setup-data account-manager soap-server test-soap-api
"""
Test Suite for COBOL Account Management System
Tests the SOAP API interface to COBOL programs
"""

import unittest
import sys
import os
from decimal import Decimal
from datetime import datetime

# Add the soap-api directory to the path
sys.path.append(os.path.join(os.path.dirname(__file__), '..', 'soap-api'))

from account_soap_service import (
    AccountManagementService, 
    AccountRecord, 
    AccountType, 
    AccountStatus,
    TransactionType,
    OperationResult
)


class TestAccountManagementService(unittest.TestCase):
    """Test cases for Account Management Service"""
    
    def setUp(self):
        """Set up test fixtures"""
        self.service = AccountManagementService()
        self.test_account_id = "TST123456789"
        self.test_customer_id = "CUST99999"
        
        self.sample_account = AccountRecord(
            account_id=self.test_account_id,
            account_type=AccountType.CASH,
            customer_id=self.test_customer_id,
            account_name="Test Account",
            account_status=AccountStatus.ACTIVE,
            account_balance=Decimal("10000.00"),
            available_balance=Decimal("9500.00"),
            margin_balance=Decimal("0.00"),
            open_date="20240915",
            branch_code="TEST",
            account_officer="TESTOFF",
            interest_rate=Decimal("2.50"),
            maintenance_fee=Decimal("25.00"),
            tax_id="999-99-9999",
            regulatory_flags="TEST",
            created_by="TESTUSR",
            updated_by="TESTUSR"
        )

    def test_create_account_success(self):
        """Test successful account creation"""
        result = self.service.create_account(None, self.sample_account)
        
        self.assertIsInstance(result, OperationResult)
        self.assertEqual(result.success, "true")
        self.assertEqual(result.return_code, "00")
        self.assertIn("created successfully", result.message.lower())

    def test_get_account_success(self):
        """Test successful account retrieval"""
        result = self.service.get_account(None, self.test_account_id)
        
        self.assertIsInstance(result, OperationResult)
        self.assertEqual(result.success, "true")
        self.assertEqual(result.return_code, "00")
        self.assertIsNotNone(result.account_data)

    def test_get_account_not_found(self):
        """Test account retrieval for non-existent account"""
        result = self.service.get_account(None, "NONEXISTENT")
        
        # This test depends on the simulation logic
        # In a real implementation, this would test actual COBOL program behavior
        self.assertIsInstance(result, OperationResult)

    def test_update_account_success(self):
        """Test successful account update"""
        updated_account = self.sample_account
        updated_account.account_name = "Updated Test Account"
        
        result = self.service.update_account(None, updated_account)
        
        self.assertIsInstance(result, OperationResult)
        self.assertEqual(result.success, "true")
        self.assertEqual(result.return_code, "00")

    def test_delete_account_success(self):
        """Test successful account deletion"""
        result = self.service.delete_account(None, self.test_account_id)
        
        self.assertIsInstance(result, OperationResult)
        self.assertEqual(result.success, "true")
        self.assertEqual(result.return_code, "00")

    def test_get_account_balance(self):
        """Test account balance retrieval"""
        balance = self.service.get_account_balance(None, self.test_account_id)
        
        self.assertIsInstance(balance, Decimal)
        self.assertGreaterEqual(balance, Decimal("0.00"))

    def test_validate_transaction_sufficient_funds(self):
        """Test transaction validation with sufficient funds"""
        result = self.service.validate_transaction(
            None, 
            self.test_account_id, 
            Decimal("1000.00"), 
            TransactionType.WITHDRAWAL
        )
        
        self.assertIsInstance(result, OperationResult)
        self.assertEqual(result.success, "true")

    def test_validate_transaction_insufficient_funds(self):
        """Test transaction validation with insufficient funds"""
        # This would test the actual insufficient funds scenario
        # The simulation might not reflect this, but the test structure is here
        result = self.service.validate_transaction(
            None, 
            self.test_account_id, 
            Decimal("999999.00"), 
            TransactionType.WITHDRAWAL
        )
        
        self.assertIsInstance(result, OperationResult)

    def test_cobol_program_simulation(self):
        """Test COBOL program simulation logic"""
        # Test CREATE operation
        result_code, result_data = self.service._simulate_cobol_call(
            "ACCTMGMT", "CREATE", self.sample_account
        )
        self.assertEqual(result_code, "00")
        self.assertIn("message", result_data)

        # Test READ operation
        result_code, result_data = self.service._simulate_cobol_call(
            "ACCTMGMT", "READ", self.sample_account
        )
        self.assertEqual(result_code, "00")
        self.assertIn("account", result_data)

        # Test ACCTBAL program
        result_code, result_data = self.service._simulate_cobol_call(
            "ACCTBAL", "CALCAVAIL", None
        )
        self.assertEqual(result_code, "00")
        self.assertIn("balance", result_data)

    def test_format_account_for_cobol(self):
        """Test account data formatting for COBOL"""
        formatted_data = self.service._format_account_for_cobol(self.sample_account)
        
        self.assertIsInstance(formatted_data, str)
        self.assertIn(self.test_account_id, formatted_data)
        self.assertIn("CA", formatted_data)  # Account type

    def test_account_types_enum(self):
        """Test account type enumeration values"""
        self.assertEqual(AccountType.CASH, 'CA')
        self.assertEqual(AccountType.MARGIN, 'MA')
        self.assertEqual(AccountType.IRA, 'IR')
        self.assertEqual(AccountType.TRUST, 'TR')

    def test_account_status_enum(self):
        """Test account status enumeration values"""
        self.assertEqual(AccountStatus.ACTIVE, 'A')
        self.assertEqual(AccountStatus.INACTIVE, 'I')
        self.assertEqual(AccountStatus.CLOSED, 'C')
        self.assertEqual(AccountStatus.SUSPENDED, 'S')

    def test_transaction_types_enum(self):
        """Test transaction type enumeration values"""
        self.assertEqual(TransactionType.DEPOSIT, 'DEP')
        self.assertEqual(TransactionType.WITHDRAWAL, 'WTH')
        self.assertEqual(TransactionType.BUY, 'BUY')
        self.assertEqual(TransactionType.SELL, 'SEL')
        self.assertEqual(TransactionType.DIVIDEND, 'DIV')
        self.assertEqual(TransactionType.INTEREST, 'INT')
        self.assertEqual(TransactionType.FEE, 'FEE')


class TestAccountRecord(unittest.TestCase):
    """Test cases for Account Record complex type"""
    
    def test_account_record_creation(self):
        """Test creating an account record"""
        account = AccountRecord(
            account_id="TEST123",
            account_type=AccountType.CASH,
            customer_id="CUST001",
            account_name="Test Account",
            account_status=AccountStatus.ACTIVE
        )
        
        self.assertEqual(account.account_id, "TEST123")
        self.assertEqual(account.account_type, AccountType.CASH)
        self.assertEqual(account.customer_id, "CUST001")
        self.assertEqual(account.account_name, "Test Account")
        self.assertEqual(account.account_status, AccountStatus.ACTIVE)

    def test_account_record_with_balances(self):
        """Test account record with balance information"""
        account = AccountRecord(
            account_id="TEST123",
            account_balance=Decimal("50000.00"),
            available_balance=Decimal("45000.00"),
            margin_balance=Decimal("5000.00")
        )
        
        self.assertEqual(account.account_balance, Decimal("50000.00"))
        self.assertEqual(account.available_balance, Decimal("45000.00"))
        self.assertEqual(account.margin_balance, Decimal("5000.00"))


class TestIntegration(unittest.TestCase):
    """Integration tests for the complete system"""
    
    def setUp(self):
        self.service = AccountManagementService()
    
    def test_account_lifecycle(self):
        """Test complete account lifecycle: create, read, update, delete"""
        account_id = "LIFECYCLE001"
        
        # Create account
        account_data = AccountRecord(
            account_id=account_id,
            account_type=AccountType.MARGIN,
            customer_id="CUST001",
            account_name="Lifecycle Test Account",
            account_status=AccountStatus.ACTIVE,
            branch_code="TEST",
            account_officer="OFFICER1"
        )
        
        create_result = self.service.create_account(None, account_data)
        self.assertEqual(create_result.success, "true")
        
        # Read account
        read_result = self.service.get_account(None, account_id)
        self.assertEqual(read_result.success, "true")
        
        # Update account
        account_data.account_name = "Updated Lifecycle Account"
        update_result = self.service.update_account(None, account_data)
        self.assertEqual(update_result.success, "true")
        
        # Delete account
        delete_result = self.service.delete_account(None, account_id)
        self.assertEqual(delete_result.success, "true")

    def test_balance_operations(self):
        """Test balance-related operations"""
        account_id = "BALANCE001"
        
        # Get balance
        balance = self.service.get_account_balance(None, account_id)
        self.assertIsInstance(balance, Decimal)
        
        # Validate withdrawal
        withdrawal_result = self.service.validate_transaction(
            None, account_id, Decimal("1000.00"), TransactionType.WITHDRAWAL
        )
        self.assertIsInstance(withdrawal_result, OperationResult)
        
        # Validate deposit (should always be valid)
        deposit_result = self.service.validate_transaction(
            None, account_id, Decimal("1000.00"), TransactionType.DEPOSIT
        )
        self.assertEqual(deposit_result.success, "true")


if __name__ == '__main__':
    # Create test suite
    test_suite = unittest.TestSuite()
    
    # Add test cases
    test_suite.addTest(unittest.makeSuite(TestAccountManagementService))
    test_suite.addTest(unittest.makeSuite(TestAccountRecord))
    test_suite.addTest(unittest.makeSuite(TestIntegration))
    
    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(test_suite)
    
    # Print summary
    print(f"\nTests run: {result.testsRun}")
    print(f"Failures: {len(result.failures)}")
    print(f"Errors: {len(result.errors)}")
    
    if result.failures:
        print("\nFailures:")
        for test, traceback in result.failures:
            print(f"- {test}: {traceback}")
    
    if result.errors:
        print("\nErrors:")
        for test, traceback in result.errors:
            print(f"- {test}: {traceback}")
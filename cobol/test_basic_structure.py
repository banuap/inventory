"""
Basic Test Suite for COBOL Account Management System
Tests the basic structure and file organization
"""

import unittest
import os
import sys


class TestCOBOLStructure(unittest.TestCase):
    """Test COBOL file structure and organization"""
    
    def setUp(self):
        self.base_path = "/home/runner/work/inventory/inventory/cobol"
    
    def test_directory_structure(self):
        """Test that all required directories exist"""
        required_dirs = [
            "copybooks",
            "programs", 
            "jcl",
            "soap-api",
            "docs"
        ]
        
        for dir_name in required_dirs:
            dir_path = os.path.join(self.base_path, dir_name)
            self.assertTrue(os.path.exists(dir_path), f"Directory {dir_name} should exist")
            self.assertTrue(os.path.isdir(dir_path), f"{dir_name} should be a directory")

    def test_copybook_files(self):
        """Test that all copybook files exist and have content"""
        copybooks_path = os.path.join(self.base_path, "copybooks")
        required_copybooks = [
            "ACCOUNT.cpy",
            "TRANSACTION.cpy", 
            "CUSTOMER.cpy"
        ]
        
        for copybook in required_copybooks:
            file_path = os.path.join(copybooks_path, copybook)
            self.assertTrue(os.path.exists(file_path), f"Copybook {copybook} should exist")
            
            # Check file has content
            with open(file_path, 'r') as f:
                content = f.read()
                self.assertGreater(len(content), 100, f"Copybook {copybook} should have substantial content")
                self.assertIn("PIC", content.upper(), f"Copybook {copybook} should contain COBOL data definitions")

    def test_cobol_programs(self):
        """Test that COBOL program files exist and have content"""
        programs_path = os.path.join(self.base_path, "programs")
        required_programs = [
            "ACCTMGMT.cob",
            "ACCTBAL.cob"
        ]
        
        for program in required_programs:
            file_path = os.path.join(programs_path, program)
            self.assertTrue(os.path.exists(file_path), f"Program {program} should exist")
            
            # Check file has content
            with open(file_path, 'r') as f:
                content = f.read()
                self.assertGreater(len(content), 500, f"Program {program} should have substantial content")
                self.assertIn("IDENTIFICATION DIVISION", content, f"Program {program} should be a valid COBOL program")
                self.assertIn("PROCEDURE DIVISION", content, f"Program {program} should have procedure division")

    def test_jcl_scripts(self):
        """Test that JCL scripts exist and have content"""
        jcl_path = os.path.join(self.base_path, "jcl")
        required_jcls = [
            "ACCTDLY.jcl",
            "ACCTCRE.jcl"
        ]
        
        for jcl in required_jcls:
            file_path = os.path.join(jcl_path, jcl)
            self.assertTrue(os.path.exists(file_path), f"JCL {jcl} should exist")
            
            # Check file has content
            with open(file_path, 'r') as f:
                content = f.read()
                self.assertGreater(len(content), 200, f"JCL {jcl} should have substantial content")
                self.assertIn("//", content, f"JCL {jcl} should contain JCL statements")
                self.assertIn("JOB", content, f"JCL {jcl} should contain JOB statement")

    def test_soap_api_files(self):
        """Test that SOAP API files exist"""
        soap_path = os.path.join(self.base_path, "soap-api")
        required_files = [
            "account_soap_service.py",
            "client_example.py",
            "requirements.txt"
        ]
        
        for file_name in required_files:
            file_path = os.path.join(soap_path, file_name)
            self.assertTrue(os.path.exists(file_path), f"SOAP API file {file_name} should exist")
            
            # Check file has content
            with open(file_path, 'r') as f:
                content = f.read()
                self.assertGreater(len(content), 30, f"File {file_name} should have content")

    def test_documentation(self):
        """Test that documentation exists"""
        docs_path = os.path.join(self.base_path, "docs")
        readme_path = os.path.join(docs_path, "README.md")
        
        self.assertTrue(os.path.exists(readme_path), "Documentation README.md should exist")
        
        with open(readme_path, 'r') as f:
            content = f.read()
            self.assertGreater(len(content), 1000, "README should have comprehensive content")
            self.assertIn("COBOL", content, "README should mention COBOL")
            self.assertIn("Account Management", content, "README should mention Account Management")


class TestCOBOLContent(unittest.TestCase):
    """Test COBOL content and structure"""
    
    def setUp(self):
        self.base_path = "/home/runner/work/inventory/inventory/cobol"

    def test_account_copybook_structure(self):
        """Test Account copybook has required fields"""
        with open(os.path.join(self.base_path, "copybooks", "ACCOUNT.cpy"), 'r') as f:
            content = f.read()
            
        required_fields = [
            "ACCOUNT-ID",
            "ACCOUNT-TYPE", 
            "CUSTOMER-ID",
            "ACCOUNT-BALANCE",
            "ACCOUNT-STATUS"
        ]
        
        for field in required_fields:
            self.assertIn(field, content, f"Account copybook should contain {field}")

    def test_transaction_copybook_structure(self):
        """Test Transaction copybook has required fields"""
        with open(os.path.join(self.base_path, "copybooks", "TRANSACTION.cpy"), 'r') as f:
            content = f.read()
            
        required_fields = [
            "TRANSACTION-ID",
            "ACCOUNT-ID",
            "TRANSACTION-TYPE",
            "TRANSACTION-AMOUNT",
            "TRANSACTION-DATE"
        ]
        
        for field in required_fields:
            self.assertIn(field, content, f"Transaction copybook should contain {field}")

    def test_customer_copybook_structure(self):
        """Test Customer copybook has required fields"""
        with open(os.path.join(self.base_path, "copybooks", "CUSTOMER.cpy"), 'r') as f:
            content = f.read()
            
        required_fields = [
            "CUSTOMER-ID",
            "CUSTOMER-TYPE",
            "FIRST-NAME",
            "LAST-NAME",
            "SSN-EIN"
        ]
        
        for field in required_fields:
            self.assertIn(field, content, f"Customer copybook should contain {field}")

    def test_acctmgmt_program_operations(self):
        """Test Account Management program has required operations"""
        with open(os.path.join(self.base_path, "programs", "ACCTMGMT.cob"), 'r') as f:
            content = f.read()
            
        required_operations = [
            "CREATE-OPERATION",
            "READ-OPERATION",
            "UPDATE-OPERATION", 
            "DELETE-OPERATION"
        ]
        
        for operation in required_operations:
            self.assertIn(operation, content, f"ACCTMGMT should support {operation}")

    def test_acctbal_program_functions(self):
        """Test Account Balance program has required functions"""
        with open(os.path.join(self.base_path, "programs", "ACCTBAL.cob"), 'r') as f:
            content = f.read()
            
        required_functions = [
            "UPDATE-BALANCE",
            "VALIDATE-TRANSACTION",
            "CALCULATE-AVAILABLE"
        ]
        
        for function in required_functions:
            self.assertIn(function, content, f"ACCTBAL should support {function}")

    def test_soap_service_operations(self):
        """Test SOAP service has required operations"""
        with open(os.path.join(self.base_path, "soap-api", "account_soap_service.py"), 'r') as f:
            content = f.read()
            
        required_operations = [
            "get_account",
            "create_account",
            "update_account",
            "delete_account",
            "get_account_balance",
            "validate_transaction"
        ]
        
        for operation in required_operations:
            self.assertIn(operation, content, f"SOAP service should have {operation} operation")


class TestIntegration(unittest.TestCase):
    """Basic integration tests"""
    
    def test_copybook_consistency(self):
        """Test that programs reference the same copybooks"""
        base_path = "/home/runner/work/inventory/inventory/cobol"
        
        # Read ACCTMGMT program
        with open(os.path.join(base_path, "programs", "ACCTMGMT.cob"), 'r') as f:
            acctmgmt_content = f.read()
        
        # Should reference ACCOUNT copybook
        self.assertIn("COPY ACCOUNT", acctmgmt_content, "ACCTMGMT should reference ACCOUNT copybook")
        
        # Read ACCTBAL program  
        with open(os.path.join(base_path, "programs", "ACCTBAL.cob"), 'r') as f:
            acctbal_content = f.read()
        
        # Should reference both ACCOUNT and TRANSACTION copybooks
        self.assertIn("COPY ACCOUNT", acctbal_content, "ACCTBAL should reference ACCOUNT copybook")
        self.assertIn("COPY TRANSACTION", acctbal_content, "ACCTBAL should reference TRANSACTION copybook")

    def test_jcl_program_references(self):
        """Test that JCL scripts reference the correct programs"""
        base_path = "/home/runner/work/inventory/inventory/cobol"
        
        # Read daily JCL
        with open(os.path.join(base_path, "jcl", "ACCTDLY.jcl"), 'r') as f:
            daily_jcl = f.read()
        
        # Should reference balance program
        self.assertIn("ACCTBAL", daily_jcl, "Daily JCL should reference ACCTBAL program")
        
        # Read creation JCL
        with open(os.path.join(base_path, "jcl", "ACCTCRE.jcl"), 'r') as f:
            create_jcl = f.read()
        
        # Should reference account management program
        self.assertIn("ACCTMGMT", create_jcl, "Creation JCL should reference ACCTMGMT program")


if __name__ == '__main__':
    # Run all tests
    unittest.main(verbosity=2)
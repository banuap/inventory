#!/usr/bin/env python3
"""
Test script for COBOL SOAP API
This script tests the basic functionality of the COBOL SOAP API
for broker dealer account management.
"""

import requests
import xml.etree.ElementTree as ET
import sys

def test_soap_api():
    """Test the COBOL SOAP API functionality"""
    base_url = "http://localhost:5000"
    
    print("COBOL SOAP API Test Suite")
    print("=" * 50)
    
    # Test 1: Check if services are available
    print("\n1. Testing services endpoint...")
    try:
        response = requests.get(f"{base_url}/api/services")
        if response.status_code == 200:
            data = response.json()
            if data.get('cobol_soap_enabled'):
                print("   ✓ COBOL SOAP API is enabled")
            else:
                print("   ✗ COBOL SOAP API is not enabled")
                return False
        else:
            print(f"   ✗ Services endpoint failed: {response.status_code}")
            return False
    except Exception as e:
        print(f"   ✗ Error testing services: {e}")
        return False
    
    # Test 2: Test WSDL endpoint
    print("\n2. Testing WSDL endpoint...")
    try:
        response = requests.get(f"{base_url}/soap/wsdl")
        if response.status_code == 200 and 'xml' in response.headers.get('content-type', ''):
            print("   ✓ WSDL endpoint working")
        else:
            print(f"   ✗ WSDL endpoint failed: {response.status_code}")
            return False
    except Exception as e:
        print(f"   ✗ Error testing WSDL: {e}")
        return False
    
    # Test 3: Test SOAP test endpoint
    print("\n3. Testing SOAP test endpoint...")
    try:
        response = requests.get(f"{base_url}/soap/test")
        if response.status_code == 200:
            data = response.json()
            if 'request' in data and 'response' in data:
                print("   ✓ SOAP test endpoint working")
            else:
                print("   ✗ SOAP test endpoint response invalid")
                return False
        else:
            print(f"   ✗ SOAP test endpoint failed: {response.status_code}")
            return False
    except Exception as e:
        print(f"   ✗ Error testing SOAP test endpoint: {e}")
        return False
    
    # Test 4: Test CREATE_ACCOUNT operation
    print("\n4. Testing CREATE_ACCOUNT operation...")
    create_request = '''<?xml version="1.0" encoding="UTF-8"?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
    <soap:Body>
        <AccountServiceRequest>
            <operation>CREATE_ACCOUNT</operation>
            <accountId>TEST001</accountId>
            <clientId>CLI001</clientId>
            <clientName>Test Client</clientName>
            <accountType>INDIVIDUAL</accountType>
        </AccountServiceRequest>
    </soap:Body>
</soap:Envelope>'''
    
    try:
        response = requests.post(
            f"{base_url}/soap/accounts",
            data=create_request,
            headers={
                'Content-Type': 'text/xml; charset=utf-8',
                'SOAPAction': 'http://inventory.broker/accounts/ManageAccount'
            }
        )
        if response.status_code == 200:
            # Parse response
            root = ET.fromstring(response.text)
            status = root.find('.//{http://inventory.broker/accounts}status')
            if status is None:
                status = root.find('.//status')
            
            if status is not None and status.text == 'SUCCESS':
                print("   ✓ CREATE_ACCOUNT operation successful")
            else:
                print(f"   ✗ CREATE_ACCOUNT operation failed: {response.text}")
                return False
        else:
            print(f"   ✗ CREATE_ACCOUNT request failed: {response.status_code}")
            return False
    except Exception as e:
        print(f"   ✗ Error testing CREATE_ACCOUNT: {e}")
        return False
    
    # Test 5: Test GET_BALANCE operation
    print("\n5. Testing GET_BALANCE operation...")
    balance_request = '''<?xml version="1.0" encoding="UTF-8"?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
    <soap:Body>
        <AccountServiceRequest>
            <operation>GET_BALANCE</operation>
            <accountId>TEST001</accountId>
        </AccountServiceRequest>
    </soap:Body>
</soap:Envelope>'''
    
    try:
        response = requests.post(
            f"{base_url}/soap/accounts",
            data=balance_request,
            headers={
                'Content-Type': 'text/xml; charset=utf-8',
                'SOAPAction': 'http://inventory.broker/accounts/ManageAccount'
            }
        )
        if response.status_code == 200:
            # Parse response
            root = ET.fromstring(response.text)
            status = root.find('.//{http://inventory.broker/accounts}status')
            if status is None:
                status = root.find('.//status')
            
            if status is not None and status.text == 'SUCCESS':
                print("   ✓ GET_BALANCE operation successful")
            else:
                print(f"   ✗ GET_BALANCE operation failed: {response.text}")
                return False
        else:
            print(f"   ✗ GET_BALANCE request failed: {response.status_code}")
            return False
    except Exception as e:
        print(f"   ✗ Error testing GET_BALANCE: {e}")
        return False
    
    # Test 6: Test LIST_ACCOUNTS operation
    print("\n6. Testing LIST_ACCOUNTS operation...")
    list_request = '''<?xml version="1.0" encoding="UTF-8"?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
    <soap:Body>
        <AccountServiceRequest>
            <operation>LIST_ACCOUNTS</operation>
        </AccountServiceRequest>
    </soap:Body>
</soap:Envelope>'''
    
    try:
        response = requests.post(
            f"{base_url}/soap/accounts",
            data=list_request,
            headers={
                'Content-Type': 'text/xml; charset=utf-8',
                'SOAPAction': 'http://inventory.broker/accounts/ManageAccount'
            }
        )
        if response.status_code == 200:
            # Parse response
            root = ET.fromstring(response.text)
            status = root.find('.//{http://inventory.broker/accounts}status')
            if status is None:
                status = root.find('.//status')
            
            if status is not None and status.text == 'SUCCESS':
                print("   ✓ LIST_ACCOUNTS operation successful")
            else:
                print(f"   ✗ LIST_ACCOUNTS operation failed: {response.text}")
                return False
        else:
            print(f"   ✗ LIST_ACCOUNTS request failed: {response.status_code}")
            return False
    except Exception as e:
        print(f"   ✗ Error testing LIST_ACCOUNTS: {e}")
        return False
    
    return True

def main():
    """Main test function"""
    print("Starting COBOL SOAP API tests...\n")
    
    success = test_soap_api()
    
    print("\n" + "=" * 50)
    if success:
        print("All tests passed! ✓")
        print("\nThe COBOL SOAP API for broker dealer account management is working correctly.")
        sys.exit(0)
    else:
        print("Some tests failed! ✗")
        print("\nPlease check the server logs and ensure the Flask application is running.")
        sys.exit(1)

if __name__ == "__main__":
    main()
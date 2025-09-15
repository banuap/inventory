"""
COBOL SOAP API Integration Module
This module provides integration between the Flask application 
and the COBOL SOAP API for account management in broker dealer operations.
"""

import subprocess
import tempfile
import os
import xml.etree.ElementTree as ET
from flask import Blueprint, request, Response
import json

# Create Blueprint for COBOL SOAP API
cobol_soap_bp = Blueprint('cobol_soap', __name__, url_prefix='/soap')

class COBOLSOAPClient:
    """Client for interfacing with COBOL SOAP API"""
    
    def __init__(self, soap_server_path="cobol/bin/soap-server"):
        self.soap_server_path = soap_server_path
        
    def call_soap_service(self, soap_request_xml):
        """
        Call the COBOL SOAP service with XML request
        Returns the SOAP response XML
        """
        try:
            # Create temporary files for request and response
            with tempfile.NamedTemporaryFile(mode='w', suffix='.xml', delete=False) as req_file:
                req_file.write(soap_request_xml)
                req_file_path = req_file.name
                
            with tempfile.NamedTemporaryFile(mode='w', suffix='.xml', delete=False) as resp_file:
                resp_file_path = resp_file.name
                
            # Call COBOL program (simulation - in practice this would be different)
            # For now, we'll simulate the SOAP response
            soap_response = self._simulate_cobol_soap_call(soap_request_xml)
            
            # Clean up temporary files
            os.unlink(req_file_path)
            os.unlink(resp_file_path)
            
            return soap_response
            
        except Exception as e:
            return self._create_error_response(str(e))
    
    def _simulate_cobol_soap_call(self, request_xml):
        """
        Simulate COBOL SOAP service call
        In a real implementation, this would invoke the compiled COBOL program
        """
        try:
            # Parse the request
            root = ET.fromstring(request_xml)
            
            # Extract operation and account data
            operation = self._extract_element_text(root, 'operation')
            account_id = self._extract_element_text(root, 'accountId')
            
            # Simulate responses based on operation
            if operation == 'CREATE_ACCOUNT':
                return self._create_success_response(f"Account {account_id} created successfully")
            elif operation == 'GET_ACCOUNT':
                return self._create_account_response(account_id)
            elif operation == 'LIST_ACCOUNTS':
                return self._create_success_response("Total accounts: 5")
            elif operation == 'GET_BALANCE':
                return self._create_balance_response(account_id)
            else:
                return self._create_error_response("Unknown operation")
                
        except ET.ParseError:
            return self._create_error_response("Invalid XML request")
    
    def _extract_element_text(self, root, element_name):
        """Extract text from XML element"""
        element = root.find(f".//{element_name}")
        return element.text if element is not None else ""
    
    def _create_success_response(self, message):
        """Create a successful SOAP response"""
        return f"""<?xml version="1.0" encoding="UTF-8"?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
    <soap:Body>
        <AccountServiceResponse>
            <status>SUCCESS</status>
            <message>{message}</message>
        </AccountServiceResponse>
    </soap:Body>
</soap:Envelope>"""

    def _create_account_response(self, account_id):
        """Create account details response"""
        return f"""<?xml version="1.0" encoding="UTF-8"?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
    <soap:Body>
        <AccountServiceResponse>
            <status>SUCCESS</status>
            <account>
                <accountId>{account_id}</accountId>
                <clientName>John Doe</clientName>
                <accountType>INDIVIDUAL</accountType>
                <balance>10000.00</balance>
                <availableBalance>9500.00</availableBalance>
                <currency>USD</currency>
                <status>ACTIVE</status>
            </account>
        </AccountServiceResponse>
    </soap:Body>
</soap:Envelope>"""

    def _create_balance_response(self, account_id):
        """Create balance response"""
        return f"""<?xml version="1.0" encoding="UTF-8"?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
    <soap:Body>
        <AccountServiceResponse>
            <status>SUCCESS</status>
            <message>Account Balance: 10000.00 Available: 9500.00</message>
        </AccountServiceResponse>
    </soap:Body>
</soap:Envelope>"""
    
    def _create_error_response(self, error_message):
        """Create error SOAP response"""
        return f"""<?xml version="1.0" encoding="UTF-8"?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
    <soap:Body>
        <AccountServiceResponse>
            <status>ERROR</status>
            <errorCode>500</errorCode>
            <message>{error_message}</message>
        </AccountServiceResponse>
    </soap:Body>
</soap:Envelope>"""

# Initialize COBOL SOAP client
cobol_client = COBOLSOAPClient()

@cobol_soap_bp.route('/accounts', methods=['POST'])
def soap_service():
    """
    SOAP endpoint for account management operations
    Accepts SOAP XML requests and returns SOAP XML responses
    """
    try:
        # Get SOAP request from body
        soap_request = request.data.decode('utf-8')
        
        if not soap_request:
            return Response(
                cobol_client._create_error_response("Empty request body"),
                content_type='text/xml',
                status=400
            )
        
        # Call COBOL SOAP service
        soap_response = cobol_client.call_soap_service(soap_request)
        
        # Return SOAP response
        return Response(
            soap_response,
            content_type='text/xml; charset=utf-8'
        )
        
    except Exception as e:
        error_response = cobol_client._create_error_response(f"Internal server error: {str(e)}")
        return Response(
            error_response,
            content_type='text/xml',
            status=500
        )

@cobol_soap_bp.route('/wsdl', methods=['GET'])
def get_wsdl():
    """
    Return WSDL definition for the account management service
    """
    wsdl_content = """<?xml version="1.0" encoding="UTF-8"?>
<definitions xmlns="http://schemas.xmlsoap.org/wsdl/"
             xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/"
             xmlns:tns="http://inventory.broker/accounts"
             targetNamespace="http://inventory.broker/accounts">
             
    <types>
        <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                   targetNamespace="http://inventory.broker/accounts">
            <xs:element name="AccountServiceRequest">
                <xs:complexType>
                    <xs:sequence>
                        <xs:element name="operation" type="xs:string"/>
                        <xs:element name="accountId" type="xs:string" minOccurs="0"/>
                        <xs:element name="clientId" type="xs:string" minOccurs="0"/>
                        <xs:element name="clientName" type="xs:string" minOccurs="0"/>
                        <xs:element name="accountType" type="xs:string" minOccurs="0"/>
                    </xs:sequence>
                </xs:complexType>
            </xs:element>
            
            <xs:element name="AccountServiceResponse">
                <xs:complexType>
                    <xs:sequence>
                        <xs:element name="status" type="xs:string"/>
                        <xs:element name="message" type="xs:string" minOccurs="0"/>
                        <xs:element name="errorCode" type="xs:string" minOccurs="0"/>
                        <xs:element name="account" type="tns:AccountType" minOccurs="0"/>
                    </xs:sequence>
                </xs:complexType>
            </xs:element>
            
            <xs:complexType name="AccountType">
                <xs:sequence>
                    <xs:element name="accountId" type="xs:string"/>
                    <xs:element name="clientName" type="xs:string"/>
                    <xs:element name="accountType" type="xs:string"/>
                    <xs:element name="balance" type="xs:decimal"/>
                    <xs:element name="availableBalance" type="xs:decimal"/>
                    <xs:element name="currency" type="xs:string"/>
                    <xs:element name="status" type="xs:string"/>
                </xs:sequence>
            </xs:complexType>
        </xs:schema>
    </types>
    
    <message name="AccountServiceRequestMessage">
        <part name="parameters" element="tns:AccountServiceRequest"/>
    </message>
    
    <message name="AccountServiceResponseMessage">
        <part name="parameters" element="tns:AccountServiceResponse"/>
    </message>
    
    <portType name="AccountServicePortType">
        <operation name="ManageAccount">
            <input message="tns:AccountServiceRequestMessage"/>
            <output message="tns:AccountServiceResponseMessage"/>
        </operation>
    </portType>
    
    <binding name="AccountServiceBinding" type="tns:AccountServicePortType">
        <soap:binding style="document" transport="http://schemas.xmlsoap.org/soap/http"/>
        <operation name="ManageAccount">
            <soap:operation soapAction="http://inventory.broker/accounts/ManageAccount"/>
            <input>
                <soap:body use="literal"/>
            </input>
            <output>
                <soap:body use="literal"/>
            </output>
        </operation>
    </binding>
    
    <service name="AccountService">
        <port name="AccountServicePort" binding="tns:AccountServiceBinding">
            <soap:address location="http://localhost:5000/soap/accounts"/>
        </port>
    </service>
</definitions>"""
    
    return Response(wsdl_content, content_type='text/xml')

@cobol_soap_bp.route('/test', methods=['GET'])
def test_soap_api():
    """
    Test endpoint to demonstrate SOAP API functionality
    """
    test_request = """<?xml version="1.0" encoding="UTF-8"?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
    <soap:Body>
        <AccountServiceRequest>
            <operation>GET_ACCOUNT</operation>
            <accountId>ACC001</accountId>
        </AccountServiceRequest>
    </soap:Body>
</soap:Envelope>"""
    
    response = cobol_client.call_soap_service(test_request)
    
    return {
        'message': 'COBOL SOAP API Test',
        'request': test_request,
        'response': response
    }
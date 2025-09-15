"""
SOAP Client Example for Account Management Service
"""

import requests
from lxml import etree


class AccountSOAPClient:
    """SOAP Client for Account Management Service"""
    
    def __init__(self, service_url="http://localhost:8000"):
        self.service_url = service_url
        self.namespace = "http://broker.dealer.account.management"
        
    def _create_soap_envelope(self, body_content):
        """Create SOAP envelope with body content"""
        envelope = f"""<?xml version="1.0" encoding="UTF-8"?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/"
               xmlns:tns="{self.namespace}">
    <soap:Header/>
    <soap:Body>
        {body_content}
    </soap:Body>
</soap:Envelope>"""
        return envelope

    def _send_soap_request(self, soap_envelope):
        """Send SOAP request and return response"""
        headers = {
            'Content-Type': 'text/xml; charset=utf-8',
            'SOAPAction': ''
        }
        
        response = requests.post(self.service_url, 
                               data=soap_envelope, 
                               headers=headers)
        
        if response.status_code == 200:
            return response.text
        else:
            raise Exception(f"SOAP request failed: {response.status_code} - {response.text}")

    def get_account(self, account_id):
        """Get account information"""
        body = f"""
        <tns:get_account>
            <tns:account_id>{account_id}</tns:account_id>
        </tns:get_account>"""
        
        envelope = self._create_soap_envelope(body)
        response = self._send_soap_request(envelope)
        return self._parse_response(response)

    def create_account(self, account_data):
        """Create new account"""
        body = f"""
        <tns:create_account>
            <tns:account_data>
                <tns:account_id>{account_data.get('account_id', '')}</tns:account_id>
                <tns:account_type>{account_data.get('account_type', '')}</tns:account_type>
                <tns:customer_id>{account_data.get('customer_id', '')}</tns:customer_id>
                <tns:account_name>{account_data.get('account_name', '')}</tns:account_name>
                <tns:account_status>{account_data.get('account_status', '')}</tns:account_status>
                <tns:branch_code>{account_data.get('branch_code', '')}</tns:branch_code>
                <tns:account_officer>{account_data.get('account_officer', '')}</tns:account_officer>
                <tns:tax_id>{account_data.get('tax_id', '')}</tns:tax_id>
            </tns:account_data>
        </tns:create_account>"""
        
        envelope = self._create_soap_envelope(body)
        response = self._send_soap_request(envelope)
        return self._parse_response(response)

    def update_account(self, account_data):
        """Update existing account"""
        body = f"""
        <tns:update_account>
            <tns:account_data>
                <tns:account_id>{account_data.get('account_id', '')}</tns:account_id>
                <tns:account_type>{account_data.get('account_type', '')}</tns:account_type>
                <tns:customer_id>{account_data.get('customer_id', '')}</tns:customer_id>
                <tns:account_name>{account_data.get('account_name', '')}</tns:account_name>
                <tns:account_status>{account_data.get('account_status', '')}</tns:account_status>
                <tns:branch_code>{account_data.get('branch_code', '')}</tns:branch_code>
                <tns:account_officer>{account_data.get('account_officer', '')}</tns:account_officer>
                <tns:tax_id>{account_data.get('tax_id', '')}</tns:tax_id>
            </tns:account_data>
        </tns:update_account>"""
        
        envelope = self._create_soap_envelope(body)
        response = self._send_soap_request(envelope)
        return self._parse_response(response)

    def delete_account(self, account_id):
        """Delete account"""
        body = f"""
        <tns:delete_account>
            <tns:account_id>{account_id}</tns:account_id>
        </tns:delete_account>"""
        
        envelope = self._create_soap_envelope(body)
        response = self._send_soap_request(envelope)
        return self._parse_response(response)

    def get_account_balance(self, account_id):
        """Get account balance"""
        body = f"""
        <tns:get_account_balance>
            <tns:account_id>{account_id}</tns:account_id>
        </tns:get_account_balance>"""
        
        envelope = self._create_soap_envelope(body)
        response = self._send_soap_request(envelope)
        return self._parse_response(response)

    def validate_transaction(self, account_id, amount, transaction_type):
        """Validate transaction"""
        body = f"""
        <tns:validate_transaction>
            <tns:account_id>{account_id}</tns:account_id>
            <tns:amount>{amount}</tns:amount>
            <tns:transaction_type>{transaction_type}</tns:transaction_type>
        </tns:validate_transaction>"""
        
        envelope = self._create_soap_envelope(body)
        response = self._send_soap_request(envelope)
        return self._parse_response(response)

    def _parse_response(self, response_xml):
        """Parse SOAP response"""
        try:
            root = etree.fromstring(response_xml.encode('utf-8'))
            # Extract response data - simplified parsing
            body = root.find('.//{http://schemas.xmlsoap.org/soap/envelope/}Body')
            if body is not None:
                return etree.tostring(body, pretty_print=True, encoding='unicode')
            return response_xml
        except Exception as e:
            return f"Error parsing response: {e}"


def example_usage():
    """Example usage of the SOAP client"""
    client = AccountSOAPClient()
    
    try:
        # Example: Create a new account
        account_data = {
            'account_id': 'ACC123456789',
            'account_type': 'CA',  # Cash Account
            'customer_id': 'CUST12345',
            'account_name': 'John Doe Trading Account',
            'account_status': 'A',  # Active
            'branch_code': 'BR01',
            'account_officer': 'OFFICER1',
            'tax_id': '123-45-6789'
        }
        
        print("Creating account...")
        create_result = client.create_account(account_data)
        print(f"Create Result: {create_result}")
        
        # Example: Get account information
        print("\nGetting account information...")
        get_result = client.get_account('ACC123456789')
        print(f"Get Result: {get_result}")
        
        # Example: Get account balance
        print("\nGetting account balance...")
        balance_result = client.get_account_balance('ACC123456789')
        print(f"Balance Result: {balance_result}")
        
        # Example: Validate transaction
        print("\nValidating transaction...")
        validate_result = client.validate_transaction('ACC123456789', 1000.00, 'WTH')
        print(f"Validation Result: {validate_result}")
        
    except Exception as e:
        print(f"Error: {e}")


if __name__ == '__main__':
    example_usage()
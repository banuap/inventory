#!/usr/bin/env python3
"""
Demo script to populate the inventory sensor system with sample data
"""

import requests
import json
import time

BASE_URL = "http://localhost:5000/api"

# Sample sensor data
SAMPLE_SENSORS = [
    {
        'inventory_id': 'TEMP-001',
        'model': 'ThermoMax Pro 2000',
        'serial_number': 'TM-2000-001',
        'manufacturer': 'ThermoTech Industries',
        'sensor_type': 'Temperature',
        'location': 'Warehouse A - Zone 1',
        'status': 'Active'
    },
    {
        'inventory_id': 'HUMID-001',
        'model': 'HumidSense Elite',
        'serial_number': 'HS-E-001',
        'manufacturer': 'MoistureTech Corp',
        'sensor_type': 'Humidity',
        'location': 'Warehouse A - Zone 2',
        'status': 'Active'
    },
    {
        'inventory_id': 'PRESS-001',
        'model': 'PressureGuard 500',
        'serial_number': 'PG-500-001',
        'manufacturer': 'PressureTech Solutions',
        'sensor_type': 'Pressure',
        'location': 'Factory Floor - Section B',
        'status': 'Maintenance'
    },
    {
        'inventory_id': 'TEMP-002',
        'model': 'ThermoMax Pro 2000',
        'serial_number': 'TM-2000-002',
        'manufacturer': 'ThermoTech Industries',
        'sensor_type': 'Temperature',
        'location': 'Cold Storage - Unit 1',
        'status': 'Active'
    },
    {
        'inventory_id': 'MOTION-001',
        'model': 'MotionDetect X1',
        'serial_number': 'MD-X1-001',
        'manufacturer': 'SecureSense Ltd',
        'sensor_type': 'Motion',
        'location': 'Security Perimeter - Gate A',
        'status': 'Active'
    },
    {
        'inventory_id': 'LIGHT-001',
        'model': 'LuminaSense 350',
        'serial_number': 'LS-350-001',
        'manufacturer': 'BrightTech Instruments',
        'sensor_type': 'Light',
        'location': 'Greenhouse - Section 1',
        'status': 'Inactive'
    },
    {
        'inventory_id': 'VIBRAT-001',
        'model': 'VibroMonitor Pro',
        'serial_number': 'VM-P-001',
        'manufacturer': 'VibrationTech Systems',
        'sensor_type': 'Vibration',
        'location': 'Manufacturing Line 1',
        'status': 'Active'
    },
    {
        'inventory_id': 'TEMP-003',
        'model': 'ThermoBasic 100',
        'serial_number': 'TB-100-001',
        'manufacturer': 'BasicSensor Co',
        'sensor_type': 'Temperature',
        'location': 'Office Building - Floor 2',
        'status': 'Retired'
    }
]

def wait_for_server():
    """Wait for the server to be available"""
    print("Waiting for server to be available...")
    max_attempts = 30
    for attempt in range(max_attempts):
        try:
            response = requests.get("http://localhost:5000/")
            if response.status_code == 200:
                print("Server is ready!")
                return True
        except requests.exceptions.ConnectionError:
            pass
        
        time.sleep(1)
        print(f"Attempt {attempt + 1}/{max_attempts}...")
    
    print("Server did not become available in time.")
    return False

def create_sample_sensors():
    """Create sample sensors in the system"""
    print("Creating sample sensors...")
    created_sensors = []
    
    for sensor_data in SAMPLE_SENSORS:
        try:
            response = requests.post(f"{BASE_URL}/sensors", json=sensor_data)
            if response.status_code == 201:
                sensor = response.json()
                created_sensors.append(sensor)
                print(f"✓ Created sensor: {sensor['inventory_id']} - {sensor['model']}")
            else:
                print(f"✗ Failed to create sensor {sensor_data['inventory_id']}: {response.text}")
        except Exception as e:
            print(f"✗ Error creating sensor {sensor_data['inventory_id']}: {e}")
    
    return created_sensors

def demonstrate_updates(created_sensors):
    """Demonstrate updating sensors and tracking history"""
    print("\nDemonstrating sensor updates...")
    
    if len(created_sensors) < 2:
        print("Not enough sensors created for demo.")
        return
    
    # Update location of first sensor
    sensor1 = created_sensors[0]
    update_data = {
        'location': 'Warehouse B - Zone 3',
        'changed_by': 'demo_script'
    }
    
    try:
        response = requests.put(f"{BASE_URL}/sensors/{sensor1['id']}", json=update_data)
        if response.status_code == 200:
            print(f"✓ Updated location of sensor {sensor1['inventory_id']}")
        else:
            print(f"✗ Failed to update sensor: {response.text}")
    except Exception as e:
        print(f"✗ Error updating sensor: {e}")
    
    # Update status of second sensor
    if len(created_sensors) > 1:
        sensor2 = created_sensors[1]
        update_data = {
            'status': 'Maintenance',
            'status_reason': 'Scheduled calibration',
            'changed_by': 'demo_script'
        }
        
        try:
            response = requests.put(f"{BASE_URL}/sensors/{sensor2['id']}", json=update_data)
            if response.status_code == 200:
                print(f"✓ Updated status of sensor {sensor2['inventory_id']}")
            else:
                print(f"✗ Failed to update sensor: {response.text}")
        except Exception as e:
            print(f"✗ Error updating sensor: {e}")

def show_statistics():
    """Display system statistics"""
    print("\nSystem Statistics:")
    try:
        response = requests.get(f"{BASE_URL}/sensors/stats")
        if response.status_code == 200:
            stats = response.json()
            print(f"Total Sensors: {stats['total_sensors']}")
            print("Status Breakdown:")
            for status, count in stats['status_breakdown'].items():
                print(f"  {status.title()}: {count}")
            print("By Type:")
            for sensor_type, count in stats['by_type'].items():
                print(f"  {sensor_type}: {count}")
        else:
            print(f"Failed to get statistics: {response.text}")
    except Exception as e:
        print(f"Error getting statistics: {e}")

def demonstrate_search():
    """Demonstrate search functionality"""
    print("\nDemonstrating search functionality...")
    
    # Search for temperature sensors
    try:
        response = requests.get(f"{BASE_URL}/sensors/search", params={'q': 'temperature'})
        if response.status_code == 200:
            sensors = response.json()
            print(f"Found {len(sensors)} temperature sensors")
        else:
            print(f"Search failed: {response.text}")
    except Exception as e:
        print(f"Search error: {e}")
    
    # Filter by status
    try:
        response = requests.get(f"{BASE_URL}/sensors", params={'status': 'Active'})
        if response.status_code == 200:
            sensors = response.json()
            print(f"Found {len(sensors)} active sensors")
        else:
            print(f"Filter failed: {response.text}")
    except Exception as e:
        print(f"Filter error: {e}")

def main():
    print("=== Inventory Sensor System Demo ===")
    
    if not wait_for_server():
        print("Cannot connect to server. Please start the server first with: python app.py")
        return
    
    created_sensors = create_sample_sensors()
    
    if created_sensors:
        demonstrate_updates(created_sensors)
        show_statistics()
        demonstrate_search()
        
        print(f"\n=== Demo Complete ===")
        print(f"Created {len(created_sensors)} sample sensors")
        print("You can now use the CLI tool to interact with the system:")
        print("  python cli.py list                    # List all sensors")
        print("  python cli.py stats                   # Show statistics")
        print("  python cli.py search temperature      # Search sensors")
        print("  python cli.py get 1                   # Get sensor details")
        print("  python cli.py --help                  # Show all commands")
    else:
        print("Failed to create sample data.")

if __name__ == '__main__':
    main()
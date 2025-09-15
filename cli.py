#!/usr/bin/env python3
"""
Command Line Interface for the Inventory Sensor Management System
This CLI provides a simple way to interact with the sensor inventory API
"""

import requests
import json
import argparse
import sys
from datetime import datetime

BASE_URL = "http://localhost:5000/api"

def format_sensor(sensor):
    """Format sensor data for display"""
    return f"""
Sensor ID: {sensor['id']}
Inventory ID: {sensor['inventory_id']}
Model: {sensor['model']}
Serial Number: {sensor['serial_number']}
Manufacturer: {sensor['manufacturer']}
Type: {sensor['sensor_type']}
Location: {sensor['location']}
Status: {sensor['status']}
Created: {sensor['created_at']}
Updated: {sensor['updated_at']}
"""

def list_sensors(args):
    """List all sensors with optional filtering"""
    params = {}
    if args.status:
        params['status'] = args.status
    if args.type:
        params['type'] = args.type
    if args.manufacturer:
        params['manufacturer'] = args.manufacturer
    if args.location:
        params['location'] = args.location
    
    try:
        response = requests.get(f"{BASE_URL}/sensors", params=params)
        response.raise_for_status()
        sensors = response.json()
        
        if not sensors:
            print("No sensors found.")
            return
        
        print(f"\nFound {len(sensors)} sensor(s):")
        for sensor in sensors:
            print(format_sensor(sensor))
            print("-" * 50)
    
    except requests.exceptions.ConnectionError:
        print("Error: Could not connect to the sensor API. Make sure the server is running.")
        sys.exit(1)
    except requests.exceptions.RequestException as e:
        print(f"Error: {e}")
        sys.exit(1)

def add_sensor(args):
    """Add a new sensor to the inventory"""
    sensor_data = {
        'inventory_id': args.inventory_id,
        'model': args.model,
        'serial_number': args.serial_number,
        'manufacturer': args.manufacturer,
        'sensor_type': args.type,
        'location': args.location,
        'status': args.status or 'Active'
    }
    
    try:
        response = requests.post(f"{BASE_URL}/sensors", json=sensor_data)
        
        if response.status_code == 201:
            sensor = response.json()
            print("Sensor created successfully!")
            print(format_sensor(sensor))
        elif response.status_code == 409:
            error_data = response.json()
            print(f"Error: {error_data['error']}")
        else:
            response.raise_for_status()
    
    except requests.exceptions.ConnectionError:
        print("Error: Could not connect to the sensor API. Make sure the server is running.")
        sys.exit(1)
    except requests.exceptions.RequestException as e:
        print(f"Error: {e}")
        sys.exit(1)

def get_sensor(args):
    """Get details of a specific sensor"""
    try:
        response = requests.get(f"{BASE_URL}/sensors/{args.id}")
        
        if response.status_code == 200:
            sensor = response.json()
            print(format_sensor(sensor))
        elif response.status_code == 404:
            print(f"Error: Sensor with ID {args.id} not found.")
        else:
            response.raise_for_status()
    
    except requests.exceptions.ConnectionError:
        print("Error: Could not connect to the sensor API. Make sure the server is running.")
        sys.exit(1)
    except requests.exceptions.RequestException as e:
        print(f"Error: {e}")
        sys.exit(1)

def update_sensor(args):
    """Update a sensor's information"""
    update_data = {}
    
    if args.location:
        update_data['location'] = args.location
    if args.status:
        update_data['status'] = args.status
    if args.model:
        update_data['model'] = args.model
    if args.manufacturer:
        update_data['manufacturer'] = args.manufacturer
    if args.type:
        update_data['sensor_type'] = args.type
    
    if args.reason:
        update_data['status_reason'] = args.reason
    
    if not update_data:
        print("Error: No update fields specified.")
        return
    
    try:
        response = requests.put(f"{BASE_URL}/sensors/{args.id}", json=update_data)
        
        if response.status_code == 200:
            sensor = response.json()
            print("Sensor updated successfully!")
            print(format_sensor(sensor))
        elif response.status_code == 404:
            print(f"Error: Sensor with ID {args.id} not found.")
        else:
            response.raise_for_status()
    
    except requests.exceptions.ConnectionError:
        print("Error: Could not connect to the sensor API. Make sure the server is running.")
        sys.exit(1)
    except requests.exceptions.RequestException as e:
        print(f"Error: {e}")
        sys.exit(1)

def delete_sensor(args):
    """Delete a sensor from the inventory"""
    if not args.force:
        confirm = input(f"Are you sure you want to delete sensor {args.id}? (y/N): ")
        if confirm.lower() != 'y':
            print("Deletion cancelled.")
            return
    
    try:
        response = requests.delete(f"{BASE_URL}/sensors/{args.id}")
        
        if response.status_code == 200:
            print(f"Sensor {args.id} deleted successfully.")
        elif response.status_code == 404:
            print(f"Error: Sensor with ID {args.id} not found.")
        else:
            response.raise_for_status()
    
    except requests.exceptions.ConnectionError:
        print("Error: Could not connect to the sensor API. Make sure the server is running.")
        sys.exit(1)
    except requests.exceptions.RequestException as e:
        print(f"Error: {e}")
        sys.exit(1)

def search_sensors(args):
    """Search sensors by query"""
    try:
        response = requests.get(f"{BASE_URL}/sensors/search", params={'q': args.query})
        response.raise_for_status()
        sensors = response.json()
        
        if not sensors:
            print(f"No sensors found matching '{args.query}'.")
            return
        
        print(f"\nFound {len(sensors)} sensor(s) matching '{args.query}':")
        for sensor in sensors:
            print(format_sensor(sensor))
            print("-" * 50)
    
    except requests.exceptions.ConnectionError:
        print("Error: Could not connect to the sensor API. Make sure the server is running.")
        sys.exit(1)
    except requests.exceptions.RequestException as e:
        print(f"Error: {e}")
        sys.exit(1)

def show_stats(args):
    """Show sensor statistics"""
    try:
        response = requests.get(f"{BASE_URL}/sensors/stats")
        response.raise_for_status()
        stats = response.json()
        
        print("\n=== Sensor Inventory Statistics ===")
        print(f"Total Sensors: {stats['total_sensors']}")
        print(f"\nStatus Breakdown:")
        for status, count in stats['status_breakdown'].items():
            print(f"  {status.title()}: {count}")
        
        if stats['by_type']:
            print(f"\nBy Type:")
            for sensor_type, count in stats['by_type'].items():
                print(f"  {sensor_type}: {count}")
        
        if stats['by_manufacturer']:
            print(f"\nBy Manufacturer:")
            for manufacturer, count in stats['by_manufacturer'].items():
                print(f"  {manufacturer}: {count}")
        print()
    
    except requests.exceptions.ConnectionError:
        print("Error: Could not connect to the sensor API. Make sure the server is running.")
        sys.exit(1)
    except requests.exceptions.RequestException as e:
        print(f"Error: {e}")
        sys.exit(1)

def show_history(args):
    """Show location or status history for a sensor"""
    try:
        if args.type == 'location':
            response = requests.get(f"{BASE_URL}/sensors/{args.id}/location-history")
        else:
            response = requests.get(f"{BASE_URL}/sensors/{args.id}/status-history")
        
        if response.status_code == 200:
            history = response.json()
            if not history:
                print(f"No {args.type} history found for sensor {args.id}.")
                return
            
            print(f"\n{args.type.title()} History for Sensor {args.id}:")
            for record in history:
                if args.type == 'location':
                    print(f"  {record['changed_at']}: {record['previous_location']} → {record['new_location']}")
                else:
                    print(f"  {record['changed_at']}: {record['previous_status']} → {record['new_status']}")
                    if record['reason']:
                        print(f"    Reason: {record['reason']}")
        elif response.status_code == 404:
            print(f"Error: Sensor with ID {args.id} not found.")
        else:
            response.raise_for_status()
    
    except requests.exceptions.ConnectionError:
        print("Error: Could not connect to the sensor API. Make sure the server is running.")
        sys.exit(1)
    except requests.exceptions.RequestException as e:
        print(f"Error: {e}")
        sys.exit(1)

def main():
    parser = argparse.ArgumentParser(description='Inventory Sensor Management CLI')
    subparsers = parser.add_subparsers(dest='command', help='Available commands')
    
    # List sensors command
    list_parser = subparsers.add_parser('list', help='List sensors')
    list_parser.add_argument('--status', help='Filter by status')
    list_parser.add_argument('--type', help='Filter by sensor type')
    list_parser.add_argument('--manufacturer', help='Filter by manufacturer')
    list_parser.add_argument('--location', help='Filter by location')
    list_parser.set_defaults(func=list_sensors)
    
    # Add sensor command
    add_parser = subparsers.add_parser('add', help='Add a new sensor')
    add_parser.add_argument('inventory_id', help='Unique inventory ID')
    add_parser.add_argument('model', help='Sensor model')
    add_parser.add_argument('serial_number', help='Serial number')
    add_parser.add_argument('manufacturer', help='Manufacturer')
    add_parser.add_argument('type', help='Sensor type')
    add_parser.add_argument('location', help='Current location')
    add_parser.add_argument('--status', default='Active', help='Initial status (default: Active)')
    add_parser.set_defaults(func=add_sensor)
    
    # Get sensor command
    get_parser = subparsers.add_parser('get', help='Get sensor details')
    get_parser.add_argument('id', type=int, help='Sensor ID')
    get_parser.set_defaults(func=get_sensor)
    
    # Update sensor command
    update_parser = subparsers.add_parser('update', help='Update sensor')
    update_parser.add_argument('id', type=int, help='Sensor ID')
    update_parser.add_argument('--location', help='New location')
    update_parser.add_argument('--status', help='New status')
    update_parser.add_argument('--model', help='New model')
    update_parser.add_argument('--manufacturer', help='New manufacturer')
    update_parser.add_argument('--type', help='New sensor type')
    update_parser.add_argument('--reason', help='Reason for status change')
    update_parser.set_defaults(func=update_sensor)
    
    # Delete sensor command
    delete_parser = subparsers.add_parser('delete', help='Delete sensor')
    delete_parser.add_argument('id', type=int, help='Sensor ID')
    delete_parser.add_argument('--force', action='store_true', help='Skip confirmation')
    delete_parser.set_defaults(func=delete_sensor)
    
    # Search sensors command
    search_parser = subparsers.add_parser('search', help='Search sensors')
    search_parser.add_argument('query', help='Search query')
    search_parser.set_defaults(func=search_sensors)
    
    # Stats command
    stats_parser = subparsers.add_parser('stats', help='Show statistics')
    stats_parser.set_defaults(func=show_stats)
    
    # History command
    history_parser = subparsers.add_parser('history', help='Show sensor history')
    history_parser.add_argument('id', type=int, help='Sensor ID')
    history_parser.add_argument('type', choices=['location', 'status'], help='History type')
    history_parser.set_defaults(func=show_history)
    
    args = parser.parse_args()
    
    if not args.command:
        parser.print_help()
        return
    
    args.func(args)

if __name__ == '__main__':
    main()
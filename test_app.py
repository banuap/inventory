"""
Test suite for the Inventory Sensor Management System
"""
import pytest
import json
from app import app, db, Sensor, LocationHistory, StatusHistory

@pytest.fixture
def client():
    app.config['TESTING'] = True
    app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite:///:memory:'
    
    with app.test_client() as client:
        with app.app_context():
            db.create_all()
            yield client
            db.session.remove()
            db.drop_all()

@pytest.fixture
def sample_sensor_data():
    return {
        'inventory_id': 'SENS-001',
        'model': 'TempSensor Pro',
        'serial_number': 'TS123456',
        'manufacturer': 'SensorTech Inc',
        'sensor_type': 'Temperature',
        'location': 'Warehouse A - Zone 1',
        'status': 'Active'
    }

def test_index_endpoint(client):
    """Test the index endpoint returns API information"""
    response = client.get('/')
    assert response.status_code == 200
    data = json.loads(response.data)
    assert 'message' in data
    assert 'Inventory Sensor Management API' in data['message']

def test_create_sensor(client, sample_sensor_data):
    """Test creating a new sensor"""
    response = client.post('/api/sensors',
                          data=json.dumps(sample_sensor_data),
                          content_type='application/json')
    
    assert response.status_code == 201
    data = json.loads(response.data)
    assert data['inventory_id'] == sample_sensor_data['inventory_id']
    assert data['model'] == sample_sensor_data['model']
    assert data['serial_number'] == sample_sensor_data['serial_number']

def test_create_sensor_missing_fields(client):
    """Test creating a sensor with missing required fields"""
    incomplete_data = {
        'inventory_id': 'SENS-002',
        'model': 'Test Sensor'
        # Missing required fields
    }
    
    response = client.post('/api/sensors',
                          data=json.dumps(incomplete_data),
                          content_type='application/json')
    
    assert response.status_code == 400
    data = json.loads(response.data)
    assert 'error' in data

def test_get_sensors_empty(client):
    """Test getting sensors when database is empty"""
    response = client.get('/api/sensors')
    assert response.status_code == 200
    data = json.loads(response.data)
    assert data == []

def test_get_sensors_with_data(client, sample_sensor_data):
    """Test getting sensors when data exists"""
    # First create a sensor
    client.post('/api/sensors',
               data=json.dumps(sample_sensor_data),
               content_type='application/json')
    
    # Then retrieve all sensors
    response = client.get('/api/sensors')
    assert response.status_code == 200
    data = json.loads(response.data)
    assert len(data) == 1
    assert data[0]['inventory_id'] == sample_sensor_data['inventory_id']

def test_get_sensor_by_id(client, sample_sensor_data):
    """Test getting a specific sensor by ID"""
    # Create a sensor
    create_response = client.post('/api/sensors',
                                 data=json.dumps(sample_sensor_data),
                                 content_type='application/json')
    
    created_sensor = json.loads(create_response.data)
    sensor_id = created_sensor['id']
    
    # Get the sensor by ID
    response = client.get(f'/api/sensors/{sensor_id}')
    assert response.status_code == 200
    data = json.loads(response.data)
    assert data['id'] == sensor_id
    assert data['inventory_id'] == sample_sensor_data['inventory_id']

def test_get_nonexistent_sensor(client):
    """Test getting a sensor that doesn't exist"""
    response = client.get('/api/sensors/999')
    assert response.status_code == 404

def test_update_sensor(client, sample_sensor_data):
    """Test updating a sensor"""
    # Create a sensor
    create_response = client.post('/api/sensors',
                                 data=json.dumps(sample_sensor_data),
                                 content_type='application/json')
    
    created_sensor = json.loads(create_response.data)
    sensor_id = created_sensor['id']
    
    # Update the sensor
    update_data = {
        'location': 'Warehouse B - Zone 2',
        'status': 'Maintenance',
        'status_reason': 'Scheduled calibration'
    }
    
    response = client.put(f'/api/sensors/{sensor_id}',
                         data=json.dumps(update_data),
                         content_type='application/json')
    
    assert response.status_code == 200
    data = json.loads(response.data)
    assert data['location'] == update_data['location']
    assert data['status'] == update_data['status']

def test_delete_sensor(client, sample_sensor_data):
    """Test deleting a sensor"""
    # Create a sensor
    create_response = client.post('/api/sensors',
                                 data=json.dumps(sample_sensor_data),
                                 content_type='application/json')
    
    created_sensor = json.loads(create_response.data)
    sensor_id = created_sensor['id']
    
    # Delete the sensor
    response = client.delete(f'/api/sensors/{sensor_id}')
    assert response.status_code == 200
    
    # Verify it's deleted
    get_response = client.get(f'/api/sensors/{sensor_id}')
    assert get_response.status_code == 404

def test_search_sensors(client, sample_sensor_data):
    """Test searching sensors"""
    # Create a sensor
    client.post('/api/sensors',
               data=json.dumps(sample_sensor_data),
               content_type='application/json')
    
    # Search by model
    response = client.get('/api/sensors/search?q=TempSensor')
    assert response.status_code == 200
    data = json.loads(response.data)
    assert len(data) == 1
    assert data[0]['model'] == sample_sensor_data['model']

def test_filter_sensors_by_status(client, sample_sensor_data):
    """Test filtering sensors by status"""
    # Create sensors with different statuses
    sensor1 = sample_sensor_data.copy()
    sensor1['inventory_id'] = 'SENS-001'
    sensor1['serial_number'] = 'TS123456'
    sensor1['status'] = 'Active'
    
    sensor2 = sample_sensor_data.copy()
    sensor2['inventory_id'] = 'SENS-002'
    sensor2['serial_number'] = 'TS123457'
    sensor2['status'] = 'Maintenance'
    
    client.post('/api/sensors', data=json.dumps(sensor1), content_type='application/json')
    client.post('/api/sensors', data=json.dumps(sensor2), content_type='application/json')
    
    # Filter by Active status
    response = client.get('/api/sensors?status=Active')
    assert response.status_code == 200
    data = json.loads(response.data)
    assert len(data) == 1
    assert data[0]['status'] == 'Active'

def test_get_sensor_stats(client, sample_sensor_data):
    """Test getting sensor statistics"""
    # Create a sensor
    client.post('/api/sensors',
               data=json.dumps(sample_sensor_data),
               content_type='application/json')
    
    response = client.get('/api/sensors/stats')
    assert response.status_code == 200
    data = json.loads(response.data)
    assert 'total_sensors' in data
    assert 'status_breakdown' in data
    assert 'by_type' in data
    assert 'by_manufacturer' in data
    assert data['total_sensors'] == 1

def test_duplicate_inventory_id(client, sample_sensor_data):
    """Test creating sensors with duplicate inventory IDs"""
    # Create first sensor
    response1 = client.post('/api/sensors',
                           data=json.dumps(sample_sensor_data),
                           content_type='application/json')
    assert response1.status_code == 201
    
    # Try to create second sensor with same inventory ID
    duplicate_data = sample_sensor_data.copy()
    duplicate_data['serial_number'] = 'DIFFERENT123'  # Different serial but same inventory ID
    
    response2 = client.post('/api/sensors',
                           data=json.dumps(duplicate_data),
                           content_type='application/json')
    assert response2.status_code == 409
    data = json.loads(response2.data)
    assert 'error' in data
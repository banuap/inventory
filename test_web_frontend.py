"""
Simple test to verify web frontend functionality
"""
import pytest
from app import app, db, User, Sensor, create_default_users

@pytest.fixture
def client():
    """Create test client with in-memory database"""
    app.config['TESTING'] = True
    app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite:///:memory:'
    app.config['WTF_CSRF_ENABLED'] = False
    
    with app.test_client() as client:
        with app.app_context():
            db.create_all()
            create_default_users()
        yield client

def test_homepage_loads(client):
    """Test that homepage loads correctly"""
    response = client.get('/')
    assert response.status_code == 200
    assert b'Inventory Management System' in response.data
    assert b'Login to Dashboard' in response.data

def test_login_page_loads(client):
    """Test that login page loads correctly"""
    response = client.get('/login')
    assert response.status_code == 200
    assert b'Login' in response.data
    assert b'Username' in response.data
    assert b'Password' in response.data

def test_login_functionality(client):
    """Test login with valid credentials"""
    response = client.post('/login', data={
        'username': 'admin',
        'password': 'admin123'
    }, follow_redirects=True)
    assert response.status_code == 200
    assert b'Welcome back, admin!' in response.data
    assert b'Dashboard' in response.data

def test_login_invalid_credentials(client):
    """Test login with invalid credentials"""
    response = client.post('/login', data={
        'username': 'invalid',
        'password': 'wrong'
    })
    assert response.status_code == 200
    assert b'Invalid username or password' in response.data

def test_dashboard_requires_login(client):
    """Test that dashboard requires authentication"""
    response = client.get('/dashboard')
    assert response.status_code == 302  # Redirect to login
    
def test_dashboard_with_admin_login(client):
    """Test dashboard access with admin login"""
    # Create a test sensor for this test
    with app.app_context():
        sensor = Sensor(
            inventory_id='TEST-001',
            model='Test Sensor',
            serial_number='TEST123',
            manufacturer='Test Corp',
            sensor_type='Temperature',
            location='Test Location',
            status='Active'
        )
        db.session.add(sensor)
        db.session.commit()
    
    # Login as admin
    client.post('/login', data={
        'username': 'admin',
        'password': 'admin123'
    })
    
    response = client.get('/dashboard')
    assert response.status_code == 200
    assert b'Dashboard' in response.data
    assert b'Admin' in response.data
    assert b'TEST-001' in response.data

def test_dashboard_with_technician_login(client):
    """Test dashboard access with technician login (role-based filtering)"""
    # Login as technician
    client.post('/login', data={
        'username': 'technician',
        'password': 'tech123'
    })
    
    response = client.get('/dashboard')
    assert response.status_code == 200
    assert b'Dashboard' in response.data
    assert b'Technician' in response.data
    assert b'Active & Maintenance Only' in response.data

def test_sensor_detail_access(client):
    """Test sensor detail page access"""
    # Create a test sensor for this test
    with app.app_context():
        sensor = Sensor(
            inventory_id='TEST-002',
            model='Test Sensor 2',
            serial_number='TEST456',
            manufacturer='Test Corp',
            sensor_type='Temperature',
            location='Test Location',
            status='Active'
        )
        db.session.add(sensor)
        db.session.commit()
        sensor_id = sensor.id
    
    # Login as admin
    client.post('/login', data={
        'username': 'admin',
        'password': 'admin123'
    })
    
    response = client.get(f'/sensor/{sensor_id}')
    assert response.status_code == 200
    assert b'Sensor Details' in response.data
    assert b'TEST-002' in response.data

def test_logout_functionality(client):
    """Test logout functionality"""
    # Login first
    client.post('/login', data={
        'username': 'admin',
        'password': 'admin123'
    })
    
    response = client.get('/logout', follow_redirects=True)
    assert response.status_code == 200
    assert b'You have been logged out' in response.data
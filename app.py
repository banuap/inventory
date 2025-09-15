"""
Inventory Sensor Management System
A simple Flask-based API for managing sensor inventory
"""

from flask import Flask, request, jsonify
from flask_sqlalchemy import SQLAlchemy
from datetime import datetime
import os

app = Flask(__name__)

# Configuration
app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite:///inventory_sensors.db'
app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False

db = SQLAlchemy(app)

# Database Models
class Sensor(db.Model):
    """Sensor model for inventory management"""
    id = db.Column(db.Integer, primary_key=True)
    inventory_id = db.Column(db.String(50), unique=True, nullable=False)
    model = db.Column(db.String(100), nullable=False)
    serial_number = db.Column(db.String(100), unique=True, nullable=False)
    manufacturer = db.Column(db.String(100), nullable=False)
    sensor_type = db.Column(db.String(50), nullable=False)
    location = db.Column(db.String(200), nullable=False)
    status = db.Column(db.String(20), nullable=False, default='Active')
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    updated_at = db.Column(db.DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    
    # Relationships
    location_history = db.relationship('LocationHistory', backref='sensor', lazy=True, cascade='all, delete-orphan')
    status_history = db.relationship('StatusHistory', backref='sensor', lazy=True, cascade='all, delete-orphan')

    def to_dict(self):
        return {
            'id': self.id,
            'inventory_id': self.inventory_id,
            'model': self.model,
            'serial_number': self.serial_number,
            'manufacturer': self.manufacturer,
            'sensor_type': self.sensor_type,
            'location': self.location,
            'status': self.status,
            'created_at': self.created_at.isoformat() if self.created_at else None,
            'updated_at': self.updated_at.isoformat() if self.updated_at else None
        }

class LocationHistory(db.Model):
    """Track location changes for sensors"""
    id = db.Column(db.Integer, primary_key=True)
    sensor_id = db.Column(db.Integer, db.ForeignKey('sensor.id'), nullable=False)
    previous_location = db.Column(db.String(200), nullable=True)
    new_location = db.Column(db.String(200), nullable=False)
    changed_at = db.Column(db.DateTime, default=datetime.utcnow)
    changed_by = db.Column(db.String(100), nullable=True)

    def to_dict(self):
        return {
            'id': self.id,
            'sensor_id': self.sensor_id,
            'previous_location': self.previous_location,
            'new_location': self.new_location,
            'changed_at': self.changed_at.isoformat() if self.changed_at else None,
            'changed_by': self.changed_by
        }

class StatusHistory(db.Model):
    """Track status changes for sensors"""
    id = db.Column(db.Integer, primary_key=True)
    sensor_id = db.Column(db.Integer, db.ForeignKey('sensor.id'), nullable=False)
    previous_status = db.Column(db.String(20), nullable=True)
    new_status = db.Column(db.String(20), nullable=False)
    reason = db.Column(db.String(500), nullable=True)
    changed_at = db.Column(db.DateTime, default=datetime.utcnow)
    changed_by = db.Column(db.String(100), nullable=True)

    def to_dict(self):
        return {
            'id': self.id,
            'sensor_id': self.sensor_id,
            'previous_status': self.previous_status,
            'new_status': self.new_status,
            'reason': self.reason,
            'changed_at': self.changed_at.isoformat() if self.changed_at else None,
            'changed_by': self.changed_by
        }

# API Routes

@app.route('/', methods=['GET'])
def index():
    """Welcome endpoint with API information"""
    return jsonify({
        'message': 'Inventory Sensor Management API',
        'version': '1.0',
        'endpoints': {
            'sensors': '/api/sensors',
            'sensor_detail': '/api/sensors/<id>',
            'search': '/api/sensors/search',
            'location_history': '/api/sensors/<id>/location-history',
            'status_history': '/api/sensors/<id>/status-history'
        }
    })

@app.route('/api/sensors', methods=['GET'])
def get_sensors():
    """Get all sensors with optional filtering"""
    # Query parameters for filtering
    status = request.args.get('status')
    sensor_type = request.args.get('type')
    manufacturer = request.args.get('manufacturer')
    location = request.args.get('location')
    
    query = Sensor.query
    
    if status:
        query = query.filter(Sensor.status == status)
    if sensor_type:
        query = query.filter(Sensor.sensor_type == sensor_type)
    if manufacturer:
        query = query.filter(Sensor.manufacturer.ilike(f'%{manufacturer}%'))
    if location:
        query = query.filter(Sensor.location.ilike(f'%{location}%'))
    
    sensors = query.all()
    return jsonify([sensor.to_dict() for sensor in sensors])

@app.route('/api/sensors', methods=['POST'])
def create_sensor():
    """Create a new sensor"""
    data = request.get_json()
    
    # Validate required fields
    required_fields = ['inventory_id', 'model', 'serial_number', 'manufacturer', 'sensor_type', 'location']
    for field in required_fields:
        if field not in data or not data[field]:
            return jsonify({'error': f'Missing required field: {field}'}), 400
    
    # Check if inventory_id or serial_number already exists
    existing_sensor = Sensor.query.filter(
        (Sensor.inventory_id == data['inventory_id']) | 
        (Sensor.serial_number == data['serial_number'])
    ).first()
    
    if existing_sensor:
        return jsonify({'error': 'Sensor with this inventory ID or serial number already exists'}), 409
    
    # Create new sensor
    sensor = Sensor(
        inventory_id=data['inventory_id'],
        model=data['model'],
        serial_number=data['serial_number'],
        manufacturer=data['manufacturer'],
        sensor_type=data['sensor_type'],
        location=data['location'],
        status=data.get('status', 'Active')
    )
    
    try:
        db.session.add(sensor)
        db.session.commit()
        return jsonify(sensor.to_dict()), 201
    except Exception as e:
        db.session.rollback()
        return jsonify({'error': 'Failed to create sensor'}), 500

@app.route('/api/sensors/<int:sensor_id>', methods=['GET'])
def get_sensor(sensor_id):
    """Get a specific sensor by ID"""
    sensor = Sensor.query.get_or_404(sensor_id)
    return jsonify(sensor.to_dict())

@app.route('/api/sensors/<int:sensor_id>', methods=['PUT'])
def update_sensor(sensor_id):
    """Update a sensor"""
    sensor = Sensor.query.get_or_404(sensor_id)
    data = request.get_json()
    
    # Track changes for history
    location_changed = 'location' in data and data['location'] != sensor.location
    status_changed = 'status' in data and data['status'] != sensor.status
    
    # Update sensor fields
    updatable_fields = ['model', 'manufacturer', 'sensor_type', 'location', 'status']
    for field in updatable_fields:
        if field in data:
            setattr(sensor, field, data[field])
    
    sensor.updated_at = datetime.utcnow()
    
    try:
        # Record location history if location changed
        if location_changed:
            location_history = LocationHistory(
                sensor_id=sensor.id,
                previous_location=sensor.location,
                new_location=data['location'],
                changed_by=data.get('changed_by', 'system')
            )
            db.session.add(location_history)
        
        # Record status history if status changed
        if status_changed:
            status_history = StatusHistory(
                sensor_id=sensor.id,
                previous_status=sensor.status,
                new_status=data['status'],
                reason=data.get('status_reason', ''),
                changed_by=data.get('changed_by', 'system')
            )
            db.session.add(status_history)
        
        db.session.commit()
        return jsonify(sensor.to_dict())
    except Exception as e:
        db.session.rollback()
        return jsonify({'error': 'Failed to update sensor'}), 500

@app.route('/api/sensors/<int:sensor_id>', methods=['DELETE'])
def delete_sensor(sensor_id):
    """Delete a sensor"""
    sensor = Sensor.query.get_or_404(sensor_id)
    
    try:
        db.session.delete(sensor)
        db.session.commit()
        return jsonify({'message': 'Sensor deleted successfully'})
    except Exception as e:
        db.session.rollback()
        return jsonify({'error': 'Failed to delete sensor'}), 500

@app.route('/api/sensors/search', methods=['GET'])
def search_sensors():
    """Search sensors by various criteria"""
    query_param = request.args.get('q', '')
    
    if not query_param:
        return jsonify([])
    
    # Search across multiple fields
    sensors = Sensor.query.filter(
        (Sensor.inventory_id.ilike(f'%{query_param}%')) |
        (Sensor.model.ilike(f'%{query_param}%')) |
        (Sensor.serial_number.ilike(f'%{query_param}%')) |
        (Sensor.manufacturer.ilike(f'%{query_param}%')) |
        (Sensor.sensor_type.ilike(f'%{query_param}%')) |
        (Sensor.location.ilike(f'%{query_param}%'))
    ).all()
    
    return jsonify([sensor.to_dict() for sensor in sensors])

@app.route('/api/sensors/<int:sensor_id>/location-history', methods=['GET'])
def get_location_history(sensor_id):
    """Get location history for a sensor"""
    sensor = Sensor.query.get_or_404(sensor_id)
    history = LocationHistory.query.filter_by(sensor_id=sensor_id).order_by(LocationHistory.changed_at.desc()).all()
    return jsonify([record.to_dict() for record in history])

@app.route('/api/sensors/<int:sensor_id>/status-history', methods=['GET'])
def get_status_history(sensor_id):
    """Get status history for a sensor"""
    sensor = Sensor.query.get_or_404(sensor_id)
    history = StatusHistory.query.filter_by(sensor_id=sensor_id).order_by(StatusHistory.changed_at.desc()).all()
    return jsonify([record.to_dict() for record in history])

@app.route('/api/sensors/stats', methods=['GET'])
def get_sensor_stats():
    """Get sensor statistics and counts"""
    total_sensors = Sensor.query.count()
    active_sensors = Sensor.query.filter_by(status='Active').count()
    inactive_sensors = Sensor.query.filter_by(status='Inactive').count()
    maintenance_sensors = Sensor.query.filter_by(status='Maintenance').count()
    retired_sensors = Sensor.query.filter_by(status='Retired').count()
    
    # Get sensor counts by type
    type_counts = db.session.query(Sensor.sensor_type, db.func.count(Sensor.id)).group_by(Sensor.sensor_type).all()
    
    # Get sensor counts by manufacturer
    manufacturer_counts = db.session.query(Sensor.manufacturer, db.func.count(Sensor.id)).group_by(Sensor.manufacturer).all()
    
    return jsonify({
        'total_sensors': total_sensors,
        'status_breakdown': {
            'active': active_sensors,
            'inactive': inactive_sensors,
            'maintenance': maintenance_sensors,
            'retired': retired_sensors
        },
        'by_type': dict(type_counts),
        'by_manufacturer': dict(manufacturer_counts)
    })

# Error handlers
@app.errorhandler(404)
def not_found(error):
    return jsonify({'error': 'Resource not found'}), 404

@app.errorhandler(400)
def bad_request(error):
    return jsonify({'error': 'Bad request'}), 400

@app.errorhandler(500)
def internal_error(error):
    return jsonify({'error': 'Internal server error'}), 500

if __name__ == '__main__':
    with app.app_context():
        db.create_all()
    app.run(debug=True, host='0.0.0.0', port=5000)
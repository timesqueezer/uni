import uuid

from sqlalchemy.dialects.postgresql import UUID, JSONB

from fmexp.extensions import db


class User(db.Model):
    __tablename__ = 'users'

    uuid = db.Column(UUID(as_uuid=True), primary_key=True, default=uuid.uuid4)

    email = db.Column(db.String(255), nullable=True, unique=True)

    password_salt = db.Column(db.LargeBinary)
    password_hash = db.Column(db.LargeBinary)

    first_name = db.Column(db.String, nullable=True)
    last_name = db.Column(db.String, nullable=True)

    @property
    def is_authenticated(self):
        pass

    @property
    def is_active(self):
        return self.email is not None

    @property
    def is_anonymous(self):
        return False

    def get_id(self):
        return self.uuid

    def get_json(self):
        data = {}
        for attr in ['email', ]


class DataPoint(db.Model):
    __tablename__ = 'data_points'

    id = db.Column(db.Integer, primary_key=True)

    created = db.Column(db.DateTime, nullable=False)

    user_uuid = db.Column(UUID(as_uuid=True), db.ForeignKey('users.uuid'), nullable=False)
    user = db.relationship('User', backref='datapoints')

    data = db.Column(JSONB)

    def __init__(self, created, user_uuid, data):
        self.created = created
        self.user_uuid = user_uuid
        self.data = data

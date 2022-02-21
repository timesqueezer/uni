import uuid

from sqlalchemy.dialects.postgresql import UUID

from fmexp.extensions import db


class User(db.Model):
    __tablename__ = 'users'

    uuid = db.Column(UUID(as_uuid=True), primary_key=True, default=uuid.uuid4)

    email = db.Column(db.String(255), nullable=False, unique=True)

    password_salt = db.Column(db.LargeBinary)
    password_hash = db.Column(db.LargeBinary)

    @property
    def is_authenticated(self):
        pass

    @property
    def is_active(self):
        return True

    @property
    def is_anonymous(self):
        return False

    def get_id(self):
        return self.uuid

from flask_wtf import FlaskForm
from wtforms import (
    PasswordField,
    EmailField,
    BooleanField,
)
from wtforms.validators import (
    DataRequired,
    Email,
    Length,
    ValidationError,
)

from fmexp.models import User


class UserLoginForm(FlaskForm):
    email = EmailField('Email', validators=[DataRequired(), Email()])
    password = PasswordField('Password', validators=[DataRequired(), Length(min=8)])


class UserRegisterForm(FlaskForm):
    email = EmailField('Email', validators=[DataRequired(), Email()])
    password = PasswordField('Password', validators=[DataRequired(), Length(min=8)])
    password2 = PasswordField('Repeat Password', validators=[DataRequired(), Length(min=8)])
    not_robot = BooleanField('I am not a robot ;)', validators=[DataRequired()])

    def validate_email(form, field):
        existing_user = User.query.filter_by(email=field.data).first()
        if existing_user:
            raise ValidationError('Email already exists')

    def validate_password2(form, field):
        if field.data != form.password.data:
            raise ValidationError('Passwords do not match')

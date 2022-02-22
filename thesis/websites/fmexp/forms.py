from flask_wtf import FlaskForm
from wtforms import (
    PasswordField,
    EmailField,
)
from wtforms.validators import (
    DataRequired,
    Email,
    Length,
)


class UserLoginForm(FlaskForm):
    email = EmailField('Email', validators=[DataRequired(), Email()])
    password = PasswordField('Password', validators=[DataRequired(), Length(min=8)])


class UserRegisterForm(FlaskForm):
    email = EmailField('Email', validators=[DataRequired(), Email()])
    password = PasswordField('Password', validators=[DataRequired(), Length(min=8)])
    password2 = PasswordField('Repeat Password', validators=[DataRequired(), Length(min=8)])

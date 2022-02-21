from flask_wtf import FlaskForm
from wtforms import StringField, EmailField
from wtforms.validators import DataRequired


class UserLoginForm(FlaskForm):
    email = EmailField('email', validators=[DataRequired()])
    password = StringField('password', validators=[DataRequired()])

class UserRegisterForm(FlaskForm):
    email = EmailField('email', validators=[DataRequired(), ])
    password = StringField('password', validators=[DataRequired()])


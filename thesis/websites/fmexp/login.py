from flask import (
    request,
    abort,
    redirect,
    url_for,
)
from flask_login import login_user, current_user

from fmexp.extensions import login_manager
from fmexp.forms import (
    UserLoginForm,
    UserRegisterForm,
)
from fmexp.models import User
from fmexp.main import main
from fmexp.utils import (
    render_template_fmexp,
    is_safe_url,
    json_response,
)


@login_manager.user_loader
def load_user(user_id):
    return User.query.get(user_id)


@main.route('/user')
def user():
    if not current_user.is_active:
        abort(400)

    return json_response(current_user.get_json())


@main.route('/register', methods=['POST'])
def register():
    assert not current_user.is_active

    form = UserRegisterForm()
    if form.validate_on_submit():
        if form.password.data != form.password2.data:
            return json_response({ 'errors': [{ 'password': 'Passwords do not match' }] })

        current_user.email = form.email.data

        db.session.commit()

        return json_response(current_user.get_json())


@main.route('/content/register', methods=['GET'])
def register_content():
    form = UserRegisterForm()
    return render_template_fmexp('register.html', form=form)


@main.route('/login', methods=['POST'])
def login():
    form = UserLoginForm()
    if form.validate_on_submit():
        user = User.query.filter_by(email=form.email).first()
        login_user(user)

        flash('Logged in successfully.')

        next_url = request.args.get('next')

        if not is_safe_url(next_url):
            return abort(400)


@main.route('/content/login', methods=['GET'])
def login_content():
    form = UserLoginForm()
    return render_template_fmexp('login.html', form=form)

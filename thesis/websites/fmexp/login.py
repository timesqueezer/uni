from flask import (
    request,
    abort,
    redirect,
    url_for,
)
from flask_login import login_user

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
)


@login_manager.user_loader
def load_user(user_id):
    return User.query.get(user_id)


@main.route('/register', methods=['GET', 'POST'])
def register():
    form = UserRegisterForm
    return render_template_fmexp('register.html')


@main.route('/login', methods=['GET', 'POST'])
def login():
    form = UserLoginForm()
    if form.validate_on_submit():
        user = User.query.filter_by(email=form.email).first()
        login_user(user)

        flash('Logged in successfully.')

        next_url = request.args.get('next')

        if not is_safe_url(next_url):
            return abort(400)

        return redirect(next_url or url_for('index'))
    return render_template_fmexp('login.html', form=form)

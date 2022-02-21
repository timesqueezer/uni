from flask import Blueprint

from fmexp.utils import render_template_fmexp


main = Blueprint('main', __name__, template_folder='templates', static_folder='static')


@main.route('/')
def home():
    return render_template_fmexp('home.html')

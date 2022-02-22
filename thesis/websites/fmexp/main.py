import os

from flask import Blueprint, send_file

from fmexp.utils import render_template_fmexp


main = Blueprint('main', __name__, template_folder='templates', static_folder='static')


@main.route('/')
def home():
    return render_template_fmexp('home.html')


@main.route('/blog')
def home():
    return render_template_fmexp('blog.html')

@main.route('/dist')
@main.route('/dist/<path:path>')
def webpack_dist(path=None):
    return send_file(os.path.join('templates/frontend/dist/', path))

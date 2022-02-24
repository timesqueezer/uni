import os
import uuid

from dateutil.parser import parse

from flask import Blueprint, send_file, request, abort
from flask_login import current_user, login_user

from fmexp.extensions import db
from fmexp.models import DataPoint, User
from fmexp.utils import render_template_fmexp


main = Blueprint('main', __name__, template_folder='templates', static_folder='static')


@main.route('/')
@main.route('/<path:path>')
def index(path=None):
    return render_template_fmexp('index.html')


@main.route('/user-uuid', methods=['POST'])
def user_uuid():
    print('active', current_user.is_active)
    if current_user.is_authenticated:
        return { 'user_uuid': str(current_user.uuid) }

    already_set_uuid = request.cookies.get('user_uuid')
    if already_set_uuid:
        return { 'user_uuid': already_set_uuid }

    # new_user_uuid = uuid.uuid4()
    new_user = User()
    db.session.add(new_user)
    db.session.commit()

    login_user(new_user)

    new_user_uuid = new_user.uuid
    print('new_user_uuid', new_user_uuid)

    return { 'user_uuid': str(new_user_uuid) }


@main.route('/data-capture', methods=['POST'])
def data_capture():
    payload = request.get_json()

    if not payload.get('meta') or not payload['meta'].get('user_uuid'):
        abort(400)

    if current_user.is_authenticated and \
        str(current_user.uuid) != payload['meta']['user_uuid']:
        abort(400)

    for dp in payload['data']:
        created = parse(dp['dt'])

        new_datapoint = DataPoint(created, payload['meta']['user_uuid'], dp['data'])
        db.session.add(new_datapoint)

    db.session.commit()

    return '', 200


@main.route('/content/blog')
def blog():
    return render_template_fmexp('blog.html')


@main.route('/content/home')
def home():
    return render_template_fmexp('home.html')


@main.route('/dist')
@main.route('/dist/<path:path>')
def webpack_dist(path=None):
    return send_file(os.path.join('templates/frontend/dist/', path))

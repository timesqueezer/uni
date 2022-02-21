from urllib.parse import urlparse, urljoin

from flask import render_template, current_app


def render_template_fmexp(template_name, **kwargs):
    layout_name = current_app.config.get('LAYOUT_NAME', 'layout1')
    layout = current_app.jinja_env.get_template(f'{layout_name}.html')

    return render_template(template_name, layout=layout, **kwargs)


def is_safe_url(target):
    ref_url = urlparse(request.host_url)
    test_url = urlparse(urljoin(request.host_url, target))
    return test_url.scheme in ('http', 'https') and \
           ref_url.netloc == test_url.netloc

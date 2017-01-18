import flask as fl
from app import app
import markdown2 as md


@app.route('/documentation/main')
def documentation_main():
    file_uri = app.config['STATIC_FOLDER'] + r"markdown/documentation_main.md"
    content = open(file_uri, "r")
    content_out = md.markdown(content.read())
    return fl.render_template('html/documentation.html', content=content_out)


@app.route('/documentation/hurricane')
def hurricane_documentation():
    file_uri = app.config['STATIC_FOLDER'] + r"markdown/hurricane_documentation.md"
    content = open(file_uri, "r")
    content_out = md.markdown(content.read(), extras=["tables", "fenced-code-blocks", "code-friendly"])
    return fl.render_template('html/hurricane_documentation.html', content=content_out)


# @app.route('/documentation/noaa_nws23.pdf')
# def get_nws_pdf():
#     nws_pdf_dir = 'Documentation/Hurricane/NWS23/'
#     nws_pdf_name = 'NOAA_NWS23.pdf'
#     return fl.send_from_directory(nws_pdf_dir, nws_pdf_name)
#
#
# @app.route('/documentation/hurricane_nws23.py')
# def get_hurricane_nws_py():
#     nws_pdf_dir = 'hurricane/'
#     nws_pdf_name = 'hurricane_nws23.py'
#     return fl.send_from_directory(nws_pdf_dir, nws_pdf_name)


@app.route('/get_file/<path:file_path>')
def get_file(file_path):
    # Note, this actually lets you download any file...
    return fl.send_from_directory('', file_path)

from flask import current_app as flapy_app
from flask_restful import Resource


class StaticWebFiles(Resource):
    def get(self):
        return flapy_app.send_static_file('index.html')

from flask import Flask
from flask_restful import Api
from flask_cors import CORS

from flapy_disaster.endpoints import v1

import flapy_disaster.endpoints.v1

# API Imports

# logging config

# Flask app

flapy_app = Flask(__name__)

api = Api(flapy_app)
flapy_app.config['ERROR_404_HELP'] = False
CORS(flapy_app, resources={r"/v1/*": {"origins": "*"}})

# Register routes
api_version: str = "v1"

# events
prefix: str = "hurricane"
api.add_resource(v1.hurricane.HurricaneEventHurdat,
                 f'/{api_version}/{prefix}/event/hurdat',
                 endpoint=f"HurricaneEventHurdat_{prefix}_{api_version}")

api.add_resource(v1.hurricane.HurricaneCatalogHurdat,
                 f'/{api_version}/{prefix}/catalog/hurdat',
                 endpoint=f"HurricaneCatalogHurdat_{prefix}_{api_version}")
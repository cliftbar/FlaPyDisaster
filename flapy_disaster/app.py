from marshmallow import missing
from typing import Dict

from autoapi.autoapi import AutoAPI
from webargs.flaskparser import parser, abort

from flask import Flask, Request
from flask_restful import Api
from flask_cors import CORS

from autoapi.registration import app_registration

# logging config

# Flask app
from webargs.multidictproxy import MultiDictProxy
from werkzeug.datastructures import MultiDict

from flapy_disaster.utilities.ConfigurationContainers import AppConfigContainer, Configurations
from flapy_disaster.utilities.ConfigurationLoader import ConfigurationLoader

flapy_app = Flask(__name__)

config_loader_key: str = "fd_config"
config: ConfigurationLoader = ConfigurationLoader()
flapy_app.config[config_loader_key] = config

# API Imports after setting up Flask App
from flapy_disaster.endpoints import v1

api = Api(flapy_app)
flapy_app.config['ERROR_404_HELP'] = False
CORS(flapy_app, resources={r"/v1/*": {"origins": "*"}})

app_config: AppConfigContainer = AppConfigContainer(
    config.get_config(Configurations.app_config)
)
spec: AutoAPI = app_registration(api, app_config.title, app_config.version, app_config.openapi_version)


# This error handler is necessary for usage with Flask-RESTful
@parser.error_handler
def handle_request_parsing_error(err, req, schema, *, error_status_code, error_headers):
    """webargs error handler that uses Flask-RESTful's abort function to return
    a JSON error response to the client.
    """
    abort(error_status_code, errors=err.messages)


# Custom location to allow both query params and json.  query params overrides json
# May be reliant on werkzeug
@parser.location_loader("query_and_json")
def load_query_and_json(request, schema):
    newdata: MultiDict = request.args.copy()
    json_data: Dict = parser.load_json(request, schema)
    if json_data is not missing:
        newdata.update(json_data)
    return MultiDictProxy(newdata, schema)


# Register routes
api_version: str = "v1"
prefix: str

# Status
prefix = "status"
api.add_resource(v1.status.Healthcheck,
                 f"/{api_version}/{prefix}/healthcheck",
                 endpoint=f"Healthcheck_{prefix}_{api_version}")

prefix = "hurricane"
# Events
api.add_resource(v1.hurricane.HurricaneEventEndpoint,
                 f"/{api_version}/{prefix}/event",
                 endpoint=f"HurricaneCatalogEvent_{prefix}_{api_version}")

# Catalogs
api.add_resource(v1.hurricane.HurricaneCatalog,
                 f"/{api_version}/{prefix}/catalog",
                 endpoint=f"HurricaneCatalog_{prefix}_{api_version}")

api.add_resource(v1.hurricane.HurricaneCatalogs,
                 f"/{api_version}/{prefix}/catalogs",
                 endpoint=f"HurricaneCatalogs_{prefix}_{api_version}")

api.add_resource(v1.hurricane.HurricaneCatalogHurdat,
                 f"/{api_version}/{prefix}/catalog/hurdat",
                 endpoint=f"HurricaneCatalogHurdat_{prefix}_{api_version}")



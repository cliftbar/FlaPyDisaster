from pathlib import Path

from marshmallow import missing
from typing import Dict

from webargs.flaskparser import parser, abort

from flask import Flask
from flask_restful import Api
from flask_cors import CORS

from automd.registration import AutoMDApp

# logging config

# Flask app
from webargs.multidictproxy import MultiDictProxy
from werkzeug.datastructures import MultiDict

from flapy_disaster.utilities.ConfigurationContainers import AppConfigContainer, Configurations
from flapy_disaster.utilities.ConfigurationLoader import ConfigurationLoader, config_loader_key

flapy_app = Flask(__name__, static_url_path="", static_folder=str(Path(Path.cwd(), "web", "static")))

config: ConfigurationLoader = ConfigurationLoader()
flapy_app.config[config_loader_key] = config

# API Imports after setting up Flask App
from flapy_disaster.endpoints import v1

api = Api(flapy_app)
CORS(flapy_app, resources={r"/v1/*": {"origins": "*"}})

app_config: AppConfigContainer = AppConfigContainer(
    config.get_config(Configurations.app_config)
)
spec: AutoMDApp = AutoMDApp(api, app_config.title, app_config.app_version, app_config.openapi_version)


# Flask-RESTful setup
flapy_app.config['ERROR_404_HELP'] = False


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

# Static Index
api.add_resource(v1.web.StaticWebFiles,
                 f"/",
                 endpoint=f"StaticWebFiles")

api.add_resource(v1.web.StaticWebFiles,
                 f"/{api_version}",
                 endpoint=f"StaticWebFiles_{api_version}")


# Test
prefix = "test"
api.add_resource(v1.test.Test,
                 f"/{api_version}/{prefix}/test",
                 endpoint=f"Test_{prefix}_{api_version}")


# Status
prefix = "status"
api.add_resource(v1.status.HealthCheck,
                 f"/{api_version}/{prefix}/healthcheck",
                 endpoint=f"HealthCheck_{prefix}_{api_version}")

prefix = "hurricane"
# Events
api.add_resource(v1.hurricane.HurricaneEventEndpoint,
                 f"/{api_version}/{prefix}/event",
                 endpoint=f"HurricaneCatalogEvent_{prefix}_{api_version}")

api.add_resource(v1.hurricane.HurricaneEventRender,
                 f"/{api_version}/{prefix}/event/render",
                 endpoint=f"HurricaneEventRender_{prefix}_{api_version}")

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



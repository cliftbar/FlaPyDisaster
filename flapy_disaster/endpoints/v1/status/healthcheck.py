from typing import Dict

from automd.decorators import automd
from flask_restful import Resource

open_api_catalog_tag: Dict = {"name": "Status"}


class HealthCheck(Resource):
    @automd(summary="Simple health check endpoint",
            description="Returns 200-OK if it can",
            tags=[open_api_catalog_tag])
    def get(self):
        return "OK"

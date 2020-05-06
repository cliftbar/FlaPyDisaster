import logging
import time
from typing import Dict, Any, List

from automd.decorators import automd
from flask import current_app as flapy_app
from flask_restful import Resource

from webargs import fields
from webargs.flaskparser import use_kwargs

from flapy_disaster.services.hurricane.HurricaneCatalog import HurricaneEvent
from flapy_disaster.services.hurricane.HurricaneService import HurricaneService
from flapy_disaster.utilities.ConfigurationLoader import config_loader_key, ConfigurationLoader
from flapy_disaster.utilities.flapy_types import JSON

logger: logging.Logger = logging.getLogger(__name__)


class HurricaneEventEndpoint(Resource):
    get_args = {
        "event_id": fields.String(required=True, description="Hurricane Event ID")
    }

    @automd(get_args,
            summary="Get Hurricane Event",
            description="Get the JSON representation of a Hurricane Event")
    @use_kwargs(get_args, location="query_and_json")
    def get(self, event_id: str) -> JSON:
        config: ConfigurationLoader = flapy_app.config[config_loader_key]
        hurricane_service: HurricaneService = HurricaneService(config)
        event: HurricaneEvent = hurricane_service.get_event(event_id)

        return event.to_json()

    def post(self): pass

    def delete(self): pass

    def put(self): pass


class HurricaneEventRender(Resource):
    get_args: Dict[str, fields.Field] = {
        "render_id": fields.String(required=True)
    }

    @use_kwargs(get_args, location="query_and_json")
    def get(self, render_id: str): pass

    post_args: Dict[str, fields.Field] = {
        "event_id": fields.String(required=True),
        "pixels_per_degree_x": fields.Integer(required=False,
                                              doc_default=10,
                                              summary="Pixels Per Degree Longitude (X)",
                                              description="Model render resolution in the Longitude (X) axis"),
        "pixels_per_degree_y": fields.Integer(required=False,
                                              doc_default=10,
                                              summary="Pixels Per Degree Latitude (Y)",
                                              description="Model render resolution in the Latitude (Y) axis"),
        "radius_max_wind": fields.Float(required=False,
                                        doc_default=10,
                                        summary="Radius of Maximum Wind",
                                        description="Radius of Maximum Wind (in Nautical Miles to use for the render")
    }

    @automd(post_args,
            summary="Create Event Render",
            description="Create a render of an event using the specified parameters")
    @use_kwargs(post_args, location="query_and_json")
    def post(self,
             event_id: str,
             pixels_per_degree_x: int = 10,
             pixels_per_degree_y: int = 10,
             radius_max_wind: float = 15) -> str:
        start_time: float = time.time()
        config: ConfigurationLoader = flapy_app.config[config_loader_key]
        hurricane_service: HurricaneService = HurricaneService(config)
        event: HurricaneEvent = hurricane_service.get_event(event_id)
        event_restult = hurricane_service.calculate_event(event,
                                                          px_per_deg_x=pixels_per_degree_x,
                                                          px_per_deg_y=pixels_per_degree_y,
                                                          radius_max_wind=radius_max_wind)

        hurricane_service.render_event()
        end_time: float = time.time()
        print(f"event render took {int(end_time - start_time)}s")
        return "OK"

    def delete(self): pass

    def put(self): pass


class HurricaneEventsRenders(Resource):
    get_args: Dict[str, fields.Field] = {
        "catalog_id": fields.String(required=False)
    }
    @use_kwargs(get_args, location="query_and_json")
    def get(self, catalog_id: str = None) -> List[str]: pass

    post_args: Dict[str, fields.Field] = {
        "render_id": fields.Field(required=True)
    }

    @use_kwargs(post_args, location="query_and_json")
    def post(self, render_args: List[Any]): pass

    def delete(self): pass

    def put(self): pass

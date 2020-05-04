import logging
from typing import Dict, Any, List

from automd.decorators import automd
from flask_restful import Resource

from webargs import fields
from webargs.flaskparser import use_kwargs

from flapy_disaster.services.hurricane.HurricaneCatalog import HurricaneEvent
from flapy_disaster.services.hurricane.HurricaneService import HurricaneService
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
        hurricane_service: HurricaneService = HurricaneService()
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
        "render_args": fields.Field(required=True)
    }

    @use_kwargs(post_args, location="query_and_json")
    def post(self, render_args: Any) -> str: pass

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

from typing import Dict, List

from autoapi.decorators import autodoc
from flask_restful import Resource
from webargs import fields
from webargs.flaskparser import use_kwargs

from flapy_disaster.services.hurricane.HurricaneService import HurricaneService
from flapy_disaster.utilities.flapy_types import JSON

open_api_catalog_tag: Dict = {"name": "Catalog"}


# TODO: Break into Catalog
class HurricaneCatalog(Resource):
    get_args = {
        "catalog_id": fields.String(required=True)
    }

    @autodoc(summary='Get Disaster Catalog',
             description='Gets the JSON representation of a Flapy Disaster Catalog',
             tags=[open_api_catalog_tag])
    @use_kwargs(get_args, location="query_and_json")
    def get(self, catalog_id: str) -> JSON:
        hurricane_service: HurricaneService = HurricaneService()
        return hurricane_service.get_catalog(catalog_id).to_json()

    def post(self): pass

    def delete(self): pass

    def put(self): pass


# TODO: Break into Catalog
class HurricaneCatalogs(Resource):
    @autodoc(summary='Get Catalog IDs',
             description='Gets the IDs of any available Flapy Disaster catalogs.',
             tags=[open_api_catalog_tag])
    def get(self) -> List[str]:
        hurricane_service: HurricaneService = HurricaneService()
        catalog_ids: List[str] = hurricane_service.get_catalog_ids()
        return catalog_ids

    def post(self): pass

    def delete(self): pass

    def put(self): pass


class HurricaneCatalogHurdat(Resource):
    def get(self): pass

    post_args: Dict[str, fields.Field] = {
        "catalog_unique_name": fields.String(required=True)
    }

    @autodoc(post_args,
             summary="Upload Hurdat Data File",
             description="Upload a Hurdat2 datafile as a new Catalog",
             tags=[open_api_catalog_tag])
    @use_kwargs(post_args, location="query_and_json")
    def post(self, catalog_unique_name: str) -> str:
        catalog_id: str = r"Documentation\Hurricane\HURDAT\hurdat2-1851-2018-041818_with_header.txt"

        hurricane_service: HurricaneService = HurricaneService()
        hurricane_service.load_hurdat_catalog(catalog_unique_name, catalog_id)

        # TODO allow just string returns without value responses
        return "OK"

    def delete(self): pass

    def put(self): pass

from typing import Dict

from flask_restful import Resource
from webargs import fields
from webargs.flaskparser import use_kwargs

from flapy_disaster.models.MongoInterface import MongoInterface
from flapy_disaster.services.hurricane.hurricane_utils import HurdatCatalog
from pathlib import Path

class HurricaneCatalogs(Resource):
    def get(self): pass

    def post(self): pass

    def delete(self): pass

    def put(self): pass


class HurricaneCatalogHurdat(Resource):
    def get(self): pass

    post_args: Dict[str, fields.Field] = {
        "hurdat_catalog_filename": fields.String(required=False),
        "catalog_name": fields.String(required=False)
    }

    @use_kwargs(post_args)
    def post(self, hurdat_catalog_filename: str = "hurdat2-1851-2015-070616_with_header"):

        hurdat_file: Path = Path("Documentation", "Hurricane", "HURDAT", f"{hurdat_catalog_filename}.txt")
        catalog: HurdatCatalog = HurdatCatalog(str(hurdat_file))
        mongo: MongoInterface = MongoInterface()
        # mongo.insert_document("flapy_test", "catalog", {"catalog": "hurdat", "data": "value"})
        doc = mongo.find_one_document("flapy_test", "catalog", {"catalog": "hurdat"})
        print(doc)

        return "OK"

    def delete(self): pass

    def put(self): pass


class HurricaneCatalogEventNames(Resource):
    def get(self): pass

    def post(self): pass

    def delete(self): pass

    def put(self): pass
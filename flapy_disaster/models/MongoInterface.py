from typing import Dict, Optional, Any, List

from pymongo import MongoClient
from pymongo.database import Database
from pymongo.results import InsertOneResult, UpdateResult

from flapy_disaster.utilities.ConfigurationContainers import MongoConfiguration, Configurations
from flapy_disaster.utilities.ConfigurationLoader import ConfigurationLoader
from flapy_disaster.utilities.flapy_types import JSON


class MongoInterface:
    def __init__(self, config: ConfigurationLoader):
        mongo_config: MongoConfiguration = MongoConfiguration(config.get_config(Configurations.mongo))
        self.client: MongoClient = MongoClient(mongo_config.host, mongo_config.port)

    def create_db(self, database: str) -> Database:
        db: Database = self.client[database]
        return db

    def insert_document(self, database: str, table: str, document: Dict) -> str:
        db: Database = self.client[database]
        result: InsertOneResult = db[table].insert_one(document)
        return result.inserted_id

    def upsert_document(self, database: str, table: str, match_filter: JSON, document: Dict) -> int:
        db: Database = self.client[database]
        result: UpdateResult = db[table].replace_one(match_filter, document, upsert=True)
        return result.modified_count

    def find_document(self, database: str, table: str, find_filter: Dict) -> Dict:
        db: Database = self.client[database]
        result: Optional[Any] = db[table].find_one(find_filter)
        return result

    def find_documents(self, database: str, table: str, find_filter: Dict) -> List[JSON]:
        db: Database = self.client[database]
        result: List[JSON] = db[table].find(find_filter)
        return result

    def get_collection_field(self, database: str, table: str, field_key: str) -> List[Any]:
        db: Database = self.client[database]
        result: List[JSON] = db[table].distinct(field_key)
        return result

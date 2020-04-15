from typing import Dict, Optional, Any

from pymongo import MongoClient
from pymongo.database import Database
from pymongo.results import InsertOneResult


class MongoInterface:
    def __init__(self, host: str = "localhost", port: int = 27017):
        self.client: MongoClient = MongoClient(host, port)

    def create_db(self, database: str) -> Database:
        db: Database = self.client[database]
        return db

    def insert_document(self, database: str, table: str, document: Dict) -> str:
        db: Database = self.client[database]
        result: InsertOneResult = db[table].insert_one(document)
        return result.inserted_id

    def find_one_document(self, database: str, table: str, find_filter: Dict) -> Dict:
        db: Database = self.client[database]
        result: Optional[Any] = db[table].find_one(find_filter)
        return result

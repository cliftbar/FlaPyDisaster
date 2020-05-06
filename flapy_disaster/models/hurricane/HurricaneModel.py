from typing import List

from flapy_disaster.models.MongoInterface import MongoInterface

from flapy_disaster.services.hurricane.HurricaneCatalog import HurricaneEvent, FDCatalog
from flapy_disaster.utilities.ConfigurationLoader import ConfigurationLoader
from flapy_disaster.utilities.flapy_types import JSON
from flapy_disaster.utilities.general_utils import FDEnum


class HurricaneModel:
    class HurricaneModelTables(FDEnum):
        catalogs = "catalogs"
        events = "events"

    def __init__(self, config: ConfigurationLoader):
        self.mongo: MongoInterface = MongoInterface(config)
        self.database: str = "flapy_test"

    def get_catalog_ids(self) -> List[str]:
        result: List[str] = self.mongo.get_collection_field(self.database,
                                                            self.HurricaneModelTables.catalogs.value,
                                                            FDCatalog.FDCatalogFields.catalog_id.value)
        return result

    def get_catalog(self, catalog_id: str) -> FDCatalog:
        catalog: FDCatalog = FDCatalog.from_json(
            self.mongo.find_document(self.database,
                                     self.HurricaneModelTables.catalogs.value,
                                     {FDCatalog.FDCatalogFields.catalog_id.value: catalog_id})
        )
        return catalog

    def get_catalogs(self) -> List[FDCatalog]:
        catalog_docs: List[JSON] = self.mongo.find_documents(self.database,
                                                             self.HurricaneModelTables.catalogs.value,
                                                             {})
        catalogs: List[FDCatalog] = [FDCatalog.from_json(catalog)
                                     for catalog
                                     in catalog_docs]
        return catalogs

    def save_catalog(self, catalog: FDCatalog) -> bool:
        self.mongo.upsert_document(self.database,
                                   self.HurricaneModelTables.catalogs.value,
                                   {FDCatalog.FDCatalogFields.catalog_id.value: catalog.catalog_id},
                                   catalog.to_json())
        return True

    def get_event(self, event_id: str) -> HurricaneEvent:
        event: HurricaneEvent = HurricaneEvent.from_json(
            self.mongo.find_document(self.database,
                                     self.HurricaneModelTables.events.value,
                                     {HurricaneEvent.HurricaneEventFields.event_id.value: event_id})
        )
        return event

    def get_events(self, event_ids: List[str]) -> List[HurricaneEvent]:
        events: List[HurricaneEvent] = []
        for event_id in event_ids:
            event: HurricaneEvent = self.get_event(event_id)
            events.append(event)
        return events

    def store_event(self, event: HurricaneEvent) -> bool:
        self.mongo.upsert_document(self.database,
                                   self.HurricaneModelTables.events.value,
                                   {HurricaneEvent.HurricaneEventFields.event_id.value: event.event_id},
                                   event.to_json())
        return True

    def store_events(self, events: List[HurricaneEvent]) -> bool:
        for event in events:
            self.store_event(event)
        return True

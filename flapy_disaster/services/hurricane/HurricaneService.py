from typing import List

from flapy_disaster.models.hurricane.HurricaneModel import HurricaneModel
from flapy_disaster.services.hurricane.HurricaneCatalog import FDCatalog, HurricaneEvent
from flapy_disaster.services.hurricane.hurricane_utils import HurdatCatalog, old_to_new_tp
from flapy_disaster.utilities.flapy_types import VelocityKnots, DistanceNauticalMiles


class HurricaneService:
    def __init__(self):
        self.model: HurricaneModel = HurricaneModel()

    ###################
    # Catalog Methods #
    ###################
    # TODO: Break into Catalog only Service
    def get_catalog(self, catalog_id: str) -> FDCatalog:
        catalog: FDCatalog = self.model.get_catalog(catalog_id)
        return catalog

    # TODO: Break into Catalog only Service
    def get_catalog_ids(self) -> List[str]:
        catalog_ids: List[str] = self.model.get_catalog_ids()
        return catalog_ids

    def load_hurdat_catalog(self, catalog_unique_name: str, catalog_id: str) -> bool:
        hurdat_catalog: HurdatCatalog = HurdatCatalog(catalog_id)

        events: List[HurricaneEvent] = [HurricaneEvent(event.unique_name,
                                                       event.name,
                                                       event.track_points[0].timestamp,
                                                       event.rmax_nmi,
                                                       event.fspeed_kts,
                                                       [old_to_new_tp(tp) for tp in event.track_points],
                                                       catalog_id)
                                        for event
                                        in hurdat_catalog.storm_catalog]

        catalog: FDCatalog = FDCatalog(str(hurdat_catalog.catalog_file_uri),
                                       catalog_unique_name,
                                       [event.event_id for event in events])

        self.model.save_catalog(catalog)
        self.model.store_events(events)

        return True

    #################
    # Event Methods #
    #################
    def get_event(self, event_id: str) -> HurricaneEvent:
        event: HurricaneEvent = self.model.get_event(event_id)
        return event

    #####################
    # Calculate Methods #
    #####################
    def calculate_event(self,
                        event: HurricaneEvent,
                        px_per_deg_x: int,
                        px_per_deg_y: int):
        pass

    def calculate_grid_python(self,
                              px_per_deg_x: int,
                              px_per_deg_y: int,
                              radius_max_wind: DistanceNauticalMiles):
        pass

    #################
    # Render Events #
    #################
    def render_event(self):
        pass
import math
from typing import List, Tuple

from flapy_disaster.models.hurricane.HurricaneModel import HurricaneModel
from flapy_disaster.services.hurricane.HurricaneCatalog import FDCatalog, HurricaneEvent, HurricaneTrackPoint
from flapy_disaster.services.hurricane.hurricane_nws23 import calc_windspeed
from flapy_disaster.services.hurricane.hurricane_utils import HurdatCatalog, old_to_new_tp, calc_bearing_north_zero
from flapy_disaster.utilities.ConfigurationLoader import ConfigurationLoader
from flapy_disaster.utilities.flapy_types import VelocityKnots, DistanceNauticalMiles, ResolutionPixelsPerDegree, \
    PositionCoordinate, AngleDegrees
from flapy_disaster.utilities.general_objects import BoundingBox, LatLonGrid
from flapy_disaster.utilities.general_units import haversine_degrees_to_meters, quick_degress_to_meters

from numpy import ndarray, flipud, array


class HurricaneService:
    def __init__(self, config: ConfigurationLoader):
        self.model: HurricaneModel = HurricaneModel(config)

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
                        lat_lon_grid: LatLonGrid,
                        radius_max_wind: DistanceNauticalMiles = 15):
        return self.calculate_grid_python(event, lat_lon_grid, rmax_nmi=radius_max_wind)

    def create_bounds_from_track(self, event: HurricaneEvent, lat_buffer: int = 2, lon_buffer: int = 2) -> BoundingBox:
        lat_list = list(map(lambda x: x.lat_y, event.track_points))
        lon_list = list(map(lambda x: x.lon_x, event.track_points))

        return BoundingBox(max(lat_list) + lat_buffer,
                           min(lat_list) - lat_buffer,
                           max(lon_list) + lon_buffer,
                           min(lon_list) - lon_buffer)

    def calculate_grid_python(self,
                              event: HurricaneEvent,
                              lat_lon_grid: LatLonGrid,
                              max_calculation_distance: DistanceNauticalMiles = 350,
                              rmax_nmi: DistanceNauticalMiles = None,
                              override_fspeed_kts: VelocityKnots = None) -> List[Tuple[float, float, int]]:

        lat_lon_list = lat_lon_grid.get_lat_lon_list()
        rmax: DistanceNauticalMiles = rmax_nmi or event.radius_max_wind

        max_dist_deg_squared: float = (max_calculation_distance / 60) ** 2

        return [self.lat_lon_calc_loop(event.track_points, point[0], point[1], rmax, max_dist_deg_squared)
                for point
                in lat_lon_list]

    def lat_lon_calc_loop(self,
                          track_points: List[HurricaneTrackPoint],
                          lat_y: PositionCoordinate,
                          lon_x: PositionCoordinate,
                          rmax_nmi: DistanceNauticalMiles,
                          max_dist_degrees_sq: float) -> Tuple[float, float, int]:
        """
        Run the model calculation at a specific lat/long point, given a track and rmax
        :param track_points: list of hurricane.hurricane_utils.HurdatCatalog.HurdatStormSystem.HurdatTrackPoint
        :param lat_y: float
        :param lon_x: float
        :param rmax_nmi: int
        :return: list as [lat, lon, max_wind]
        """
        max_wind: VelocityKnots = 0
        # hits = 0
        # total = 0
        for track_point in track_points:
            # total += 1
            dy: AngleDegrees = lat_y - track_point.lat_y
            dx: AngleDegrees = lon_x - track_point.lon_x

            if ((dy * dy) + (dx * dx)) < max_dist_degrees_sq:
                # hits += 1
                # accurate_distance: DistanceNauticalMiles = haversine_degrees_to_meters(lat_y,
                #                                                                        lon_x,
                #                                                                        track_point.lat_y,
                #                                                                        track_point.lon_x) / 1000 * 0.539957  # convert to nautical miles
                #
                accurate_distance: DistanceNauticalMiles = quick_degress_to_meters(lat_y,
                                                                                   lon_x,
                                                                                   track_point.lat_y,
                                                                                   track_point.lon_x) / 1000 * 0.539957  # convert to nautical miles
                angle_to_center = calc_bearing_north_zero(track_point.lat_y, track_point.lon_x, lat_y, lon_x)
                # TODO just accept pressure in calc_windspeed
                windspeed_temp = calc_windspeed(track_point.min_pressure,
                                                accurate_distance,
                                                track_point.lat_y,
                                                track_point.forward_speed,
                                                rmax_nmi,
                                                angle_to_center,
                                                track_point.heading,
                                                None,
                                                track_point.max_wind)
                max_wind = max(max_wind, windspeed_temp)
        # print(hits/total)
        return lat_y, lon_x, round(max_wind)

    #################
    # Render Events #
    #################
    def render_event(self):
        pass

    def event_results_to_numpy(self, event_results, lat_lon_grid: LatLonGrid) -> ndarray:
        temp_array: array = array(list(map((lambda x: x[2]), event_results)))
        reshaped_array: ndarray = temp_array.reshape(lat_lon_grid.get_block_height_y(),
                                                     lat_lon_grid.get_block_width_x())
        flipped_array: ndarray = flipud(reshaped_array)
        return flipped_array

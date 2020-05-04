from datetime import datetime, timedelta
from typing import List
from abc import ABC, abstractmethod

import pytz

from flapy_disaster.services.hurricane.hurricane_utils import interpolate_lat_lon_from_time, interpolate_vmax_from_time, \
    calc_bearing_north_zero
from flapy_disaster.utilities.general_units import haversine_degrees_to_meters
from flapy_disaster.utilities.general_utils import FDEnum
from flapy_disaster.utilities.flapy_types import (DistanceNauticalMiles,
                                                  VelocityKnots,
                                                  AngleDegrees,
                                                  PressureMillibar,
                                                  JSON,
                                                  DistanceMeters,
                                                  TimeHour, Point, PointLatLon)


class ModelPoint(ABC):
    def __init__(self, lat_y: AngleDegrees, lon_x: AngleDegrees):
        self.lat_y: AngleDegrees = lat_y
        self.lon_x: AngleDegrees = lon_x

    @staticmethod
    @abstractmethod
    def from_json(input_dict: JSON) -> "ModelPoint":
        pass

    @abstractmethod
    def to_json(self) -> JSON:
        pass


class HurricaneTrackPoint(ModelPoint):
    def __init__(self,
                 tp_timestamp: datetime,
                 sequence: int,
                 lat_y: AngleDegrees,
                 lon_x: AngleDegrees,
                 heading: AngleDegrees,
                 max_wind: VelocityKnots,
                 min_pressure: PressureMillibar,
                 forward_speed: VelocityKnots,
                 is_landfall: bool,
                 is_source_data: bool = True):
        super().__init__(lat_y, lon_x)
        self.tp_timestamp: datetime = tp_timestamp
        self.sequence: int = sequence
        self.heading: AngleDegrees = heading
        self.max_wind: VelocityKnots = max_wind
        self.min_pressure: PressureMillibar = min_pressure
        self.forward_speed: VelocityKnots = forward_speed
        self.is_landfall: bool = is_landfall
        self.is_source_data: bool = is_source_data

    @staticmethod
    def from_json(input_dict: JSON) -> "HurricaneTrackPoint":
        tp_timestamp: datetime = datetime.fromisoformat(input_dict["tp_timestamp"])

        return HurricaneTrackPoint(
            tp_timestamp,
            input_dict["sequence"],
            input_dict["lat_y"],
            input_dict["lon_x"],
            input_dict["heading"],
            input_dict["max_wind"],
            input_dict["min_pressure"],
            input_dict["forward_speed"],
            input_dict["is_landfall"],
            input_dict["is_source_data"]
        )

    def to_json(self) -> JSON:
        return {
            "tp_timestamp": self.tp_timestamp.isoformat(),
            "sequence": self.sequence,
            "lat_y": self.lat_y,
            "lon_x": self.lon_x,
            "heading": self.heading,
            "max_wind": self.max_wind,
            "min_pressure": self.min_pressure,
            "forward_speed": self.forward_speed,
            "is_landfall": self.is_landfall,
            "is_source_data": self.is_source_data
        }


class FDEventTypes(FDEnum):
    base = "base"
    hurricane = "hurricane"
    hurdat = "hurdat"
    unisys = "unisys"


class FDEvent(ABC):
    def __init__(self,
                 event_type: FDEventTypes,
                 event_id: str,
                 event_name: str,
                 event_timestamp: datetime,
                 source_data: str):
        self.event_type: FDEventTypes = event_type
        self.event_id: str = event_id
        self.event_name: str = event_name
        self.event_timestamp: datetime = event_timestamp
        self.source_data: str = source_data

    @staticmethod
    @abstractmethod
    def from_json(input_json: JSON) -> "FDEvent":
        pass

    @abstractmethod
    def to_json(self) -> JSON:
        pass


class HurricaneEvent(FDEvent):
    class HurricaneEventFields(FDEnum):
        event_id = "event_id"
        event_name = "event_name"
        event_timestamp = "event_timestamp"
        radius_max_wind = "radius_max_wind"
        forward_speed = "forward_speed"
        track_points = "track_points"
        source_data = "source_data"

    def __init__(self,
                 event_id: str,
                 event_name: str,
                 event_timestamp: datetime,
                 radius_max_wind: DistanceNauticalMiles,
                 forward_speed: VelocityKnots,
                 track_points: List[HurricaneTrackPoint],
                 source_data: str = None):
        super().__init__(FDEventTypes.hurricane, event_id, event_name, event_timestamp, source_data)

        self.radius_max_wind: DistanceNauticalMiles = radius_max_wind
        self.forward_speed: VelocityKnots = forward_speed
        self.track_points: List[HurricaneTrackPoint] = track_points

    @staticmethod
    def from_json(input_json: JSON) -> "HurricaneEvent":
        event_timestamp: datetime = datetime.fromisoformat(
            input_json[HurricaneEvent.HurricaneEventFields.event_timestamp.value]
        )
        trackpoints: List[HurricaneTrackPoint] = [HurricaneTrackPoint.from_json(tp_json)
                                                  for tp_json
                                                  in input_json[HurricaneEvent.HurricaneEventFields.track_points.value]]

        return HurricaneEvent(input_json[HurricaneEvent.HurricaneEventFields.event_id.value],
                              input_json[HurricaneEvent.HurricaneEventFields.event_name.value],
                              event_timestamp,
                              input_json[HurricaneEvent.HurricaneEventFields.radius_max_wind.value],
                              input_json[HurricaneEvent.HurricaneEventFields.forward_speed.value],
                              trackpoints,
                              input_json[HurricaneEvent.HurricaneEventFields.source_data.value])

    def to_json(self) -> JSON:
        return {
            self.HurricaneEventFields.event_id.value: self.event_id,
            self.HurricaneEventFields.event_name.value: self.event_name,
            self.HurricaneEventFields.event_timestamp.value: self.event_timestamp.isoformat(),
            self.HurricaneEventFields.radius_max_wind.value: self.radius_max_wind,
            self.HurricaneEventFields.forward_speed.value: self.forward_speed,
            self.HurricaneEventFields.track_points.value: [tp.to_json() for tp in self.track_points],
            self.HurricaneEventFields.source_data.value: self.source_data
        }

    def calculate_forward_speed(self):
        if len(self.track_points) == 1:
            self.track_points[0].forward_speed = 0
            return True

        # Speed is what the movement is going into a point
        # First point is assumed to have the same speed as the second point
        for idx in range(len(self.track_points) - 1):
            curr_track_point: HurricaneTrackPoint = self.track_points[idx]
            next_track_point: HurricaneTrackPoint = self.track_points[idx + 1]

            distance: DistanceMeters = haversine_degrees_to_meters(curr_track_point.lat_y,
                                                                   curr_track_point.lon_x,
                                                                   next_track_point.lat_y,
                                                                   next_track_point.lon_x)
            distance: DistanceNauticalMiles = distance / 1000.0 * 0.539957
            time_delta: timedelta = (next_track_point.tp_timestamp - curr_track_point.tp_timestamp)
            time_delta: TimeHour = time_delta.total_seconds() / 3600

            # TODO figure out a cleaner way of handling duplicate time stamps, but really
            #   there shouldn't be any at this point
            if time_delta == 0:
                time_delta += 1 / 3600

            next_track_point.forward_speed = distance / time_delta

            if idx == 0:
                curr_track_point.forward_speed = next_track_point.forward_speed

    def time_interpolate_track_points(self,
                                      time_step: timedelta = timedelta(hours=1),
                                      recalculate_fspeed: bool = True,
                                      recalculate_heading: bool = True):
        """
        Interpolates track to at least the interval given
        :param time_step: timedelta default 1hr
        :return:
        """

        temp_tps = [self.track_points[0]]
        for pos in range(len(self.track_points) - 1):
            temp_start_tp = self.track_points[pos]
            end_tp = self.track_points[pos + 1]

            temp_tps.append(temp_start_tp)
            while (end_tp.timestamp - temp_start_tp.timestamp) > time_step:
                temp_ts = temp_start_tp.timestamp + time_step
                start_lat_lon = temp_start_tp.point_lat_lon()
                end_lat_lon = end_tp.point_lat_lon()
                new_lat_lon = interpolate_lat_lon_from_time(temp_start_tp.timestamp,
                                                            start_lat_lon[0],
                                                            start_lat_lon[1],
                                                            end_tp.timestamp,
                                                            end_lat_lon[0],
                                                            end_lat_lon[1],
                                                            temp_ts)
                new_wind = interpolate_vmax_from_time(temp_start_tp.timestamp,
                                                      temp_start_tp.max_wind_kts,
                                                      end_tp.timestamp,
                                                      end_tp.max_wind_kts,
                                                      temp_ts)
                temp_tp = HurricaneTrackPoint(temp_ts,
                                              temp_start_tp.sequence,
                                              new_lat_lon[0],
                                              new_lat_lon[1],
                                              temp_start_tp.heading,
                                              new_wind,
                                              temp_start_tp.min_pressure,
                                              temp_start_tp.forward_speed,
                                              temp_start_tp.is_landfall,
                                              temp_start_tp.is_source_data)
                temp_tps.append(temp_tp)
                temp_start_tp = temp_tp
                pos += 1

        for i in range(len(temp_tps)):
            temp_tps[i].sequence = i
        self.track_points = temp_tps

        if recalculate_fspeed:
            self.calculate_forward_speed()

        if recalculate_heading:
            self.calc_trackpoint_headings()

    def calc_trackpoint_headings(self):
        """
        Calculate the heading for every track point.  For all points except the last,
        heading is defined as the direction to the next point.  For the last point,
        heading is the same as the previous points heading.
        :return: None
        """
        if len(self.track_points) == 1:
            self.track_points[0].heading = 0
        for i in range(len(self.track_points)):
            if i == len(self.track_points) - 1:
                self.track_points[i].heading = self.track_points[i - 1].heading
                continue

            next_lat_lng: PointLatLon = [self.track_points[i + 1].lat_y, self.track_points[i + 1].lon_x]
            curr_lat_lng: PointLatLon = [self.track_points[i].lat_y, self.track_points[i].lon_x]

            heading = calc_bearing_north_zero(curr_lat_lng[0], curr_lat_lng[1], next_lat_lng[0], next_lat_lng[1])
            self.track_points[i].heading = heading


class FDCatalog:
    class FDCatalogFields(FDEnum):
        catalog_id = "catalog_id"
        catalog_name = "catalog_name"
        event_ids = "event_ids"
        date_created = "date_created"

    def __init__(self,
                 catalog_id: str,
                 catalog_name: str,
                 event_ids: List[str],
                 date_created: datetime = None):
        self.catalog_id: str = catalog_id
        self.catalog_name: str = catalog_name

        self.date_created: datetime = datetime.now(tz=pytz.utc) if date_created is None else date_created
        self.event_ids: List[str] = event_ids

    def to_json(self) -> JSON:
        ret_json: JSON = {
            self.FDCatalogFields.catalog_id.value: self.catalog_id,
            self.FDCatalogFields.catalog_name.value: self.catalog_name,
            self.FDCatalogFields.date_created.value: self.date_created.isoformat(),
            self.FDCatalogFields.event_ids.value: self.event_ids
        }

        return ret_json

    @staticmethod
    def from_json(input_json: JSON) -> "FDCatalog":
        date_created: datetime = datetime.fromisoformat(input_json[FDCatalog.FDCatalogFields.date_created.value])

        return FDCatalog(input_json[FDCatalog.FDCatalogFields.catalog_id.value],
                         input_json[FDCatalog.FDCatalogFields.catalog_name.value],
                         input_json[FDCatalog.FDCatalogFields.event_ids.value],
                         date_created)



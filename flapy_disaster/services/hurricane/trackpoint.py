from abc import abstractmethod, ABC
from datetime import datetime

from flapy_disaster.utilities.flapy_types import JSON, VelocityKnots, PressureMillibar, AngleDegrees


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

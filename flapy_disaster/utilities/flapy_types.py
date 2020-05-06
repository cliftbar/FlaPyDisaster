from typing import TypeVar, List, Dict, Union, Any, Tuple

Velocity = TypeVar("Velocity", bound=float)
VelocityKnots = TypeVar("VelocityKnots", bound=float)
VelocityMetersPerSecond = TypeVar("VelocityMetersPerSecond", bound=float)
DistanceNauticalMiles = TypeVar("DistanceNauticalMiles", bound=float)
DistanceMeters = TypeVar("DistanceMeters", bound=float)
AngleDegrees = TypeVar("AngleDegrees", bound=float)
FrequencyHours = TypeVar("FrequencyHours", bound=float)
PressureInHg = TypeVar("PressureInHg", bound=float)
PressureMillibar = TypeVar("PressureMillibar", bound=float)
PressureKilopascal = TypeVar("PressureKilopascal", bound=float)
PositionCoordinate = TypeVar("PositionCoordinate", bound=float)
Point = TypeVar("Point", bound=Tuple[float, float])
PointLatLon = TypeVar("PointLatLng", bound=Tuple[float, float])
PointValue = TypeVar("PointValue", bound=Tuple[float, float, Any])
GeojsonPoint = TypeVar("GeojsonPoint", bound=List)
TimeSecond = TypeVar("TimeSecond", bound=float)
TimeHour = TypeVar("TimeHour", bound=float)
ResolutionPixelsPerDegree = TypeVar("ResolutionPixelsPerDegree", bound=int)

JSON = Dict[str, Union[str, int, Dict, List, bool, None]]

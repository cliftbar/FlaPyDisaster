from typing import TypeVar, Tuple, List, Dict, Union

Velocity = TypeVar('Velocity', float, int)
VelocityKnots = TypeVar('VelocityKnots', float, int)
VelocityMetersPerSecond = TypeVar('VelocityMetersPerSecond', float, int)
DistanceNauticalMiles = TypeVar('DistanceNauticalMiles', float, int)
DistanceMeters = TypeVar('DistanceMeters', float, int)
AngleDegrees = TypeVar('AngleDegrees', float, int)
FrequencyHours = TypeVar('FrequencyHours', float, int)
PressureInHg = TypeVar('PressureInHg', float, int)
PressureMillibar = TypeVar('PressureMillibar', float, int)
PressureKilopascal = TypeVar('PressureKilopascal', float, int)
PositionCoordinate = TypeVar('PositionCoordinate', float, int)
Point = TypeVar('Point', List, Tuple)
PointValue = TypeVar('PointValue', Tuple, List)
GeojsonPoint = TypeVar('GeojsonPoint', Tuple, List)
TimeSecond = TypeVar('TimeSeconds', float, int)
TimeHour = TypeVar('TimeHour', float, int)

JSON = Dict[str, Union[str, int, Dict, List, bool, None]]

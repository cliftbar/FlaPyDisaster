import math


class GeneralMapping:
    # see http://www.movable-type.co.uk/scripts/latlong.html  "Spherical Law of Cosines"
    # radiusOfEarth = 3959;
    @staticmethod
    def great_circle_distance(lat_1, lon_1, lat_2, lon_2):
        lat_1 *= 0.0174532925
        lon_1 *= 0.0174532925
        lat_2 *= 0.0174532925
        lon_2 *= 0.0174532925
        return math.acos((math.sin(lat_1) * math.sin(lat_2)) + (math.cos(lat_1) + math.cos(lat_2) * math.cos(lon_2 - lat_1))) * 3959

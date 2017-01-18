###########
# imports #
###########
import math

#############
# Constants #
#############
# Distance
feet_per_meter = 3.28084

# Velocity
mps_per_mph = 0.44704

# Energy
joules_per_megatontnt = 0.000000000000000239


######################
# Conversion "Enums" #
######################
class DistanceUnits:
    """Enum like class containing the supported distance units"""
    feet = 'ft'
    meter = 'm'
    kilometer = 'km'
    degrees = 'deg'

    def get_units_list(self):
        return [self.feet, self.meter, self.kilometer, self.degrees]

    @staticmethod
    def get_pretty_units():
        return ['feet', 'meter', 'kilometer']

    @staticmethod
    def get_units_pair():
        return [('feet', 'ft'), ('meter', 'm'), ('kilometer', 'km')]


class VelocityUnits:
    """Enum like class containing the supported velocity units"""
    kmps = 'kmps'
    mps = 'mps'
    mph = 'mph'

    def get_units_list(self):
        return [self.kmps, self.mps, self.mph]

    @staticmethod
    def get_pretty_units():
        return ['km/s', 'm/s', 'mph']

    @staticmethod
    def get_units_pair():
        return [('km/s', 'kmps'), ('m/s', 'mps'), ('mph', 'mph')]


class EnergyUnits:
    """Enum like class containing the supported energy units"""
    joules = 'j'
    Megaton_TNT = 'mt_tnt'
    Kiloton_TNT = 'kt_tnt'

    def get_units_list(self):
        return [self.joules, self.Megaton_TNT, self.Kiloton_TNT]

    @staticmethod
    def get_pretty_units():
        return ['J', 'Mt-TNT', 'Kt-TNT']

    @staticmethod
    def get_units_pair():
        return [('J', 'j'), ('Mt-TNT', 'mt_tnt'), ('Kt-TNT', 'kt_tnt')]


def hello():
    ret_string = "This is the unit conversion package! This will contain some help text."
    print(ret_string)


def distance_conversion(value_in, unit_in, unit_out):
    """
    Convert value from one supported distance unit to another
    :param value_in: float
    :param unit_in: str
    :param unit_out: str
    :return: float
    """
    value_out = None

    if unit_in == DistanceUnits.feet:
        if unit_out == DistanceUnits.feet:
            value_out = value_in
        elif unit_out == DistanceUnits.meter:
            value_out = value_in / feet_per_meter
        elif unit_out == DistanceUnits.kilometer:
            value_out = value_in / feet_per_meter / 1000
        else:
            raise ValueError("Output Unit Not Supported: " + unit_out)
    elif unit_in == DistanceUnits.meter:
        if unit_out == DistanceUnits.feet:
            value_out = value_in * feet_per_meter
        elif unit_out == DistanceUnits.meter:
            value_out = value_in
        elif unit_out == DistanceUnits.kilometer:
            value_out = value_in / 1000
        else:
            raise ValueError("Output Unit Not Supported: " + unit_out)
    elif unit_in == DistanceUnits.kilometer:
        if unit_out == DistanceUnits.feet:
            value_out = value_in * 1000 * feet_per_meter
        elif unit_out == DistanceUnits.meter:
            value_out = value_in * 1000
        elif unit_out == DistanceUnits.kilometer:
            value_out = value_in
        else:
            raise ValueError("Output Unit Not Supported: " + unit_out)
    else:
        raise ValueError("Input Unit Not Supported: " + unit_in)

    return value_out


def velocity_conversion(value_in, unit_in, unit_out):
    """
    Convert value from one supported velocity unit to another
    :param value_in: float
    :param unit_in: str
    :param unit_out: str
    :return: float
    """
    value_out = None

    if unit_in == VelocityUnits.mph:
        if unit_out == VelocityUnits.mph:
            value_out = value_in
        elif unit_out == VelocityUnits.mps:
            value_out = value_in / mps_per_mph
        elif unit_out == VelocityUnits.kmps:
            value_out = value_in / mps_per_mph / 1000
        else:
            raise ValueError("Output Unit Not Supported: " + unit_out)
    elif unit_in == VelocityUnits.mps:
        if unit_out == VelocityUnits.mph:
            value_out = value_in * mps_per_mph
        elif unit_out == VelocityUnits.mps:
            value_out = value_in
        elif unit_out == VelocityUnits.kmps:
            value_out = value_in / 1000
        else:
            raise ValueError("Output Unit Not Supported: " + unit_out)
    elif unit_in == VelocityUnits.kmps:
        if unit_out == VelocityUnits.mph:
            value_out = value_in * 1000 * mps_per_mph
        elif unit_out == VelocityUnits.mps:
            value_out = value_in * 1000
        elif unit_out == VelocityUnits.kmps:
            value_out = value_in
        else:
            raise ValueError("Output Unit Not Supported: " + unit_out)
    else:
        raise ValueError("Input Unit Not Supported: " + unit_in)

    return value_out


def energy_conversion(value_in, unit_in, unit_out):
    """
    Convert value from one supported energy unit to another
    :param value_in: float
    :param unit_in: str
    :param unit_out: str
    :return: float
    """
    value_out = None
    if unit_in == EnergyUnits.joules:
        if unit_out == EnergyUnits.joules:
            value_out = value_in
        elif unit_out == EnergyUnits.Megaton_TNT:
            value_out = value_in * joules_per_megatontnt
        elif unit_out == EnergyUnits.Kiloton_TNT:
            value_out = value_in * joules_per_megatontnt * 1000
        else:
            raise ValueError("Output Unit Not Supported: " + unit_out)
    elif unit_in == EnergyUnits.Megaton_TNT:
        if unit_out == EnergyUnits.joules:
            value_out = value_in / joules_per_megatontnt
        elif unit_out == EnergyUnits.Megaton_TNT:
            value_out = value_in
        elif unit_out == EnergyUnits.Kiloton_TNT:
            value_out = value_in * 1000
        else:
            raise ValueError("Output Unit Not Supported: " + unit_out)
    elif unit_in == EnergyUnits.Kiloton_TNT:
        if unit_out == EnergyUnits.joules:
            value_out = value_in / 1000 / joules_per_megatontnt
        elif unit_out == EnergyUnits.Megaton_TNT:
            value_out = value_in / 1000
        elif unit_out == EnergyUnits.Kiloton_TNT:
            value_out = value_in
        else:
            raise ValueError("Output Unit Not Supported: " + unit_out)
    else:
        raise ValueError("Input Unit Not Supported: " + unit_in)

    return value_out


def haversine_degrees_to_meters(lat_1, lon_1, lat_2, lon_2):
    """
    Haversine equation for finding the distance between two lat-lon points in meters.
    :param lat_1: first latitude point
    :param lon_1: first longitude point
    :param lat_2: second latitude point
    :param lon_2: second longitude point
    :returns: distance in meters
    :reference: http://www.movable-type.co.uk/scripts/latlong.html
    :reference: http://stackoverflow.com/questions/4102520/how-to-transform-a-distance-from-degrees-to-metres
    """
    r = 6371000
    delta_lat = math.radians(lat_2 - lat_1)
    delta_lon = math.radians(lon_2 - lon_1)

    a = ((math.sin(delta_lat / 2) ** 2) +
         math.cos(math.radians(lat_1)) * math.cos(math.radians(lat_2)) *
         (math.sin(delta_lon / 2) ** 2))
    c = 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))
    return r * c

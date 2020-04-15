# from general.general_objects import BoundingBox
import geojson


# class to mirror javascript leaflet layer server side
class LeafletLayer:
    def __init__(self):
        self.geo_json = []


# Class to mirror javascript leaflet map server side
class LeafletMap:
    def __init__(self):
        self.center_latlng = (0, 0)
        self.zoom = 13
        self.layers = {}


class GeojsonGeometry:
    """
    Container enum-like class containing the different GeoJSON geometry types
    """
    point = 'point'
    multipoint = 'multipoint'
    line = 'line'
    multiline = 'multiline'
    polygon = 'polygon'
    multipolygon = 'multipolygon'
    geo_feature = 'feature'
    geo_featurecollection = 'featurecollection'

    def get_geometry_names(self):
        return [self.point, self.multipoint, self.line, self.multiline, self.polygon, self.multipolygon, self.geo_feature, self.geo_featurecollection]


def create_feature(geometry, geo_type, val, feature_id=None, color=(255, 0, 0), weight=10, opacity=1.0, props={}):
    """
    :param geometry: Geometry structure that creates geojson string.  Options are:
                     Point: (lng, lat) as tuple
                     MultiPoint: [Point, Point] as array of points
                     Line(string): [Point, Point, Point] as array of points
                     Multiline(string): [Line, Line] as array of lines
                     Polygon without holes: [Point1, Point, Point, Point1] as array of points,
                        first and last point in array are the same point (example makes a triangle).
                    Polygon with hole: [[Point1, Point, Point, Point1], [Point2, Point, Point, Point2]] as array of polygons,
                        second polygon is the hole.
                    Multipolygon: [Polygon, Polygon] as array of polygons (must confirm...?)
    :param geo_type: string indicating the geometry type, must match id strings from class geojson_geometry
    :param val: value to put into properties.value for mapping and style color matching
    :param feature_id: id for the geojson string
    :param color: a 3 value tuple containing an rgb value
    :param weight: for lines/polygons, line width; for points, point size
    :param opacity: opacity of layer in leaflet, 1.0 = 100%, 0 = 0%
    :returns: dictionary with geojson feature string and a leaflet style created from input parameters
    """

    try:
        if geo_type == GeojsonGeometry.point:
            geo = geojson.Point(geometry)
        elif geo_type == GeojsonGeometry.multipoint:
            geo = geojson.MultiPoint(geometry)
        elif geo_type == GeojsonGeometry.line:
            geo = geojson.LineString(geometry)
        elif geo_type == GeojsonGeometry.multiline:
            geo = geojson.MultiLineString(geometry)
        elif geo_type == GeojsonGeometry.polygon:
            geo = geojson.Polygon(geometry)
        elif geo_type == GeojsonGeometry.multipolygon:
            geo = geojson.MultiPolygon(geometry)
        else:
            print("Unsupported geometry type: " + geo_type)
            return
    except Exception as e:
        print(e, "\n probably wrong input data structure for " + geo_type)
        return

    style = None  # leaflet_style_creator()
    props['value'] = val
    geo = geojson.Feature(id=feature_id, geometry=geo, properties=props)

    ret = {'geojson': geo, 'style': style}

    return ret

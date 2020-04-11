from general import general_objects as geno
import csv


###########################################
# struct like classes for hurricane event #
###########################################


#########################
# hurricane event class #
#########################
class ModelHurricaneEvent:
    class TrackPoint:
        def __init__(self, catalog_number, storm_name, basin, timestamp, lat_y, lon_x, max_wind_kts, min_cp_mb, sequence, rmax_nmi, bearing, fspeed_kts, gwaf):
            pass

        pass

    model_headers = ["catalog_number", "name", "basin", "timestamp", "lat_y", "lon_x", "max_wind_kts", "min_cp_mb",
                     "sequence", "rmax_nmi", "fspeed_kts", "gwaf"]

    def __init__(self, name, track_file_uri, bbox, block_per_deg_x=10, block_per_deg_y=10):
        """
        :param geno.BoundingBox bbox: input bounding box
        """
        self.name = name
        self.track_file_uri = track_file_uri
        self.bbox = bbox
        self.track_points = []
        self.grid = geno.LatLonGrid(bbox.top_lat_y, bbox.bot_lat_y, bbox.left_lon_x, bbox.right_lon_x, block_per_deg_x, block_per_deg_y)

    def load_track_file(self, track_file_uri=None):
        if track_file_uri is not None:
            self.track_file_uri = track_file_uri

        with open(self.track_file_uri, 'r') as tsv:
            tsv_reader = csv.reader(tsv, delimiter='\t')
            for row in tsv_reader:
                # process row into track
                pass
        pass

    def parse_data_row(self):
        pass

    def from_hurdat_source(self):
        pass

    def get_2d_grid(self):
        pass

    def get_1d_grid(self):
        pass

    def get_grid_to_geojson_collection(self):
        pass

    def get_track_to_geojson_collection(self):
        pass

    def get_storm_to_geojson_collection(self):
        pass

    def save(self, save_file_uri, save_type='image', incl_track=True, incl_readme=True):
        pass

    def calculate_windfield(self, model='nws23', resolution_pxperdeg=10):
        pass

    def get_inf(self):
        pass

import datetime
import pandas as pd
import mapping.leaflet_map as lm
import copy
import math
import general.general_objects as geno
import general.general_units as genu
import hurricane.hurricane_nws23 as hm
import joblib as job
import configparser as cpar
import numpy as np
import mapping.gdal_mapping as gdm
import requests
import shutil
import globes as gb
from PIL import Image
from io import BytesIO
import os


def calc_bearing_north_zero(lat_ref, lon_ref, lat_loc, lon_loc):
    """
    Calculate the simple bearing (pythagorean angles)
    :param lat_ref: float
    :param lon_ref: float
    :param lat_loc: float
    :param lon_loc: float
    :return: float bearing in degrees (north zero)
    """
    lon_delta = lon_loc - lon_ref
    lat_delta = lat_loc - lat_ref

    angle_deg = math.degrees(math.atan2(lon_delta, lat_delta))
    return (angle_deg + 360) % 360


def calc_bearing_great_circle(lat_ref, lon_ref, lat_loc, lon_loc):
    """
    Calculate the great circle bearing
    :param lat_ref: float
    :param lon_ref: float
    :param lat_loc: float
    :param lon_loc: float
    :return: float bearing in degrees (north zero deg)
    """
    y = math.sin(lon_loc - lon_ref) * math.cos(lat_loc)
    x = math.cos(lat_ref) * math.sin(lat_loc) - math.sin(lat_ref) * math.cos(lat_loc) * math.cos(lon_loc - lon_ref)
    brng = math.degrees(math.atan2(y, x))
    return (brng + 360) % 360


def interpolate_lat_lon_from_time(start_time, start_lat, start_lon, end_time, end_lat, end_lon, current_time):
    time_diff = end_time - start_time
    curr_diff = current_time - start_time

    lat_diff = end_lat - start_lat
    lon_diff = end_lon - start_lon

    time_frac = curr_diff.total_seconds() / time_diff.total_seconds()

    ret_lon = start_lon + (lon_diff * time_frac)
    ret_lat = start_lat + (lat_diff * time_frac)

    return [ret_lat, ret_lon]


def interpolate_vmax_from_time(start_time, start_wind, end_time, end_wind, current_time):
    time_diff = end_time - start_time
    curr_diff = current_time - start_time

    wind_diff = end_wind - start_wind

    time_frac = curr_diff.total_seconds() / time_diff.total_seconds()

    ret_wind = start_wind + (wind_diff * time_frac)

    return ret_wind


class HurdatCatalog:
    """
    Class representing a catalog of hurdat hurricanes
    """

    class HurdatStormSystem:
        """
        Class representing a Hurricane from a hurdat data source
        """
        hurdat_headers = ["StormID/Date", "Name/Hour", "Rows/SpecialRow", "System Status", "Lat", "Lon",
                          "Max Wind (kts)", "Min Pressure (mBar)", "R34 NE(Nauts; kts)", "R34 SE (Nauts; kts)",
                          "R34 SW (Nauts; kts)", "R34 NW (Nauts; kts)", "R50 NE (Nauts; kts)", "R50 SE (Nauts; kts)",
                          "R50 SW (Nauts; kts)", "R50 NW (Nauts; kts)", "R64 NE (Nauts; kts)", "R64 SE (Nauts; kts)",
                          "R64 SW (Nauts; kts)", "R64 NW (Nauts; kts)"]
        df_hurdat_headers = ["storm_id/date", "name/time", "records/record_identifier", "system_status", "lat", "lon",
                             "max_wind_kts", "min_pressure_mb", "r34_ne_nmi", "r34_se_nmi", "r34_sw_nmi", "r34_nw_nmi",
                             "r50_ne_nmi", "r500_se_nmi", "r50_sw_nmi", "r50_nw_nmi", "r64_ne_nmi", "r64_se_nmi",
                             "r64_sw_nmi", "r64_nw_nmi"]
        model_headers = ["catalog_number", "name", "basin", "timestamp", "lat_y", "lon_x", "max_wind_kts", "min_cp_mb",
                         "sequence", "fspeed_kts", "is_landfall_point", "heading", "rmax_nmi", "gwaf"]

        unisys_headers = ["NAME", "ADV", "LAT", "LON", "TIME", "WIND", "PR", "STAT"]

        hurdat_wind_no_data = -99
        hurdat_no_data = -999

        class HurdatTrackPoint:
            """
            Class representing a single track point of a hurdat storm
            """

            def __init__(self, year, month, day, hour, minute, lat_y, lon_x, max_wind_kts, min_pressure_mb, sequence, fspeed_kts
                         , is_landfall=None, record_identifier=None, status=None, hemisphere_ns=None, hemisphere_ew=None
                         , r34_ne_nmi=None, r34_se_nmi=None, r34_sw_nmi=None, r34_nw_nmi=None
                         , r50_ne_nmi=None, r50_se_nmi=None, r50_sw_nmi=None, r50_nw_nmi=None
                         , r64_ne_nmi=None, r64_se_nmi=None, r64_sw_nmi=None, r64_nw_nmi=None
                         , interpolated_point=False):
                """
                Each parameter is a field from the hurdat specification, except the sequence and fspeed
                :param year: int
                :param month: int
                :param day: int
                :param hour: int
                :param minute: int
                :param record_identifier: str
                :param status: str
                :param lat_y: float
                :param hemisphere_ns: str
                :param lon_x: float
                :param hemisphere_ew: str
                :param max_wind_kts: int
                :param min_pressure_mb: int
                :param r34_ne_nmi: int
                :param r34_se_nmi: int
                :param r34_sw_nmi: int
                :param r34_nw_nmi: int
                :param r50_ne_nmi: int
                :param r50_se_nmi: int
                :param r50_sw_nmi: int
                :param r50_nw_nmi: int
                :param r64_ne_nmi: int
                :param r64_se_nmi: int
                :param r64_sw_nmi: int
                :param r64_nw_nmi: int
                :param sequence: int
                :param fspeed_kts: float
                """
                # Time
                self.year = year
                self.month = month
                self.day = day
                self.hour = hour
                self.minute = minute
                self.timestamp = datetime.datetime(year, month, day, hour, minute)

                # Identifiers
                self.record_identifier = record_identifier
                self.status = status

                # Position
                # self.lat_y = lat_y * (-1 if hemisphere_ns == 'S' else 1)
                self.lat_y = lat_y
                self.hemisphere_ns = hemisphere_ns
                # self.lon_x = lon_x * (-1 if hemisphere_ew == 'W' else 1)
                self.lon_x = lon_x
                self.hemisphere_ew = hemisphere_ew

                # Intensities
                self.max_wind_kts = max_wind_kts
                self.min_pressure_mb = min_pressure_mb

                # Radius of 34kt winds
                self.r34_ne_nmi = r34_ne_nmi
                self.r34_se_nmi = r34_se_nmi
                self.r34_sw_nmi = r34_sw_nmi
                self.r34_nw_nmi = r34_nw_nmi

                # Radius of 50kt winds
                self.r50_ne_nmi = r50_ne_nmi
                self.r50_se_nmi = r50_se_nmi
                self.r50_sw_nmi = r50_sw_nmi
                self.r50_nw_nmi = r50_nw_nmi

                # Radius of 64kt winds
                self.r64_ne_nmi = r64_ne_nmi
                self.r64_se_nmi = r64_se_nmi
                self.r64_sw_nmi = r64_sw_nmi
                self.r64_nw_nmi = r64_nw_nmi

                self.sequence = sequence
                self.heading_to_next_point = None
                self.fspeed_kts = fspeed_kts

                if is_landfall is None:
                    self.is_landfall = 1 if self.record_identifier == "L" else 0
                else:
                    self.is_landfall = is_landfall

                self.interpolated_point = interpolated_point

            def point_to_xyz(self):
                """
                return the point in a lat, lon, value list format
                :return: list as [lat, lon, value]
                """
                lat = self.lat_y * -1 if self.hemisphere_ns == 'S' else self.lat_y
                lon = self.lon_x * -1 if self.hemisphere_ew == 'W' else self.lon_x
                val = self.max_wind_kts
                return [lat, lon, val]

            def point_lat_lon(self):
                """
                Get the lat and lon of the point in list format
                :return: list [lat, lon]
                """
                lat = self.lat_y * -1 if self.hemisphere_ns == 'S' else self.lat_y
                lon = self.lon_x * -1 if self.hemisphere_ew == 'W' else self.lon_x
                return [lat, lon]

            def for_geojson_point(self):
                """
                Get the point formatted for geojson
                :return: list [[lon, lat], value]
                """
                lon = self.lon_x * -1 if self.hemisphere_ew == 'W' else self.lon_x
                lat = self.lat_y * -1 if self.hemisphere_ns == 'S' else self.lat_y
                val = self.max_wind_kts
                seq = self.sequence
                return [[lon, lat], val, seq]

            def to_hurdat_list(self):
                """
                Convert the point to a list representation, hurdat format
                :return: list
                """
                return [str(self.year) + str(self.month).zfill(2) + str(self.day).zfill(2)
                        , str(self.hour).zfill(2) + str(self.minute).zfill(2)
                        , self.record_identifier
                        , self.status
                        , str(math.fabs(self.lat_y)) + 'S' if self.lat_y < 0 else 'N'
                        , str(math.fabs(self.lon_x)) + 'E' if self.lon_x < 0 else 'W'
                        , self.max_wind_kts
                        , self.min_pressure_mb
                        , self.r34_ne_nmi
                        , self.r34_se_nmi
                        , self.r34_sw_nmi
                        , self.r34_nw_nmi
                        , self.r50_ne_nmi
                        , self.r50_se_nmi
                        , self.r50_sw_nmi
                        , self.r50_nw_nmi
                        , self.r64_ne_nmi
                        , self.r64_se_nmi
                        , self.r64_sw_nmi
                        , self.r64_nw_nmi]

            def to_model_list(self):
                """
                Convert the point to a list representation, model format
                :return: list
                """
                return [self.timestamp.strftime("%Y-%m-%d-%H-%M")
                        , self.lat_y
                        , self.lon_x
                        , self.max_wind_kts
                        , self.min_pressure_mb
                        , self.sequence
                        , self.fspeed_kts
                        , self.is_landfall
                        , self.heading_to_next_point]

        def __init__(self, hurdat_storm_data=None, fspeed_kts=15, rmax_nmi=15, gwaf=0.9):
            """
            Storm initializer function.  If storm_data is provided, the storm data is parsed.  Otherwise, and empty storm is created
            :param hurdat_storm_data: list of list
            :param fspeed_kts: float
            :param rmax_nmi: int
            :param gwaf: float
            """
            self.storm_data = hurdat_storm_data
            self.source_data = None
            self.fspeed_kts = fspeed_kts
            self.rmax_nmi = rmax_nmi
            self.gwaf = gwaf
            self.basin = None
            self.catalog_number = None
            self.year = None
            self.name = None
            self.track_point_count = None
            self.track_points = []
            self.lat_lon_grid = None
            self.result_array = None
            self.px_per_degree = 10
            self.max_calc_dist = 360

            self.unique_name = ''
            self.fspeed_from_points = False
            self.raster_bands = None
            self.raster_output_band = None
            self.data_source_type = None
            self.misc_1 = None

            if hurdat_storm_data is not None:
                self.parse_hurdat_storm_data(hurdat_storm_data)
                self.data_source_type = 'hurdat'

        def parse_hurdat_storm_data(self, hurdat_storm_data):
            """
            Parse a storm_data input into a track.  Passed list is not altered
            :param hurdat_storm_data: list of lists
            :return:
            """
            self.data_source_type = 'hurdat'
            # todo cache the list data
            self.source_data = copy.deepcopy(hurdat_storm_data)
            # parse storm level parameters from first row of data table
            self.parse_hurdat_header_row(hurdat_storm_data[0])

            # drop first row from local copy of the dataframe
            # storm_data.drop(storm_data.index[[0]], inplace = True)
            hurdat_storm_data.pop(0)
            seq = 0
            # parse data rows
            for row in hurdat_storm_data[1:]:
                self.parse_hurdat_data_row(row, seq)
                seq += 1

        def parse_from_saved_event(self, ini_uri):
            """
            Parse an existing event in program format (ini file, event raster, track file)
            :param ini_uri:
            :return: None
            """
            config_file = cpar.ConfigParser(allow_no_value=True)
            config_file.read(ini_uri)

            storm_parameters_section = 'StormParameters'
            self.unique_name = config_file.get(storm_parameters_section, 'unique_name')
            self.rmax_nmi = config_file.getfloat(storm_parameters_section, 'rmax_nmi')
            self.basin = config_file.get(storm_parameters_section, 'basin')
            self.gwaf = config_file.getfloat(storm_parameters_section, 'gwaf')
            self.catalog_number = config_file.get(storm_parameters_section, 'cyclone_number')
            if self.catalog_number is not None:
                self.catalog_number = float(self.catalog_number)
            self.year = config_file.getint(storm_parameters_section, 'year')
            self.name = config_file.get(storm_parameters_section, 'name')
            self.fspeed_from_points = config_file.getboolean(storm_parameters_section, 'fspeed_from_points')

            event_parameter_configuration = 'EventParameters'
            self.raster_bands = config_file.getfloat(event_parameter_configuration, 'raster_bands')
            self.raster_output_band = config_file.getfloat(event_parameter_configuration, 'raster_output_band')
            top_lat_y = config_file.getfloat(event_parameter_configuration, 'top_lat_y')
            bot_lat_y = config_file.getfloat(event_parameter_configuration, 'bot_lat_y')
            right_lon_x = config_file.getfloat(event_parameter_configuration, 'right_lon_x')
            left_lon_x = config_file.getfloat(event_parameter_configuration, 'left_lon_x')
            px_per_degree = config_file.getfloat(event_parameter_configuration, 'px_per_degree')

            self.lat_lon_grid = geno.LatLonGrid(top_lat_y, bot_lat_y, left_lon_x, right_lon_x, px_per_degree, px_per_degree)

            file_paths_configuration = 'FilePathConfiguration'
            raster_uri = config_file.get(file_paths_configuration, 'raster_path')

            raster_array = gdm.read_raster_to_array(raster_uri)

            self.result_array = []
            for (y, x), element in np.ndenumerate(np.flipud(raster_array)):
                self.result_array.append([self.lat_lon_grid.get_lat(y), self.lat_lon_grid.get_lon(x), int(element)])

            file_paths_configuration = 'FilePathConfiguration'
            data_uri = config_file.get(file_paths_configuration, 'data_path')
            self.track_points = []
            self.parse_model_data(data_uri, True)

        def calc_trackpoint_heading(self):
            """
            Calculate the heading for every track point.  For all points except the last,
            heading is defined as the direction to the next point.  For the last point,
            heading is the same as the previous points heading.
            :return: None
            """
            if len(self.track_points) == 1:
                self.track_points[0].heading_to_next_point = 0
            for i in range(len(self.track_points)):
                if i == len(self.track_points) - 1:
                    self.track_points[i].heading_to_next_point = self.track_points[i-1].heading_to_next_point
                    continue

                next_lat_lng = self.track_points[i+1].point_lat_lon()
                curr_lat_lng = self.track_points[i].point_lat_lon()
                heading = calc_bearing_north_zero(curr_lat_lng[0], curr_lat_lng[1], next_lat_lng[0], next_lat_lng[1])
                self.track_points[i].heading_to_next_point = heading

        def calc_fspeed_per_point(self):
            """
            Calculate the forward speed at each point.  For every point except the first, forward speed is defined
            as the distance/time between the previous and current point.
            :return: None
            """
            if len(self.track_points) == 1:
                self.track_points[0].fspeed_kts = 0
            for i in range(len(self.track_points)-1, -1, -1):
                if i == 0:
                    self.track_points[i].fspeed_kts = self.track_points[i+1].fspeed_kts
                    continue

                next_lat_lng = self.track_points[i-1].point_lat_lon()
                curr_lat_lng = self.track_points[i].point_lat_lon()
                distance = genu.haversine_degrees_to_meters(curr_lat_lng[0], curr_lat_lng[1], next_lat_lng[0], next_lat_lng[1]) / 1000 * 0.539957
                time = (self.track_points[i].timestamp - self.track_points[i-1].timestamp).total_seconds() / 3600
                if time == 0:
                    self.track_points[i].fspeed_kts = self.track_points[i+1].fspeed_kts
                else:
                    self.track_points[i].fspeed_kts = distance / time

            self.fspeed_from_points = True

        def set_fspeed_to_generic(self):
            """
            Set every points forward speed to the storm default (storm.fpseed_kts)
            :return:
            """
            for tps in self.track_points:
                tps.fspeed_kts = self.fspeed_kts

            self.fspeed_from_points = False

        def parse_hurdat_header_row(self, header_row):
            """
            Parse the hurdat header row of the storm.
            :param header_row: list
            :return: None
            """
            self.basin = header_row[0][:2]
            self.catalog_number = int(header_row[0][2:4])
            self.year = int(header_row[0][4:])
            self.name = header_row[1]
            self.track_point_count = int(header_row[2])

            self.unique_name = str(self.year) + "_" + str(self.catalog_number) + "_" + self.name + "_" + self.basin

        def parse_hurdat_data_row(self, data_row, sequence):
            """
            Parse a hurdat data row of the storm.
            :param data_row: list
            :param sequence: int
            :return: None
            """
            # Time
            year = int(data_row[0][:4])
            month = int(data_row[0][4:6])
            day = int(data_row[0][6:])
            hour = int(data_row[1][:2])
            minute = int(data_row[1][2:])

            # Identifiers
            record_identifier = data_row[2]
            status = data_row[3]

            # Position
            lat_y = float(data_row[4][:-1])
            hemisphere_ns = data_row[4][-1]
            lon_x = float(data_row[5][:-1])
            hemisphere_ew = data_row[5][-1]

            # Intensities
            max_wind_kts = int(data_row[6])  # 1min-10m average
            if max_wind_kts == self.hurdat_wind_no_data:
                max_wind_kts = math.nan

            min_pressure_mb = int(data_row[7])
            if min_pressure_mb == self.hurdat_no_data:
                min_pressure_mb = math.nan

            # Radius of 34kt winds
            r34_ne_nmi = int(data_row[8])
            if r34_ne_nmi == self.hurdat_no_data:
                r34_ne_nmi = math.nan
            r34_se_nmi = int(data_row[9])
            if r34_se_nmi == self.hurdat_no_data:
                r34_se_nmi = math.nan
            r34_sw_nmi = int(data_row[10])
            if r34_sw_nmi == self.hurdat_no_data:
                r34_sw_nmi = math.nan
            r34_nw_nmi = int(data_row[11])
            if r34_nw_nmi == self.hurdat_no_data:
                r34_nw_nmi = math.nan

            # Radius of 50kt winds
            r50_ne_nmi = int(data_row[12])
            if r50_ne_nmi == self.hurdat_no_data:
                r50_ne_nmi = math.nan
            r50_se_nmi = int(data_row[13])
            if r50_se_nmi == self.hurdat_no_data:
                r50_se_nmi = math.nan
            r50_sw_nmi = int(data_row[14])
            if r50_sw_nmi == self.hurdat_no_data:
                r50_sw_nmi = math.nan
            r50_nw_nmi = int(data_row[15])
            if r50_nw_nmi == self.hurdat_no_data:
                r50_nw_nmi = math.nan

            # Radius of 64kt winds
            r64_ne_nmi = int(data_row[16])
            if r64_ne_nmi == self.hurdat_no_data:
                r64_ne_nmi = math.nan
            r64_se_nmi = int(data_row[17])
            if r64_se_nmi == self.hurdat_no_data:
                r64_se_nmi = math.nan
            r64_sw_nmi = int(data_row[18])
            if r64_sw_nmi == self.hurdat_no_data:
                r64_sw_nmi = math.nan
            r64_nw_nmi = int(data_row[19])
            if r64_nw_nmi == self.hurdat_no_data:
                r64_nw_nmi = math.nan
                
            # create and store track point
            self.track_points.append(
                self.HurdatTrackPoint(year, month, day, hour, minute, lat_y, lon_x, max_wind_kts, min_pressure_mb, sequence, self.fspeed_kts
                                      , None, record_identifier, status, hemisphere_ns, hemisphere_ew
                                      , r34_ne_nmi, r34_se_nmi, r34_sw_nmi, r34_nw_nmi
                                      , r50_ne_nmi, r50_se_nmi, r50_sw_nmi, r50_nw_nmi
                                      , r64_ne_nmi, r64_se_nmi, r64_sw_nmi, r64_nw_nmi))

        def parse_model_data_row(self, row):
            timestamp = datetime.datetime.strptime(row[3], "%Y-%m-%d-%H-%M")
            lat_y = row[4]
            lon_x = row[5]
            max_wind = row[6]
            min_cp = row[7]
            if (min_cp == None or min_cp == ''):
                min_cp = math.nan
            seq = row[8]
            fspeed = row[9]
            is_landfall = row[10]

            self.track_points.append(self.HurdatTrackPoint(timestamp.year, timestamp.month, timestamp.day, timestamp.hour, timestamp.minute,lat_y, lon_x, max_wind, min_cp, seq, fspeed, is_landfall))

        def parse_model_header_data(self, first_row):
            self.catalog_number = first_row[0]
            self.name = first_row[1]
            self.basin = first_row[2]
            self.rmax_nmi = first_row[11]
            self.gwaf = first_row[12]

            self.unique_name = str(self.year) + "_" + str(self.catalog_number) + "_" + self.name + "_" + self.basin

        def parse_model_data(self, data_file_uri, from_ini=False):
            self.source_data = pd.DataFrame()
            self.source_data = pd.read_csv(data_file_uri, sep='\t')
            self.source_data = self.source_data.applymap((lambda x: str.strip(x) if isinstance(x, str) else x))
            self.source_data.fillna('', inplace=True)

            self.storm_data = self.source_data.values.tolist()

            if not from_ini:
                first_row = self.storm_data[0]
                self.parse_model_header_data(first_row)

            for line in self.storm_data:
                self.parse_model_data_row(line)
            self.track_point_count = len(self.track_points)
            self.calc_trackpoint_heading()
            pass

        def parse_unisys_data(self, unisys_file_uri):
            self.data_source_type = 'unisys'
            self.fspeed_from_points = True

            with open(unisys_file_uri, 'r') as fi:
                self.year = int(str.strip(fi.readline().split(" ")[-1]))
                name_header = fi.readline().replace(' ', '_')
                self.misc_1 = self.name = name_header.split("_")[0]
                self.name = str.strip(name_header.split("_")[1])
                self.unique_name = self.name + "_" + str(self.year)
                fi.readline()
                self.storm_data = fi.readlines()

            seq = 0
            for line in self.storm_data:
                self.parse_unisys_data_row(line, self.year, seq)
                seq += 1

            self.track_point_count = len(self.track_points)

            self.source_data = pd.DataFrame()
            # self.source_data = pd.read_csv(unisys_file_uri, delim_whitespace=True, skiprows=2)
            print(self.storm_data)
            # self.source_data = self.source_data.applymap((lambda x: str.strip(x) if isinstance(x, str) else x))
            # self.source_data.fillna('-', inplace=True)

            self.storm_data = self.source_data.values.tolist()

            self.time_interpolate_track_points(fspeed=False, heading=False)
            self.calc_fspeed_per_point()
            self.calc_trackpoint_heading()

        def parse_unisys_data_row(self, row, year, seq):
            row = str.strip(row)

            adv, row = row.split(maxsplit=1)
            adv = adv.strip()
            row = row.strip()

            lat_y, row = row.split(maxsplit=1)
            lat_y = float(lat_y.strip())
            row = row.strip()

            lon_x, row = row.split(maxsplit=1)
            lon_x = float(lon_x.strip())
            row = row.strip()

            ts, row = row.split(maxsplit=1)
            ts = ts.strip()
            row = row.strip()
            timestamp = datetime.datetime.strptime(str(year) + "/" + ts[:-1], "%Y/%m/%d/%H")

            max_wind, row = row.split(maxsplit=1)
            max_wind = float(max_wind.strip()) if max_wind.strip().isdigit() else 0
            row = row.strip()

            min_cp, row = row.split(maxsplit=1) 
            if min_cp == '-':
                min_cp = math.nan
            else:
                min_cp = float(min_cp.strip())
            row = row.strip()

            stat = row

            self.track_points.append(self.HurdatTrackPoint(timestamp.year, timestamp.month, timestamp.day, timestamp.hour, timestamp.minute, lat_y, lon_x, max_wind, min_cp, seq, None, None, record_identifier=adv, status=stat))

        def time_interpolate_track_points(self, time_step=None, fspeed=True, heading=True):
            """
            Interpolates track to at least the interval given
            :param time_step: timedelta default 1hr
            :return:
            """
            if time_step is None:
                time_step = datetime.timedelta(hours=1)

            temp_tps = []
            for pos in range(self.track_point_count-1):
                temp_start_tp = self.track_points[pos]
                end_tp = self.track_points[pos+1]

                temp_tps.append(temp_start_tp)
                while (end_tp.timestamp - temp_start_tp.timestamp) > time_step:
                    temp_ts = temp_start_tp.timestamp + time_step
                    start_lat_lon = temp_start_tp.point_lat_lon()
                    end_lat_lon = end_tp.point_lat_lon()
                    new_lat_lon = interpolate_lat_lon_from_time(temp_start_tp.timestamp, start_lat_lon[0], start_lat_lon[1], end_tp.timestamp, end_lat_lon[0], end_lat_lon[1], temp_ts)
                    new_wind = interpolate_vmax_from_time(temp_start_tp.timestamp, temp_start_tp.max_wind_kts, end_tp.timestamp, end_tp.max_wind_kts, temp_ts)
                    temp_tp = HurdatCatalog.HurdatStormSystem.HurdatTrackPoint(temp_ts.year, temp_ts.month, temp_ts.day, temp_ts.hour, temp_ts.minute, new_lat_lon[0], new_lat_lon[1]
                                                                               , new_wind, temp_start_tp.min_pressure_mb, temp_start_tp.sequence, temp_start_tp.fspeed_kts
                                                                               , 0, temp_start_tp.record_identifier, None, None, None
                                                                               , temp_start_tp.r34_ne_nmi, temp_start_tp.r34_se_nmi, temp_start_tp.r34_sw_nmi, temp_start_tp.r34_nw_nmi
                                                                               , temp_start_tp.r50_ne_nmi, temp_start_tp.r50_se_nmi, temp_start_tp.r50_sw_nmi, temp_start_tp.r50_nw_nmi
                                                                               , temp_start_tp.r64_ne_nmi, temp_start_tp.r64_se_nmi, temp_start_tp.r64_sw_nmi, temp_start_tp.r64_nw_nmi
                                                                               , True)
                    temp_tps.append(temp_tp)
                    temp_start_tp = temp_tp
                    pos += 1

            for i in range(len(temp_tps)):
                temp_tps[i].sequence = i
            self.track_points = temp_tps
            self.track_point_count = len(temp_tps)

            if fspeed:
                self.calc_fspeed_per_point()

            if heading:
                self.calc_trackpoint_heading()

            print((temp_tps[-1].timestamp - temp_tps[0].timestamp).total_seconds() / 3600.0)
            print(self.track_point_count)

        def parse_data_frame(self):
            pass

        def header_to_list(self, pad=False):
            """
            Returns the header as a list in hurdat format.  Optionally pads the list with empty strings to the correct length to put into a hurdat format csv file
            :param pad: Bool
            :return: list
            """
            ret = [self.basin + str(self.catalog_number).zfill(2) + str(self.year)
                   , self.name
                   , str(self.track_point_count)]

            if pad:
                ret.extend(['', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''])

            return ret

        def to_hurdat_dataframe(self):
            """
            Returns the source hurdat data as a dataframe.  Based on storm.source_data
            :return: pandas Dataframe
            """

            # data = []
            # if(self.source_data == None):
            #    data.append(self.header_to_list(True))
            #    for point in self.track_points:
            #        data.append(point.to_hurdat_list())
            # else:
            #    data = self.source_data

            data = self.source_data
            return pd.DataFrame(data, columns=self.df_hurdat_headers)

        def to_model_dataframe(self):
            """
            Returns the source data as a dataframe in model format.  Based on storm.source_data
            :return: pandas Dataframe
            """

            data = []
            data_front = [self.catalog_number
                          , self.name
                          , self.basin]
            data_back = [self.rmax_nmi
                         , self.gwaf]
            for point in self.track_points:
                temp_row = data_front + point.to_model_list() + data_back
                data.append(temp_row)
            return pd.DataFrame(data, columns=self.model_headers)

        def track_to_xyz_list(self):
            print(str(self.track_points[0].lon_x))
            pass

        def grid_to_xyz_list(self):
            return list(map((lambda x: [x[1], x[0], x[2]]), self.result_array))

        def track_to_geojson(self):
            """
            Return the track in geojson format (a geojson collection of points)
            :return: list of geojson points
            """
            temp_list = list(map((lambda x: x.for_geojson_point()), self.track_points))
            geojson_collection = list(map((lambda x: lm.create_feature(x[0], lm.GeojsonGeometry.point, x[1], props = {"sequence": x[2]})['geojson']), temp_list))
            return geojson_collection

        def track_to_json(self):
            return list(map((lambda x: {"catalogNumber": self.catalog_number, "stormName": self.name, "basin": self.basin, "timestamp": x.timestamp.strftime("%Y-%m-%d-%H-%M"), "eyeLat_y": x.point_lat_lon()[0], "eyeLon_x": x.point_lat_lon()[1], "maxWind_kts": None if math.isnan(x.max_wind_kts) else x.max_wind_kts, "minCp_mb": None if math.isnan(x.min_pressure_mb) else x.min_pressure_mb, "sequence": x.sequence, "fSpeed_kts": x.fspeed_kts, "isLandfallPoint": bool(x.is_landfall), "rMax_nmi": self.rmax_nmi, "gwaf": self.gwaf, "heading": x.heading_to_next_point}), self.track_points))

        def calculate_grid_scala(self, px_per_deg_x, px_per_deg_y, fspeed_kts, rmax_nmi=None, bbox=None, do_parallel=False, num_parallel=None, auto_fspeed=False, force_recalc=False):
            calc_method = 'nws23'
            # Send event to scala
            formData = {}
            formData = self.track_to_json()

            formDict = {"track": formData}
            formDict['BBox'] = {}
            formDict['BBox']['topLatY'] = gb.flapy_app.hurricane_catalog.current_storm.lat_lon_grid.top_lat_y
            formDict['BBox']['botLatY'] = gb.flapy_app.hurricane_catalog.current_storm.lat_lon_grid.bot_lat_y
            formDict['BBox']['leftLonX'] = gb.flapy_app.hurricane_catalog.current_storm.lat_lon_grid.left_lon_x
            formDict['BBox']['rightLonX'] = gb.flapy_app.hurricane_catalog.current_storm.lat_lon_grid.right_lon_x
            formDict['BBox']['pxPerDegreeX'] = gb.flapy_app.hurricane_catalog.current_storm.lat_lon_grid.block_per_degree_x
            formDict['BBox']['pxPerDegreeY'] = gb.flapy_app.hurricane_catalog.current_storm.lat_lon_grid.block_per_degree_y

            formDict['rmax'] = gb.flapy_app.hurricane_catalog.current_storm.rmax_nmi
            formDict['fspeed'] = None if auto_fspeed else gb.flapy_app.hurricane_catalog.current_storm.fspeed_kts
            formDict['par'] = num_parallel if do_parallel else -1
            formDict['maxDist'] = self.max_calc_dist
            formDict['call_id'] = self.unique_name

            # send request
            host = gb.GlobalConfig.get('ScalaServer', 'host')
            port = int(gb.GlobalConfig.get('ScalaServer', 'port'))
            scala_url = "http://{0}:{1}/calculate/hurricane/{2}".format(host, port, calc_method)
            # r = requests.get("http://192.168.88.28:9001/calculate/hurricane/nws23", json = formDict)
            # r = requests.get("http://localhost:9001/calculate/hurricane/nws23", json = formDict)
            r = requests.get(scala_url, json = formDict)
            
            footprintImage = Image.open(BytesIO(r.content))

            # Build event data from scala image file, as a byproduct saves the event to disk completely
            self.raster_bands = 4
            self.raster_output_band = 1
            # base_uri = gb.USER_FOLDER # app.config.get('USER_FOLDER')
            base_uri = os.path.join(gb.USER_FOLDER, 'events', 'hurricane')
            base_name = self.unique_name
            
            # save_name += "_" + "scala_temp"
            #self.unique_name = base_name

            raster_uri = os.path.join(base_uri, base_name + ".png")

            ini_uri = self.save_event_ini(base_uri, base_name)

            self.save_base_data(base_uri, base_name)

            # copy image from scala land to python land.  Assumes running on the same server with acess to both server directories
            # shutil.copy2(r.json()['imageUri'], raster_uri)
            footprintImage.save(raster_uri)

            # set current event what's read from 
            self.parse_from_saved_event(ini_uri)

            print(r.status_code)
            # print(r.json()['imageUri'])

        def calculate_grid_python(self, px_per_deg_x, px_per_deg_y, fspeed_kts, rmax_nmi=None, bbox=None, do_parallel=False, num_parallel=None, auto_fspeed=False, force_recalc=False):
            """
            Calculate the hurricane footprint from the track, using the NWS 23 model
            :param px_per_deg_x: int
            :param px_per_deg_y: int
            :param fspeed_kts: float
            :param rmax_nmi: int
            :param bbox: general.general_objects.BoundingBox
            :param do_parallel: bool
            :param num_parallel: int
            :param auto_fspeed: bool
            :param force_recalc: bool
            :return:
            """
            lat_lon_list = self.lat_lon_grid.get_lat_lon_list()

            results = []

            if not do_parallel:
                print('single thread')
                results = [self.lat_lon_calc_loop(self.track_points, point[0], point[1], rmax_nmi, self.max_calc_dist) for point in lat_lon_list]
            else:
                if num_parallel is None:
                    num_parallel = max(job.cpu_count() - 1, 1)

                print('multi thread ' + str(num_parallel))
                results = job.Parallel(n_jobs=num_parallel)(job.delayed(self.lat_lon_calc_loop)(self.track_points, point[0], point[1], rmax_nmi, self.max_calc_dist) for point in lat_lon_list)

            self.result_array = results

        def calculate_grid(self, px_per_deg_x, px_per_deg_y, fspeed_kts, rmax_nmi=None, bbox=None, do_parallel=False, num_parallel=None, auto_fspeed=False, force_recalc=False, lang="python"):
            if self.result_array is not None and force_recalc == 0:
                return

            if auto_fspeed:
                self.calc_fspeed_per_point()

            if rmax_nmi is None:
                rmax_nmi = self.rmax_nmi

            self.BuildLatLonGrid(px_per_deg_x, px_per_deg_y, bbox)

            if lang == "python":
                self.calculate_grid_python(px_per_deg_x, px_per_deg_y, fspeed_kts, rmax_nmi, bbox, do_parallel, num_parallel, auto_fspeed, force_recalc)
            else: 
                self.calculate_grid_scala(px_per_deg_x, px_per_deg_y, fspeed_kts, rmax_nmi, bbox, do_parallel, num_parallel, auto_fspeed, force_recalc)
            pass

        def lat_lon_calc_loop(self, tps, lat_y, lon_x, rmax_nmi, max_dist):
            """
            Run the model calculation at a specific lat/long point, given a track and rmax
            :param tps: list of hurricane.hurricane_utils.HurdatCatalog.HurdatStormSystem.HurdatTrackPoint
            :param lat_y: float
            :param lon_x: float
            :param rmax_nmi: int
            :return: list as [lat, lon, max_wind]
            """
            max_wind = 0
            for track_point in tps:
            # track_point = tps[12]
                eye_lat_lon = track_point.point_lat_lon()
                distance = genu.haversine_degrees_to_meters(lat_y, lon_x, eye_lat_lon[0], eye_lat_lon[1]) / 1000 * 0.539957  # convert to nautical miles
                if distance < max_dist:
                    angle_to_center = calc_bearing_north_zero(eye_lat_lon[0], eye_lat_lon[1], lat_y, lon_x)
                    windspeed_temp = hm.calc_windspeed(track_point.min_pressure_mb, distance, eye_lat_lon[0], track_point.fspeed_kts, rmax_nmi, angle_to_center, track_point.heading_to_next_point, None, track_point.max_wind_kts, None)
                    max_wind = max(max_wind, windspeed_temp)
            return [lat_y, lon_x, round(max_wind)]

        def bBoxFromTrack(self):
            lat_list = list(map(lambda x: x.point_lat_lon()[0], self.track_points))
            lon_list = list(map(lambda x: x.point_lat_lon()[1], self.track_points))
            # diff_lat = max(int(max(lat_list) - min(lat_list)), 1)
            # diff_lon = max(int(max(lon_list) - min(lon_list)), 1)
            diff_lat = 2
            diff_lon = 2
            return geno.BoundingBox(max(lat_list) + diff_lat, min(lat_list) - diff_lat, max(lon_list) + diff_lon, min(lon_list) - diff_lon)

        def BuildLatLonGrid(self, px_per_deg_x, px_per_deg_y, bbox = None):
            if bbox is None:
                bbox = self.bBoxFromTrack()
            self.lat_lon_grid = geno.LatLonGrid(bbox.top_lat_y, bbox.bot_lat_y, bbox.left_lon_x, bbox.right_lon_x, px_per_deg_x, px_per_deg_y)

        def grid_to_geojson(self):
            """
            Returns the storm footprint as a geojson collection (list of geojson points).
            :return: list of geojson points
            """
            if self.result_array is None:
                return None
            # flat_grid = [item for sublist in self.result_array for item in sublist]
            for_geojson_list = list(map((lambda x: [[x[1], x[0]], x[2]]), self.result_array))
            geojson_collection = list(map((lambda x: lm.create_feature(x[0], lm.GeojsonGeometry.point, x[1])['geojson']), for_geojson_list))
            # print(geojson_collection)
            return geojson_collection

        def save_event(self, raster_bands, raster_output_band, base_uri, event_suffix=''):
            """
            Save the event to file (ini file, raster, and track data file), with an optional event suffix
            :param raster_bands: int number of color bands for the raster (1: grey, 3: rgb, 4: rgba)
            :param raster_output_band: int what band to place the max windspeed data into
            :param event_suffix: optional event suffix, for saving multiple versions of an event
            :param base_uri: str uri of save location, with trailing '/'
            :return: None
            """
            self.raster_bands = raster_bands
            self.raster_output_band = raster_output_band

            base_uri = os.path.join(base_uri, 'events', 'hurricane')
            base_name = self.unique_name
            if event_suffix != '':
                base_name += "_" + event_suffix
            self.unique_name = base_name
            raster_uri = os.path.join(base_uri, base_name + ".png")
            static_image_file_uri = os.path.join(gb.STATIC_FOLDER, 'images', 'tmp/', base_name + ".png")

            self.save_event_raster(raster_uri, raster_bands, raster_output_band)

            # copy image to static folder for display, feels bad
            shutil.copy2(raster_uri, static_image_file_uri)

            # ini_uri = base_uri + self.unique_name + "_" + event_suffix + ".ini"
            self.save_event_ini(base_uri, base_name)

            # data_path = base_uri + self.unique_name +  "_" +  event_suffix + ".txt"
            self.save_base_data(base_uri, base_name)

        def save_event_raster(self, raster_uri, raster_bands, raster_output_band):
            """
            Save the event footprint to a png raster
            :param raster_uri: string uri for raster file
            :param raster_bands: int number of color bands for the raster (1: grey, 3: rgb, 4: rgba)
            :param raster_output_band: int what band to place the max windspeed data into
            :return: None
            """
            two_d_gdm_list = np.flipud(np.array(list(map((lambda x: x[2]), self.result_array))).reshape(self.lat_lon_grid.get_block_height_y(), self.lat_lon_grid.get_block_width_x()))
            gdm.save_array_to_raster(two_d_gdm_list, raster_uri, True, raster_bands, raster_output_band)

        def save_event_ini(self, base_uri, ini_name=None):
            """
            Save the event ini file.
            :param ini_name: string name for the ini file (without extension).  Default is the storm unique name
            :param base_uri: str base uri of save location with trailing '/'
            :return: None
            """
            config_file = cpar.ConfigParser(allow_no_value=True)

            # Storm parameter configuration
            storm_parameters_section = 'StormParameters'
            config_file.add_section(storm_parameters_section)
            config_file.set(storm_parameters_section, 'unique_name', self.unique_name)
            config_file.set(storm_parameters_section, 'rmax_nmi', str(self.rmax_nmi))
            config_file.set(storm_parameters_section, 'basin', self.basin)
            config_file.set(storm_parameters_section, 'gwaf', str(self.gwaf))
            config_file.set(storm_parameters_section, 'cyclone_number', str(self.catalog_number) if self.catalog_number is not None else None)
            config_file.set(storm_parameters_section, 'year', str(self.year))
            config_file.set(storm_parameters_section, 'name', self.name)
            config_file.set(storm_parameters_section, 'fspeed_from_points', str(self.fspeed_from_points))

            # Event parameter configuration
            event_parameter_configuration = 'EventParameters'
            config_file.add_section(event_parameter_configuration)
            config_file.set(event_parameter_configuration, 'raster_bands', str(self.raster_bands))
            config_file.set(event_parameter_configuration, 'raster_output_band', str(self.raster_output_band))
            config_file.set(event_parameter_configuration, 'top_lat_y', str(self.lat_lon_grid.top_lat_y))
            config_file.set(event_parameter_configuration, 'bot_lat_y', str(self.lat_lon_grid.bot_lat_y))
            config_file.set(event_parameter_configuration, 'right_lon_x', str(self.lat_lon_grid.right_lon_x))
            config_file.set(event_parameter_configuration, 'left_lon_x', str(self.lat_lon_grid.left_lon_x))
            config_file.set(event_parameter_configuration, 'px_per_degree', str(self.px_per_degree))
            config_file.set(event_parameter_configuration, 'max_calc_dist', str(self.max_calc_dist))

            if ini_name is not None:
                name = ini_name
            else:
                name = self.unique_name
            # File paths Configuration
            raster_path = os.path.join(base_uri, name + ".png")
            data_path = os.path.join(base_uri, name + ".txt")

            file_paths_configuration = 'FilePathConfiguration'
            config_file.add_section(file_paths_configuration)
            config_file.set(file_paths_configuration, 'raster_path', raster_path)
            config_file.set(file_paths_configuration, 'data_path', data_path)

            ini_uri = os.path.join(base_uri, name + ".ini")

            gb.save_config(config_file, ini_uri)

            return ini_uri

        def save_base_data(self, base_uri, file_name=None):
            """
            Save the storm track data to file as a tsv
            :param file_name: string name for the track file (no extension)
            :param base_uri: str base save location with trailing '/'
            :return:
            """

            if file_name is None:
                data_path = os.path.join(base_uri, self.unique_name + ".txt")
            else:
                data_path = os.path.join(base_uri, file_name + ".txt")

            df = self.to_model_dataframe()
            df.to_csv(data_path, sep='\t', line_terminator='\n', index=False)

    def __init__(self, catalog_file_uri=None):
        """
        Initialize a hurdat catalog from hurdat data file
        :param catalog_file_uri: str uri
        """
        self.catalog_file_uri = catalog_file_uri
        self.storm_catalog = []
        self.storm_data = None
        self.current_storm = self.HurdatStormSystem()

        if catalog_file_uri is not None:
            self.parse_hurdat_catalog(catalog_file_uri)

    def parse_hurdat_catalog(self, catalog_file_uri):
        """
        Parse a hurdat catalog file
        :param catalog_file_uri:
        :return: None
        """
        self.storm_data = pd.DataFrame()
        self.storm_data = pd.read_csv(catalog_file_uri)
        self.storm_data = self.storm_data.applymap((lambda x: str.strip(x) if isinstance(x, str) else x))
        # dropping last column because hurdat has trailing commas -.-
        self.storm_data.drop(self.storm_data.columns[-1], 1, inplace=True)
        self.storm_data.fillna('', inplace=True)

        storm_list = self.storm_data.values.tolist()

        storm_temp = None
        catalog_iter = 0
        while catalog_iter < len(storm_list):
            curr_row = storm_list[catalog_iter]

            # if row is header row, 4th element will be empty
            if not curr_row[3]:
                storm_temp = self.HurdatStormSystem()
                storm_temp.parse_hurdat_header_row(curr_row)
                catalog_iter += 1
                seq = 0
                storm_count = catalog_iter + storm_temp.track_point_count
                while catalog_iter < storm_count:
                    storm_temp.parse_hurdat_data_row(storm_list[catalog_iter], seq)
                    seq += 1
                    catalog_iter += 1
            storm_temp.calc_trackpoint_heading()
            self.storm_catalog.append(storm_temp)

    def get_names(self):
        """
        Get the names of all of the storms in the catalog
        :return:
        """
        return list(map((lambda x: x.unique_name), self.storm_catalog))

    def get_storm_by_name(self, name):
        """
        Get all storms of a given name (though there should only be one)
        :param name: str storm name
        :return: list of storms
        """
        storm = list(filter((lambda x: x.unique_name == name), self.storm_catalog))
        return storm

    def add_storm_from_ini(self, ini_file_uri):
        curr_storm = self.HurdatStormSystem()
        curr_storm.parse_from_saved_event(ini_file_uri)
        self.storm_catalog.append(curr_storm)
        print(curr_storm.unique_name)

    def add_unysis_storm(self, unisys_file_uri):
        curr_storm = self.HurdatStormSystem()
        curr_storm.parse_unisys_data(unisys_file_uri)
        self.storm_catalog.append(curr_storm)
        print(curr_storm.unique_name)
    pass

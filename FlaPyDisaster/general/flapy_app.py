# import flask as fl
# import general.general_utils as genu
import general.general_colors as genc
import general.general_units as gen_units
import general.general_objects as geno
from explosion.asteroid import asteroid_event
import os
from hurricane import hurricane_utils as hu
import math
# import geojson
# import mapping.leaflet_map as lm
import time
# import numpy as np
# import mapping.gdal_mapping as gdm
import pandas as pd
import globes as gb
# from app import app


class FlaPyApp:
    """
    Controller class for the application, providing functionality for the view app
    """
    def __init__(self):
        # Asteroid parameters
        self.asteroid_catalog = None
        self.asteroid_event = None
        self.asteroid_observation_radius_m = None

        # Hurricane parameters
        self.hurricane_catalog = None
        self.current_hurricane_name = None
        self.type_of_parallel = 'single'
        self.level_of_parallelism = None
        self.guess_parallelism = 0
        self.force_calculate = 0
        self.raster_bands = 1
        self.raster_output_band = 1

    def hello(self):
        print("Hello FlaPyApp")

    ########################
    # Hurricane Interfaces #
    ########################
    def hurricane_init_catalog(self, hurdat_file_uri=None):
        self.hurricane_catalog = hu.HurdatCatalog(hurdat_file_uri)

    def get_event_table(self, name='null'):
        ret_table = pd.DataFrame(columns=self.hurricane_catalog.HurdatStormSystem.model_headers).to_html(classes='track_table', index=False)
        if name == 'null':
            pass
        elif name == 'current':
            ret_table = self.hurricane_catalog.current_storm.to_model_dataframe().to_html(classes='track_table', index=False)
        else:
            ret_table = self.hurricane_catalog.get_storm_by_name(name)[0].to_model_dataframe().to_html(classes='track_table', index=False)
        return ret_table

    def hurricane_add_single_event(self, ini_file_uri, overwrite=False):
        self.hurricane_catalog.add_storm_from_ini(ini_file_uri)

    def hurricane_add_unisys_event(self, unisys_file_uri):
        self.hurricane_catalog.add_unysis_storm(unisys_file_uri)
        pass

    def set_current_event(self, name):
        if name != 'null':
            self.current_hurricane_name = name
            self.hurricane_catalog.current_storm = self.hurricane_catalog.get_storm_by_name(name)[0]

    def has_event_calcd(self, name):
        if name == 'current':
            has_calcd = self.hurricane_catalog.current_storm.result_array is not None
        else:
            has_calcd = self.hurricane_catalog.get_storm_by_name(name)[0].result_array is not None

        return has_calcd

    def calculate_event(self, name, lang="python"):
        storm = self.hurricane_catalog.get_storm_by_name(name)[0]

        print("Start Event Calculation")
        start = time.time()
        parallel = False
        if self.type_of_parallel == 'multi':
            parallel = True

        storm.calculate_grid(storm.px_per_degree, storm.px_per_degree, storm.fspeed_kts, storm.rmax_nmi, do_parallel=parallel, num_parallel=self.level_of_parallelism, force_recalc=self.force_calculate, lang=lang)
        end = time.time()
        print("Calculation Time: " + str(end - start))
        # print("num points: " + str(len(storm.result_array)))

    def hurricane_set_event_settings(self, input_dict):
        storm = self.hurricane_catalog.current_storm
        if storm is not None:
            storm.gwaf = float(input_dict['gwaf'])

            storm.max_calc_dist = float(input_dict['max_calc_dist'])

            automatic_fspeed = 0
            if 'automatic_fspeed' in input_dict.keys():
                automatic_fspeed = int(input_dict['automatic_fspeed'])
                storm.calc_fspeed_per_point()
            else:
                storm.fspeed_kts = int(input_dict['fspeed_kts_input'])
                storm.set_fspeed_to_generic()

            storm.rmax_nmi = int(input_dict['rmax_nmi'])

            storm.px_per_degree = int(input_dict['px_per_degree'])

            out = "gwaf: {0}, fspeed: {1}, automatic_fspeed: {2}, rmax: {3}, px_per_degree: {4}".format(storm.gwaf, storm.fspeed_kts, automatic_fspeed, storm.rmax_nmi, storm.px_per_degree)

            print(out)

    def hurricane_set_calculation_settings(self, input_dict):
        if 'guess_parallel' in input_dict.keys():
            self.guess_parallelism = int(input_dict['guess_parallel'])
        else:
            self.level_of_parallelism = int(input_dict['level_of_parallel'])

        if 'force_calc' in input_dict.keys():
            self.force_calculate = int(input_dict['force_calc'])

        self.raster_bands = int(input_dict['raster_bands'])

        self.raster_output_band = int(input_dict['raster_output_band'])

        self.type_of_parallel = input_dict['type_of_parallel']

        out = "level_of_parallelism: {0}, raster_bands: {1}, force_calc: {2}, type_of_parallel: {3}, guess parallel: {4}, raster_output_band: {5}".format(self.level_of_parallelism, self.raster_bands, self.force_calculate, self.type_of_parallel, self.guess_parallelism, self.raster_output_band)

        print(out)

    def hurricane_set_scala_settings(self, input_dict):
        address = str(input_dict['address']).replace('http://', '')
        address = address.replace('https://', '')
        host, port = address.split(':', maxsplit = 1)

        if 'ScalaServer' not in gb.GlobalConfig.sections():
            gb.GlobalConfig.add_section('ScalaServer')

        gb.GlobalConfig.set('ScalaServer', 'host',  host)
        gb.GlobalConfig.set('ScalaServer', 'port',  str(port))
        gb.GlobalConfig.set('ScalaServer', 'worker_count',  str(input_dict['worker_count']))

        gb.save_config()

    def hurricane_save_event_to_raster(self, event_save_suffix, user_folder):
        storm = self.hurricane_catalog.current_storm
        start = time.time()

        storm.save_event(self.raster_bands, self.raster_output_band, user_folder, event_save_suffix)
        end = time.time()
        print("Raster Save Time: " + str(end - start))

    def get_saved_events(self):
        saved_events = []
        events_folder = os.path.join(gb.USER_FOLDER, 'events', 'hurricane')

        for file in os.listdir(events_folder):
            if file.endswith(".ini"):
                file_name = os.path.splitext(os.path.basename(file))[0]
                saved_events.append(file_name)

        return saved_events

    def calculate_catalog(self, lang = 'python'):
        for storm in self.hurricane_catalog.storm_catalog:
            try:
                self.calculate_event(storm.name, lang)
            except:
                print('{0} failed to calculate'.format(storm.name))


    #######################
    # Asteroid Interfaces #
    #######################
    # todo remove flask references
    def asteroid_calculate_event(self, diameter_in, diameter_unit, angle_in, angle_unit, velocity_in, velocity_unit, density_kgpm3, target_density_kgpm3, radius_obs_in, radius_obs_unit):
        # run any necessary unit conversions
        angle_rad = angle_in
        if angle_unit == 'deg':
            angle_rad = math.radians(float(angle_in))

        diameter_m = gen_units.distance_conversion(float(diameter_in), diameter_unit, gen_units.DistanceUnits.meter)
        velocity_mps = gen_units.velocity_conversion(float(velocity_in), velocity_unit, gen_units.VelocityUnits.mps)
        self.asteroid_observation_radius_m = gen_units.distance_conversion(float(radius_obs_in), radius_obs_unit, gen_units.DistanceUnits.meter)

        # create asteroid event from input parameters
        latlon_grid = geno.LatLonGrid(30, 20, 10, 30, 2, 2)

        self.asteroid_event = asteroid_event.AsteroidEvent(diameter_m, angle_rad, velocity_mps, density_kgpm3, target_density_kgpm3, latlon_grid, (25, 20))
        grid_res = self.asteroid_event.get_effect_2d_grid(True, 5)

        if os.path.isfile("test_out.txt"):
            os.remove("test_out.txt")
            with open("test_out.txt", "w") as write_file:
                for row in grid_res:
                    out = ""
                    for val in row:
                        curr_str = str(format(round(val[0], 5), 'f'))
                        out = out + curr_str + "\t"
                    out.rstrip()
                    write_file.write(out + "\n")

    def asteroid_get_overpressure_at_radius(self, radius_obs_m=None):
        if radius_obs_m is None:
            radius_obs_m = self.asteroid_observation_radius_m
        return self.asteroid_event.get_newmark_overpressure(radius_obs_m)

    ######################
    # Leaflet Interfaces #
    ######################
    # def leaflet_redirect(self, sender_include):
    #     color_ramp = genc.ColorPalettes.simple_escalating_5
    #     user_color_num = gb.number_config_options('UserStyles')
    #     named_color_schemes = genc.get_named_color_schemes_from_config()
    #     return fl.render_template('html/leaflet_test.html'
    #                               , title="Leaflet"
    #                               # , sender_include='html/hurricane_map_base.html'
    #                               , sender_include=sender_include
    #                               , default_colors=color_ramp
    #                               , user_color_num=user_color_num
    #                               , named_color_schemes=named_color_schemes)
    #
    # def leaflet_test_latlng(self, lat, lng):
    #     print("leaflet test in Flask.")
    #     print("lat: " + lat)
    #     print("lng: " + lng)
    #     return "Success"

    # def leaflet_test_js(self):
    #     return fl.render_template('/js/leaflet_test.js')
    #
    # def leaflet_geojson_test(self):
    #     # 42.4, -71.15   42.4, -71.12        42.4, -71.05
    #     points = [(-71.15, 42.4), (-71.12, 42.4), (-71.05, 42.4)]
    #
    #     # noinspection PyTypeChecker
    #     line_str = geojson.LineString(points)
    #
    #     line_feature = geojson.Feature(geometry=line_str, properties={"value": 5})
    #
    #     return fl.jsonify(result=line_feature, max=10, min=2)

    # def leaflet_geojson_points_test(self):
    #     # 42.4, -71.15   42.4, -71.12        42.4, -71.05
    #     points = [(-71.15, 42.4), (-71.12, 42.4), (-71.05, 42.4)]
    #     # multi_pt = geojson.MultiPoint(points)
    #     # pt_feature = geojson.Feature(geometry = multi_pt, properties = {"value":1})
    #
    #     pt_feature_dict = lm.create_feature(points, lm.GeojsonGeometry.multipoint, 1)
    #
    #     return fl.jsonify(result=pt_feature_dict['geojson'], max=10, min=2)

    def leaflet_get_color_array_from_config(self, form_dict):
        hex_color_scheme = self.hex_scheme_from_form(form_dict)
        ret_array = [genc.ColorPalettes.hex_to_rgb(hex_color_scheme[0], 255), hex_color_scheme[1]]

        if 'save_color_scheme' in form_dict.keys():
            name = form_dict['color_scheme_name']
            print('Saving Color Scheme')
            if 'UserStyles' not in gb.GlobalConfig.sections():
                gb.GlobalConfig.add_section('UserStyles')
            cnt = len(dict(gb.GlobalConfig.items('UserStyles')).keys())
            gb.GlobalConfig.set('UserStyles', str(cnt) + "!" + name, str(hex_color_scheme))
            gb.save_config()

        print("got color scheme: " + form_dict['color_scheme_name'])
        print(hex_color_scheme)
        print(ret_array)

        return ret_array

    def hex_scheme_from_form(self, form_dict):
        hex_color_scheme = [[], []]
        for i in range(int(form_dict['number_colors'])):
            hex_color_scheme[0].append(form_dict['color_' + str(i)])
            hex_color_scheme[1].append(form_dict['color_val_' + str(i)])
        return hex_color_scheme


    # def leaflet_change_named_color_scheme(self, scheme_name):
    #     color_scheme = genc.gen_named_color_scheme_colors_from_config(scheme_name)
    #     print(color_scheme)
    #     return fl.jsonify(color_scheme=color_scheme)

    ############################
    # Documentation Interfaces #
    ############################


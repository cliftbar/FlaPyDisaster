import explosion.asteroid.asteroid_math as astr_math
import explosion.explosion_math as expl_math
from general import general_units, general_geometry
import os
import mapping.leaflet_map as lm


class AsteroidEvent:
    """
    Class defining an Asteroid Event from a set of parameters.  Handles calculating effects and creating output grids.
    """

    def __init__(self, diameter_m, angle_rad, init_velocity_mps, density_kgpm3, target_density_kgpm3, grid, ground_zero_latlon):
        # input params
        self.diameter_m = diameter_m
        self.angle_rad = angle_rad
        self.init_velocity_mps = init_velocity_mps
        self.density_kgpm3 = density_kgpm3
        self.target_density_kgpm3 = target_density_kgpm3
        self.grid = grid
        self.effect_2d_grid = None
        self.effect_flat_grid = None
        self.ground_zero_latlon = ground_zero_latlon

        # derived params
        self.breakup_alt_m = astr_math.breakup_altitude(self.density_kgpm3, self.diameter_m, self.init_velocity_mps, self.angle_rad)
        self.airburst_alt_m = astr_math.airburst_altitude(self.breakup_alt_m, self.diameter_m, self.density_kgpm3, self.angle_rad)
        self.is_airburst = self.airburst_alt_m > 0
        self.initial_energy_j = astr_math.kenetic_energy(self.density_kgpm3, self.diameter_m, self.init_velocity_mps)
        self.ret_period_yr = astr_math.return_period_earth(general_units.energy_conversion(self.initial_energy_j, general_units.EnergyUnits.joules, general_units.EnergyUnits.Megaton_TNT))
        self.breakup_velocity_mps = astr_math.velocity_at_altitude_pre_breakup(self.breakup_alt_m, self.init_velocity_mps, self.diameter_m, self.density_kgpm3, self.angle_rad)
        self.airburst_velocity_mps = astr_math.post_breakup_velocity(self.breakup_alt_m, self.breakup_velocity_mps, self.diameter_m, self.density_kgpm3, self.angle_rad, self.is_airburst)
        self.airburst_energy_j = astr_math.kenetic_energy(self.density_kgpm3, self.diameter_m, self.airburst_velocity_mps)

        return

    def get_newmark_overpressure(self, radius_gz_m):
        """
        Get the overpressure of the event at a given radius from ground zero, using the Newmark overpressure equation.
        :param float radius_gz_m: radius from ground zero in meters
        :returns: peak overpressure in bars
        """
        airburst_mttnt = general_units.energy_conversion(self.airburst_energy_j, general_units.EnergyUnits.joules, general_units.EnergyUnits.Megaton_TNT)
        if radius_gz_m == 0:
            radius_gz_m = 0.1
        hyp_distance_m = general_geometry.find_hypotenuse_right_triangle(radius_gz_m, self.airburst_alt_m)
        return expl_math.newmark_overpressure(airburst_mttnt, hyp_distance_m)

    def get_effect_2d_grid(self, as_string=False, num_digits=2, force_calc=False):
        """
        Get the peak overpressure at each point of the event grid in a 2d structure (list of list).
        :param as_string: makes the outpu values strings instead of floats
        :param num_digits: rounds the string values to the specified number of digits.  default is 2
        :param force_calc: bool
        :returns: peak overpressure in a list of lists (list of rows)
        """
        if self.effect_2d_grid is not None and as_string is False and force_calc is False:
            return self.effect_2d_grid

        block_grid = []
        y_max = self.grid.get_block_height_y()
        x_max = self.grid.get_block_width_x()
        try:
            for block_y in range(y_max):
                grid_row = []
                for block_x in range(x_max):
                    lat_lon = self.grid.get_lat_lon(block_x, block_y)
                    radius_gz_m = general_units.haversine_degrees_to_meters(lat_lon[0], lat_lon[1], self.ground_zero_latlon[0], self.ground_zero_latlon[1])
                    newmark_pressure = self.get_newmark_overpressure(radius_gz_m)
                    earth_impacts = 0
                    point_info = newmark_pressure
                    # if as_string:
                    #    point_info = str(format(round(point_info, num_digits), 'f'))
                    point_lng_lat = (lat_lon[1], lat_lon[0])
                    grid_row.append((point_info, point_lng_lat))
                # endfor
                block_grid.append(grid_row)
                # endfor
        except Exception as e:
            print(e)

        self.effect_2d_grid = block_grid

        # if(as_string):
        #    for block_y_2 in range(y_max):
        #        for block_x_2 in range(x_max):
        #            block_grid[block_y_2][block_x_2] = (str(format(round(block_grid[block_y_2][block_x_2][0], num_digits), 'f')), block_grid[block_y_2][block_x_2][1])

        return block_grid

    def get_effect_flat_grid(self, as_string=False, num_digits=2, force_calc=False):
        """
        Get the peak overpressure at each point of the event grid in a flat list.
        :param as_string: makes the output values strings instead of floats
        :param num_digits: rounds the string values to the specified number of digits.  default is 2
        :param force_calc: whether to use the cached grid
        :returns: list of grid points with value as (val, (lng, lat))
        """
        if self.effect_flat_grid is not None and as_string is False and force_calc is False:
            return self.effect_flat_grid

        twod_grid = self.get_effect_2d_grid(as_string, num_digits, force_calc)

        y_max = self.grid.get_block_height_y()
        x_max = self.grid.get_block_width_x()

        out_grid = []

        for block_y in range(y_max):
            for block_x in range(x_max):
                point = twod_grid[block_y][block_x]
                out_grid.append(point)
                # endfor
        # endfor

        self.effect_flat_grid = out_grid
        return out_grid

    def save_grid_2d_text(self, file_name, file_dir="", delim='\t', with_inf=False, with_info_header=False, overwrite=False):
        file_uri_no_ext = file_dir + file_name
        if os.path.isfile(file_uri_no_ext + ".txt") and overwrite is False:
            raise FileExistsError

        os.remove(file_uri_no_ext + ".txt")
        os.remove(file_uri_no_ext + ".inf")

        with open(file_uri_no_ext + ".txt", "w") as write_file:
            if with_info_header:
                out_str = "Top Lat: " + str(self.grid.top_lat_y) + "\t"
                out_str = out_str + "Left lon: " + str(self.grid.left_lon_x) + "\t"
                out_str = out_str + "Block per degree X: " + str(self.grid.block_per_degree_x) + "\t"
                out_str = out_str + "Block per degree y: " + str(self.grid.block_per_degree_t)
                write_file.write(out_str + "\n")

                # for row in grid_res:
                #     out = ""
                #     for val in row:
                #         out = out + val + "\t"
                #     out.rstrip()
                #     write_file.write(out + "\n")

        with open(file_uri_no_ext + ".inf", "w") as write_file_inf:
            write_file_inf.write("Top Lat\t" + str(self.grid.top_lat_y) + "\n")
            write_file_inf.write("Left lon\t" + str(self.grid.left_lon_x) + "\n")
            write_file_inf.write("Block per degree X\t" + str(self.grid.block_per_degree_x) + "\n")
            write_file_inf.write("Block per degree y\t" + str(self.grid.block_per_degree_t) + "\n")
            # write_file_inf.write("create date\t" + 

    def grid_to_geojson(self, val=0):
        grid = self.get_effect_flat_grid()
        points = list(map((lambda x: x[1]), grid))

        # points = []
        # for row in grid:
        #    for block in row:
        #        points.append(block[1])

        ret = lm.create_feature(points, lm.GeojsonGeometry.multipoint, val)
        return ret['geojson']

    def grid_to_geojson_collection_stepped(self, step=1, max_val=None, min_val=None):
        """
        min <= value < max
        geojson val is max of bin
        """
        flat_grid = self.get_effect_flat_grid()
        val_list = []

        if max_val is None or min_val is None:
            val_list = list(map((lambda x: x[0]), flat_grid))

        if max_val is None:
            max_val = max(val_list)
        if min_val is None:
            min_val = min(val_list)

        geojson_collection = []

        curr_max = min_val + step
        curr_min = min_val
        while curr_max < max_val:
            curr_points = list(map((lambda y: y[1]), filter((lambda x: curr_min <= x[0] < curr_max), flat_grid)))
            if len(curr_points) > 0:
                curr_geojson = lm.create_feature(curr_points, lm.GeojsonGeometry.multipoint, curr_max)
                geojson_collection.append(curr_geojson['geojson'])
            curr_max += step
            curr_min += step

        return geojson_collection

    def grid_to_geojson_collection(self):
        flat_grid = self.get_effect_flat_grid()

        geojson_collection = list(map((lambda x: lm.create_feature(x[1], lm.GeojsonGeometry.point, x[0])['geojson']), flat_grid))

        return geojson_collection

    def get_event_res_maxmin(self):
        val_list = list(map((lambda x: x[0]), self.get_effect_flat_grid()))
        return [max(val_list), min(val_list)]

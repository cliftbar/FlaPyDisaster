import flask as fl
from app import app
from globes import *
import general.general_units as gen_units
import general.general_colors as genc


# Asteroid Pages
# asteroid main
@app.route('/asteroid', methods=['GET'])
def asteroid_page():
    return fl.render_template('html/asteroid.html'
                              , title="Asteroid"
                              , distance_units=gen_units.DistanceUnits.get_units_pair()
                              , velocity_units=gen_units.VelocityUnits.get_units_pair())


# Asteroid Functions
@app.route('/asteroid/main_function', methods=['POST'])
def asteroid_function_form():
    print("Hello Asteroid")
    return fl.redirect(fl.url_for('asteroid_page'))


@app.route('/asteroid/input_params_function', methods=['POST'])
def asteroid_input_params_form():
    diameter_in = fl.request.form['diameter']
    diameter_unit = fl.request.form['diameter_unit']

    angle_in = fl.request.form['angle']
    angle_unit = fl.request.form['angle_unit']

    velocity_in = fl.request.form['velocity']
    velocity_unit = fl.request.form['velocity_unit']

    density_kgpm3 = float(fl.request.form['density_kgm3'])
    target_density_kgpm3 = float(fl.request.form['target_density_kgm3'])

    radius_obs_in = fl.request.form['radius_obs']
    radius_obs_unit = fl.request.form['radius_obs_unit']

    flapy_app.asteroid_calculate_event(diameter_in, diameter_unit, angle_in, angle_unit, velocity_in, velocity_unit, density_kgpm3, target_density_kgpm3, radius_obs_in, radius_obs_unit)

    return fl.render_template('html/asteroid_results.html'
                              , title="Asteroid Results"
                              , t_diameter_m=(diameter_in + " " + diameter_unit)
                              , t_angle_deg=(angle_in + " " + angle_unit)
                              , t_velocity_kms=(velocity_in + " " + velocity_unit)
                              , t_density_kgpm3=(str(density_kgpm3) + " kg/m^3")
                              , t_target_density_kgpm3=(str(target_density_kgpm3) + " kg/m^3")
                              # start calculated parameters
                              , t_breakup_alt_m=(str(round(flapy_app.asteroid_event.breakup_alt_m, 2)) + " m")
                              , t_airburst_alt_m=(str(round(flapy_app.asteroid_event.airburst_alt_m, 2)) + " m")
                              , t_energy_MtTnt=(str(round(gen_units.energy_conversion(flapy_app.asteroid_event.initial_energy_j, gen_units.EnergyUnits.joules, gen_units.EnergyUnits.Megaton_TNT), 2)) + " " + gen_units.EnergyUnits.Megaton_TNT)
                              , t_retperiod_yr=(str(round(flapy_app.asteroid_event.ret_period_yr, 2)) + " yr")
                              , t_airburst_velocity_mps=(str(round(flapy_app.asteroid_event.airburst_velocity_mps, 2)) + " " + gen_units.VelocityUnits.mps)
                              , t_airburst_energy_MtTnt=(str(round(gen_units.energy_conversion(flapy_app.asteroid_event.airburst_energy_j, gen_units.EnergyUnits.joules, gen_units.EnergyUnits.Megaton_TNT), 2)) + " " + gen_units.EnergyUnits.Megaton_TNT)
                              , t_radius_obs=(radius_obs_in + radius_obs_unit)
                              , t_overpressure_obs_bar=(str(round(flapy_app.asteroid_get_overpressure_at_radius(), 2)) + " bar") )


@app.route('/asteroid/map_event')
def asteroid_map_event():
    geo = flapy_app.asteroid_event.grid_to_geojson()
    return fl.jsonify(result=geo, max=10, min=2)


@app.route('/asteroid/map_event_geojsoncollection', methods=['GET'])
def asteroid_map_event_geojsoncollection():
    color_ramp = genc.ColorPalettes.hex_to_rgb(genc.ColorPalettes.simple_escalating_5, 255)
    geo_collect = flapy_app.asteroid_event.grid_to_geojson_collection()
    sorted_values = list(map((lambda x: x.properties['value']), geo_collect))
    sorted_values.sort()
    value_bins = genc.ColorPalettes.even_value_breaks(sorted_values, len(color_ramp))
    return fl.jsonify(result=geo_collect, colors=color_ramp, bins=value_bins)

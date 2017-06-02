import flask as fl
from app import app
# import global variables
import globes as gb
import general.general_utils as genu
import general.general_colors as genc
import time
import urllib.request as urlr
import numpy as np
import requests
import json

# global variables
catalog = None


# hurricane main
@app.route('/hurricane', methods=['GET'])
def hurricane_page():
    return fl.render_template('html/hurricane.html', title="Hurricane")


@app.route('/hurricane/main_file', methods=['POST'])
def hurricane_file_form():
    web_file = fl.request.files['hurdat_file']
    file_uri = genu.Web.get_web_file_uri(web_file)

    gb.flapy_app.hurricane_init_catalog(file_uri)
    names = gb.flapy_app.hurricane_catalog.get_names()
    empty_storm_table = gb.flapy_app.get_event_table('null')

    return fl.render_template("html/hurricane_table_test.html", table_name="Catalog Data Frame", data=empty_storm_table, catalog_names=names)


@app.route('/hurricane/load_single_event', methods=['POST'])
def hurricane_load_single_event():
    event_ini_file = fl.request.files['event_ini_file']
    file_uri = genu.Web.get_web_file_uri(event_ini_file)

    gb.flapy_app.hurricane_init_catalog()
    gb.flapy_app.hurricane_add_single_event(file_uri)
    names = gb.flapy_app.hurricane_catalog.get_names()
    empty_storm_table = gb.flapy_app.get_event_table('null')

    return fl.render_template("html/hurricane_table_test.html", table_name="Catalog Data Frame", data=empty_storm_table, catalog_names=names)


@app.route('/hurricane/main_function', methods=['POST'])
def hurricane_function_form():
    hurdat_file = 'Documentation\Hurricane\HURDAT\hurdat2-1851-2015-070616_with_header.txt'

    gb.flapy_app.hurricane_init_catalog(hurdat_file)
    names = gb.flapy_app.hurricane_catalog.get_names()
    names.reverse()
    empty_storm_table = gb.flapy_app.get_event_table('null')

    return fl.render_template("html/hurricane_table_test.html", table_name="Catalog Data Frame", data=empty_storm_table, catalog_names=names)


@app.route('/hurricane/table_test', methods=['GET'])
def table_test():
    hurdat_file = 'Documentation\Hurricane\HURDAT\hurdat2-1851-2015-070616_with_header.txt'

    gb.flapy_app.hurricane_init_catalog(hurdat_file)
    storm_data_table = gb.flapy_app.hurricane_catalog.storm_catalog[0].to_model_dataframe().to_html()
    return fl.render_template("html/hurricane_table_test.html", title="Hurricane Table Test", name="Catalog Data Frame", data=storm_data_table)


@app.route('/hurricane/change_table')
def change_table():
    name = fl.request.args.get('name', '', type=str)
    do_calc = fl.request.args.get('do_calc', 0, type=int)

    if name != 'null':
        gb.flapy_app.set_current_event(name)

        if do_calc == 1:
            gb.flapy_app.calculate_event(name)
        elif do_calc == 2:
            formData = {}
            formData = gb.flapy_app.hurricane_catalog.current_storm.to_model_dataframe().values.tolist()
            formData = [[None if str(x) == "nan" else x for x in l] for l in formData]
            formDict = {}
            #for i in range(len(formData)):
            #    formDict["tp_" + str(i)] = formData[i]
            # formData = ["|".join(map(str, ls)) for ls in formData]
            
            
            # formData = {"track": "test"}
            formDict = {"track": formData}
            gb.flapy_app.hurricane_catalog.current_storm.BuildLatLonGridFromTrack(10, 10)
            formDict['BBox']['topLatY'] = gb.flapy_app.hurricane_catalog.current_storm.lat_lon_grid.top_lat_y
            formDict['BBox']['botLatY'] = gb.flapy_app.hurricane_catalog.current_storm.lat_lon_grid.bot_lat_y
            formDict['BBox']['leftLonX'] = gb.flapy_app.hurricane_catalog.current_storm.lat_lon_grid.left_lon_x
            formDict['BBox']['rightLonX'] = gb.flapy_app.hurricane_catalog.current_storm.lat_lon_grid.right_lon_x

            formDict['rmax'] = gb.flapy_app.hurricane_catalog.current_storm.rmax_nmi
            formDict['fspeed'] = gb.flapy_app.hurricane_catalog.current_storm.fspeed_kts
            # formJson = json.dumps(formData)
            # headers = {'Content-type': 'application/json'}
            # r = requests.post("http://localhost:9000/hurTest", data = formJson, headers=headers)
            r = requests.post("http://localhost:9000/hurTest", json = formDict)
            print(r.status_code)
        ret_data = gb.flapy_app.get_event_table('current')
        calcd = gb.flapy_app.has_event_calcd('current')

        
    else:
        ret_data = gb.flapy_app.get_event_table('null')
        calcd = 0

    return fl.jsonify(table=ret_data, has_calc=calcd)


@app.route('/hurricane/hurricane_table_js.js')
def hurricane_tables_js():
    return fl.render_template('/js/hurricane_tables.js')


@app.route('/hurricane/hurdat_track_to_geojson', methods=['GET'])
def hurricane_track_to_geojson():

    color_ramp = genc.ColorPalettes.hex_to_rgb(genc.ColorPalettes.simple_escalating_5, 255)
    geo_collect = gb.flapy_app.hurricane_catalog.current_storm.track_to_geojson()

    sorted_values = list(map((lambda x: x.properties['value']), geo_collect))
    sorted_values.sort()
    value_bins = genc.ColorPalettes.even_value_breaks(sorted_values, len(color_ramp))

    return fl.jsonify(result=geo_collect, colors=color_ramp, bins=value_bins)


@app.route('/hurricane/set_event_settings', methods=['POST'])
def hurricane_set_event_settings():
    print(fl.request.form)
    form_dict = fl.request.form
    gb.flapy_app.hurricane_set_event_settings(form_dict)
    return 'Success'


@app.route('/hurricane/set_calculation_settings', methods=['POST'])
def hurricane_set_calculation_settings():
    print(fl.request.form)
    form_dict = fl.request.form
    gb.flapy_app.hurricane_set_calculation_settings(form_dict)
    return 'Success'


@app.route('/hurricane/hurricane_event_to_geojson')
def hurricane_event_to_geojson():
    geo_collect = gb.flapy_app.hurricane_catalog.current_storm.grid_to_geojson()

    sorted_values = list(map((lambda x: x.properties['value']), geo_collect))
    sorted_values.sort()
    color_ramp = genc.ColorPalettes.hex_to_rgb(genc.ColorPalettes.simple_escalating_5, 255)
    value_bins = genc.ColorPalettes.even_value_breaks(sorted_values, len(color_ramp))

    print("sending geojson events")
    return fl.jsonify(result=geo_collect, colors=color_ramp, bins=value_bins)


@app.route('/hurricane/geojson_event_canvas')
def map_hurricane_event_canvas():
    storm = gb.flapy_app.hurricane_catalog.current_storm

    sorted_values = list(map((lambda x: x[2]), storm.result_array))
    sorted_values.sort()
    color_ramp = genc.ColorPalettes.hex_to_rgb(genc.ColorPalettes.simple_escalating_5, 255)
    value_bins = genc.ColorPalettes.even_value_breaks(sorted_values, len(color_ramp))

    print("sending canvas events")
    return fl.jsonify(colors=color_ramp, bins=value_bins, data=storm.result_array)


@app.route('/hurricane/geojson_event_canvas_nocolor')
def map_hurricane_event_canvas_nocolor():
    storm = gb.flapy_app.hurricane_catalog.current_storm

    print("sending canvas events no_color")
    return fl.jsonify(data=storm.result_array)


@app.route('/hurricane/save_event_to_raster', methods=['POST', 'GET'])
def hurricane_save_event_to_raster():
    event_save_suffix = fl.request.args.get('event_save_suffix')
    print("Event save suffix: " + event_save_suffix)

    version = ''
    static_uri = ''
    if gb.flapy_app.hurricane_catalog.current_storm.result_array is not None:
        user_folder = app.config.get('USER_FOLDER')
        gb.flapy_app.hurricane_save_event_to_raster(event_save_suffix, user_folder)
        base_name = gb.flapy_app.current_hurricane_name
        with open('test_out.txt', 'w') as fi:
            fi.write("x\ty\tz\n")
            for line in gb.flapy_app.hurricane_catalog.current_storm.grid_to_xyz_list():
                fi.write(str(line[0]) + "\t" + str(line[1]) + "\t" + str(line[2]) + "\n")
        if event_save_suffix != '':
            base_name += "_" + event_save_suffix
        static_uri = r"images/" + base_name + ".png"
        version = time.time()

    return fl.jsonify(file_uri=fl.url_for('static', filename=static_uri, ver=version))


@app.route('/hurricane/load_unisys_event', methods=['POST'])
def hurricane_load_unisys_event():
    unisys_event_file = fl.request.form['unisys_event_file']
    unisys_event_local_uri = app.config['UPLOAD_FOLDER'] + 'unisys_track.dat'
    urlr.urlretrieve(unisys_event_file, unisys_event_local_uri)

    gb.flapy_app.hurricane_init_catalog()
    gb.flapy_app.hurricane_add_unisys_event(unisys_event_local_uri)
    names = gb.flapy_app.hurricane_catalog.get_names()
    empty_storm_table = gb.flapy_app.get_event_table('null')

    return fl.render_template("html/hurricane_table_test.html", table_name="Catalog Data Frame", data=empty_storm_table, catalog_names=names)

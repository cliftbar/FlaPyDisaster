import flask as fl
from app import app
import globes as gb
import general.general_colors as genc
import geojson
import mapping.leaflet_map as lm
import json
import pandas as pd


@app.route('/leaflet')
@app.route('/leaflet/<string:sender_include>')
def leaflet_redirect(sender_include='none'):
    color_ramp = genc.ColorPalettes.simple_escalating_5
    color_scheme = genc.get_named_color_scheme_colors_from_config('0!default')
    print("test")
    print(color_scheme)
    
    user_color_num = gb.number_config_options('UserStyles')
    named_color_schemes = genc.get_named_color_schemes_from_config()
    return fl.render_template('html/leaflet_test.html'
                              , title="Leaflet"
                              # , sender_include='html/hurricane_map_base.html'
                              , sender_include=sender_include
                              , default_colors=color_scheme
                              , user_color_num=user_color_num
                              , named_color_schemes=named_color_schemes
                              , current_user=fl.session['username'] )

@app.route('/leaflet/mapping')
def leaflet_mapping(sender_include='none'):
    color_ramp = genc.ColorPalettes.simple_escalating_5
    color_scheme = genc.get_named_color_scheme_colors_from_config('0!default')
    user_color_num = gb.number_config_options('UserStyles')
    named_color_schemes = genc.get_named_color_schemes_from_config()
    
    storm_name = gb.flapy_app.hurricane_catalog.current_storm.unique_name
    storm_df = gb.flapy_app.hurricane_catalog.current_storm.to_model_dataframe()
    # storm_cols = pd.DataFrame(list([storm_df.columns.values])).to_html(classes="track_table_header", index=False, header=False)
    storm_table = storm_df.to_html(classes='track_table table table-responsive table-hover" id="eventTable', index=False)
    return fl.render_template('html/leaflet_mapping.html'
                              #, title="Mapping"
                              , sender_include=sender_include
                              , default_colors=color_scheme
                              , user_color_num=user_color_num
                              , named_color_schemes=named_color_schemes
                              , event_name = storm_name
                              # , headers = storm_cols
                              , data = storm_table
                              , current_user=fl.session['username'] )

@app.route('/leaflet/test_latlng', methods=['POST'])
def leaflet_test_latlng():
    lat = fl.request.json['lat']
    lng = fl.request.json['lng']

    print("leaflet test in Flask.")
    print("lat: " + lat)
    print("lng: " + lng)
    return "Success"


@app.route('/leaflet/test_js.js')
def leaflet_test_js():
    return fl.render_template('/js/leaflet_test.js')


@app.route('/leaflet/geojson_test')
def leaflet_geojson_test():
    points = [(-71.15, 42.4), (-71.12, 42.4), (-71.05, 42.4)]

    # noinspection PyTypeChecker
    line_str = geojson.LineString(points)

    line_feature = geojson.Feature(geometry=line_str, properties={"value": 5})

    return fl.jsonify(result=line_feature, max=10, min=2)


@app.route('/leaflet/geojson_points_test')
def leaflet_geojson_points_test():
    # 42.4, -71.15   42.4, -71.12        42.4, -71.05
    points = [(-71.15, 42.4), (-71.12, 42.4), (-71.05, 42.4)]
    # multi_pt = geojson.MultiPoint(points)
    # pt_feature = geojson.Feature(geometry = multi_pt, properties = {"value":1})

    pt_feature_dict = lm.create_feature(points, lm.GeojsonGeometry.multipoint, 1)
    return fl.jsonify(result=pt_feature_dict['geojson'], max=10, min=2)


@app.route('/leaflet/get_color_array', methods=['POST'])
def leaflet_get_color_array():
    color_scheme = flapy_app.leaflet_get_color_array_from_config(fl.request.form)
    next_num = number_config_options('UserStyles')

    return fl.jsonify(color_scheme=color_scheme, next_default_num=next_num)


@app.route('/leaflet/change_named_color_scheme', methods=['POST', 'GET'])
def leaflet_change_named_color_scheme():
    print("leaflet_change_named_color_scheme")
    scheme_name = fl.request.form['scheme_name']
    print(scheme_name)
    color_scheme = genc.get_named_color_scheme_colors_from_config(scheme_name)
    print(color_scheme)
    return fl.jsonify(color_scheme=color_scheme)

@app.route('/leaflet/get_named_color_scheme', methods=['GET'])
def leaflet_get_named_color_scheme():
    print("leaflet_get_named_color_scheme")
    scheme_name = fl.request.args['scheme_name']
    hex_color_scheme = genc.get_named_color_scheme_colors_from_config(scheme_name)
    return fl.jsonify(color_scheme=hex_color_scheme)


@app.route('/leaflet/save_named_color_scheme', methods=['POST'])
def leaflet_save_color_scheme():
    print("leaflet_save_color_scheme")
    color_data = json.loads(fl.request.form['color_data'])
    hex_color_scheme = [color_data['color_scheme_colors'], color_data['color_scheme_values']] # flapy_app.hex_scheme_from_form(form_dict)
    name = color_data['color_scheme_name']
    genc.save_named_color_scheme_to_config(name, hex_color_scheme)
    print("scheme saved")
    return json.dumps({'result': "Scheme Saved"})

@app.route('/leaflet/draw_event')
@app.route('/leaflet/draw_event/<string:sender_include>')
def leaflet_draw_event(sender_include='none'):
    return fl.render_template('/html/leaflet_draw_hurricane_event.html'
                              , title='Draw Event'
                              , sender_include=sender_include)


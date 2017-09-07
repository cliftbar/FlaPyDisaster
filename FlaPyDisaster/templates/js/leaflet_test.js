/********************
 * Global Variables *
 ********************/
var mymap;
var draw_control;
var popup = L.popup();
var last_point_clicked;
var layers = {};



function leaflet_draw_monkey_patches(){
    // Override leaflet draw function here for 1.0 support.  spliceLatLngs is no longer supported
    L.Edit.Poly.prototype._removeMarker = function(marker) {
        console.log("Imma monkey! L.Edit.Poly.prototype._removeMarker")
        var i = marker._index;

        this._markerGroup.removeLayer(marker);
        this._markers.splice(i, 1);
        this._poly.getLatLngs().splice(i, 1);
        this._updateIndexes(i, -1);

        marker
            .off('drag', this._onMarkerDrag, this)
            .off('dragend', this._fireEdit, this)
            .off('click', this._onMarkerClick, this);
    };

    L.Edit.Poly.prototype._createMiddleMarker = function (marker1, marker2) {
        console.log("Imma monkey! Patching L.Edit.Poly.prototype._createMiddleMarker")
        var latlng = this._getMiddleLatLng(marker1, marker2),
            marker = this._createMarker(latlng),
            onClick,
            onDragStart,
            onDragEnd;

        marker.setOpacity(0.6);

        marker1._middleRight = marker2._middleLeft = marker;

        onDragStart = function () {
            var i = marker2._index;

            marker._index = i;

            marker
                .off('click', onClick, this)
                .on('click', this._onMarkerClick, this);

            latlng.lat = marker.getLatLng().lat;
            latlng.lng = marker.getLatLng().lng;
            var curr_latlngs = this._poly.getLatLngs()
            curr_latlngs.splice(i, 0, latlng)
            this._poly.setLatLngs(curr_latlngs)
            this._markers.splice(i, 0, marker);

            marker.setOpacity(1);

            this._updateIndexes(i, 1);
            marker2._index++;
            this._updatePrevNext(marker1, marker);
            this._updatePrevNext(marker, marker2);

            this._poly.fire('editstart');
        };

        onDragEnd = function () {
            marker.off('dragstart', onDragStart, this);
            marker.off('dragend', onDragEnd, this);

            this._createMiddleMarker(marker1, marker);
            this._createMiddleMarker(marker, marker2);
        };

        onClick = function () {
            onDragStart.call(this);
            onDragEnd.call(this);
            this._fireEdit();
        };

        marker
            .on('click', onClick, this)
            .on('dragstart', onDragStart, this)
            .on('dragend', onDragEnd, this);

        this._markerGroup.addLayer(marker);
    };

    L.Draw.Polyline.prototype.deleteLastVertex = function () {
        console.log("Imma monkey! L.Draw.Polyline.prototype.deleteLastVertex")
        if (this._markers.length <= 1) {
            return;
        }

        var lastMarker = this._markers.pop(),
            poly = this._poly,
            latlng = this._poly.getLatLngs().splice(poly.getLatLngs().length - 1, 1)[0];

        this._markerGroup.removeLayer(lastMarker);

        if (poly.getLatLngs().length < 2) {
            this._map.removeLayer(poly);
        }

        this._vertexChanged(latlng, false);
    };


}


/*****************************
 * Leaflet Map Initalization *
 *****************************/

/*
 * Initialization function for leaflet page, runs on the html.body.onload
 */
function leaflet_init() {
    //window.alert("Hello Leaflet")
    init_map();

    //L.tileLayer('https://api.mapbox.com/styles/v1/mapbox/streets-v9/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoidW5nYXdhdGt0IiwiYSI6ImNpcHU1aXJ0bDA5MHJma20yN3QwaGthZW8ifQ.oCspKOOXmELA6ETDQK8J1w').addTo(mymap);

    // Add a map tile layer to the leaflet map.  Uses the standard streets maps
    L.tileLayer('https://api.mapbox.com/styles/v1/mapbox/streets-v9/tiles/256/{z}/{x}/{y}?access_token={accessToken}', {
        accessToken: 'pk.eyJ1IjoidW5nYXdhdGt0IiwiYSI6ImNpcHU1aXJ0bDA5MHJma20yN3QwaGthZW8ifQ.oCspKOOXmELA6ETDQK8J1w'
    }).addTo(mymap);

    // Create a layer to hold user placed markers through the standard function (which needs way more work)
    create_layer('marker_layer');

    // Create a standard geoJSON layer for lines, polygons, etx
    var myLayer = L.geoJson().addTo(mymap);
    layers['geoJSON'] = myLayer;

    // Create a geogjson layer to handle points.  The function passed to "pointsToLayer" converts points to an orange circle, instead of markers.
    // Need to figure out how to update that style post creation
    var pointsLayer = L.geoJson([], {
        pointToLayer: function (feature, latlng) {
            var geojsonMarkerOptions = {
                radius: 8,
                fillColor: "#ff7800",
                //fillColor: get_interpolated_color(feature.properties.value, 10, 2),
                color: "#000",
                weight: 1,
                opacity: 1,
                fillOpacity: 0.8
            };
            return L.circleMarker(latlng, geojsonMarkerOptions);
        }
    }).addTo(mymap);
    layers['point_geoJSON'] = pointsLayer;

    // Create a canvas layer
    var canvas = L.canvasLayer().delegate(this).addTo(mymap);
    layers['canvas'] = canvas;

    // call a function that handles adding any required event handlers
    add_handlers();

    L.control.layers(null, layers).addTo(mymap);

    // prove we got here in the log
    console.log("Did leaflet init");


}

/*
 * Add event handlers for map
 */
function add_handlers() {
    // mymap.on('click', onMapClick);
}

/*
 * On Click handler to add a marker to the map on click if the control checkbox is checked.
 * Also stores the click location for debugging
 */
//function onMapClick(e) {
//    // Popup example
//    popup
//        .setLatLng(e.latlng)
//        .setContent("You clicked the map at " + e.latlng.toString())
//        .openOn(mymap);
//
//    last_point_clicked = e.latlng;
//
//    var add_point_cbx = document.getElementById('add_marker_CBX');
//    if (add_point_cbx.checked) {
//        place_marker(e.latlng, true)
//    }
//}

/*
 * Initialize leaflet map, setting the view to Somerville, MA, with a zoom level of 13
 */
function init_map() {
    mymap = L.map('mapid',
        {
            zoomControl: 'True'
            //}).setView([42.39, -71.11], 13);
        }
    ).setView([0, 0], 2);

    if (document.getElementById('leaflet_draw_script')) {
        leaflet_draw_monkey_patches();
        init_draw();
    }
}

function init_draw() {
    var drawn_items = new L.FeatureGroup();
    mymap.addLayer(drawn_items);
    layers['drawn_items'] = drawn_items;

    draw_control = new L.Control.Draw({
        draw: {
            position: 'topleft',
            polygon: {
                title: 'Draw Controls',
                allowIntersection: false,
                drawError: {
                    color: '#b00b00',
                    timeout: 1000
                },
                shapeOptions: {
                    color: '#bada55'
                },
                showArea: true
            },
            polyline: {
                metric: false
            },
            rectangle: {
                shapeOptions: {
                    color: '#ffff00'
                }
            },
            circle: {
                shapeOptions: {
                    color: '#662d91'
                }
            },
            marker: false
        },
        edit: {
            featureGroup: layers['drawn_items']
        }
    });

    mymap.addControl(draw_control);

    mymap.on('draw:created', function (e) {
        layers['drawn_items'].addLayer(e.layer);

        var shapes = getShapes(layers['drawn_items']);
        //console.log('got ' + shapes.length.tostring() + ' shapes')
    });
}

//http://stackoverflow.com/questions/24018630/how-to-save-a-completed-polygon-points-leaflet-draw-to-mysql-table
var getShapes = function(drawnItems, type) {

    var shapes = [];

    drawnItems.eachLayer(function(layer) {

        // Note: Rectangle extends Polygon. Polygon extends Polyline.
        // Therefore, all of them are instances of Polyline
        if (layer instanceof L.Polyline) {
            shapes.push(layer.getLatLngs())
        }
        if (layer instanceof L.Polyline) {
            shapes.push(layer.getLatLngs())
        }

        if (layer instanceof L.Circle) {
            shapes.push([layer.getLatLng()])
        }

        if (layer instanceof L.Marker) {
            shapes.push([layer.getLatLng()]);
        }

    });

    return shapes;
};

/*********************
 * Layer Controllers *
 *********************/

/*
 * Function to add a generic layer to the leaflet map, and store a reference to the layer
 */
function create_layer(layer_name){
    var Layer = L.layerGroup().addTo(mymap);
    layers[layer_name] = Layer
}

/*
 * Function to clear the content from a leaflet layer.  Does NOT remove the layer
 */
function clear_layer(layer_name) {
    if(layer_name == 'canvas'){
        canvas_settings.canvas_data = [];
        layers['canvas'].needRedraw();
    }
    else if (layer_name == 'legend'){
        layers['legend'].remove();
    }else{
        layers[layer_name].clearLayers();
    }

}

/*
 * Remove all layers from the map (excluding map tile layers).
 */
function clear_map() {
    for (var layer in layers) {
        clear_layer(layer);
    }
}

/*
 * Places a marker at the given point.
 * If 'log_point' is true, logs the placement to javascript and python the console.
 * Python server logging is accomplished through a JQuery ajax POST
 */
function place_marker(latlng, log_point) {

    // Add marker to map
    var marker = L.marker(latlng);
    layers['marker_layer'].addLayer(marker);

    // Log point to javascript console
    if (log_point) {
        var data = {};
        data["lat"] = latlng.lat.toString();
        data["lng"] = latlng.lng.toString();
        console.log(data);
    }

    // Ajax call if logging point to python, stick with this format probably, more standard
    if (log_point) {
        $.ajax({
            type: "POST"
            , url: "{{ url_for('leaflet_test_latlng') }}"
            , data: JSON.stringify(data, null, '\t')
            , contentType: 'application/json;charset=UTF-8'
            , success: function (result) { return undefined }
        });
    }
}

/*
 * TEST METHOD
 * Test function to place a marker at the last point click on the map
 */
function place_last_marker_test() {
    place_marker(last_point_clicked, true);
}

/*
 * TEST METHOD
 * Little test event function to log the state of the add marker checkbox
 */
function alert_state() {
    var cb = document.getElementById('add_marker_CBX');
    
    console.log(cb.checked);
}


/*******************
 * GeoJSON methods *
 *******************/

/*
 * TEST METHOD
 * Function to get a geoJSON line from the server and plot it on the leaflet map.
 * The color of the map is controlled by interpolating the properties.value value associated with the geoJSON object
 */
function geojson_test() {
    // JQuery getJSON call to python server to get geoJSON string
    $.getJSON("{{ url_for('leaflet_geojson_test') }}", {},
        function (data) {
            // Add new jsoon to layer
            layers['geoJSON'].addData(data.result).setStyle(
                function(feature){
                    return {
                        // Interpolate the color based on the value, and passed in max and min
                        color: get_interpolated_color(feature.properties.value, data.max, data.min),
                        weight: 5,
                        opacity: 1
                    }
                })
            /*
             var geojson_layer = L.geoJSON(data.result, {
                style: function(feature){
                    return {
                        color: get_graduated_style_test(feature, data.max, data.min),
                        weight: 5,
                        opacity: 1
                    }
                }
            }).addTo(mymap);
             */
        }
    )
}

/*
 * TEST METHOD
 * funtion to get a geoJSON multipoint layer from the server and plot it on the leaflet map.
 */
function geojson_points_test() {
    $.getJSON("{{ url_for('leaflet_geojson_points_test') }}", {},
        function (data) {
            // Add with static style.  Need to implement dynamic styles somehow
            layers['point_geoJSON'].addData(data.result);
        }
    )
}

/*
 * TEST METHOD
 * function to get a geoJSON multipoint layer from the asteroid event and plot it on the leaflet map.
 */
function geojson_asteroid_points_test() {
    $.getJSON("{{ url_for('asteroid_map_event') }}", {},
        function (data) {
            // Add with static style.  Need to implement dynamic styles somehow
            layers['point_geoJSON'].addData(data.result);
        }
    )
}

/*
 * TEST METHOD
 * funtion to get a geoJSON multipoint layer from the asteroid event and plot it on the leaflet map with a dynamic point style
 */
function geojson_asteroid_points_test_style() {
    $.getJSON("{{ url_for('asteroid_map_event_geojsoncollection') }}", {},
        function (data) {
            // Add with static style.  Need to implement dynamic styles somehow

            if (!layers.hasOwnProperty('point_geoJSON_style')) {
                var pointsLayer = L.geoJson([], {
                    pointToLayer: function (feature, latlng) {
                        var geojsonMarkerOptions = {
                            radius: 8
                            //,fillColor: "#ff7800"
                            //,fillColor: get_interpolated_color(feature.properties.value, data.max, data.min)
                            ,fillColor: color_pretty_breaks(feature.properties.value, data.colors, data.bins)
                            ,color: "#000"
                            ,weight: 1
                            ,opacity: 1
                            ,fillOpacity: 1.0
                        };
                        return L.circleMarker(latlng, geojsonMarkerOptions);
                    }
                }).addTo(mymap);
                layers['point_geoJSON_style'] = pointsLayer;
            }

            for (var i = 0; i < data.result.length; i++) {
                var geojson = data.result[i];
                layers['point_geoJSON_style'].addData(geojson);
            }
            
        }
    )
}

/*********************
 * Hurricane Methods *
 *********************/

/*
 * TEST METHOD
 * funtion to get a geoJSON multipoint layer from the hurdat hurricane event and plot it on the leaflet map with a dynamic point style
 */
function hurricane_track_to_geojson() {
    $.getJSON("{{ url_for('hurricane_track_to_geojson') }}", {},
        function (data) {
            // Add with static style.  Need to implement dynamic styles somehow

            if (!layers.hasOwnProperty('point_geoJSON_style')) {
                var pointsLayer = L.geoJson([], {
                    pointToLayer: function (feature, latlng) {
                        var geojsonMarkerOptions = {
                            radius: 8
                            //,fillColor: "#ff7800"
                            //,fillColor: get_interpolated_color(feature.properties.value, data.max, data.min)
                            ,fillColor: color_pretty_breaks(feature.properties.value, data.colors, data.bins)
                            ,color: "#000"
                            ,weight: 1
                            ,opacity: 1
                            ,fillOpacity: 1.0
                        };
                        return L.circleMarker(latlng, geojsonMarkerOptions);
                    }
                }).addTo(mymap);
                layers['point_geoJSON_style'] = pointsLayer;
            }

            for (var i = 0; i < data.result.length; i++) {
                var geojson = data.result[i];
                layers['point_geoJSON_style'].addData(geojson);
            }

        }
    )
}

function hurricane_event_to_geojson() {
    $.getJSON("{{ url_for('hurricane_event_to_geojson') }}", {},
        function (data) {
            // Add with static style.  Need to implement dynamic styles somehow

            if (!layers.hasOwnProperty('point_geoJSON_style')) {
                var pointsLayer = L.geoJson([], {
                    pointToLayer: function (feature, latlng) {
                        var geojsonMarkerOptions = {
                            radius: 8
                            //,fillColor: "#ff7800"
                            //,fillColor: get_interpolated_color(feature.properties.value, data.max, data.min)
                            ,fillColor: color_pretty_breaks(feature.properties.value, data.colors, data.bins)
                            ,color: "#000"
                            ,weight: 1
                            ,opacity: 0
                            ,fillOpacity: 1.0
                        };
                        return L.circleMarker(latlng, geojsonMarkerOptions).bindPopup(String(feature.properties.value));
                    },
                }).addTo(mymap);
                layers['point_geoJSON_style'] = pointsLayer;
            }

            for (var i = 0; i < data.result.length; i++) {
                var geojson = data.result[i];
                layers['point_geoJSON_style'].addData(geojson);
            }

            layers['point_geoJSON_style'].bindPopup(feature.properties.value);
        }
    )
}

function geojson_hurdat_event_nocolor() {
    $.getJSON("{{ url_for('hurricane_event_to_geojson') }}", {},
        function (data) {
            if (!layers.hasOwnProperty('point_geoJSON_style')) {
                var pointsLayer = L.geoJson([], {
                    pointToLayer: function (feature, latlng) {
                        var geojsonMarkerOptions = {
                            radius: 8
                            ,fillColor: color_pretty_breaks(feature.properties.value, data.colors, data.bins)
                            ,color: "#000"
                            ,weight: 1
                            ,opacity: 0
                            ,fillOpacity: 1.0
                        };
                        return L.circleMarker(latlng, geojsonMarkerOptions).bindPopup(String(feature.properties.value));
                    },
                }).addTo(mymap);
                layers['point_geoJSON_style'] = pointsLayer;
            }

            for (var i = 0; i < data.result.length; i++) {
                var geojson = data.result[i];
                layers['point_geoJSON_style'].addData(geojson);
            }
        }
    )
}

/********************
 * Canvas Functions *
 ********************/
 //http://stackoverflow.com/questions/13916066/speed-up-the-drawing-of-many-points-on-a-html5-canvas-element
 //http://bl.ocks.org/sumbera/11114288
 //https://github.com/Sumbera/gLayers.Leaflet

//var canvas_data = []
//var canvas_values = []
// Set default color ramp
// var canvas_colors = [[0,0,255], [0,128,0], [255,255,0], [255,165,0], [255,0,0]]
var canvas_colors = ['#0000FF', '#008000', '#FFFF00', '#FFA500', '#FF0000']
var current_color_scheme = []

// var hurricane_break_colors = [[220,220,220], [0,0,255], [0,128,0], [255,255,0], [255,165,0], [255,0,0], [255,0,255]]
var hurricane_break_colors = ['#DCDCDC', '#0000ff', '#008000', '#ffff00', '#ffa500', '#ff0000', '#ff00ff']
//var hurricane_breaks = [0, 34, 65, 84, 96, 114, 135]
var hurricane_breaks = [0, 34, 65, 84, 96, 114, 135]
var color_bins = [0, 20, 40, 60, 80]
var canvas_opacity = 0.2

var canvas_settings = {
    canvas_data: []
    , canvas_values: []
    , canvas_colors: ['#0000FF', '#008000', '#FFFF00', '#FFA500', '#FF0000']
    , color_scheme: {
        values: []
        , colors: []
    }
    , canvas_opacity: 0.2
}

function onDrawLayer(info) {
    // Timing debug code
    //var t0 = Date.now();

    var ctx = info.canvas.getContext('2d');
    ctx.clearRect(0, 0, info.canvas.width, info.canvas.height);

    // color info
    var n = canvas_colors.length;
    var r = 5;
    var d = r * 2;

    var offScreen = document.createElement('canvas');
    offScreen.width = n * d;
    offScreen.height = d;
    var offCtx = offScreen.getContext('2d');

    // Debug for viewing offscreen canvas
    //offCtx.fillStyle = "black";
    //offCtx.fillRect(0, 0, offScreen.width, offScreen.height);

    for (var i = 0; i < n; ++i) {
        var color = canvas_colors[i];
        color_rgb = hexToRgb(color)
        offCtx.fillStyle = "rgba(" + color_rgb[0].toString() + ", " + color_rgb[1].toString() + ", " + color_rgb[2].toString() + ", " + canvas_opacity.toString() + ")";
        //console.log("rgb(" + color[0].toString() + ", " + color[1].toString() + ", " + color[2].toString() + ")");
        offCtx.beginPath();
        offCtx.arc(i * d + r, r, r, 0, 2 * Math.PI);
        offCtx.closePath();
        offCtx.fill();
    }

    // Lower bound version
    for (var i = 0; i < canvas_settings.canvas_data.length; ++i) {
        var p = canvas_settings.canvas_data[i];
        c = 0;
        for (pos = color_bins.length; pos >= 0; --pos){
            if(p[2] >= color_bins[pos]){
                c = pos;
                break;
            }
        }
        dot = info.layer._map.latLngToContainerPoint([p[0], p[1]]); //can move this out of loop
        ctx.drawImage(offScreen, c * d, 0, d, d, dot.x - r, dot.y - r, d, d);
    }


    // Debug to view the offscreen canvas
    //if (canvas_data.length > 0){
    //    ctx.drawImage(offScreen, 0, 0, d * n, d, 60 , 60, d * n, d);
    //}

    var t1 = Date.now();
    // Timing debug code
    //if(canvas_data.length > 0){
    //    alert((t1 - t0) + "ms");
    //}
}

function set_opacity(){
    canvas_opacity = Math.trunc(document.getElementById("opacity_value").value) / 100
    layers['canvas'].needRedraw();
}

//DEP
function hurdat_event_canvas() {
    $.getJSON("{{ url_for('map_hurricane_event_canvas') }}", {},
        function (data) {
            canvas_settings.canvas_data = data.data;
            canvas_colors = data.colors;
            color_bins = data.bins;
            layers['canvas'].needRedraw();
        }
    )
}

function hurdat_event_canvas_nocolor() {
    $.getJSON("{{ url_for('map_hurricane_event_canvas_nocolor') }}", {},
        function (data) {
            canvas_settings.canvas_data = data.data
            var vals = data.data.map(function (val){
                return val[2]
            });
            vals.sort(function(a, b){return a-b});
            canvas_settings.current_color_scheme.colors = vals;
            layers['canvas'].needRedraw();
            add_legend(color_bins, canvas_colors);
        }
    )
}

function hurdat_recalc_bins(values) {
    color_bins = even_value_breaks(values, canvas_colors.length);
}

// By Lower Bound
function add_legend(bins, colors) {
    var legend = L.control({position: 'bottomright'});
    layers['legend'] = legend;

    legend.onAdd = function (mymap) {

        var div = L.DomUtil.create('div', 'info legend');

        // loop through our density intervals and generate a label with a colored square for each interval
        for (var i = 0; i < bins.length; i++) {
            var rgb = hexToRgb(canvas_colors[i])
            color = 'rgb(' + rgb[0] + ',' + rgb[1] + ',' + rgb[2] + ')';
            div.innerHTML +=
                '<i style="background:' + color + '"></i> ' +
                Math.trunc(bins[i]) + (Math.trunc(bins[i + 1]) ? 'kts &ndash; ' + Math.trunc(bins[i + 1]) + ' kts<br>' : '+');
        }

        return div;
    };

    legend.addTo(mymap);
}

/***********************
 * Color Modal Methods *
 ***********************/

// increment color amounts
// http://www.randomsnippets.com/2008/02/21/how-to-dynamically-add-form-elements-via-javascript/
var color_count = 5
function add_color(color, value){
    if(color == null){
        color = "#ffffff";
    }

    if(value == null){
        value = 0
    }

    var newdiv = document.createElement("div");
    newdiv.setAttribute("id", "div_color_" + color_count)
    newdiv.innerHTML = "<input type='color' name='color_" + color_count + "' id='color_" + color_count + "' value=" + color + ">\n"
        + "<input type='number' name='color_val_" + color_count + "' id='color_val_" + color_count + "' value='" + value + "'>";
    document.getElementById('bin_colors').appendChild(newdiv);
    color_count++;
    document.getElementById('number_colors').value = color_count;
}

function remove_color(){
    if (color_count == 0){
        return
    }
    var element = document.getElementById("div_color_" + (color_count-1));
    element.parentNode.removeChild(element);
    color_count--;
    document.getElementById('number_colors').value = color_count;
}

//DEP
function set_colors(){
    $.ajax({
           type: "POST",
           dataType: "json",
           url: "{{ url_for('leaflet_get_color_array') }}",
           data: $("#bin_colors").serialize(),
           success: function (data) {
               set_colors_success(data.color_scheme, canvas_settings.color_scheme.values);
           }
         }
    );

    //if (document.getElementById('save_color_scheme').checked){
    //    var name = document.getElementById('color_scheme_name').value;
    //    var option = document.createElement("option");
    //    option.value = name;
    //    option.text = name;
    //    option.selected = true;
    //    document.getElementById('color_scheme_selector').add(option);
    //}
}

//DEP
function set_colors_success(color_scheme, values){
    canvas_colors = color_scheme[0];
    color_bins = color_scheme[1]
    //hurdat_recalc_bins(values);
    if(document.getElementById('color_bin_source').value == 'even'){
        hurdat_recalc_bins(canvas_settings.color_scheme.values);
    }
    layers['canvas'].needRedraw();

    if('legend' in layers) {
        layers['legend'].remove();
        add_legend(color_bins, canvas_colors);
    }
}

function apply_colors() {
    update_scheme_from_picker()

    canvas_colors = current_color_scheme[0];
    color_bins = current_color_scheme[1]

    if (document.getElementById('color_bin_source').value == 'even') {
        hurdat_recalc_bins(canvas_settings.color_scheme.values);
    }
    layers['canvas'].needRedraw();

    if ('legend' in layers) {
        layers['legend'].remove();
        add_legend(color_bins, canvas_colors);
    }
}

function set_hurricane_colors(){
    canvas_colors = hurricane_break_colors;
    color_bins = hurricane_breaks;
    update_color_picker()

    $.ajax({
           type: "POST",
           dataType: "json",
           url: "{{ url_for('leaflet_get_color_array') }}",
           data: $("#bin_colors").serialize(),
           success: function (data) {
                canvas_colors = data.color_array;
                layers['canvas'].needRedraw();

                if('legend' in layers) {
                    layers['legend'].remove();
                    add_legend(color_bins, canvas_colors);
                }
           }
         }
    );
}

function get_named_color_scheme() {
    var index = document.getElementById('color_scheme_selector').selectedIndex
    var item = document.getElementById('color_scheme_selector')[index]
    var val = item.value
    var text = item.text

    $.ajax({
        type: "GET"
        , url: "{{ url_for('leaflet_get_named_color_scheme') }}"
        , data: {
            scheme_name: val //document.getElementById('color_scheme_selector').value
        }
        , success: function (data) {
            current_color_scheme = data.color_scheme
            canvas_colors = data.color_scheme[0];
            if (document.getElementById('color_bin_source').value == 'even') {
                hurdat_recalc_bins(canvas_settings.color_scheme.values);
            } else {
                color_bins = data.color_scheme[1];
            }

            update_color_picker();
        }
    });
}

//DEP
function change_named_color_scheme(){
    var item = document.getElementById('color_scheme_selector')
    var val = item.value
    var text = item.text
    $.ajax({
           type: "POST"
           ,url: "{{ url_for('leaflet_change_named_color_scheme') }}"
           ,data: {
                scheme_name: document.getElementById('color_scheme_selector').value
            }
           ,success: function (data) {
                canvas_colors = data.color_scheme[0];
                if(document.getElementById('color_bin_source').value == 'even'){
                    hurdat_recalc_bins(canvas_settings.color_scheme.values);
                }else{
                    color_bins = data.color_scheme[1];
                }

                update_color_picker()
                set_colors()
           }
    });
}

function save_color_scheme() {
    update_scheme_from_picker()
    var name = document.getElementById('color_scheme_name').value;
    if (name == "") {
        alert("Supply a name")
        return
    }
    $.ajax({
        type: "POST",
        dataType: "json",
        url: "{{ url_for('leaflet_save_color_scheme') }}",
        data: {
            color_data: JSON.stringify({
                color_scheme_colors: current_color_scheme[0],
                color_scheme_values: current_color_scheme[1],
                color_scheme_name: name
            })
        },
        success: function () {

            // Update color scheme selector
            var option = document.createElement("option");
            option.value = name;
            option.text = name;
            option.selected = true;
            document.getElementById('color_scheme_selector').add(option);
        }
    });
}

function update_color_picker(){
    while (color_count > 0){
        remove_color();
    }

    while (color_count < canvas_colors.length){
        add_color(canvas_colors[color_count], color_bins[color_count]);
    }
}

function update_scheme_from_picker() {
    var new_scheme = [[],[]]
    for (var i = 0; i < color_count; ++i) {
        var element = document.getElementById("div_color_" + (color_count - 1))
        new_color = document.getElementById("color_" + i).value
        new_value = parseInt(document.getElementById("color_val_" + i).value)
        new_scheme[0].push(new_color)
        new_scheme[1].push(new_value)
    }
    current_color_scheme = new_scheme
}
/*****************
 * Style methods *
 *****************/

/*
 * TEST METHOD
 * function to get an example leaflet style
 */
function get_simple_style_test() {
    var myStyle = {
        "color": "#ff7800"
        ,"weight": 5
        ,"opacity": 0.65
    };

    return myStyle;
}

/*
 * Return the interpolated color in rgb format for a leaflet style
 */
function get_interpolated_color(value, max, min) {
    var c = color_interp(value, max, min);
    return "rgb(" + c.r + ", " + c.g + ", " + c.b + ")";

}

/*
 * Function that uses linear interpolation between a max and min color/value pair to get the color associated with an input value.
 * Colors are interpolated in the RGB space, each channel separately
 * Opacity (A) can be toggled
 * if no colors are supplied, the default is from blue to red
 */
function color_interp(value, max, min, color_max, color_min, incl_a) {
    incl_a = typeof incl_a !== 'undefined' ? incl_a : false;
    color_max = typeof color_max !== 'undefined' ? color_max : { a: 255, r: 255, g: 0, b: 0 };
    color_min = typeof color_min !== 'undefined' ? color_min : { a: 255, r: 0, g: 0, b: 255 };
    max = typeof max !== 'undefined' ? max : 100;
    min = typeof min !== 'undefined' ? min : 0;

    if (value >= max) {
        return color_max;
    }

    if (value <= min) {
        return color_min;
    }

    a_new = incl_a ? Math.round(linear_interpolate(value, min, max, Math.min(color_min.a, color_max.a), Math.max(color_min.a, color_max.a))) : 255;
    r_new = Math.round(linear_interpolate(value, min, max, Math.min(color_min.r, color_max.r), Math.max(color_min.r, color_max.r)));
    g_new = Math.round(linear_interpolate(value, min, max, Math.min(color_min.g, color_max.g), Math.max(color_min.g, color_max.g)));
    b_new = Math.round(linear_interpolate(value, min, max, Math.min(color_min.b, color_max.b), Math.max(color_min.b, color_max.b)));

    return { a: a_new, r: r_new, g: g_new, b: b_new };
}

/*
 * returns a color for a value given a list of colors and corrosponding list of value bins
 */
function color_pretty_breaks(value, colors, bins, opacity) {
    opacity = typeof opacity !== 'undefined' ? opacity : 1.0;

    var color = [0, 0, 0];
    for (var pos = 0; pos < bins.length; pos++) {
        if (value <= bins[pos]) {
            color = colors[pos];
            break;
        }
    }

    return "rgba(" + color[0].toString() + ", " + color[1].toString() + ", " + color[2].toString() + ',' + opacity.toString() + ")";
}

function color_pretty_breaks_upper(value, colors, bins, opacity) {
    opacity = typeof opacity !== 'undefined' ? opacity : 1.0;

    var color = [0, 0, 0];
    for (var pos = bins.length; pos >= 0; --pos) {
        if (value > bins[pos]) {
            color = colors[pos];
            break;
        }
    }

    return "rgba(" + color[0].toString() + ", " + color[1].toString() + ", " + color[2].toString() + ',' + opacity.toString() + ")";
}

/******************
 * Helper methods *
 ******************/

/*
 * Perform a linear interpolation between two "Points"
 */
function linear_interpolate(x, x0, x1, y0, y1){
    return  y0 + ((y1 - y0)*((x-x0) / (x1-x0)));
}

function even_value_breaks(values, num_breaks){
    break_num = Math.trunc(values.length / num_breaks);
    ret_breaks = [];
    pos = break_num;
    for (pos=break_num; pos < values.length; ++pos){
        if (pos % break_num == 0){
            var val = values[pos]
            ret_breaks.push(val);
        }
    }

    return ret_breaks;
}

//Adapted from https://stackoverflow.com/questions/5623838/rgb-to-hex-and-hex-to-rgb
function hexToRgb(hex) {
    hex = hex.replace('#', '');
    var bigint = parseInt(hex, 16);
    var r = (bigint >> 16) & 255;
    var g = (bigint >> 8) & 255;
    var b = bigint & 255;

    return [r, g, b]
}

/**************************
 * Leaflet Draw Functions *
 **************************/

function draw_event_add_point_row(lat, lng){
    var event_name = document.getElementById('event_name');
    var event_gwaf = document.getElementById('event_gwaf');
    var event_rmax = document.getElementById('event_rmax');
    var event_fspeed = document.getElementById('event_fspeed');

    var table = document.getElementById('draw_event_table');

    var row = table.insertRow(-1);

    // Catalog Number 0

    var cell_name = row.insertCell(1);
    cell_name.innerHTML = event_name;

    // Basin 2

    var cell_time = row.insertCell(4);
    cell_time.innerHTML = row.rowIndex;

    var cell_lat = row.insertCell(5);
    cell_lat.innerHTML = lat;

    var cell_lon = row.insertCell(6);
    cell_lon.innerHTML = lon;

    var cell_max_wind = row.insertCell(6);
    cell_lon.innerHTML = lon;

    var cell_gwaf = row.insertCell(12);
    cell_gwaf.innerHTML = event_gwaf;
}
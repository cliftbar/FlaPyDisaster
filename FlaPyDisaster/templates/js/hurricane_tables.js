
function change_table(source_id, target_class, docalc) {
    calc_type = 0
    if(docalc){
        document.getElementById('event_has_calc_indicator').className = "indicator status_warn"
        if (document.getElementById('python_calc').checked) {
            calc_type = 1
        }
        else {
            calc_type = 2
        }
            
    }
    var me = $("#" + source_id);
    var storm_name = me.val()

    if (me.val() == "<select a table>"){
        storm_name = "null"
    }

    $.getJSON(
        $SCRIPT_ROOT + "{{ url_for('change_table') }}"
        ,{
            name: storm_name,
            do_calc: calc_type
         }
        , function (data) {
            var target = $("." + target_class);
            target.empty().append(data.table)
            var title = document.getElementById('event_settings_title')
            if (storm_name == 'null'){
                storm_name = "Event"
            }
            //title.innerText = storm_name + " Footprint Settings"

            if(data.has_calc){
                document.getElementById('event_has_calc_indicator').className = "indicator status_good"
            }
            else{
                document.getElementById('event_has_calc_indicator').className = "indicator status_bad"
            }
        });
}

function save_event_to_raster(name) {
    var event_suffix_input = document.getElementById("event_save_suffix")
    if ($("#" + name).val() != '<select a table>') {
        $.getJSON("{{ url_for('hurricane_save_event_to_raster') }}"
        , {event_save_suffix: event_suffix_input.value}
        , function (data) {
                console.log('Got raster response');
                if(data.file_uri != ''){
                    document.getElementById("event_pic").src = data.file_uri + "?ver=" + data.ver
                }
            }
        );
    }
}

function set_event_settings(){
    $.ajax({
           type: "POST",
           url: "{{ url_for('hurricane_set_event_settings') }}",
           data: $("#event_settings").serialize(),
           success: function(data) {
               console.log(data)
           }
         }
     );
     change_table('table_selector', 'track_table')
}

function set_calculation_settings(){
    $.ajax({
           type: "POST",
           url: "{{ url_for('hurricane_set_calculation_settings') }}",
           data: $("#calculation_settings").serialize(),
           success: function(data) {
               console.log(data)
           }
         }
     );
}

function set_scala_settings() {
    $.ajax({
        type: "POST",
        url: "{{ url_for('hurricane_set_scala_settings') }}",
        data: $("#scala_settings").serialize(),
        success: function (data) {
            console.log(data)
        }
    }
    );
}


$('#automatic_fspeed').change(function() {
    $('#fspeed_kts_input').attr('disabled',this.checked)
});

$('#guess_parallel').change(function() {
    $('#level_of_parallel').attr('disabled',this.checked)
});
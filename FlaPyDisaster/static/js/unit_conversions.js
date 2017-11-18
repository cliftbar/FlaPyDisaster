
/*
Function to convert one unit to another.
Uses JQuery to send an asychronous(?) AJAX request to the server.
Updates the page value without requiring a full page re-render.
*/
function unit_conversion(source_id, target_id) {
    var me = $("#" + source_id);

    if (me.val() != me.data("prev")) {
        $.getJSON($SCRIPT_ROOT + '/distance_unit_conversion', {
            number: 10,
            unit_in: me.data("prev"),
            unit_out: me.val()

        }, function (data) {
            //window.alert(data.result)
            var target = $("#" + target_id);
            target.text(data.result + " " + me.val())
        });
        me.data("prev", me.val())
    }
}


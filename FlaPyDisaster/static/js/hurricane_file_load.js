$('#saved_event_selector').change(function () {
    if ($('#saved_event_selector').val() != "null") {
        $('#submit_load_saved_event').attr('disabled', false)
    } else {
        $('#submit_load_saved_event').attr('disabled', true)
    }
});
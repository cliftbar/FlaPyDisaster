function scala_hello() { 
    $.ajax({
        type: "get",
        url: "http://localhost:9000/helloApp",
        data: $("#event_settings").serialize(),
        success: function(data) {
            console.log(data)
        }
        , error: function () { console.log("Scala Unavailable") }
    }
    );
}
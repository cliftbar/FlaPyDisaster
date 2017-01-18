var toggle_visibility = (function(){
    //http://stackoverflow.com/questions/17642422/javascript-toggle-visibility-multiple-divs
    function toggle(cl){
        var els = document.getElementsByClassName(cl);
        for(var i=0; i < els.length; ++i){
            var s = els[i].style;
            s.display = s.display==='none' ? 'block' : 'none';
        };
    }
    return function(cl){
        if(cl instanceof Array){
            for(var i=0; i<cl.length; ++i){
                toggle(cl[i]);
            };
        }else{
            toggle(cl);
        }
    };
})();
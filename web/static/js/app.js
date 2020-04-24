let app = new Vue({
    el: '#app',
    data: {
        message: 'Hello Vue!',
        flapy_api: "http://127.0.0.1:5000/v1/"
    },
    methods: {
        getCatalogs: function () {
            axios.get(
                this.flapy_api + "hurricane/catalogs"
            ).then(
                response => (this.message = response.data)
            )
        }
    }
});


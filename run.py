from flapy_disaster.app import flapy_app

# Label server as development
flapy_app.env = 'development'

# Enable better exceptions and hot reloading
DEBUG = True
USE_RELOADER = False

# Sever ip:port.  Use HOST = 0.0.0.0 for access outside dev machine
HOST = "127.0.0.1"
PORT = 5000

# Enable server to use multiple threads
THREADED = True

if __name__ == '__main__':
    flapy_app.run(host=HOST, port=PORT, debug=DEBUG, threaded=THREADED, use_reloader=USE_RELOADER)


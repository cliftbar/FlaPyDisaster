# FlaPyDisaster
Hurricane modelling and mapping program written in Python and Flask, with Leaflet for mapping, implementing the NOAA NWS 23 parametric hurricane model.

## Installation

### Installation with Anaconda
Anaconda is recommended for setting up the python environment for this program.  Everything targets Python 3.5, and everything should work on Windows, OSS, and Linux.  Add the condaforge channel install from the appropriate requirements file using `conda install --file conda_requirements_XXXX.txt` to set up package dependancies.  Explicit files have web URLs for the conda packages, and `top_level_requirements.txt` has each of the required top level packages if requirements files aren't working.

### OSX Alternative
For OSX users, the dependancy install method below has also been tested:
```
pip install geojson
conda install -c anaconda joblib
brew install gdal
import sys
conda install gdal
pip install markdown2
```

## Starting
To run application on `localhost:5555` run
```
python app.py
```

## Configuration
Application configuration is available in `users/user_config/user_config.ini`

## Documentation
Check the Wiki for documentation [here](https://github.com/cliftbar/FlaPyDisaster/wiki/)

## Scala Calculation Server
There is a companion program for running calculations in Scala, [AkkaDisaster](https://github.com/cliftbar/AkkaDisaster/).  This is a Akka Http progam to run hurricane calculations separate from the application server.

## License
MIT

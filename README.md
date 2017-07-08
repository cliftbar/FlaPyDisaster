# FlaPyDisaster
Hurricane modelling and mapping program written in Python and Flask, with Leaflet for mapping, implementing the NOAA NWS 23 parametric hurricane model. 
[FlaPyScala](https://github.com/cliftbar/FlaPyScala) is a companion app for FlaPyDisaster.  FlaPyScala will implement the model calculations in Scala, using the play framework for an api, for better performance thaN the Python implementation.
## Installation

### Environments
Anaconda is recommended for setting up the python environment for this program.  Install from the appropriate requirements file using `conda install --file conda_requirements_XXXX.txt`.  Explicit has web URLs for the conda packages.

Pip can also work, but its not supported right now because GDAL has dependency probelms with pip.  GDAL has to be installed on the host machine, and the gdal package will have to be replaced with pygdal.  Note that these restrictions on GDAL might not matter when not using a virtual evnironment.

## Development
#### IMPORTANT: To run in IDEs in windows with anaconda, the anaconda environment directory (ex: ...\Anaconda2\envs\EnvironmentName) must be included in PATH
It can either be included globably or as a setting in the IDE, doesn't really matter.  Alternatively, activate the anaconda environment and run app.py, that will handle setting the PATH.

I've used both IntellJ Idea and Visual Studio 2015 Community Edition with success, there shouldn't be anything preventing other IDEs though.

## Documentation
Documentation is [here](https://github.com/cliftbar/FlaPyDisaster/wiki/Hurricane-Documentation)

## Scala Calculation Server
There is a companion program for running calculations in Scala, [FlaPyScala](https://github.com/cliftbar/FlaPyScala/).  This is a Scala/Play server to run hurricane calculations.  Early prototyping on my Intel i5 is showing that in Python:
* Hurricane Matthew 2016 10px per degree, 3 processes, 9 minutes
* Hurricane Matthew 2016  100px per dgree, 3 processes, 2 hours

And for Scala:
* For Matthew 2016 10px per degree, Scala at 2 cores runs in approximately 10 seconds
* For Matthew 2016 100px per degree, Scala at 2 cores runs in approximately 6 min

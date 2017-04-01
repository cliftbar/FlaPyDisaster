# FlaPyDisaster

## Installation
Anaconda is recommended for setting up the python environment for this program.  It will work for everything but configparser.  Install that with pip.  Install from the appropriate requirements file using `conda install --file conda_requirements_XXXX.txt`.

Pip can also work, using `pip install -r requirements.txt`, though GDAL will have to be installed specially.  GDAL has to be installed on the host machine, and the gdal package will be replaced with pygdal.  GDAL should be commented out in the pip requirements file.  I followed the instructions [here](http://stackoverflow.com/questions/32066828/install-gdal-in-virtualenvwrapper-environment) to get everything working with pip on linux.  Note that these restrictions on GDAL might not matter when not using a virtual evnironment.

## Development
I've used both IntellJ Idea and Visual Studio 2015 Community Edition with success, there shouldn't be anything preventing other IDEs though.

## Documentation
Documentation is linked here because its included in the program
[Documentation link](https://github.com/cliftbar/FlaPyDisaster/blob/master/FlaPyDisaster/static/markdown/documentation_main.md)

# FlaPyDisaster

## Installation
Anaconda is recommended for setting up the python environment for this program.  Install from the appropriate requirements file using `conda install --file conda_requirements_XXXX.txt`.  There should not be a difference between conda_requirements win32 and linux, but that could change in the future.  conda_requirements_explicit has different paths for each OS.

Pip can also work, but its not supported right now because GDAL has dependency probelms with pip.  GDAL has to be installed on the host machine, and the gdal package will have to be replaced with pygdal.  Note that these restrictions on GDAL might not matter when not using a virtual evnironment.

## Development
I've used both IntellJ Idea and Visual Studio 2015 Community Edition with success, there shouldn't be anything preventing other IDEs though.

## Documentation
Documentation is linked here because its included in the program
[Documentation link](https://github.com/cliftbar/FlaPyDisaster/blob/master/FlaPyDisaster/static/markdown/documentation_main.md)

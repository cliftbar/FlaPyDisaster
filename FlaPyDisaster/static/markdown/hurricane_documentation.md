# Hurricane Documentation

## Overview
This program takes a hurricane track, calculates a wind footprint for the track, and maps the results.  Note: If viewing this through GitHub, images are located at [FlaPyDisaster/Documentation/Hurricane/NWS23](/FlaPyDisaster/Documentation/Hurricane/NWS23), but links in the document will not work.

## Hurricane Track
A hurricane track is the measurements taken of a hurricane over time.  These are generally provided in flat text files.
Currently, this program supports HURDAT as a track file source, as well as its own internal format.  Track files provide
the base inputs for model calculations.

### Hurdat Specification
The NOAA HURDAT2 Catalog is available from the NOAA Hurricane Research Division, along with alot of other data,
[here](http://www.aoml.noaa.gov/hrd/hurdat/Data_Storm.html).
The [raw data](http://www.aoml.noaa.gov/hrd/hurdat/hurdat2-1851-2015-070616.txt)
and [format specification](http://www.aoml.noaa.gov/hrd/hurdat/newhurdat-format.pdf) are available as well.  The catalog is
updated about once a year, and contains data back to 1891 (though realistically, only the last 20-30 years or so have any real
accuracy).  The model uses the following fields from the track files:

* The cyclone number, name, and year of the storm to generate a unique name
* Latitude as the Eye Latitude (converted from an EW/NS format to a +/- format)
* Maximum Sustained Windspeed in Knots (1-min 10m average) (-99 indicates no data).  Note that currently the model does not
correct the wind speed from 1-min 10m to 10-min 10m
* Central pressure in millibars (-999 indicates no data)

Other fields are saved when parsing the data file, but are currently unused.

The model expects headers when loading the file, for a bad reason.  The program includes a hurdat file current to 2015
[here](/get_file/Documentation/Hurricane/HURDAT/hurdat2-1851-2015-070616_with_header.txt).  Use this as a reference
to format any new files.
 
### Other sources
For real time tracks, [Unysis](http://weather.unisys.com/hurricane/) maintains tracks from
current TPC advisories.  Eventually, this program will support imports of Unysis tracks.

The program also saves hurricane events in an internal format.  This will be detailed more in later sections, but includes an
.ini file with event info and parameters, a tab delimited text file with track information, and optionally a raster file (.png)
containing the wind footprint.
 
## NOAA NWS 23
This program uses the NOAA NWS 23 as the model for footprint calculation.  The model technical paper is linked
[here](/get_file/Documentation/Hurricane/NWS23/NOAA_NWS23.pdf) as a pdf.

### Model implementation
The model is contained in the hurricane module, in [hurricane_nws23.py](/get_file/hurricane/hurricane_nws23.py).  This file
is restricted the scope of the mode: calculating the windspeed at a given point from a set of inputs.  The logic and organization
for creating a footprint is in [hurricane_utils.py](/get_file/hurricane/hurricane_utils.py).

### Model Inputs
The model inputs are detailed below:
cp_mb, r_nmi, lat_deg, fspeed_kts, rmax_nmi, angle_to_center, track_heading, pw_kpa=Pw_PMH_kPa, vmax_kts=None, gwaf=0.9

* cp_mb
  * Unit: Millibars
  * Required: Ignored if vmax_kts is not provided
  * Minimum central pressure at the current point.  Used to calculate the maximum gradient wind speed (10-min 10m) at the point
   if it is not provided.
* r_nmi
  * Unit: Nautical Miles
  * Required
  * Distance of the point from the center (eye) of the storm.
* lat_deg
  * Unit: Degrees
  * Required
  * Latitude of the current point. (-180 to 180).
* fspeed_kts
  * Unit: Knots
  * Required
  * Forward speed of the storm.
* rmax_nmi
  * Unit: Nautical Miles
  * Required
  * Radius of maximum winds of the storm (approximately the eye wall).
* angle_to_center
  * Unit: Degrees
  * Required
  * Simple bearing angle from the center of the storm to the current point.  (Not the great circle distance)
* track_heading
  * Unit: Degrees
  * Required
  * Current bearing of the storm
* pw_kpa
  * Unit: kilopascals
  * Optional: Default of 102.0 is provided
  * Peripheral pressure of the storm (pressure at the edge of the storm).  Should be near Mean Sea Level Pressure
* vmax_kts
  * Unit: Knots
  * Optional: Overrides the cp_mb max windspeed calculation
  * Maximum Gradient Wind Speed of the storm.  10-min 10m observation
* gwaf
  * Unit: Unitless
  * Optional: Default of 0.9 is provided
  * Gradient Wind Reduction Factor.  Basically a fudge factor for model uncertainty.  Ranges from 0.75 to 1.05

### Model Steps
####Calculate maximum gradient wind
This step calculates the maximum windspeed in the storm (knots, 10-min 10m) from the Central Pressure, Peripheral Pressure
Distance from the center of the storm, and the latitude.  If the maximum windspeed is provided, as it is in hurdat, this step
is skipped.  It makes use of an emperical relationship derived from the following graph for an air density factor
(see page 50 of the pdf) and defined in the excel file [here](/get_file/Documentation/Hurricane/NWS23/NWS_23_RadialDecay.xlsx):

**K Density Factor, PMH**

![K Density Factor, PMH](/get_file/Documentation/Hurricane/NWS23/K_Factor.PNG)

The line for Knots and Inches Mercury is used, and the relationship is for the PMH.

The maximum wind also takes into account the coriolis factor (1/hr), using the code below:

```
w = 2.0 * math.pi / 24
coriolis_factor = 2.0 * w * math.sin(math.radians(lat_deg))
```

The final equation for the maximum gradient wind is:

```
max_wind = k_density_factor * ((peripheral_pressure - central_pressure) ^ 0.5) - (distance_to_eye * coriolis_factor) / 2
```

#### Calculate the radial decay factor
The radial decay factor scales the maximum wind to the windspeed at the point.  Wind speed drops in a predictable way as
distance from the center increases.  Radial decay is calculated using a set of emperical equation derived from the graphs
on page 53 and 54 of the pdf. The relationships are based off of the graphs below and created in the Excel file
[here](/get_file/Documentation/Hurricane/NWS23/NWS_23_RadialDecay.xlsx):

**Radial Decay: r < rmax**

![Radial Decay: r < rmax](/get_file/Documentation/Hurricane/NWS23/RadialDecay_Rmax_Inward.PNG)

**Radial Decay: r >= rmax**

![Radial Decay: r < rmax](/get_file/Documentation/Hurricane/NWS23/RadialDecay_Rmax_Outward.PNG)

####Calculate the asymmetry factor
The asymmetry factor accounts for the forward movement of the storm.  The major components of this factor the heading of the
storm, the angle from the center of the storm to the current point, the distance from center, and the radius of maximum winds.
The pdf pages are 51, 55, 263, and 281

The inflow angle (phi) is the radial angle that the wind takes compared to the concentric circle intersecting the current point
, and is dependant on the radius of max winds and the current radius.
The inflow angle is calculated using an emperical equation from the graph below and created in the excel
file [here](/get_file/Documentation/Hurricane/NWS23/NOAA_NWS23_Inflow_Calcs.xlsx):

**Inflow Angle, PMH**

![Inflow Angle, PMH](/get_file/Documentation/Hurricane/NWS23/InflowAngle_PMH.PNG)

Using the inflow angle of the current point, inflow angle of the maximum winds, the angle from the center of the storm,
and the track heading, a beta angle is calculated as below (Note the 90 shift to convert from a bearing to a cartesian
notation, and modulus operators to keep the angle confined to 360 degrees):

```
phi_beta = (phi_radius - phi_max_radius) % 360
bearing_shift = (90 - angle_from_center + track_bearing) % 360
beta = (phi_beta + bearing_shift) % 360
```

The beta angle is the major component of the final asymmetry calculation, as it accounts for the forward speed of the storm
and the radial position of the current point, as well as the rotational direction of the wind at the current point.  The final
asymmetry equation is below:

```
asymmetry_factor = 1.5 * (forward_speed ^ 0.63) * (to ^ 0.37) * math.cos(math.radians(beta))
```

The **to** factor is a unit conversion, and is 1 for the units used in the model.  The exponents are split to make the units
a separate term.

####Calculate the windspeed
The final windspeed equation is: 

```
windspeed = (maximum_gradient_wind * gwaf * radial_decay_factor) + asymmetry_factor
```

### Model Outputs
The model returns a wind speed for a given point in a storm, in knots (10-min 10m observation)

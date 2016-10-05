NetCDF Output format for the BADC Trajectory Model
==================================================

The trajectory output file is a netCDF file. Each file contains a description of the model and winds used 
as well as the parcel positions. The description of the model is given by a set of netCDF global attributes. 
The parcel positions, and any auxilliary variables (such as temperature or theta), are simply netCDF variables. 
The parcel positions and auxilliary variables are dimensioned by the number of parcels 
and the number of output times.

A simple ascii dump of the file can be obtianed using the ncdump utility provided with the netcdf distribution.

Attributes describing Trajectory run
------------------------------------

- `History`: General History of the file. Creation date, and modifications etc.
- `Version`: Version of the trajectory model used to create the parcel trajectories in this file.
- `3DTrajectory`: If vertical advection has been included in the model run this will be true ('T'). False ('F') otherwise.
- `VerticalVelocity`: Describes what vertical velocity was used in the model run. 
  Normally this will be omega, the vertical velocity in pressure coordinates.
- `TimeStep`: Time step used in the model run
- `WindSource`: Describes the source of the winds used to determine the parcel motion. For instance 
  ECMWF analyses, or forecasts.
- `TrajectoryBaseTime`: Gives the start time of the trajectory run. If forward trajectories were 
  run then the times in the file will be positive and denote times after the TrajectoryBaseTime. 
  If backward trajectories were run then the times in the file will be negative and denote times before 
  the TrajectoryBaseTime. The format for TrajectoryBaseTime is yyyymmddhh.
- `ForecastBaseTime`: If the trajectory run has used any forecast fields then ForecastBaseTime is the date from 
  which the forecast was run. At times after ForecastBaseTime the trajectories will have been run with 
  forecast fields. For times before ForecastBaseTime the trajectories will have been run with analysis fields.
- `SourceResolution`: The horizontal resolution, in degrees, of the analyses or forecast fields used 
  to determine the parcel trajectories.
- `SourceLevelType`: Type of level for the wind fields. This will usually be one of pressure or hybrid sigma-pressure
- `SourceLevelNumber`: Number of levels the source wind fields are on.
- `SourceTimeInterval`: The time between source files. Normally this is 6hours for ECMWF data.

Trajectory variables
--------------------

All trajectory variables, except time, are labeled with three attributes. These give the full name of the 
variable, the units it is in, and the flag for any undefined points. Trajectories can become undefined 
if they hit the ground or leave the top of the domain.

- `time`: Time along the parcel paths. This will be negative for back trajectories and positive for 
  forward trajectories. Dimensioned by the number of times of output in the trajectory file.
- `lon`: Longitudes of the parcels in degrees. Dimensioned by the number of parcels and the number of output times.
- `lat`: Latitudes of the parcels in degrees. Dimensioned by the number of parcels and the number of output times
- `lev`: The levels of the parcels. The level type will be given by the attribute 'name' to level. 
  Normally level type will be pressure, though potential temperature is also possible.


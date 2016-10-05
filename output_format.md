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

NASA Ames Output format for the BADC trajectory service
=======================================================

The BADC trajectory service produces NetCDF files by default which is a binary based output format. 
It is possible to produce readable output files using the 'View NASA Ames' links on the Output file page, 
and plot page. The NASA Ames files are a self describing ASCII based files that have seen extensive use 
in experimental campaigns. Each file begins with a header, which describes the contents of the file, and 
defines and scaling factors or missing value flags. The data follows the header. For more detailed 
information on the NASA Ames Data transfer file format refer to the document by Gaines and Hipskind.

The NASA Ames trajectory files are file format index 2110. This file format is appropriate for fields which have 
two independent variables (or dimesions). In the case of trajectories these dimensions are time and trajectory 
index which labels each trajectory within the file. Some campaigns use file format index 2160 for trajectory files. 
In this case the second dimension is a character label for the trajectory start position rather than a trajectory 
index. For the BADC trajectory service the file format index 2110 has been chosen since the trajectory start 
positions are arbitray and do not neccessarily coincide with any named station.

The current implementation of the 2110 file format index is minimalist. This will change with time: more 
primary variables, such as temperature, more auxillary variables, and more comments will be added. 
All changes will be consistent with the definition of the 2110 file format index.

An example of a typical file, along with explanations follows. The variable names, such as NLHEAD are 
taken from Gaines and Hipskind.

    22 2110                                         [NLHEAD FFI]
    BADC User Support (badc@rl.ac.uk)
    British Atmospheric Data Centre
    WWW Trajectory Service
    No Mission: Produced as part of a regular service
    1 1
    1999 01 01 1999 07 06                           [DATE RDATE]
    2400.0 1.0                                      [DX(1) DX(2)]
    Time (seconds) from 00 on start date            [XNAME(1)]
    Trajectory Index                                [XNAME(2)]
    3                                               [NV]
    1.0 1.0 1.0                                     [VSCAL(1) VSCAL(2) VSCAL(3)]
    999.99 999.99 9999.99                           [VMISS(1) VMISS(2) VMISS(3)]  
    Latitude (degrees North)                        [VNAME(1)]
    Longitude (degrees East)                        [VNAME(2)]
    Pressure (hPa)                                  [VNAME(3)]
    1                                               [NAUXV]
    1.0                                             [ASCAL(1)]
    9999.99                                         [AMISS(1)]
    Number of output times along trajectory         [ANAME(1)]
    0                                               [NSCOML]
    0                                               [NNCOML]
    1 5 
          0   50.00    0.00   50.000
       2400   50.60    0.78   49.325
       4800   51.18    1.58   48.738
       7200   51.74    2.41   48.262
       9600   52.31    3.31   47.885

`NLHEAD=22` - The number of lines in the header   
`FFI=2110` - The file format index    
`DATE=1999 01 01` - The start date of the trajectory run.   
`RDATE=1999 07 06` - The date on which the trajectory model was run.   
`DX(1)=2400.0` - The number of seconds between output times   
`DX(2)=1.0` - The trajectory index will always increment by one   
`XNAME(1)=Time (seconds) from 00 on start date` - The name of the most rapidly varying dimension. 
In this case time from midnight on the start date. This leads to non-zero initial times, 
for trajectory integrations that start at any other time, e.g. 12UT.   
`XNAME(2)=Trajectory Index` - The name of the second independant (dimension) variable. In this case it is 
simply the index of the trajectory in the file. Trajectories are labeled by indices 1, 2, 3, etc.     
`NV=3` - The number of primary dependant variables   
`VSCALE(1) VSCALE(2) VSCALE(3)=1.0 1.0 1.0` - Scale factors for each of the dependent variables.   
`VMISS(1) VMISS(2) VMISS(3)=999.99 999.99 9999.99` - Values for undefines/missing data points 
for each of the dependent variables.   
`VNAME(1)=Latitude (degrees North)` - Name of the first dependent variable.   
`VNAME(2)=Longitude (degrees East)` - Name of the first dependent variable.   
`VNAME(3)=Pressure (hPa)` - Name of the first dependent variable.   
`NAUXV=1` - Number of auxillary variables.   
`ASCALE(1)=1.0` - Scale factor for the auxillary variable   
`AMISS(1)=9999.99` - Values for undefines/missing data points for each of the auxillary variables.   
`ANAME(1)=Number of output times along trajectory` - For the 2110 format there is always one auxillary
variable which gives information on the length of the most rapidly varying dimension. In this case 
this is the number of output times along the trajectory.   
`NSCOML=0` - In the current, minimalist, implementation of the NASA Ames format for the trajectory 
files there are no special comments. This will change.   
`NNCOML=0` - In the current, minimalist, implementation of the NASA Ames format for the trajectory 
files there are no normal comments. This will change.   

Data Records:
Each data record begins with the trajectory index, followed by the number of output times for this trajectory. 
The columns that follow are the time, latitude, longitude, and pressure along that trajectory.


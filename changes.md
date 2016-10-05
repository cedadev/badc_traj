
BADC Trajectories - Changes
===========================

This page lists the debugs, improvements, and changes made to the trajectory model

March 04
--------

- The output of trajectory integrations now goes to your requests directory. You can get to this directory from My BADC in your browser. You can also ftp your trajectory files from this directory.

Version: 1.12 (September 24th 2003)
-----------------------------------

- Add ERA-40 2.5x2.5 gridded pressure level data. To use this data you must have access to the ERA-40 dataset

Version: 1.11 (August 11th 2003)
--------------------------------

- Add ECMWF 1.125 model level forecast data. To use this data you must contact the BADC.

Version: 1.10 (May 2nd 2003)
----------------------------

- Add option to use UM global model pressure level data. To use this data you must have access to the Met Office global model data.

Version:1.7 (March 6th 2000)
----------------------------

- Isentropic advection option.
- Allow access to forecast data, for those with appropriate permission.

Version:1.6 (Not released)
--------------------------

- Output Temperature and potential temperature on the parcel positions.

Version:1.5 (November 2nd 1999)
-------------------------------

- Fixed a bug in the routines reading wind files. This has no effect on trajectories already run, it just prevents the model from crashing with some wind files.

Version:1.4 (September 17th 1999)
---------------------------------

- Fixed an array indexing bug which affects integrations with undefined parcels. Parcels become undefined if they go off the top or the bottom of the model. The bug means that integrations with undefined parcels from earlier versions of the model will have incorrect parcel paths.
- Multiple release of trajectories added. This allows several initial times to be used for trajectories release from the same initial positions.
- The time step is changed from 40 minutes to 30 minutes. This makes it easier to deal with the multiple release of trajectories.
- The destination of the output files has changed. Each trajectory integration is given an identifier expnnn, when nnn is a three digit integer.
- Added access to the gridded ECMWF Reanalysis data. This means that integrations can be made back to 1979.
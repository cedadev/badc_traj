
Trajectory model numerics and tests
===================================

This page outlines some of the details of the trajectory model for reference. The performance and accuracy of the model is demonstrated by some results of numerical tests.

Model Code and Numerics
-----------------------

The trajectory model is based on a parcel advection code used in the contour dynamics/advection algorithm of D. Dritschel and W. A. Norton. 
The current version has been implimented in idl by M. Bithell. The horizontal advection on a sphere is performed in Cartesian coordinates, 
which avoids having to include any special treatment at the poles. Vertical advection can be included. Time stepping uses a fourth 
order Runge-Kutta scheme. The winds are interpolated onto parcel positions at the current time by linear interpolation. 
The order of the time and space interpolations depends on the number of parcels being advected by the model. 
For few parcels the interpolation in space is performed first. For many parcels the interpolation in time is performed first. 
The reason for this is one of code efficiency and trying to minimise the number of interpolations that have to be done. 
It may mean, however, that the same initial positions may give slightly different results depending on whether the parcel 
with those initial conditions is advected with few other parcels or advected with a large number of other parcels.

The default timestep for the model is 30 minutes. Output can be written at any multiple of timesteps. The default is at every timestep.

Model Tests
-----------

Several tests have been performed with the model. These have been used to eliminate bugs as far as possible and assess 
the performance and accuracy of the model.

All tests shown here have been performed with the default 2.5x2.5 degree grid and with a 40 minute timestep.

### Time constant solid body rotation

The figure opposite shows the error evolution of the trajectory model when the horizontal winds are those for solid body 
rotation with a constant rotation rate. In this case the rotation period is 5 days, the rotation axis is through 0 longitude, 
0 latitude, and the model has been run for 10 days. The model has been run with 4 parcels. The initial conditions for these 
parcels are 0E,0N (magenta), 30E, 30N (red), 60E, 60N (green), and 90E, 90N (blue). The upper panel shows the parcel paths. 
The lower panel shows the difference between the modeled parcel position and the analytical parcel position. The difference, 
or error, is show in degrees along the great circle joining the analytical and numerical solutions. After 10 days, or two 
rotation periods, the model has an error of less than 0.2 degrees for all of the parcels. The parcel initially at 0E, 0N, 
has an even smaller error. This is simply a consequnce of the fact that the parcel is on one of the cartesian coordinate 
planes and never has an E-W component of wind. This parcel goes over the pole, with no noticable extra error. 
This is simply a consequence of the Cartesian formulation of the model.

This solid body rotation test gives a base line for the errors of the scheme. The errors in this case are due to the space 
interpolation of winds and the time scheme. The velocities are constant and so there are no errors 
associated with the time interpolation.

### Time varying solid body rotation

To assess the errors introduced by interpolating linearly in time between 6 hourly winds the solid body rotation tests 
have been repeated, but with a time varying rotation rate. If the rotation rate increases or decreases linearly with time 
then the difference between the model solution and the analytical solution is of similar size to the case of a constant 
rotation rate. This is not suprising since a linear interpolation in time will capture a linear increase in the 
rotation rate well. The figure shows the differences between the model and analytical solution when the rotation 
rate varies sinusoidally with time. In the case shown the rotation rate is given by

    omega(t)=a+bsin(ct)

where `omega` is the rotation rate, `t` is time, and `a=b=2pi/2.5` per day, `c=2pi/5.` per day. The errors are larger 
than in the case with constant rotation rate. By 10 days the parcel initially at 30N,30E has an error of almost 0.9 degrees. 
This, however, is still quite small and acceptable over the 10 days that the model has been run for.

### Vertical advection

Tests have been performed using both constant vertical advection and vertical advection varying linearly in the vertical. 
With vertical velocities of the order of 5hPa/day the error in the vertical position of the parcels is less than 0.1Pa. 
This of course says nothing about the accuracy or performance of the model when analysed wind fields are used. In this 
case the errors in the diagnosed vertical winds are likely to dominate the numerical errors of the model.


;
;-----------------------------------------------------------------------
;
;                                                                             
;     Off-line parcel advection                                               
;     --------------------------                                              
;                                                                             
;     Runge-kutta routine nicked from contour dynamics code of D.G. Dritschel 
;     as modified June 1993 by W.A. Norton to use specified velocity field    
;                                                                             
;     modified June 1995 by M. Bithell for use as a parcel advector           
;     modified Jan  1996 by M. Bithell for use as a 3-D parcel advector       
;     modified Jan  1996 by M. Bithell converted from FORTRAN to IDL          
;     modified July 1996 by M. Bithell use an auxiliary array for heights     
;     modified Apr  1997 by S. Pepler  use of ecmwf grib files                
;     modified Jan  1999 by J. Kettleborough use of ecmwf files with hybrid vc
;                                                                             
;-----------------------------------------------------------------------
;
;     This code uses a fourth-order Runge-Kutta scheme to advect a set of
;     points in three dimensions. The velocity field is read in from separate
;     files and must be prescribed beforehand. The number 
;     of advection steps per day may be specified separately from the number
;     of velocity timesteps available - the velocities are linearly
;     interpolated to the parcel positions in both space and time. The routine
;     uses cartesian coordinates for the horizontal advection, to avoid
;     problems near the poles - the integration therefore carries four velocity
;     and position components - three cartesians for the horizontal advection
;     and a vertical component. Horizontal cartesian co-ordinates are 
;     adjusted to keep the
;     horizontal advection tied to the unit sphere after each step of the 
;     runge-kutta calculation- the co-ordinates are mapped back to 
;     longitude-latitude and are used with the separately calculated vertical
;     position when the interpolation routine is called.
;
;-------------------------------------------------------------------------
;
;     EXPECTED INPUTS
;
;     A REGULAR three dimensional grid of the three velocity components - 
;     Longitude,latitude and height, at a set of times.
;     Longitudes are assumed to lie in the range 0-360.
;     The velocities at each point are expected to be the usual local
;     meteorolgical set u,v,w.
;
;     A set of parcel positions -Longitudes latitudes and heights
;
;-----------------------------------------------------------------------
;
;     PARAMETERS
;  
;     nlon     : number of longitudes in the velocity field
;     nlat     : number of latitudes in the velocity field
;     nht      : number of height co-ordinates
;     tspd     : the number of runge-kutta advection steps per day
;     tspduv   : the number of times per day to read the velocity field
;     ktotal   : the total number of advection steps required
;     kout     : the step interval at which to output the parcel results
;             
;
;     CONSTANTS
;
;     unorm    : normalization factor to convert horizontal velocities from
;                metres per second to variables on the unit sphere
;     dt       : 1/tspd    advection timestep
;     dtuv     : 1/tspduv  time between reading velocity files
;     dt2      : dt/2      constants for Runge-Kutta step
;     dt3      : dt/3           ditto
;     dt6      : dt/6           ditto
;
;     VARIABLES
;     npt      : The number of parcels advected
;     a,b,c    : Parcel longitude,latitude and height
;     x,y,z    : Cartesians corresponding to a,b on the unit sphere
;     zhm       : vertical position (=c)
;     inunit_p : unit number for the initial parcel positions
;     outunit  : unit number for the resulting parcel positions
;     fname    : file names for the wind data
;     fnameindex : the index of fname from which the wind data is taken
;     longitude,latitude,height: The velocity grid co-ordinates
;
;
;-----------------------------------------------------------------------------
;
; Functions:
;    find_vert:  find the vertical position of each parcel using the vertical
;                coordinate of the gridded data
;    read_data_filenames: read the gridded data filenames (not used)
;    int3:       3d interpolation of a variable onto the parcel position
; Procedures:
;    adjust:     adjustment to keep parcels on a sphere
;    advect:     main routine - initialises variables, gets grid, reads initial
;                conditions passes control to advect_loop
;    advect_loop: time loop control
;    compute_coordinates: compute the grid of the gridded data
;    find_parcel_cartesians: convert lat lon of parcels to cartesian x y z
;    get_fields: interface to grib reading routines.  get the fields from the
;                gridded data sets
;    inc_vec:    increment the position vector using the velocity vector
;    output_label: label the output trajectory file
;    read_parcel_positions: read the parcel initial conditions
;    runge_kutta: runge kutta timestepping routine
;    set_up_constants: calculate some constants
;    solid_rotation: solid body rotation velocities used in tests
;    time_interp: interpolate fields in time
;    uvw:        interpolate the velocites onto the parcel positions
;    write_parcels: write output at a particular time
;    grib_chk:   check the model can cope with the gridded data
;    grib_grid:  get information on the grid from the grib header
;    grib_get_fields: get the gridded data arrays from the grib file
;
;    get_files:   get the file name for a variable and date
;
;    Calling Tree
;    ------- ----
;
;    advect -- setup_constants
;            | get_files
;            | grib_open
;            | grib_grid
;            | grib_close
;            | compute_coordinats
;            | read_parcel_positions
;            | find_parcel_cartesians
;            | output_label
;            | advect_loop            -- get_fields -- grib_get_fields
;                                      | (time_interp)
;                                      | runge_kutta -- uvw        -- find_vert
;                                      |              | time_interp
;                                      |              | incr_vec
;                                      |              | adjust
;                                      | write_parcels
;            
;------------------------------------------------------------------------

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;+
; NAME:
;       GETLONG
;
; PURPOSE:
;       Read the contents of a byte array into a long integer bigendian. 
;
; CATEGORY:
;       type conversion.
;
; CALLING SEQUENCE:
;       GETLONG(bytearray)  
;
; INPUTS:
;       bytearray:   An array of bytes.
;
; OUTPUTS:
;       : A long integer 
;
; KEYWORD Parameters:
;       /sign : uses the first bit as a sign bit.
; SIDE EFFECTS:
;       None.
; RESTRICTIONS:
;       bytearray must be of length 1 to 4
;
; EXAMPLE:
;               print, getlong([129b,1b], /sign)
;
; MODIFICATION HISTORY:
;       Written dec, 1996, S.J.Pepler.
;---

function getlong, bytesin, sign=sign

	bytes=bytesin
	nbytes=(size(bytes))(1)			;NUMBER OF BYTES 
	s=1
	if (keyword_set(sign)) then begin	;FIND SIGN
		s=(-2)*(bytes(0) gt 127b) + 1
		bytes(0)=(bytes(0) and 127b)
	endif
		
	longarr=reverse(bytes)*[1L,256L,65536L,16777216L]	;FIND LONG
	longout=0L
	for i=0,nbytes-1 do longout=longout+longarr(i)
	longout= s * longout

	return, longout
end

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;+
; NAME:
;       GETREAL
;
; PURPOSE:
;       Read the contents of a byte array into a real (IBM format). 
;
; CATEGORY:
;       type conversion.
;
; CALLING SEQUENCE:
;       GETREAL(bytearray)  
;
; INPUTS:
;       bytearray:   An array of bytes.
;
; OUTPUTS:
;       : A float
;
; SIDE EFFECTS:
;       None.
; RESTRICTIONS:
;       bytearray must be of length 2 to 4
;
; EXAMPLE:
;               print, getreal([129b,1b])
;
; MODIFICATION HISTORY:
;       Written dec, 1996, S.J.Pepler.
;---

function getreal, bytesin

	bytes=bytesin
	nbytes=(size(bytes))(1)
	s=(-2)*(bytes(0) gt 128b) + 1.0		;FIND SIGN
	bytes(0)=(bytes(0) and 127b)
		
	longarr=reverse(bytes(1:*))*[1L,256L,65536L,16777216L]	;FIND MANTISSA
	longout=0L
	for i=0,nbytes-2 do longout=longout+longarr(i)
	real_exp= 4*(bytes(0)-64)-24				;FIND EXPONENT
	realout= s*longout*2.0^(real_exp)

	return, realout
end

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;+
; NAME:
;       GRIB_READ
;
; PURPOSE:
;       Read a grib file. 
;
; CATEGORY:
;       I/O.
;
; CALLING SEQUENCE:
;       GRIB_READ(unit)
;
; INPUTS:
;       unit:   Logical unit number.
;
; OUTPUTS:
;       :   an anonymous structure containing the pds (product definition
;		section), gds (grid definition section) and the data
;		(binary data section). 
;	    or the raw grib message if the no_decode keyword is used
; KEYWORD Parameters:
;       error= 	0 read was good.
;		1 read was bad
; 	no_decode : returns an array of bytes (raw grib message).
; SIDE EFFECTS:
;       None.
; RESTRICTIONS:
;       None.
;
; MODIFICATION HISTORY:
;       Written dec, 1996, S.J.Pepler.
;---

function grib_read, unit, error=error, no_decode=no_decode

;	!except=1
	b1=bytarr(4) & b2=bytarr(4) & b=0b
        if (EOF(unit)) then begin & error=-1 & return, -1 &endif 
	readu,unit,b1
	while (string(b1) ne 'GRIB') do begin 		;SEARCH FOR GRIB
	  if (EOF(unit)) then begin & error=-1 & return, -1 &endif
	  readu,unit,b
	  b1=[(shift(b1,-1))(0:2),b]
	endwhile
	readu,unit,b2
	grib_length=getlong(b2(0:2))	;FIND GRIB LENGTH IN BYTES
	grib=bytarr(grib_length-8)
	readu,unit,grib			;READ THE REST OF THE GRIB MESSAGE
	grib=[b1,b2,grib]
	error= ( string(grib(grib_length-4:*)) ne '7777') 	;CHECK

	if (not keyword_set(no_decode)) then begin
		grib_sec,grib,s0,s1,s2,s3,s4,s5
		grib= {pds:s1, gds:s2, data:s4}
	endif
;        !except=2
	return, grib
end
;
;-----------------------------------------------------------------------
;
function grib_block,para,fname,nlev
        print, 'INFO file name: ', fname
	grib_open,unit,fname
	
	x=0
	data=0.0
	for i=1,nlev*6 do begin
		x=grib_read(unit,/no,error=err)
		if (x(16) eq para) then begin
			grib_sec,x,s0,s1,s2,s3,s4,s5
			data=[data,s4]
		endif
	endfor	
	
	data=data(1:*)
	data=reform(data,s2.nlong,s2.nlat,nlev)
	print,'INFO parameter, file, lon, lat, lev: ', para,fname,s2.nlong,s2.nlat,nlev
	
	grib_close, unit
	return,data
end
;
;-----------------------------------------------------------------------
;
FUNCTION ec_vc2p, lnps, vct
;
;+
;   NAME:
;     ec_vc2p
;
;   PURPOSE:
;     this function converts ECMWF lnsp and vertical coordinate into pressure
;
;   MODIFICATION HISTORY:
;     Written by: J.A. Kettleborough 30/6/00
;-
;
   is=Size(lnps)
   ilon=is[1]
   ilat=is[2]
   ilev=(N_Elements(vct)/2)-1
   p=Fltarr(ilon,ilat,ilev)

   ps=Exp(lnps)
   pmid=Fltarr(ilon,ilat,ilev+1,/nozero)
   FOR jlev=0,ilev DO pmid[*,*,jlev]=vct[jlev]+vct[ilev+1+jlev]*ps
   p[*,*,0:ilev-1]=0.5*(pmid[*,*,0:ilev-1]+pmid[*,*,1:ilev])
   return,p
END
;
;-----------------------------------------------------------------------
;
FUNCTION ppcode, var
;
; Function ppcode(var)
;
; Purpose: returns the pp code of a variable
;
  CASE StrLowCase(var) OF
   'u': pp=56
   'v': pp=57
   't': pp=16
   'w': pp=40
  ELSE: message, 'pp variable code not known: '+var
  ENDCASE
  return, pp
END
;
;-----------------------------------------------------------------------
;
FUNCTION get_vars, source
   CASE StrLowCase(source) OF
     'ecmwf_2.5p': $
            vars=['u','v','w','t']
     'ecmwf_2.5mf': $
            vars=['u','v','w','t']
     'ecmwf_2.5p_e4t': $
            vars=['u','v','w','t']
     'ecmwf_1.125m': $
            vars=['u','v','w','t','lnsp']
     'ecmwf_1.125mf': $
            vars=['u','v','w','t','lnsp']
     'ukmo_gs': $
            vars=['u','v','w','t']
   ENDCASE
   return, vars
END
;
;-----------------------------------------------------------------------
;
FUNCTION get_file_dt, source
;
; Purpose: takes the time between files from the source 
;
   CASE StrLowCase(source) OF
     'ecmwf_2.5p': $
            tspduv=4
     'ecmwf_2.5mf': $
            tspduv=4
     'ecmwf_2.5p_e4t': $
            tspduv=4
     'ecmwf_1.125m': $
            tspduv=4
     'ecmwf_1.125mf': $
            tspduv=4
     'ukmo_gs': $
            tspduv=2 ; JAK UM check this
   ENDCASE
   return, tspduv
END
;
;----------------------------------------------------------------
;
FUNCTION sec_diff, date1, date2
;
; Purpose: take the difference in seconds between
;          two date strings
; Interface: secs=sec_diff(date1,date2)
;            date1= first idl date variable
;            date2= second idl date variable
;            sec_diff difference: date1-date2, in seconds
;
  days=date1.julian-date2.julian
  mins=date1.minute-date2.minute
  secs=date1.second-date2.second
  sdiff=days*86400.+mins*60.+secs

  return, sdiff
END
;
;-----------------------------------------------------------------------
;
FUNCTION forecast_base
;
; Purpose: return from which forecast fields will
;          be used instead of analysis
;
       tfore=Today()                  ; assume that the forecast
       tfore.hour=0                   ; for yesterday at 12 is present
       tfore.minute=0                 ; all files before 12 yesterday
       tfore.second=0                 ; are taken as analysis,  files after
       tfore.recalc=1B                ; are taken as forecast from this time
       tfore=Dt_Add(tfore, hour=-12)
       return, tfore
END
;
;-----------------------------------------------------------------------
;
FUNCTION source_label, source
;
; Purpose: return a string for labelling the output file
;          determined by the source
;
   CASE StrLowCase(source) OF
     'ecmwf_2.5p': $
            label='ECMWF Analyses'
     'ecmwf_2.5mf': $
            label='ECMWF Forecast/Analyses'
     'ecmwf_2.5p_e4t': $
            label='ECMWF ERA-40 Analyses'
     'ecmwf_1.125m': $
            label='ECMWF Analyses'
     'ecmwf_1.125mf': $
            label='ECMWF Forecast/Analyses'
     'ukmo_gs': $
            label='Met Office Global NWP'
   ENDCASE
   return, label
END
;
;-----------------------------------------------------------------------
;
FUNCTION get_format, source
;
; Purpose: returns the format of a source
;
   CASE StrLowCase(source) OF
     'ecmwf_2.5p': $
            format='grib'
     'ecmwf_2.5mf': $
            format='grib'
     'ecmwf_2.5p_e4t': $
            format='grib'
     'ecmwf_1.125m': $
            format='grib'
     'ecmwf_1.125mf': $
            format='grib'
     'ukmo_gs': $
            format='pp'
   ENDCASE
   return, format
END
;
;-----------------------------------------------------------------------
;
FUNCTION get_files, date, vars, source
;
; Purpose: creates a filename or set of filenames
;          for variables vars and date date from 
;          a source source
;
; Interface: file=get_files(date,vars,source)
;            date= and IDL date structure
;            vars= array of variables wanted
;            source= data source
;                    ecmwf_1.125m - gridded on model levels
;
;-----------------------------------------------------------------------
;
  ivar=N_Elements(vars)
  CASE StrLowCase(source) OF
    'ecmwf_2.5p': BEGIN
       Dt_To_Var, date, Year=yyyy, Month=mm, Day=dd, Hour=hh
       yy=yyyy mod 100
       IF (yyyy LT 1994 OR (yyyy EQ 1994 AND mm LT 03)) THEN $
           base='/badc/ecmwf-era/data/gridded_2.5/' $
       ELSE $
           base='/badc/ecmwf-op/data/gridded_2.5/'
       file=base+String(yyyy,yy,mm,yy,mm,dd,hh,$
                        format='(i4.4,"/lipr",i2.2,i2.2,"/lipr",'+$
                                 'i2.2,i2.2,i2.2,i2.2)')
    END
    'ecmwf_1.125m': BEGIN
      base='/badc/ecmwf-op/data/gridded_1.125/'
       Dt_To_Var, date, Year=yy, Month=mm, Day=dd, Hour=hh
       file=strarr(ivar)
       FOR jvar=0,ivar-1 DO BEGIN
         file[jvar]=base+String(yy,mm,dd,vars[jvar],yy,mm,dd,hh,$
  	     format='(i4.4,"/",i2.2,"/",i2.2,"/model/ML.",'+$
                      '(a),".",i4.4,i2.2,i2.2,i2.2,".grib")')
       ENDFOR
       IF (ivar EQ 1) THEN file=file[0]
    END
    'ecmwf_2.5mf': BEGIN
       base='/badc/ecmwf-op/data/forecast/gridded/'
       tfore=forecast_base()
       IF (date.julian LE tfore.julian) THEN BEGIN
         Dt_To_Var, date, Year=yy, Month=mm, Day=dd, Hour=hh
         yfile=String(yy,mm,dd,hh,format='("liua",i4.4,i2.2,i2.2,i2.2)')
       ENDIF ELSE BEGIN
         Dt_To_Var, tfore, Year=yy, Month=mm, Day=dd, Hour=hh
         ihour=sec_diff(date,tfore)/3600.
         yfile=String(yy,mm,dd,hh,ihour,$
                      format='("liua",i4.4,i2.2,i2.2,i2.2,i3.3)')
       ENDELSE
       file=base+yfile
    END
    'ecmwf_2.5p_e4t': BEGIN
       Dt_To_Var, date, Year=yyyy, Month=mm, Day=dd, Hour=hh
       base='/badc/ecmwf-e40/data/li/ap/'
       file=base+String(yyyy,mm,yyyy,mm,dd,hh,$
                        format='(i4.4,"/",i2.2,"/liap",i4.4,i2.2,'+$
                                 'i2.2,i2.2,".grb")')
    END
    'ecmwf_1.125mf': BEGIN
       base='/badc/ecmwf-for/data/'
       file=strarr(ivar)
       tfore=forecast_base()
       tfore=dt_subtract(tfore, second=43200)  ; temporary - remove later
       IF (date.julian - tfore.julian lt 1./3600.) THEN BEGIN
         Dt_To_Var, date, Year=yy, Month=mm, Day=dd, Hour=hh
         FOR jvar=0,ivar-1 DO BEGIN
           file[jvar]=base+String(yy,mm,dd,vars[jvar],yy,mm,dd,hh,$
             format='(i4.4,"/",i2.2,"/",i2.2,"/model/ML.",(a),'+$
                   '".",i4.4,i2.2,i2.2,i2.2,".grib")')
         ENDFOR
       ENDIF ELSE BEGIN
         Dt_To_Var, tfore, Year=yy, Month=mm, Day=dd, Hour=hh
         ihour=round(sec_diff(date,tfore)/3600.)
         FOR jvar=0,ivar-1 DO BEGIN
           file[jvar]=base+String(yy,mm,dd,vars[jvar],yy,mm,dd,hh,ihour,$
             format="(i4.4,'/',i2.2,'/',i2.2,'/model/FC.',(a),"+$
                    "'.',i4.4,i2.2,i2.2,i2.2,'.',i3.3,'.grib')")
         ENDFOR
       ENDELSE
       IF (ivar EQ 1) THEN file=file[0]
    END
    'ukmo_gs': BEGIN
       base='/badc/ukmo-um/data/global/sg/'
       Dt_To_Var, date, Year=yyyy, Month=mm, Day=dd, Hour=hh
       file=strarr(ivar)
       FOR jvar=0,ivar-1 DO BEGIN
         ppco=ppcode(vars[jvar])
         ppstring=strcompress(string(ppco),/remove_all)
         tmp=base+String(yyyy,mm,dd,hh,yyyy,mm,dd,hh,$
  	     format='(i4.4,"/",i2.2,"/",i2.2,"/",i2.2,"/sg",i4.4,i2.2,i2.2,i2.2,"p")')
         file[jvar]=tmp+ppstring+'s00.pp'
       ENDFOR
       IF (ivar EQ 1) THEN file=file[0]
    END
    ELSE: message, 'get_files: data source not recognised'
  ENDCASE
  return, file
END
;
;-----------------------------------------------------------------------
;
FUNCTION calc_theta, t, p
;
; Purpose: Calculate potential temperature from temperature and pressure
  zkapa=(287.04/1005)
  theta=t*(1.e5/p)^zkapa
  return, theta
END
;            
;------------------------------------------------------------------------
;
FUNCTION calc_p, t, theta
;
; Purpose: Calculate potential temperature from temperature and pressure
  zkapai=(1005/287.04)
  p=1.e5*(t/theta)^zkapai
  return, p
END
;
;-----------------------------------------------------------------------
;
FUNCTION find_parcel_cartesians, lon, lat, lev
;
; Purpose: Convert the parcel geographical coordinates to cartesian
;
  th=(90.0-lat)*!pi/180.0
  ph=lon*!pi/180.0
  x=sin(th)*cos(ph)
  y=sin(th)*sin(ph)
  z=cos(th)
  zhm=lev
  return, {x:x,y:y,z:z,zhm:zhm}
END
;            
;------------------------------------------------------------------------
;
FUNCTION find_vert, vert, xi, yi, z, in_range

; .1 interpolate the vertical coordinate onto parcel positions
sze=size(vert)
ilev=sze(3)
ipar=n_elements(z)
ztmp=fltarr(ilev,ipar,/nozero)  ; profile of vert coor at horiz parcel pos
z1=replicate(-1.,ipar)

for jlev=0,ilev-1 do begin
  ztmp(jlev,*)=interpolate(vert(*,*,jlev),xi,yi)
endfor

; .2 find the vertical interpolation indices
ii=intarr(ipar)
if (ztmp(0,0) gt ztmp(ilev-1,0)) then begin ; probably pressure
   ir=where(z(in_range) ge ztmp(ilev-1,in_range) and $
            z(in_range) le ztmp(0,in_range))
   IF (ir[0] gt -1) THEN BEGIN
     in_range=in_range(ir)
     for jpt=0,n_elements(in_range)-1 do begin
      ipt=in_range[jpt]
      iit=where(z[ipt] gt ztmp[*,ipt])
      ii[ipt]=(iit[0] gt -1 ? iit[0] : 1)
     endfor
   ENDIF ELSE BEGIN
     in_range=replicate(-1,1) 
   ENDELSE
endif else begin                            ; probably theta
   ir=where (z(in_range) le ztmp(ilev-1,in_range) and $
             z(in_range) ge ztmp(0,in_range))
   IF (ir[0] gt -1) THEN BEGIN
     in_range=in_range(ir)
     for jpt=0,n_elements(in_range)-1 do begin
      ipt=in_range(jpt)
      iit=where(z(ipt) le ztmp(*,ipt))
      ii(ipt)=(iit(0) gt -1 ? iit(0) : 1)
     endfor
   ENDIF ELSE BEGIN
     in_range=replicate(-1,1) 
   ENDELSE
endelse

IF (in_range[0] gt -1) THEN BEGIN
  ii=ii(in_range)
  iit=in_range*ilev+ii
  z1(in_range)=ii+(z(in_range)-ztmp(iit))/(ztmp(iit)-ztmp(iit-1)) ; divide by zero?
  return, z1
ENDIF ELSE BEGIN
  return, -1
ENDELSE

END
;
;----------------------------------------------------------------------------
;
FUNCTION geophys_param
;
; Purpose: Set the geophysical Parameters
;
  return, {rad_earth:6.371e6, omega:86400.e0}
END
;
;-----------------------------------------------------------------------
;
FUNCTION index_complement, ilen, index
;
; Purpose: returns the complement of the index array
; 
  ia=Intarr(ilen)
  IF (index[0] GT -1) THEN BEGIN
    ia(index)=-1
    icomp=where(ia eq 0)
  ENDIF ELSE BEGIN
    icomp=ia
  ENDELSE
  return, icomp
END
;
;-----------------------------------------------------------------------
;
FUNCTION ini_calc, kpar, kcvert
;
; Purpose: initialise new array for auxillary variables
;

  CASE kcvert OF
   0: rncalc={theta:Fltarr(kpar)}
   1: rncalc={press:Fltarr(kpar)}
  ENDCASE
  return, rncalc
END
;
;-------------------------------------------------------------------------
;
FUNCTION make_file_list, adate, no_days, tspduv, dtuv, source
;
; Purpose; make the list of files to be used in the integration
;
   date=adate
   iforc=1+abs(no_days)*tspduv
   fname=StrArr(iforc)
   FOR j=0,iforc-1 DO BEGIN
     fname(j)=get_files(date,[''],source)
     date=Dt_Add(date,second=dtuv)
   ENDFOR
   return, fname
END
;
;-------------------------------------------------------------------
;
FUNCTION next_winds, t, dtuv, ifile
  return, abs(t) GE (ifile-1)*abs(dtuv)
END
;
;-----------------------------------------------------------------------
;
FUNCTION next_winds2, date, wdate, dtuv
;
; Purpose: determine whether to read the next set of winds or not
; Interface: next_winds=next_winds(date,wdate)
;            date - current date
;            wdate - last set of winds to be read in
;            dtuv - time between wind files
;
  ztol=10./86400.   ; build in a tolerance of ten seconds
  IF (dtuv GT 0) THEN BEGIN
    next_winds=(wdate.julian-date.julian le ztol)
  ENDIF ELSE BEGIN
    next_winds=(wdate.julian-date.julian ge -ztol)
  ENDELSE
  IF (next_winds) THEN wdate=Dt_Add(wdate,second=dtuv)
  return, next_winds
END
;
;--------------------------------------------------------------------
;
FUNCTION copy_traj, yin, date
;
; Purpose: Copy an input file to a new file
;          The new file name is determined by the date
;
   Dt_To_Var, date, Year=yyyy, Month=mm, Day=dd, Hour=hh, Minute=mi, Second=ss
   hh=hh+Fix(mi/60.+(ss/6000.)+0.5)            ; round times to nearest hour
   date=Var_To_Dt(yyyy,mm,dd,hh,0,0)
   Dt_To_Var, date, Year=yyyy, Month=mm, Day=dd, Hour=hh, Minute=mi, Second=ss
   print, 'INFO copy_traj: ', yyyy, mm, dd, hh, mi, ss 
   ydate=String(yyyy,mm,dd,hh, Format='(i4.4,i2.2,i2.2,i2.2)')
   yout=StrMid(yin,0,StrPos(yin,'.nc'))+ydate+'.nc'
   ycommand=String(yin, yout, format='("cp ",(a),x,(a))')
   spawn, ycommand, ymess
   IF (ymess NE '') THEN message, 'cp failed'
   return, yout
END
;
;----------------------------------------------------------------------------
;
FUNCTION read_data_filenames, file_info
;
; Purpose: Read the data file names from the input file file_info
;

  stmp=''
  openr, unit, file_info, /get_lun
  while not eof(unit) do begin
    readf, unit, stmp
    if (n_elements(fname) eq 0) then fname=[stmp] else fname=[fname,stmp]
  endwhile
  free_lun, unit
  return, fname
END
;
;---------------------------------------------------------------------------
;
FUNCTION rcs_string, ystring
;
; Purpose: Extract the value from an RCS string
; Interface: string=rsc_string(ystring)
;            ystring is the rcs keyword
  is=StrPos(ystring,'$')
  it=RstrPos(ystring,'$')
  ibeg=StrPos(ystring,':')+1
  ilen=StrLen(ystring)-1
  IF (is EQ 0 AND it EQ ilen AND ibeg NE -1) THEN $
    yreturn=StrTrim(StrMid(ystring,ibeg,ilen-ibeg),2) $
  ELSE $
    yreturn=''
  return, yreturn
END

;
;---------------------------------------------------------------------------
;
FUNCTION time_step, omega, tspd
;
; Purpose: Calculate the timesteps
;
   dt=omega/tspd
   dt2=dt/2.e0
   dt3=dt/3.e0
   dt6=dt/6.e0
   return, {time_steps, dt:dt, dt2:dt2, dt3:dt3, dt6:dt6}
END
;
;----------------------------------------------------------------------------
;
FUNCTION INT3,DATA,VCA,X,Y,Z,LOG=LOG

;Interpolate linearly in the horizontal and the use splines in the 
;vertical. VCA is an auxiliary array which gives the value of the vertical
;co-ordinate at each point in the three-d data array. Both data and VCA are
;interpolated in the horizontal to each point (x,y,z) 

sz=size(data) & nn=n_elements(x)
temp=fltarr(nn,sz(3)) & nvc=temp
result=fltarr(nn)
for i=0,sz(3)-1 do begin
 temp(*,i)=interpolate(data(*,*,i),x,y)
 nvc(*,i)=interpolate(vca(*,*,i),x,y)
endfor
for i=0,nn-1 do begin
 t=temp(i,*) & nv=nvc(i,*)
 if keyword_set(log) then begin
  nv=alog(nv) & z(i)=alog(z(i))
 endif
 t=t(sort(nv)) & nv=nv(sort(nv))
 result(i)=spline(nv,t,z(i))
endfor
return,result
END
;
;---------------------------------------------------------------------------
;
FUNCTION release_new, ktime, krun, kend, krel_freq
;
; Purpose: determine whether to release a new set of parcels
; Interface: release=release_new(ktime,krun,kend,krelease)
;            ktime - current timestep number
;            krun - number of timesteps for each run
;            kend - total number of timesteps
;            krelease - frequency of release of trajectories
;
   IF (krel_freq EQ 0) THEN BEGIN
     release=ktime EQ 0
   ENDIF ELSE BEGIN
     release=((ktime+krun) LE kend) AND ((ktime mod krel_freq) EQ 0)
   ENDELSE
   return, release
END
;
;---------------------------------------------------------------------------
;
FUNCTION release_old, ktime, krun, ktotal, krel_freq
;
; Purpose: determine whether to end a set of released trajectories
; Interface: release=release_old(ktime,krun,kend,krelease)
;            ktime - current timestep number
;            krun - number of timesteps for each run
;            kend - total number of timesteps
;            krelease - frequency of release of trajectories
;
   IF (krel_freq EQ 0) THEN BEGIN
     release=ktime EQ ktotal
   ENDIF ELSE BEGIN
     release=(ktime GE krun) AND (((ktime-krun) mod krel_freq) EQ 0)
   ENDELSE
   return, release
END
;
;----------------------------------------------------------------------------
;
PRO ADJUST,r
;
; Purpose: adjusts nodes so that x**2+y**2+z**2=1.
;
  f=1./sqrt(r.x*r.x+r.y*r.y+r.z*r.z)
  r.x=f*r.x
  r.y=f*r.y
  r.z=f*r.z
  return
END
;
;----------------------------------------------------------------------------
;
PRO advect, year, month, day, hour, no_days, $
           tspd=tspd, $
           kout=kout, $
           fname=fname, $
           input_file=input_file, $
           source=source, $
           catch_error=catch_error, $
           release_freq=release_freq, $
           release_no=release_no, $
           kcvert=kcvert
;
; 1.0 check parameters to routine, set defaults
IF (NOT keyword_set(no_days)) THEN no_days=5 ; length integration (days)
IF (N_Elements(hour) lt 1) THEN hour=0       ; start hour
IF (N_Elements(tspd) lt 1) THEN tspd=36      ; number timesteps per day
IF (N_Elements(kout) lt 1) THEN kout=1       ; frequency of output (timesteps)
IF (N_Elements(input_file) lt 1) THEN input_file='part.nc'
IF (N_Elements(source) LT 1) THEN source='ecmwf_2.5p'
IF (N_Elements(release_no) LT 1)THEN release_no=1
IF (N_Elements(release_freq) LT 1) THEN BEGIN
   irel_freq=0
   irel_no=1
ENDIF ELSE BEGIN
   irel_freq=release_freq
   irel_no=release_no
ENDELSE

IF (N_Elements(kcvert) LT 1) THEN kcvert=0
IF (kcvert NE 0 AND kcvert NE 1) THEN message, 'Sorry must chose 3d (pressure) [0] or isentropic [1] trajectories'

IF (Keyword_Set(catch_error)) THEN BEGIN
  catch, ierr
  IF (ierr ne 0) THEN BEGIN
     print, 'ERROR ',!err_string
     exit, status=1
  ENDIF
ENDIF
;
; 1.2 Calculate constants
geo_param=geophys_param()               ; geophysical parameters
kstep=(no_days gt 0)? 1 : -1            ; forward or backward trajectories
dt=time_step(geo_param.omega,tspd*kstep); model time step
tspduv=get_file_dt(source)              ; number wind files per day
dtuv=geo_param.omega*kstep/tspduv       ; time between wind files
unorm=geo_param.rad_earth               ; normalisation for winds
irun=abs(no_days)*tspd                  ; number of time steps for each trajec
sdate=Var_To_Dt(year,month,day,hour)    ; start date
ltfirst=0                               ; time interpolation first
ldebug=0                                ; turn debugging on/off
IF (irel_freq GT 0) THEN $              ; total number of timesteps of run
    itotal=irun+(irel_no-1)*irel_freq $
ELSE $
    itotal=irun

;IF (N_Elements(fname) lt 1) THEN $
;             fname=make_file_list(sdate,no_days,tspduv,dtuv,source)

;
; 1.2.1 Print Information to stdout
print, 'INFO trajectory model run at ', systime()
print, 'INFO version ', rcs_string('$Revision: 888 $')
print, 'INFO Model time step: ', dt.dt
print, 'INFO Time steps per day: ', tspd
print, 'INFO Length of run: ', no_days

fname=get_files(sdate, ['u'], source) ; get example file name
;
; 1.3 Get information on the grid ; Move to a seperate routine 
format=get_format(source)
CASE format OF
  'grib': BEGIN
    grib_open, unit, fname
    grib_grid, unit, grid
    grib_close, unit
  END
  'pp': BEGIN
    get_grid_pp, fname, grid
  END
  ELSE: message, 'format not supported: '+format
ENDCASE

;
; 1.5 Initialise model grid
compute_coordinates,longitude,latitude,height,grid  ; height not needed?  should move to field/grid type struc

IF (ldebug) THEN BEGIN
  print, 'DEBUG longitudes of grid: ',longitude
  print, 'DEBUG latitutudes of grid: ', latitude
  print, 'DEBUG levels of grid: ', height
ENDIF
;
;------------------------------------------------------------
;
; 2. Main time loop
  nlon=n_elements(longitude)
  nlat=n_elements(latitude)
  nht= n_elements(height)
  vv=fltarr(nlon,nlat,nht)              ; set up variables
  fields1={x:vv,y:vv,z:vv,zhm:vv}
  fields0=fields1
  aux1={temp:vv} & aux0=aux1
  IF ltfirst THEN BEGIN
    velt=fields1
    veltdth=fields1
  ENDIF

  ifile=0
  wdate=sdate
  get_fields, wdate, source, fields1, aux1, kcvert

  IF (ltfirst) THEN veltdt=fields1
  t=0.d0
  tuv=0.d0
  FOR kount=0,itotal-1 DO BEGIN
     date=Dt_Add(sdate,second=t)           ; date at end of time step CHECK!!

     IF (ldebug) THEN BEGIN
       print, 'DEBUG ----START OF TIMESTEP---'
       dt_to_str, date, dddate, ddtime
       print, 'DEBUG timestep: ', kount, ' date: ', dddate[0], ddtime[0]
     ENDIF

     IF (next_winds2(date,wdate,dtuv)) THEN BEGIN  
       print, 'INFO reading next winds'
       tuv=tuv+dtuv
       fields0=fields1
       aux0=aux1
       get_fields, wdate, source, fields1, aux1, kcvert
     ENDIF

     IF (ltfirst) THEN BEGIN
       velt=veltdt
       time_interp, fields0, fields1, veltdth, t+dt.dt2, tuv, dtuv 
       time_interp, fields0, fields1, veltdt , t+dt.dt , tuv, dtuv
     ENDIF

     IF (release_new(kount,irun,itotal,irel_freq)) THEN BEGIN
       print, 'INFO New release ', kount
       new_file=copy_traj(input_file,date)
       nc_read_parcels, new_file, time0, lon, lat, lev, ncdf
       nc_output_label, ncdf, source, date, grid, dt, kcvert
       rnew=find_parcel_cartesians(lon,lat,lev)
       ipar=N_Elements(rnew.x)
       rnaux={temp:Fltarr(ipar)}
       rncalc=ini_calc(ipar,kcvert) 
       kin=indgen(ipar)
       IF (ltfirst) THEN BEGIN
         aux=aux1
         interp_time, aux0, aux1, aux, t, tuv, dtuv
         interp_space, aux, longitude, latitude, veltdt.zhm, rnew, rnaux, kin
       ENDIF ELSE BEGIN
         rnaux0=rnaux & rnaux1=rnaux
         interp_space, aux0, longitude, latitude, fields0.zhm, rnew, rnaux0, kin
         interp_space, aux1, longitude, latitude, fields1.zhm, rnew, rnaux1, kin
         interp_time, rnaux0, rnaux1, rnaux, t, tuv, dtuv ; in_range???
       ENDELSE
       calc_aux, rnew, rnaux, rncalc, kcvert
       nc_define_aux, ncdf, kcvert
       nc_write_parcels, t , rnew, rnaux, rncalc, ncdf, kin, kout*dt.dt, kcvert
       traj_file_add, nc_info, ncdf
       parcels_add, r, rnew, raux, rnaux, rcalc, rncalc, in_range, kcvert
     ENDIF

     runge_kutta, fields0, fields1, velt, veltdth, veltdt,$
		  unorm, dt, r, t, tuv, dtuv, $
		  longitude, latitude, in_range

     IF (ldebug) THEN BEGIN
       IF (in_range[0] eq -1) THEN ipir=0 ELSE ipir=n_elements(in_range) 
       print, 'DEBUG: number parcels in range: ', ipir
     ENDIF

     t=time0+(kount+1)*dt.dt    ; watch this!! time0=0 OK, not others
  
     IF (((kount+1) mod kout) eq 0) THEN BEGIN
       IF (ltfirst) THEN BEGIN
         interp_time, aux0, aux1, aux, t, tuv, dtuv
         interp_space, aux, longitude, latitude, veltdt.zhm, r, raux, in_range
       ENDIF ELSE BEGIN
         raux0=raux & raux1=raux
         interp_space, aux0, longitude, latitude, fields0.zhm, r, raux0, in_range
         interp_space, aux1, longitude, latitude, fields1.zhm, r, raux1, in_range
         interp_time, raux0, raux1, raux, t, tuv, dtuv ; in_range???
       ENDELSE
       calc_aux, r, raux, rcalc, kcvert
       nc_write_parcels, t , r, raux, rcalc, nc_info, in_range, kout*dt.dt, kcvert
     ENDIF

     IF (release_old(kount+1,irun,itotal,irel_freq)) THEN BEGIN
       print, 'INFO Deleting Release: ',kount
       parcels_delete, r, raux, rcalc, in_range, ipar, kcvert
       traj_file_delete, nc_info
     ENDIF  
     IF (ldebug) THEN BEGIN
       print, 'DEBUG ----END OF TIMESTEP---'
     ENDIF
  ENDFOR

print,'INFO Closing files'
close,/all
       
END
;
;-----------------------------------------------------------------------
;
PRO calc_aux, r, raux, rcalc, kcvert
;
; Purpose: calculate the auxillary variables depending on vertical coord
  CASE kcvert OF
    0: rcalc.theta=calc_theta(raux.temp,r.zhm)
    1: rcalc.press=calc_p(raux.temp,r.zhm)
  ENDCASE
  return
END
;
;-----------------------------------------------------------------------
;
PRO compute_coordinates, longitude, latitude, height, grid
;
;  Purpose: compute the coordinates from a grid structure
;
  longitude=grid.lon0+grid.dlon*findgen(grid.ilon)
  latitude=grid.lat0+grid.dlat*findgen(grid.ilat)
  longitude=longitude*!pi/180.
  latitude=latitude*!pi/180.
  if (tag_names(grid,/structure_name) eq 'GR_LL_HSP') then begin
    height=fltarr(n_elements(grid.a)-1)  ; Dummy for hybrid sigma-pressure coords
  endif else begin                       ; assume pressure levels
    height=grid.levels
  endelse
  return
END
;
;-----------------------------------------------------------------------
;
PRO get_fields_ec1125, winds, aux, yfiles
;+
;  NAME:
;      get_fields_ec1125
;
;  PURPOSE:
;      This procedure reads the winds from an ECMWF 1.125 gridded file
;
;  MODIFICATION HISTORY:
;   Written by: J. A. Kettleborough 26/6/00
;-
   print, 'INFO get_fields_ec1.125: reading winds'
   lini=(N_Elements(winds) LT 1)

   single_field_file, yfiles[0], var, lon, lat, vct ; read u
   IF (lini) THEN BEGIN
     winds=ini_winds((Size(var))[1:3])
   ENDIF ELSE BEGIN
     IF (WHERE(Size(var) NE Size(winds.x)) NE -1) THEN $
       message, 'input u fields different sizes'
   ENDELSE
   winds.x=var

   single_field_file, yfiles[1], var, lon, lat, vct ; read v
   IF (WHERE(Size(var) NE Size(winds.x)) NE -1) THEN $
       message, 'input v fields different sizes'
   winds.y=var

   single_field_file, yfiles[2], var, lon, lat, vct ; read w
   IF (WHERE(Size(var) NE Size(winds.x)) NE -1) THEN $
       message, 'input w fields different sizes'
   winds.z=var

   single_field_file, yfiles[3], var, lon, lat, vct ; read t
   IF (lini) THEN BEGIN
     aux=ini_aux((Size(var))[1:3])
   ENDIF ELSE BEGIN
     IF (WHERE(Size(var) NE Size(winds.x)) NE -1) THEN $
       message, 'input t fields different sizes'
   ENDELSE
   aux.temp=var

   single_field_file, yfiles[4], var, lon, lat, vct ; read lnsp
   p=ec_vc2p(var,vct)
   IF (WHERE(Size(var) NE Size(winds.x[*,*,0])) NE -1) THEN $
      message, 'input lnsp fields different sizes'
   winds.zhm=p
   return
END
;
;-----------------------------------------------------------------------
;
PRO single_field_file, file, f3d, lon, lat, vct, verbose=verbose
;
;+
;
; NAME: single_field_file
;
; PURPOSE:
;       Read a GRIB file which contains a single variable on model levels
;
; CATEGORY:
;       File i/o, Scientific data formats
;
; CALLING SEQUENCE:
;       SINGLE_FIELD_FILE, File, F3d, Lon, Lat, Vct
;
; MODIFICATION HISTORY:
;   Written by J. A. Kettleborough 23/06/00
;-
   lverb=Keyword_Set(verbose)
   jp_hybrid=109    ; hybrid coordinate code
   jp_regular=0     ; regular grid code
   jp_scan=0        ; scanflag code
   jp_lnsp=152      ; log surface pressure code
   IF (lverb) THEN print, 'INFO opening file: ', file
   openr, unit, file, /get_lun
   x=grib_read(unit)
   IF (x.pds.leveltype NE jp_hybrid) THEN $
                          message, 'expect model level fields'
   IF (x.gds.gridtype NE jp_regular) THEN $
                          message, 'expect regular lat-lon grid'
   IF (x.gds.scanflag NE jp_scan) THEN $
                          message, 'expect lon +ve, lat -ve scan'
   ipar=x.pds.para
   ilon=x.gds.nlong
   ilat=x.gds.nlat
   IF (ipar EQ jp_lnsp) THEN ilev=1 ELSE ilev=(x.gds.nv-1)/2
   IF (lverb) THEN print, 'DEBUG ilon ilat ilev:', ilon, ilat, ilev
   lon=(x.gds.long1+Findgen(ilon)*x.gds.dir_inc_long)/1000.
   lat=(x.gds.lat1-Findgen(ilat)*x.gds.dir_inc_lat)/1000.
;   vct=*(x.gds.vct)
   vct=(x.gds.vct)
   f3d=Fltarr(ilon,ilat,ilev)
   IF (lverb) THEN print, 'DEBUG Level: ', x.pds.level
   f3d[*,*,x.pds.level-1]=Reform(x.data,ilon,ilat)
   FOR jlev=1,ilev-1 DO BEGIN
     x=grib_read(unit)
     IF (lverb) THEN print, 'DEBUG Level: ', x.pds.level
     f3d[*,*,x.pds.level-1]=Reform(x.data,ilon,ilat)
   ENDFOR
   free_lun, unit

END
;
;-----------------------------------------------------------------------
;
PRO get_fields_ec25, winds, aux, fname
;
; Purpose: gets data from ecmwf 2.5 gridded files using routine grib_get_fields
;
  u1=winds.x
  v1=u1 & w1=u1 & vca=u1 & t=u1
  grib_open, unit, fname
  grib_get_fields, unit, u1, v1, w1, vca, temperature=t
  grib_close, unit
  winds.x=u1
  winds.y=v1
  winds.z=w1
  winds.zhm=vca
  aux.temp=t
  return
END
;
;-----------------------------------------------------------------------
;
PRO add_ghost, vari, loni, lati, varo, lono, lato,$
    reverse_polar=reverse_polar, degrees=degrees
;
; Purpose: add ghost cells to a field - for use in interpolation if 
;      longitudes don't include 0, 360 and lats don't include -90, 90
;
  IF (keyword_set(degrees)) THEN BEGIN
    znp=90
    zsp=-90
    zgr=360
  ENDIF ELSE BEGIN
    znp=!pi*0.5
    zsp=!pi*0.5
    zgr=!pi*2.
  ENDELSE

  IF (keyword_set(reverse_polar)) THEN irev=-1 ELSE irev=1

  ilon=N_Elements(loni)
  ilat=N_Elements(lati)
  isize=size(vari)
  IF (isize[1] ne ilon) THEN message, 'longitude number mismatch'
  IF (isize[2] ne ilat) THEN message, 'latitude number mismatch'

  ilono=ilon+2
  ilato=ilat+2

  IF (isize[0] eq 2) THEN BEGIN
    ilev=1
  ENDIF ELSE BEGIN
    IF (isize[0] eq 3) THEN BEGIN
      ilev=isize[3]
    ENDIF ELSE BEGIN
      message, 'input var must be 2 or 3 dimensional'
    ENDELSE
  ENDELSE

  dlon=loni[1]-loni[0]  ; longitude spacing
  IF (Abs(((loni[ilon-1]+dlon+zgr) mod zgr)-((loni[0] +zgr) mod zgr)) gt dlon*0.01) THEN $
        message, 'longitudes do not look periodic'

  dlat=lati[1]-lati[0]  ; latitude spacing
  IF (abs(lati[0]-dlat) lt znp) THEN message, 'latitudes do not reach poles'
  dlat=lati[ilat-1]-lati[ilat-2]
  IF (abs(lati[ilat-1]+dlat) lt znp) THEN message, 'latitudes do no reach poles'

  varo=Fltarr(ilono,ilato,ilev)
  lono=Fltarr(ilono)
  lato=Fltarr(ilato)

  lono[1:ilon]=loni
  lono[0]=2*loni[0]-loni[1]
  lono[ilon+1]=2*loni[ilon-1]-loni[ilon-2]

  lato[1:ilat]=lati
  lato[0]=2*lati[0]-lati[1]
  lato[ilat+1]=2*lati[ilat-1]-lati[ilat-2]

  varo[1:ilon,1:ilat,*]=vari
  varo[1:ilon,0,*]=shift(vari[*,0,*],ilon/2)*irev
  varo[1:ilon,ilat+1,*]=shift(vari[*,ilat-1,*],ilon/2)*irev
  varo[0,*,*]=varo[ilon,*,*]
  varo[ilon+1,*,*]=varo[1,*,*]
  
  varo=reform(varo)   ; removes level dim of 1
END
;
;-----------------------------------------------------------------------
;
PRO regrid3d, vari, varo, loni, lati, levi, lono, lato, levo
;
; Purpose: interpolate a 3d field onto a new grid
;
;          assume a regular grid in lon and lat
;
  x1=(lono-loni[0])/(loni[1]-loni[0])  ; may fail at end points
  y1=(lato-lati[0])/(lati[1]-lati[0])  ; may fail at end points
  lasc=(levi[1] gt levi[0])
  z1=Fltarr(N_Elements(levo))
  FOR jlev=0,N_Elements(levo)-1 DO BEGIN
    IF (lasc) THEN BEGIN
      iilev=(Where(levo[jlev] le levi))[0]-1
    ENDIF ELSE BEGIN
      iilev=(Where(levo[jlev] gt levi))[0]-1
    ENDELSE
    iilev=iilev > 0                    ; this will extrapolate?
    iilev=iilev < (N_Elements(levi)-1) ; ditto?
    z1[jlev]=iilev+(levo[jlev]-levi[iilev])/(levi[iilev+1]-levi[iilev])
  ENDFOR
  varo=interpolate(vari,x1, y1, z1, /grid)
END
;
;-----------------------------------------------------------------------
;
PRO nc_read_3d, icid, cvar, var, lon, lat, lev
;
; Purpose: read a single field from a netcdf file
;
  vinfo=ncdf_varinq(icid, cvar)
  ncdf_diminq, icid, vinfo.dim[0], ylon, ilon
  ncdf_varget, icid, ylon, lon
  ncdf_diminq, icid, vinfo.dim[1], ylat, ilat
  ncdf_varget, icid, ylat, lat
  ncdf_diminq, icid, vinfo.dim[2], ylev, ilev
  ncdf_varget, icid, ylev, lev
  ncdf_varget, icid, cvar, var
END
;
;-----------------------------------------------------------------------
;
PRO get_fields_pp, wnds, aux, fname
;
; Purpose: gets data from UKMO pp files
;
  um_makenetcdf, fname, nc_file

  icid=ncdf_open(nc_file, /nowrite)
  vnames={u:'u_1',v:'v_1',w:'omega',temp:'temp_1'}  ; structure containing names
  nc_read_3d, icid, vnames.u, var, lon_u, lat_u, lev_u
  iilev=sort(lev_u)
  lev_u=lev_u(iilev)
  add_ghost, var, lon_u, lat_u, varo, lono_u, lato_u, /degrees, /reverse_polar
  wnds.x=varo[*,*,iilev]

  nc_read_3d, icid, vnames.v, var, lon_v, lat_v, lev_v
  iilev=sort(lev_v)
  lev_v=lev_v[iilev]
  add_ghost, var, lon_v, lat_v, varo, lono, lato, /degrees, /reverse_polar
  wnds.y=varo[*,*,iilev]

  nc_read_3d, icid, vnames.w, var, lon_w, lat_w, lev_w
  iilev=sort(lev_w)
  lev_w=lev_w[iilev]
  add_ghost, var, lon_u, lat_u, varo, lono, lato, /degrees
  wnds.z=varo[*,*,iilev]

  nc_read_3d, icid, vnames.temp, var, lon_t, lat_t, lev_t
  iilev=sort(lev_t)
  lev_t=lev_t[iilev]
  temp=var[*,*,iilev]

  regrid3d, temp, tempo, lon_t, lat_t, lev_t, lon_u, lat_u, lev_u
  add_ghost, tempo, lon_u, lat_u, varo, lono, lato, /degrees
  aux.temp=varo

  ncdf_close, icid
  spawn, 'rm '+nc_file ; JAK

  ilon=N_Elements(lon_u)
  ilat=N_Elements(lat_u)
  ilev=N_Elements(lev_u)

  wnds.zhm=rebin(reform(lev_u,1,1,ilev),ilon,ilat,ilev)*100.  ; convert to Pa

END
;
;-----------------------------------------------------------------------
;
PRO get_grid_pp, file, grid
;
; Purpose: read the details of a grid from a pp file
;
  print, 'INFO reading file: ', file
  um_makenetcdf, file, nc_file
  icid=ncdf_open(nc_file, /nowrite)
  nc_read_3d, icid, 'u_1', var, lon, lat, lev
  ncdf_close, icid
  spawn, 'rm '+nc_file
  add_ghost, var, lon, lat, varo, lono, lato, /degrees
  dlat=lato[1]-lato[0]
  dlon=lono[1]-lono[0]
  ilat=N_Elements(lato)
  ilon=N_Elements(lono)
  iilev=sort(lev)
  lev=lev(iilev)*100  ; convert to Pa
  grid={gr_ll_p,lat0:lato[0], dlat:dlat, ilat:ilat, $
                     lon0:lono[0], dlon:dlon,$
                     ilon:ilon,levels:lev}
END
;
;-----------------------------------------------------------------------
;
PRO um_makenetcdf, fname, nc_file
;
; Purpose: make a netcdf file from a set of files
;
  convshpath='/usr/local/bin/convsh'
  spawn, 'echo $$', pid
  pid=pid[0]
  convshfile='/tmp/convsh'+pid
  nc_file='/tmp/umnc'+pid+'.nc'
  openw, unit, convshfile, /get_lun
  FOR j=0,N_Elements(fname)-1 DO printf, unit, 'readfile 0 '+fname[j]
  printf, unit, 'writefile netcdf '+nc_file+' -1'
  printf, unit, 'exit'
  free_lun, unit
  spawn, convshpath+' < '+convshfile
  spawn, 'rm '+convshfile
END
;
;-----------------------------------------------------------------------
;
PRO get_fields, date, source, fields, aux, kcvert
;
; Purpose: read the next set of fields from input
; Interface: get_fields2, date, source, fields
;            date - idl date structure for the date wanted
;            source - source type
;            fields - returned fields
;            aux - returned auxillary fields (temperature)
;

  print,'INFO get_fields: ',date.year,date.month,date.day, date.hour, date.second
  vars=get_vars(source)
  files=get_files(date, vars, source)
  IF ((size(files))[0] eq 0) THEN BEGIN
    print, 'INFO get_fields: ', files
  ENDIF ELSE BEGIN
    FOR jfile=0,N_Elements(files)-1 DO $
      print, 'INFO get_fields: ', files[jfile]
  ENDELSE
  CASE StrLowCase(source) OF
    'ecmwf_2.5p': BEGIN
       get_fields_ec25, fields, aux, files
     END
    'ecmwf_2.5mf': BEGIN
       get_fields_ec25, fields, aux, files
     END
    'ecmwf_2.5p_e4t': BEGIN
       get_fields_ec25, fields, aux, files
     END
    'ecmwf_1.125m': BEGIN
       get_fields_ec1125, fields, aux, files
     END
     'ecmwf_1.125mf': BEGIN
       get_fields_ec1125, fields, aux, files
     END
     'ukmo_gs': BEGIN
       get_fields_pp, fields, aux, files
     END
  ENDCASE
  IF (kcvert) THEN BEGIN    ; isentropic advection
    fields.z=0.
    fields.zhm=calc_theta(aux.temp,fields.zhm)
  ENDIF
END
;
;-----------------------------------------------------------------------
;
PRO get_horizontal_weights, latitude, longitude, th, ph, x1, y1, kinrange
;
; Purpose: find horizontal weights for regular grid
;

  ilat=N_Elements(latitude)
  ilon=N_Elements(longitude)

  dphg=longitude(1)-longitude(0)
  dthg=latitude(1)-latitude(0)
  pos=where(ph[kinrange] lt 0)
  pht=ph
  tht=th
  if (pos(0) ne  -1) then pht[kinrange[pos]]=2*!pi+pht[kinrange[pos]]
  pht[kinrange]=pht[kinrange]-longitude(0)
  tht[kinrange]=tht[kinrange]-latitude(0)
  x1[kinrange]= pht[kinrange]/dphg
  y1[kinrange]= tht[kinrange]/dthg

  ii=where(x1 lt 0 or x1 gt ilon-1,ic)
  IF (ic gt 0) THEN print, 'WARN  extrapolating in longitude'
  ii=where(y1 lt 0 or y1 gt ilat-1,ic)
  IF (ic gt 0) THEN print, 'WARN  extrapolating in latitude'
  return
END
;
;-----------------------------------------------------------------------
;
PRO incr_vec, aro, ari, au, adt, kinrange
;
; Purpose: increment the position vectors of the parcels
;
  IF (kinrange[0] lt 0) THEN return
  aro.x[kinrange]=ari.x[kinrange]+au.x[kinrange]*adt
  aro.y[kinrange]=ari.y[kinrange]+au.y[kinrange]*adt
  aro.z[kinrange]=ari.z[kinrange]+au.z[kinrange]*adt
  aro.zhm[kinrange]=ari.zhm[kinrange]+au.zhm[kinrange]*adt
  return
END
;
;-----------------------------------------------------------------------
;
PRO interp_space, data3d, longitude, latitude, level, r, rout, kinrange 
;
; Purpose: Interpolate a 3-D gridded data structure on to parcel positions
;
  IF (kinrange[0] lt 0) THEN return ; check its worth doing anything
  x1=fltarr(n_elements(r.x))
  y1=x1

  FOR j=0,N_Tags(rout)-1 DO BEGIN
    rout.(j)=0.
  ENDFOR

  latlon_from_cartesians, r, th, ph, kinrange ; map to space of wind files
  get_horizontal_weights, latitude, longitude, th, ph, x1, y1, kinrange
  z1=find_vert(level,x1,y1,r.zhm,kinrange)

  IF (kinrange[0] lt 0) THEN return
  FOR j=0,N_Tags(rout)-1 DO BEGIN
    rout.(j)[kinrange]=interpolate(data3d.(j),x1[kinrange],y1[kinrange],z1[kinrange])
  ENDFOR
END
;
;-----------------------------------------------------------------------
;
PRO interp_time, fields0, fields1, fields, t, tuv, dtuv
;
; Purpose: interpolate an arbitary structure in time
  dd=(tuv-t)/dtuv
  FOR j=0,N_Tags(fields)-1 DO BEGIN
    fields.(j)=dd*fields0.(j)+(1.-dd)*fields1.(j)
  ENDFOR
END
;
;-----------------------------------------------------------------------
;
PRO latlon_from_cartesians, r, th, ph, kinrange
;
; Purpose: convert cartesian coordinates into geographical, latitude
;          longitude coordinates
;
  IF (kinrange[0] lt 0) THEN return  ; no point in doing anything
  ipar=n_elements(r.x)
  zzz=replicate(-1.,ipar)
  ph=zzz
  th=zzz
  zzz[kinrange]=r.z[kinrange]
  pos=where(r.z[kinrange] lt -1.0)
  if (pos(0) ne -1) then zzz[kinrange[pos]]=-1.0
  pos=where(r.z[kinrange] gt 1.0)
  if (pos(0) ne -1) then zzz[kinrange[pos]]=1.0
  th[kinrange]=asin(zzz[kinrange])
  ph[kinrange]=atan(r.y[kinrange],r.x[kinrange])
END
;
;------------------------------------------------------------------
;
PRO nc_define_aux, ncdf, kcvert
;
; Purpose: defines the auxillary variables in the output netcdf file
;
  ncdf_control, ncdf.file_id, /redef
  id_time=ncdf_dimid(ncdf.file_id,'time') 
  id_parc=ncdf_dimid(ncdf.file_id,'parcel')

  id_temp=ncdf_vardef(ncdf.file_id,'temp',[id_parc,id_time])
  ncdf_attput, ncdf.file_id, id_temp, 'units', 'K'
  ncdf_attput, ncdf.file_id, id_temp, 'long_name', 'temperature'
  ncdf_attput, ncdf.file_id, id_temp, 'missing_value', -999.99

  CASE kcvert OF
    0: BEGIN
      id_theta=ncdf_vardef(ncdf.file_id,'theta',[id_parc,id_time])
      ncdf_attput, ncdf.file_id, id_theta, 'units', 'K'
      ncdf_attput, ncdf.file_id, id_theta, 'long_name', 'potential temperature'
      ncdf_attput, ncdf.file_id, id_theta, 'missing_value', -999.99
     END
    1: BEGIN
      id_press=ncdf_vardef(ncdf.file_id,'press',[id_parc,id_time])
      ncdf_attput, ncdf.file_id, id_press, 'units', 'Pa'
      ncdf_attput, ncdf.file_id, id_press, 'long_name', 'pressure'
      ncdf_attput, ncdf.file_id, id_press, 'missing_value', -999.99
     END
  ENDCASE

  ncdf_control, ncdf.file_id, /endef
END
;
;-----------------------------------------------------------------------
;
PRO nc_output_label, ncdf_info, source, date, grid, dt, kcvert
;
; Purpose: label the input file with details of the model integration.
;
  ncid=ncdf_info.file_id
  ncdf_attget, ncid, /global, 'history', yhist
  yhist=string(yhist)
  itoday=today()
  ytoday=string(itoday.year,itoday.month,itoday.day,itoday.hour,itoday.minute,$
          itoday.second,$
          format="(i4.4,'/',i2.2,'/',i2.2,2x,i2.2,':',i2.2,':',i2.2)")
  yhist=yhist+'  Trajectories calculated by advect.pro '+ytoday
  ysource=source_label(source)
  ydate=string(date.year,date.month,date.day,date.hour,$
          format="(i4.4,i2.2,i2.2,i2.2)")
  IF (StrLowCase(source) EQ 'ecmwf_2.5mf') THEN BEGIN 
    tfore=forecast_base()
    IF (itoday.julian GT tfore.julian) THEN $
      yfore=string(tfore.year,tfore.month,tfore.day,tfore.hour,$
                   format='(i4.4,i2.2,i2.2,i2.2)')
  ENDIF
  ncdf_control, ncid, /redef
  ncdf_attput, ncid, /global, 'history', yhist, /char
  ncdf_attput, ncid, /global, 'Version', rcs_string('$Revision: 888 $'), /char
  CASE kcvert OF
    0: BEGIN
      ncdf_attput, ncid, /global, '3dTrajectory', 'T', /char
      ncdf_attput, ncid, /global, 'VerticalVelocity', 'omega', /char
      ncdf_attput, ncid, /global, 'VerticalCoordinate', 'pressure', /char
    END
    1: BEGIN
      ncdf_attput, ncid, /global, '3dTrajectory', 'F', /char
      ncdf_attput, ncid, /global, 'VerticalCoordinate', 'theta', /char
    END
  ENDCASE
  ncdf_attput, ncid, /global, 'TimeStep', dt.dt, /float
  ncdf_attput, ncid, /global, 'WindSource', ysource, /char
  ncdf_attput, ncid, /global, 'TrajectoryBaseTime', ydate, /char
  IF (N_Elements(yfore) GT 0) THEN $
     ncdf_attput, ncid, /global, 'ForecastBaseTime', yfore, /char
  ncdf_attput, ncid, /global, 'SourceResolution', $
               String(Abs(grid.dlat),Abs(grid.dlon),$
               format='(f5.3,''x'',f5.3)'), /char
  IF (StrLowCase(Tag_Names(grid,/Structure_Name)) eq 'gr_ll_hsp') THEN BEGIN
    ncdf_attput, ncid, /global, 'SourceLevelType','hybrid sigma-pressure',/char
    ncdf_attput, ncid, /global, 'SourceLevelNumber', grid.nv/2-1, /long
  ENDIF ELSE BEGIN
    ncdf_attput, ncid, /global, 'SourceLevelType','pressure',/char
    ncdf_attput, ncid, /global, 'SourceLevelNumber', N_Elements(grid.levels),$
                                                      /long
  ENDELSE
  ihours=string(24/get_file_dt(source))
  ncdf_attput, ncid, /global, 'SourceTimeInterval', ihours+' hours', /char
  ncdf_control, ncid, /endef
END
;
;-------------------------------------------------------------------------
;
PRO nc_read_parcels, filein, time0, lon, lat, lev, ncdf_info

print,'INFO Opening Trajectory point initial conditions: '+filein

ncid=ncdf_open(filein,/write)
id_time=ncdf_dimid(ncid,'time')   ; needed?
id_parc=ncdf_dimid(ncid,'parcel') ; needed?
iv_time=ncdf_varid(ncid,'time')
iv_lon=ncdf_varid(ncid,'lon')
iv_lat=ncdf_varid(ncid,'lat')
iv_lev=ncdf_varid(ncid,'lev')

ncdf_varget, ncid, iv_time, time0
time0=time0(0)

ncdf_varget, ncid, iv_lon , lon
ncdf_varget, ncid, iv_lat , lat
ncdf_varget, ncid, iv_lev , lev
ncdf_attget, ncid, iv_lon, 'missing_value', udef_lon
ncdf_attget, ncid, iv_lat, 'missing_value', udef_lat
ncdf_attget, ncid, iv_lev, 'missing_value', udef_lev
ncdf_attget, ncid, iv_lev, 'units', unit_lev

IF (strupcase(unit_lev) eq 'MB' or strupcase(unit_lev) eq 'HPA') THEN BEGIN
  lev=lev*100.
  unit_lev='Pa'
  ncdf_attput, ncid, iv_lev, 'units', unit_lev
  ncdf_varput, ncid, iv_lev, lev
ENDIF
ncdf_control, ncid, /redef
ncdf_attput, ncid, iv_time, 'units', 'seconds'
ncdf_control, ncid, /endef

print, 'INFO Number of trajectories: '+string(n_elements(lon))

ncdf_info={ncdf_traj,file_id:ncid,time_vid:iv_time,lon_vid:iv_lon,$
	             lat_vid:iv_lat,lev_vid:iv_lev,$
                     lon_undef:udef_lon,lat_undef:udef_lat,$
                     lev_undef:udef_lev, t_index:0}

END
;
;-----------------------------------------------------------------------
;
PRO nc_write_parcels, t, r, raux, rcalc, nc_info, in_range, dtout, kcvert
;
; Purpose: write the parcels to the output file
; Interface: write_parcels, t, r, nc_info, in_range
;            t - current time
;            r - parcel positions
;            nc_info - netcdf information file
;            in_range - trajectories that are valid
;            dtout - time between output times
;
  undef=-999.99
  ipar=n_elements(r.x)
  
  zlon=replicate(nc_info[0].lon_undef,ipar)
  zlat=replicate(nc_info[0].lat_undef,ipar)
  zlev=replicate(nc_info[0].lev_undef,ipar)

  inund=index_complement(ipar,in_range)
  IF (inund[0] ne -1) THEN BEGIN
    raux.temp[inund]=undef
    FOR j=0,N_Tags(rcalc)-1 DO BEGIN
      rcalc.(j)[inund]=undef
    ENDFOR	
  ENDIF
  IF (in_range[0] gt -1) THEN BEGIN
    zzz=r.z(in_range)
    pos=where(r.z(in_range) lt -1.0)
    if (pos(0) ne -1) then zzz(pos)=-1.0
    pos=where(r.z gt 1.0)
    if (pos(0) ne -1) then zzz(pos)=1.0
    th=asin(zzz)
    ph=atan(r.y(in_range),r.x(in_range))
  
    zlon(in_range)=!radeg*ph
    zlat(in_range)=!radeg*th
    zlev(in_range)=r.zhm(in_range)
    ino_undef=ipar-n_elements(in_range)
  ENDIF ELSE BEGIN
    ino_undef=ipar
  ENDELSE
  
  print, 'INFO output at ', t, ' Number of undefined parcels: ',$
  	  ino_undef
  ipar=ipar/N_Elements(nc_info)
  FOR j=0,N_Elements(nc_info)-1 DO BEGIN
    tt=dtout*nc_info[j].t_index
    is=j*ipar
    ie=(j+1)*ipar-1
    ncdf_varput, nc_info[j].file_id, nc_info[j].time_vid, tt, $
  		 offset=[nc_info[j].t_index]
    ncdf_varput, nc_info[j].file_id, nc_info[j].lon_vid, zlon(is:ie), $
  		 offset=[0,nc_info[j].t_index], count=[ipar,1]
    ncdf_varput, nc_info[j].file_id, nc_info[j].lat_vid, zlat(is:ie), $
  		 offset=[0,nc_info[j].t_index], count=[ipar,1]
    ncdf_varput, nc_info[j].file_id, nc_info[j].lev_vid, zlev(is:ie), $
  		 offset=[0,nc_info[j].t_index], count=[ipar,1]
    ncdf_varput, nc_info[j].file_id, ncdf_varid(nc_info[j].file_id, 'temp'),$ 
                 raux.temp[is:ie], $
                 offset=[0,nc_info[j].t_index], count=[ipar,1]
    CASE kcvert OF
      0: $
        ncdf_varput, nc_info[j].file_id,$
                 ncdf_varid(nc_info[j].file_id, 'theta'),$ 
                 rcalc.theta[is:ie], $
                 offset=[0,nc_info[j].t_index], count=[ipar,1]
      1: $
        ncdf_varput, nc_info[j].file_id,$
                 ncdf_varid(nc_info[j].file_id, 'press'),$ 
                 rcalc.press[is:ie], $
                 offset=[0,nc_info[j].t_index], count=[ipar,1]
    ENDCASE
    nc_info[j].t_index=nc_info[j].t_index+1
  ENDFOR
  
  return
END
;
;-----------------------------------------------------------------------
;
PRO parcels_add, ar, arnew, araux, arnaux, arcalc, arncalc, kin_range, kcvert
;
; Purpose: add a new set of parcels to the parcel position array
; Interface: parcels_add, ar, arnew
;           ar - set of parcels to add to
;           ar - new set of parcels
;           kin_range - parcels that are good
;
  iadd=N_Elements(arnew.x)
  IF (Size(ar,/Type) NE 8) THEN BEGIN
    ar=arnew
    araux=arnaux
    arcalc=arncalc
    kin_range=Indgen(iadd)
  ENDIF ELSE BEGIN
    kin_range=[kin_range,Indgen(iadd)+N_elements(ar.x)]
    tmp={x:[ar.x,arnew.x], y:[ar.y,arnew.y], z:[ar.z,arnew.z], $
         zhm:[ar.zhm,arnew.zhm]}
    ar=tmp
    tmp={temp:[araux.temp,arnaux.temp]}
    araux=tmp       ; don't like this - it is not general
    CASE kcvert OF
      0: tmp={theta:[arcalc.theta,arncalc.theta]}
      1: tmp={press:[arcalc.press,arncalc.press]}
    ENDCASE
    arcalc=tmp      ; again not general!! yuk.
  ENDELSE

END
;
;-----------------------------------------------------------------------
;
PRO parcels_delete, ar, aaux, acalc, kin_range, kpar, kcvert
;
; Purpose: delete a set of parcels from the parcel position array
; Interface: parcels_delete, ar, kpar
;            ar - set of parcels to remove from
;            kpar - number of parcels to remove
;            kin_range - parcels that are good
;
  print, 'INFO Parcels delete: ', kpar, N_elements(ar.x)

  IF (N_Elements(ar.x) GT kpar) THEN BEGIN
    tmp={x:ar.x[kpar:*], y:ar.y[kpar:*], z:ar.z[kpar:*], zhm:ar.zhm[kpar:*]}
    ar=tmp
    tmp={temp:aaux.temp[kpar:*]} ; yuk, not general !!
    aaux=tmp
    CASE kcvert OF
      0: tmp={theta:acalc.theta[kpar:*]}
      1: tmp={press:acalc.press[kpar:*]}
    ENDCASE
    acalc=tmp
    itmp=where(kin_range GE kpar)
    IF (itmp[0] GT -1) THEN kin_range=kin_range[itmp]-kpar ELSE kin_range=-1
  ENDIF ELSE BEGIN
    ar=0   ; not perfect, but can't use delvar,
    aaux=0
    acalc=0
    kin_range=-1
  ENDELSE

END

;
;---------------------------------------------------------
;
PRO runge_kutta, fields0, fields1, velt, velthdt, veltdt,$
                 unorm, dt, r, t, tuv, dtuv, $
                 longitude, latitude, in_range
                
;     advects all of the nodes by a 4th order runga-kutta scheme.
;     dt2: dt/2   ; dt3: dt/3   ; dt6: dt/6

;     PRO 'uvw' below calculates the velocity field at each
;     node for the contours (u,v,w) given the contour (x,y,z) positions.

ltfirst=(n_elements(velt) gt 0)

up=r
ri=r
rf=r

if ltfirst then begin
  uvw,velt,up,unorm,r,longitude,latitude, in_range
endif else begin
  up0=up
  up1=up
  uvw,fields0,up0,unorm,r,longitude,latitude, in_range
  uvw,fields1,up1,unorm,r,longitude,latitude, in_range
  time_interp,up0,up1,up,t,tuv,dtuv
endelse

incr_vec,r,ri,up,dt.dt2,in_range
incr_vec,rf,rf,up,dt.dt6,in_range
adjust,r

if ltfirst then begin
  uvw,velthdt,up,unorm,r,longitude,latitude, in_range
endif else begin
  uvw,fields0,up0,unorm,r,longitude,latitude, in_range
  uvw,fields1,up1,unorm,r,longitude,latitude, in_range
  time_interp,up0,up1,up,t+dt.dt2,tuv,dtuv  ; do undefines matter?
endelse

incr_vec, r, ri, up, dt.dt2,in_range
incr_vec, rf,rf,up, dt.dt3,in_range
adjust,r

if ltfirst then begin
  uvw,velthdt,up,unorm,r,longitude,latitude, in_range
endif else begin
  uvw,fields0,up0,unorm,r,longitude,latitude, in_range
  uvw,fields1,up1,unorm,r,longitude,latitude, in_range
  time_interp,up0,up1,up,t+dt.dt2,tuv,dtuv
endelse
incr_vec, r, ri, up,dt.dt,in_range
incr_vec, rf, rf, up,dt.dt3,in_range
adjust,r

if ltfirst then begin
  uvw,veltdt,up,unorm,r,longitude,latitude, in_range
endif else begin
  uvw,fields0,up0,unorm,r,longitude,latitude, in_range
  uvw,fields1,up1,unorm,r,longitude,latitude, in_range
  time_interp,up0,up1,up,t+dt.dt,tuv,dtuv
endelse

incr_vec,r,rf,up,dt.dt6,in_range
adjust,r

return
END
;
;-----------------------------------------------------------------------
;
PRO solid_rotation, fields, longitude, latitude, time
;
; Purpose: routine used for test purposes
;
  isize=size(fields.x)
  ilon=isize(1)
  ilat=isize(2)
  ilev=isize(3)
  omegar=(2.*!pi/86400.)*6.371e6
  lat0=0.*!pi/180.
  lon0=0.*!pi/180.
  lon=longitude
  lat=latitude
  omegat=omegar
  ;omegat=omegar*0.4*(1.+sin(0.1*time*!pi/86400.))
  ;omegat=2.*!pi*time*6.371e6/(25.*86400*86400.)
  print, 'INFO Calculating Solid body rotation at time: ', time
  
  for jlat=0,ilat-1 do begin
    fields.x(*,jlat,0)=omegat*(cos(lat(jlat))*sin(lat0)-$
  			     cos(lon-lon0)*sin(lat(jlat))*cos(lat0))
    fields.y(*,jlat,0)=omegat*sin(lon-lon0)*cos(lat0)
  endfor
  
  for jlev=1,ilev-1 do begin
    fields.x(*,*,jlev)=fields.x(*,*,0)
    fields.y(*,*,jlev)=fields.y(*,*,0)
  endfor
  
  fields.z=500./86400.
  dlev=(1000.-100000.)/30
  levs=findgen(31)*dlev+100000.
  for jlev=0,30 do fields.z(*,*,jlev)=0.25*(50000-levs(jlev))/86400.
  for jlev=0,30 do fields.zhm(*,*,jlev)=levs(jlev)

END
;
;-------------------------------------------------------------
;
PRO time_interp, fields0, fields1, vel, t, tuv, dtuv
;
; Purpose: time interpolation of velocity/vertical coordinate
;          structure
;
  dd=(tuv-t)/dtuv
  vel.x=dd*fields0.x+(1.-dd)*fields1.x
  vel.y=dd*fields0.y+(1.-dd)*fields1.y
  vel.z=dd*fields0.z+(1.-dd)*fields1.z
  vel.zhm=dd*fields0.zhm+(1.-dd)*fields1.zhm
END
;
;-------------------------------------------------------------
;
PRO traj_file_add, kold, knew
;
; Purpose: add a new file to the list of netcdf files
; Interface: traj_file_add, kold, knew
;            kold - list of old files to add to
;            knew - new file to be added
;
  IF (Size(kold,/Type) NE 8) THEN $
    kold=knew $
  ELSE $
    kold=[kold,knew]
END
;
;-----------------------------------------------------------------------
;
PRO traj_file_delete, kold
;
; Purpose: remove an old file from the list of netcdf files
; Interface: traj_file_delete, kold
;
  ncdf_close, kold[0].file_id
  IF (N_Elements(kold) GT 1) THEN $
    kold=kold[1:*] $
  ELSE $
    kold=0
END
;
;-----------------------------------------------------------------------
;
PRO uvw,vel,up,unorm,r,longitude,latitude, in_range
;
;  Interpolation to find velocity field at parcel positions
;

  IF (in_range[0] lt 0) THEN return ; check its worth doing anything
; .1 Initialise
  up.x=0.
  up.y=0.
  up.z=0.
  up.zhm=0.

  x1=fltarr(n_elements(r.x))
  y1=x1
  latlon_from_cartesians, r, th, ph, in_range
  get_horizontal_weights, latitude, longitude, th, ph, x1, y1, in_range
  z1=find_vert(vel.zhm, x1, y1, r.zhm,in_range)

  IF (in_range[0] lt 0) THEN return

; .4 linear interpolation
  upt=interpolate(vel.x,x1(in_range),y1(in_range),z1(in_range))
  vpt=interpolate(vel.y,x1(in_range),y1(in_range),z1(in_range))
  up.zhm(in_range)=interpolate(vel.z,x1(in_range),y1(in_range),z1(in_range))

; .5 convert from longitude-latitude velocities to cartesians on unit sphere
  up.x(in_range)=(-sin(ph[in_range])*upt-sin(th[in_range])*cos(ph[in_range])*vpt)/unorm
  up.y(in_range)=( cos(ph[in_range])*upt-sin(th[in_range])*sin(ph[in_range])*vpt)/unorm
  up.z(in_range)=( cos(th[in_range])*vpt)/unorm

  return
END
;
;-----------------------------------------------------------------------
;
;***********************************************
;***********************************************
;		GRIB EXTRACTION ROUTINES
;
;	THIS SOFTWARE READS AND EXTRACTS DATA FROM GRIB FILES.
;	IT ONLY WORKS FOR SIMPLY PACKED DATA IN LAT-LONG OR GUASSIAN 
;	GRID FORM.
;		
;		IDL> .compile grib
;
;	TO READ DATA FROM A LAT-LONG GRIB FILE, PRINT SOME OF THE VALUES
;	AND THEN PLOT THEM, FOLLOW THIS EXAMPLE. 
;
;		IDL> grib_open,unit,'lat-long.grib.file' 
;		IDL> x=grib_read(unit)
;		IDL> print, x.data(0:100)
;		IDL> data_array=x.data
;		IDL> data_array = reform(data_array, x.gds.nlong, x.gds.nlat)
;		IDL> tv, bytscl(data_array)
;
;	TO READ A GUASSIAN GRID
; 		 
;		IDL> grib_open,unit,'guassian.grib.file' 
;		IDL> x=grib_read(unit)
;		IDL> grib_n80toll, x
;
;	SEE THE ROUTINE HEADERS FOR MORE DETAILS. 
;
;***********************************************
;***********************************************





;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;+
; NAME:
;       GRIB_OPEN, GRIB_CLOSE
;
; PURPOSE:
;       Open and close grib files; same as openr and close. 
;
; CATEGORY:
;       I/O.
;
; CALLING SEQUENCE:
;       GRIB_OPEN, unit, file
;	GRIB_CLOSE, unit  
;
; INPUTS:
;	file:	string of file name.
;
; OUTPUTS:
;       unit:   Logical unit number.
;
; SIDE EFFECTS:
;       None.
; RESTRICTIONS:
;       None.
;
; MODIFICATION HISTORY:
;       Written dec, 1996, S.J.Pepler.
;---

pro grib_open, unit, fname
   print, 'INFO Opening GRIB file: ',fname[0]
   openr,unit,fname,/get_lun
   return

end

pro grib_close,unit

	free_lun, unit

end


;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;+
; NAME:
;       GRIB_SEC
;
; PURPOSE:
;       Split and decode grib message. 
;
; CATEGORY:
;       conversion.
;
; CALLING SEQUENCE:
;       GRIB_SEC, grib, s0, s1, s2, s3, s4, s5  
;
; INPUTS:
;       grib:   An array of bytes (grib message).
;
; OUTPUTS:
;       s0: 	structure containing grib section 0 info.
;       s1: 	structure containing grib section 1 info.
;       s2: 	structure containing grib section 2 info.
;       s3: 	structure containing grib section 3 info.
;       s4: 	Array of floats containing data.
;       s5: 	string '7777'
;
; SIDE EFFECTS:
;       None.
; RESTRICTIONS:
;	None.
;
; MODIFICATION HISTORY:
;       Written dec, 1996, S.J.Pepler.
;---

pro grib_sec,grib,sec0,sec1,sec2,sec3,sec4,sec5,no_decode=no_decode

	sec0=grib(0:7) 					;SECTION 0
	if(not keyword_set(no_decode)) then decode0, sec0	

	start1=8					;SECTION 1
	len1=getlong(grib(start1:start1+2))
	sec1=grib(start1:start1+len1-1)
	if(not keyword_set(no_decode)) then decode1,sec1

	flag2=(grib(15) and 128b)			;SECTION 2
	if (flag2 gt 0) then begin
		start2=start1+len1 
		len2=getlong(grib(start2:start2+2))
		sec2=grib(start2:start2+len2-1)
		if(not keyword_set(no_decode)) then decode2,sec2
	endif else begin 
		start2=start1+len1
		len2=0
	endelse

	flag3=(grib(15) and 64b) 			;SECTION 3
	if (flag3 gt 0) then begin 
		start3=start2+len2 
		len3=getlong(grib(start3:start3+2))
		sec3=grib(start3:start3+len3-1)
		if(not keyword_set(no_decode)) then decode3,sec3
	endif else begin 
		start3=start2+len2
		len3=0
	endelse


	start4=start3+len3 				;SECTION 4
	len4=getlong(grib(start4:start4+2))
	sec4=grib(start4:start4+len4-1)
	if(not keyword_set(no_decode)) then decode4,sec4,paras=paras

	start5=start4+len4				;SECTION 5
	len5=4
	sec5=grib(start5:start5+len5-1)
	if(not keyword_set(no_decode)) then decode5,sec5

	return
end

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;+
; NAME:
;       DECODE0, DECODE1, DECODE2, DECODE3, DECODE4, DECODE5 
;
; PURPOSE:
;        Decodes the sections of the grib message. 
;
; CATEGORY:
;       conversion.
;
; CALLING SEQUENCE:
;       DECODEx, secx  
;
; INPUTS:
;       secx:   An array of bytes (portion of grib message).
;
; OUTPUTS:
;       secx: 	structure containing grib section info (sections 0,1,2,3).
;        	array of floats (section 4) 
;        	string '7777' (section 5)
;
; KEYWORD Parameters:
;       DECODE4:  paras = extraction parameters.
; SIDE EFFECTS:
;       None.
; RESTRICTIONS:
;	None.
;
; MODIFICATION HISTORY:
;       Written dec, 1996, S.J.Pepler.
;---

;------------------------------
pro decode0, sec0
	a=string(sec0(0:3))		;'GRIB'
	b=getlong(sec0(4:6))		; LENGTH OF SECTION 0
	c=sec0(7)			; GRIB EDITION
	sec0={sec0, grib:a, len:b, edition:c}
	return
end
;-------------------------------
pro decode1, sec1
	len=getlong(sec1(0:2))
	averages=getlong(sec1(21:22))
	scale_fac=getlong(sec1(26:27))

	sec1={sec1, len:len, table:sec1(3), center:sec1(4), $
		process:sec1(5), grid:sec1(6), secflag:sec1(7), $
		para:sec1(8), leveltype:sec1(9), level:getlong(sec1(10:11)), $
		year:sec1(12), month:sec1(13), $
		day:sec1(14), hour:sec1(15), minute:sec1(16), $
		forcast_unit:sec1(17), P1:sec1(18),P2:sec1(19), $
		time_range:sec1(20), averages:averages, ave_missing:sec1(23), $
		century:sec1(24), subcenter:sec1(25), scale_fac:scale_fac }
	return
end
;-------------------------------
pro decode2, sec2
	len=getlong(sec2(0:2))
	if (sec2(5) ne 0 and sec2(5) ne 4 ) then begin
		print, 'ERROR Not a lat-long or gaussian grid - cant decode section 2'
                stop
	endif	
        nv=sec2(3)
        pv=sec2(4)
        if nv gt 0 then begin
	  vct=Fltarr(nv)
	  for j=0,nv-1 do $
            vct(j)=getreal(sec2(pv-1+j*4:pv+2+j*4))
       	  sec2={sec2_vct, len:len, NV:sec2(3), PV:sec2(4), $
		gridtype:sec2(5), nlong:getlong(sec2(6:7)), $
		nlat:getlong(sec2(8:9)), lat1:getlong(sec2(10:12),/sign), $
		long1:getlong(sec2(13:15),/sign), resflag:sec2(16), $
		lat2:getlong(sec2(17:19),/sign), $
		long2:getlong(sec2(20:22),/sign), $
		dir_inc_long:getlong(sec2(23:24)), $
		dir_inc_lat:getlong(sec2(25:26)), $
		scanflag:sec2(27),$
                vct:vct   }
        endif else begin
       	  sec2={sec2, len:len, NV:sec2(3), PV:sec2(4), $
		gridtype:sec2(5), nlong:getlong(sec2(6:7)), $
		nlat:getlong(sec2(8:9)), lat1:getlong(sec2(10:12),/sign), $
		long1:getlong(sec2(13:15),/sign), resflag:sec2(16), $
		lat2:getlong(sec2(17:19),/sign), $
		long2:getlong(sec2(20:22),/sign), $
		dir_inc_long:getlong(sec2(23:24)), $
		dir_inc_lat:getlong(sec2(25:26)), $
		scanflag:sec2(27)}
        endelse
     	return
end
;-------------------------------
pro decode3, sec3
	len=getlong(sec3(0:2))
	print, 'ERROR cant decode bit map section'
	stop
end
;-------------------------------
pro decode4, sec4, paras=sec4out

	len=getlong(sec4(0:2))			;LENGTH OF SECTION 4
	bin_sf=getlong(sec4(4:5),/sign)		;BINARY SCALEING FACTOR
	ref=getreal(sec4(6:9))			;REFERANCE VALUE
	databits=sec4(10)			;BITS PER DATUM

	if (sec4(3) gt 15) then begin
		print, 'ERROR Not simply packed, real grid point data - stop.'
		stop
	endif
	
	sec4out={sec4, len:len, flags:sec4(3), bin_scale_factor:bin_sf, $
		ref:ref, databits:databits }

	bytes_per_datum=databits/8
	data=sec4(11:*)			
	nbytes=(size(data))(1)
	junk_bytes = nbytes mod bytes_per_datum	   ;SOMETIMES ZERO PADDED  
	data=data(0:nbytes-1-junk_bytes)  
		;CHANGE DATA INTO AN BYTES PER DATUM BY NUMBER OF DATUM ARRAY	
	data=reform(data, bytes_per_datum, nbytes/bytes_per_datum)
		;RECOVER LONG INTEGERS	
	dataout=long(data(bytes_per_datum-1,*))
	for i=0,bytes_per_datum-2 do begin
		dataout(0,*)=256L^(bytes_per_datum-i-1)*data(i,*)+dataout(0,*)
	endfor
	dataout=transpose(dataout)		;FLIP ARRAY TO VECTOR
	dataout=ref+((2.0^bin_sf)*dataout)	;RECOVER REALS
	sec4=dataout

	return
end
;-------------------------------
pro decode5, sec5
	a=string(sec5(0:3)) 		;'7777'
	sec5=a
	return
end
;------------------------------------

;================================ oOo =====================================
;================================ oOo =====================================
;================================ oOo =====================================

pro grib_chk, sec1, sec2
;
; Purpose: check the grib data is consistent with expected
;
   if (sec1.leveltype ne 109 and sec1.leveltype ne 100) then $
      message, 'grib_chk: not hybrid sigma-p or pressure levels'
   if sec2.gridtype ne 0 then message, 'grib_chk: not on a regular lat-lon grid'
   if sec2.scanflag ne 0 then message, 'grib_chk: not in lon,lat order'
   return
end

pro grib_grid, unit, grid
   grib=grib_read(unit,erro=ierr,/no)
   grib_sec, grib, sec0
   sec0=grib(0:7)
   decode0, sec0	
   start1=8
   len1=getlong(grib(start1:start1+2))
   sec1=grib(start1:start1+len1-1)
   decode1,sec1
   flag2=(grib(15) and 128b)
   if (flag2 gt 0) then begin
     start2=start1+len1 
     len2=getlong(grib(start2:start2+2))
     sec2=grib(start2:start2+len2-1)
     decode2,sec2
   endif else begin 
     start2=start1+len1
     len2=0
   endelse
   grib_chk, sec1, sec2
   nv=sec2.nv
   dlat=-1.*sec2.dir_inc_lat*1e-3
   case sec1.leveltype of 
   109: grid={gr_ll_hsp, lat0:sec2.lat1*1e-3, dlat:dlat, ilat:sec2.nlat, $
                      lon0:sec2.long1*1e-3, dlon:sec2.dir_inc_long*1e-3,$
                      ilon:sec2.nlong, $
                      nv:nv, a:sec2.vct(0:nv/2-1), b:sec2.vct(nv/2:*)}
   100: begin
         if (sec1.para eq 131) then zlev=sec1.level ; look for levels of uwind
         grib=grib_read(unit, /no, error=ierr)
         while (ierr eq 0) do begin
           if grib(16) eq 131 then begin
             grib_sec, grib, tsec0, tsec1, tsec2, tsec3, tsec4, tsec5
             if (n_elements(zlev) lt 1) then $
               zlev=sec1.level else zlev=[zlev,sec1.level]
           endif
           grib=grib_read(unit, /no, error=ierr)
         endwhile
	 grid={gr_ll_p,lat0:sec2.lat1*1e-3, dlat:dlat, ilat:sec2.nlat, $
                      lon0:sec2.long1*1e-3, dlon:sec2.dir_inc_long*1e-3,$
                      ilon:sec2.nlong,levels:zlev}
	end
   endcase
    
end

pro grib_get_fields, unit, u, v, w, p, temperature=t
;
; 1. Initialise variables
  ucode=131   ; ECMWF grib codes
  vcode=132
  wcode=135
  tcode=130
  pcode=152

  ltemp=n_elements(t) gt 0 ; is temp needed?
  codes=[ucode,vcode,wcode,pcode]
  if ltemp then codes=[codes,tcode]

  isize=size(u)
  ilon=isize(1)
  ilat=isize(2)
  ilev=isize(3)

  upres=intarr(ilev)      ; arrays to test for presence of params
  vpres=intarr(ilev)      ; at all levels
  wpres=intarr(ilev)
  if (ltemp) then tpres=intarr(ilev)
  ppres=fix(0)
  ps=fltarr(ilon,ilat,/nozero)
  press=fltarr(ilev)
  press[*]=-999.
;
; 2. Read data and put into arrays
  iilev=-1
  grib=grib_read(unit, /no, error=ierr)
  while (ierr eq 0) do begin
    itt=where(grib(16) eq codes,ic)
    if (ic gt 0) then begin  ; this field is needed
      grib_sec, grib, sec0, sec1, sec2, sec3, sec4, sec5
      grib_chk, sec1, sec2
      leveltype=sec1.leveltype

      if (leveltype eq 109) then begin
         iilev=sec1.level-1
      endif

      if (leveltype eq 100) then begin
        iii=(where(press eq sec1.level))[0]
        if (iii ge 0) then begin
          iilev=iii
        endif else begin
          iilev=iilev+1
          press(iilev)=sec1.level
        endelse
      endif

      case grib(16) of
      ucode: begin
        ulevcode=leveltype
        u(*,*,iilev)=sec4
        upres(iilev)=1
      end
      vcode: begin
        v(*,*,iilev)=sec4
        vpres(iilev)=1
      end
      wcode: begin
        w(*,*,iilev)=sec4
        wpres(iilev)=1
      end
      pcode: begin
        ps(*,*)=sec4
        ppres=1
      end
      tcode: begin
        t(*,*,iilev)=sec4
        tpres(iilev)=1
      end
      endcase
    endif ;else print, 'Field not read from grib: ',grib(16)
    grib=grib_read(unit, /no, error=ierr)
  endwhile
;
; 3. Error checking
  if (ierr ne -1) then message, 'grib_get_fields: error on grib read'
  itmp=where(upres eq 0,icount)
  if (icount gt 0) then message, 'grib_get_fields: u not present at all levels'
  itmp=where(vpres eq 0,icount)
  if (icount gt 0) then message, 'grib_get_fields: v not present at all levels'
  itmp=where(wpres eq 0,icount)
  if (icount gt 0) then message, 'grib_get_fields: w not present at all levels'
  if (leveltype eq 100) then begin
    it=where(press(sort(press)) eq press,ic1)
    it=where(reverse(press(sort(press))) eq press,ic2)
    if (ic1 ne ilev and ic2 ne ilev) $
      then message, 'grib_get_fields: pressure levels in unusable order'
  endif else begin
    if (ppres eq 0) then message, 'grib_get_fields: ln(ps) not present'
  endelse
  if (ltemp) then begin
    itmp=where(tpres eq 0,icount)
    if (icount gt 0) then message, 'grib_get_fields: T not present at all levels'
  endif
;
; 4. Calculate the Pressure at all levels
  if (ulevcode eq 109) then begin
    ps=exp(ps)   ; ln(ps) is read from input
    pmid=fltarr(ilon,ilat,ilev+1,/nozero)
    for jlev=0,ilev do pmid(*,*,jlev)=sec2.vct(jlev)+sec2.vct(ilev+1+jlev)*ps
    p(*,*,0:ilev-1)=0.5*(pmid(*,*,0:ilev-1)+pmid(*,*,1:ilev))
  endif else if (ulevcode eq 100) then begin
    for jlev=0,ilev-1 do p(*,*,jlev)=press(jlev)*100.
  endif
  return 
end

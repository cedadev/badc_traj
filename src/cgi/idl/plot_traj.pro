;
;  plot_traj.pro
;  -------------
;
;  Purpose: provide plotting facilities for the web trjectory service
;
FUNCTION which_vars, plots
;
; Purpose: Determines which variables are needed for the plots
;
  iplot=n_elements(plots)
  cvar=['null']
  FOR jplot=0,iplot-1 DO BEGIN
    IF (plots(jplot) eq 'map') THEN $
      cvar=[cvar,'lon','lat'] $
    ELSE $
      cvar=[cvar,plots[jplot]]
  ENDFOR
  cvar=cvar[1:*]          ; remove 'null'
  cvar=cvar(uniq(cvar,sort(cvar)))   ; remove repeats of 'lon' and 'lat'
  return, cvar
END

FUNCTION get_var, ncid, cvar, trajs
;
; Purpose: Read the variables from the netcdf file
;
  ivar=n_elements(cvar)
  ncdf_diminq, ncid, ncdf_dimid(ncid,'time'), cdum, itime
  ipar=n_elements(trajs)
  tmp={variable,name:"",undef:0.,long_name:"",units:"",min:0.,max:0.,$
                data:fltarr(ipar,itime)}
  avar=replicate(tmp,ivar)
  FOR jvar=0,ivar-1 DO BEGIN
    id=ncdf_varid(ncid,cvar[jvar])
    ncdf_attget, ncid, id, 'long_name', clong
    ncdf_attget, ncid, id, 'units', cunit
    ncdf_attget, ncid, id, 'missing_value', undef
    ncdf_varget, ncid, id, atmp
    avar[jvar].name=cvar[jvar]
    avar[jvar].long_name=string(clong)
    avar[jvar].undef=undef
    avar[jvar].min=undef
    avar[jvar].max=undef
    avar[jvar].units=string(cunit)
    avar[jvar].data=(atmp[trajs,*])
  ENDFOR
  return, avar
END

FUNCTION get_time, ncid
;
; Purpose: retrieve time from the netcdf file
;
  id=ncdf_varid(ncid,'time')
  ncdf_attget, ncid, id, 'units', tunits
  ncdf_varget, ncid, id, time
  IF (string(tunits) eq 'seconds') THEN BEGIN
    time=time/86400.
    tunits='Days'
  ENDIF
  return, {time, time:time, units:string(tunits)}
END

PRO press2hPa, pvar
;
; Purpose: convert the pressure to hPa from Pa
;
  j=n_elements(pvar)
  REPEAT j=j-1 UNTIL $
    (j < 0 ? 1 : (strlowcase(pvar[j].long_name) eq 'pressure'))
  IF j gt -1 THEN BEGIN
    IF (pvar[j].units eq 'Pascals' or pvar[j].units eq 'Pa') THEN BEGIN
      ii=where(pvar[j].data ne pvar[j].undef)
      IF (ii[0] gt -1) THEN  pvar[j].data[ii]=pvar[j].data[ii]*1.e-2
      pvar[j].units='hPa'
    ENDIF
  ENDIF
  return
END

PRO lon_range, pvar
;
; Purpose: bring the longitudes into the range -180., 180.
; Comments: why are they outside it in the first place?  Initial conditions
;
  j=n_elements(pvar)
  REPEAT j=j-1 UNTIL $
    (j < 0 ? 1 : (strlowcase(pvar[j].long_name) eq 'longitude'))
  IF (j gt -1) THEN BEGIN
    i1=where(pvar[j].data ne pvar[j].undef)
    IF (i1(0) ge 0) THEN BEGIN
      i2=where(pvar[j].data[i1] lt -180.)
      IF (i2(0) ge 0) THEN pvar[j].data[i1[i2]]=pvar[j].data[i1[i2]]+360.
      i2=where(pvar[j].data[i1] gt 180.)
      IF (i2(0) ge 0) THEN pvar[j].data[i1[i2]]=pvar[j].data[i1[i2]]-360.
    ENDIF
  ENDIF
  return
END

PRO process_limits, pvar, plims
;
; Purpose: add the user specified limits to the variables
;
  ivar=n_elements(pvar)
  FOR jvar=0,n_elements(plims)-1 DO BEGIN
    IF (plims[jvar].min ne plims[jvar].udef or $
        plims[jvar].max ne plims[jvar].udef) THEN BEGIN
      j=ivar
      REPEAT j=j-1 UNTIL $
        (j < 0 ? 1 : (pvar[j].name eq plims[jvar].name))
      IF j gt -1 THEN BEGIN
        pvar[j].min=plims[jvar].min
        pvar[j].max=plims[jvar].max
      ENDIF
    ENDIF
  ENDFOR
END

PRO set_multi, kplot
;
; Purpose: set the number of plots
;
  CASE kplot OF
    1: !p.multi=[0,1,1]
    2: !p.multi=[0,2,1]
    3: !p.multi=[0,3,1]
    4: !p.multi=[0,2,2]
    5: !p.multi=[0,3,2]
    6: !p.multi=[0,3,2]
    7: !p.multi=[0,4,2]
    8: !p.multi=[0,4,2]
    9: !p.multi=[0,3,3]
  ENDCASE
END

PRO plot_map, plon, plat, markers=markers
;
; Purpose: plot the lon-lat map for the required trajectories
;
  agrid=10.
  isz=size(plon.data)
  ipar=isz(1)
  itime=isz(2)

  IF (plon.min eq plon.undef) THEN BEGIN
    zlon_min=min(plon.data(where(plon.data ne plon.undef)))
    zlon_min=(long(zlon_min/agrid)-1.)*agrid
    plon.min=(zlon_min lt -180. ? zlon_min+360. : $
              zlon_min gt  180. ? zlon_min-360. : zlon_min)
  ENDIF
  IF (plon.max eq plon.undef) THEN BEGIN
    zlon_max=max(plon.data(where(plon.data ne plon.undef)))
    zlon_max=(long(zlon_max/agrid)+1.)*agrid
    plon.max=(zlon_max lt -180. ? zlon_max+360. : $
              zlon_max gt  180. ? zlon_max-360. : zlon_max)
  ENDIF
  IF (plat.min eq plat.undef) THEN BEGIN
    zlat_min=min(plat.data(where(plat.data ne plat.undef)))
    zlat_min=(long(zlat_min/agrid)-1.)*agrid
    plat.min=max([-90.,zlat_min])
  ENDIF
  IF (plat.max eq plat.undef) THEN BEGIN
    zlat_max=max(plat.data(where(plat.data ne plat.undef)))
    zlat_max=(long(zlat_max/agrid)+1.)*agrid
    plat.max=min([90.,zlat_max])
  ENDIF

  map_set, /grid, /continents, /cylindrical,$
           color=0, con_color=2, $
           limit=[plat.min,plon.min,plat.max,plon.max] , label=2, /advance
  FOR jpar=0,ipar-1 DO BEGIN
    ii=where(plon.data[jpar,*] ne plon.undef AND $
             plat.data[jpar,*] ne plat.undef)
    IF (n_elements(ii) gt 1) THEN BEGIN
       oplot, plon.data[jpar,ii], plat.data[jpar,ii],$
           color=3+(jpar mod 5)
       IF (N_Elements(markers) GT 0) THEN BEGIN
         IF (markers NE 0) THEN BEGIN
           iii=ii[Indgen(1+N_Elements(ii)/markers)*markers]
           oplot, plon.data[jpar,iii], plat.data[jpar,iii], $
              color=3+(jpar mod 5), psym=1
         ENDIF
       ENDIF
    ENDIF ELSE BEGIN
       IF (ii[0] gt -1) THEN BEGIN
          ii=[ii[0],ii[0]]
          oplot, plon.data[jpar,ii], plat.data[jpar,ii],$
           color=3+(jpar mod 5), psym=1  ; not convinced this is perfect
       ENDIF
    ENDELSE
  ENDFOR
  return
END

PRO plot_var, ptime, pvar
;
; Purpose: plot variable vs time plot
;
  isz=size(pvar.data)
  ipar=isz(1)
  itime=isz(2)
  IF (pvar.min ne pvar.undef and pvar.max ne pvar.undef) THEN BEGIN
    !y.style=1
  ENDIF ELSE BEGIN
    IF (pvar.min eq pvar.undef) THEN $
      pvar.min=min(pvar.data(where(pvar.data ne pvar.undef)))
    IF (pvar.max eq pvar.undef) THEN $
      pvar.max=max(pvar.data(where(pvar.data ne pvar.undef)))
    !y.style=0
  ENDELSE

  IF (strlowcase(pvar.long_name) eq 'pressure') THEN BEGIN
    plot,  [ptime.time(0),ptime.time(itime-1)], [pvar.max,pvar.min], /nodata, $
	   xtitle='Time ('+ptime.units+')', $
           ytitle=pvar.long_name+' ('+pvar.units+')', /ylog, $
           yrange=[pvar.max,pvar.min], color=0
  ENDIF ELSE BEGIN
    plot, [ptime.time(0),ptime.time(itime-1)], [pvar.min,pvar.max], /nodata, $
	  xtitle='Time ('+ptime.units+')', $
          ytitle=pvar.long_name+' ('+pvar.units+')',$
          yrange=[pvar.max,pvar.min], color=0 
  ENDELSE
  FOR jpar=0,ipar-1 DO oplot, ptime.time, pvar.data(jpar,*), $
                              min_value=pvar.undef+1, color=3+(jpar mod 5)
  !y.style=0
END

PRO make_plots, plot_types, cvar, avar, atime, file, markers=markers
;
; Purpose: plots the data
;          use this seperate routine cos may be called more than
;          once, depending on the output file(s) required
;

  FOR jplot=0,n_elements(plot_types)-1 DO BEGIN
    IF (plot_types[jplot] eq 'map') THEN BEGIN
      ilon=where(cvar eq 'lon')
      ilat=where(cvar eq 'lat')
      plot_map, avar[ilon], avar[ilat], markers=markers
    ENDIF ELSE BEGIN
      ivar=where(cvar eq plot_types[jplot])
      plot_var, atime, avar[ivar]
    ENDELSE
  ENDFOR
  xyouts, 0.05,0.95, 'Produced at the BADC '+file, /norm

END

PRO define_colors, lcols, no_color
;
; Purpose: define a simple color table
;          and set line colors
;          color table: white, black, grey,
;                       magenta, red,
;                       green, cyan, blue
;
  IF (n_elements(no_color) lt 1) THEN BEGIN
    r=[0,1,0.5,1,1,0,0,0]*255
    g=[0,1,0.5,0,0,1,1,0]*255
    b=[0,1,0.5,1,0,0,1,1]*255
  ENDIF ELSE BEGIN
    r=[0,1,0,0,0,0,0,0]*255  ; messy must be a better way
    g=[0,1,0,0,0,0,0,0]*255
    b=[0,1,0,0,0,0,0,0]*255
  ENDELSE
  tvlct,r,g,b
  !p.color=0        
  !p.background=1   ; white
  iline=n_elements(lcols)
  lcols=indgen(iline) mod 5
  lcols=lcols+3
  return
END

PRO plot_traj, file, plot_types, trajs, var_lims=plims, $
               output=output, psfile=yps, cgmfile=ycgm, $
               no_color=no_color, markers=markers, $
               test=test
;
; Purpose: main routine dealing with the plotting of trajectories
; Arguments:
;      file:   trajectory file name
;      plot_types: array of the plots wanted ('map' for lon-lat map, 'var' for var vs time) 
;      trajs: array of trajectory indices.  These are the trajectories read from file and plotted 
;      var_lims: structure defining the variable limits
;      output: output type (ps, x, gifstdo, pngstdo)
;      psfile: postscript filename (used if output='ps')
;      no_color: set for black and white plot
;      markers: frequency (in time steps) of markers, if set
;

IF (KeyWord_Set(test)) THEN BEGIN

  define_colors, 3, no_color


  plot, findgen(10), findgen(10), title='Test plot'
ENDIF ELSE BEGIN
user=getenv("USER")
message, 'user: '+user, /continue, /info
message, "plot_traj: "+output, /continue, /info
; 1. Read data
  cvar=which_vars(plot_types)             ; determine names of variables
  ncdf_control, 0, /verbose
  message, 'open netcdf file', /continue, /info
  ncid=ncdf_open(file)
  message, 'netcdf file opened', /continue, /info
  atime=get_time(ncid)
  avar=get_var(ncid,cvar,trajs)           ; read the required variables
  ncdf_close, ncid
  IF (n_elements(plims) gt 0) THEN $
    process_limits, avar, plims           ; set the limits
  press2hPa, avar                         ; convert pressure to hPa if needed
  lon_range, avar                         ; bring longs into range
  message,  "plot_traj: init ", /continue, /info
; 3. Do the plotting (X or gif)
  !p.thick=1.5
  !y.omargin=[0,1]
  set_multi, n_elements(plot_types)    ; set the number of plots

   message,  "plot_traj: output ", /continue, /info
  IF (N_Elements(output) GT 0) THEN BEGIN
    IF (output EQ 'pngstdo' or output EQ 'gifstdo') THEN set_plot, 'z' ELSE set_plot, output
    IF (output EQ 'ps') THEN BEGIN
       IF (N_Elements(yps) GT 0) THEN yfile=yps ELSE yfile='idl.ps'
       device, file=yfile, /color, /helvetica
    ENDIF
    IF (output EQ 'cgm') THEN BEGIN
       IF (N_Elements(ycgm) GT 0) THEN yfile=ycgm ELSE yfile='cgm.ps'
       device, file=yfile
    ENDIF
  ENDIF ELSE BEGIN
    set_plot, 'x'
    device, pseudo_color=8 ; force pseudo color display
  ENDELSE
  message,  "plot_traj: plotting ", /continue, /info
  ipar=(size(avar[0].data))[1]
  lcols=lonarr(ipar)
  define_colors, lcols, no_color
  make_plots, plot_types, cvar, avar, atime, file, markers=markers
  message,  "plot_traj: plotted ", /continue, /info
  IF (N_Elements(output) GT 0) THEN BEGIN
    IF (output EQ 'gifstdo') THEN  gif_off
    IF (output EQ 'pngstdo') THEN graphics_off, /loadct
    IF (output EQ 'ps' OR output EQ 'cgm') THEN BEGIN
        device, /close_file
    ENDIF
  ENDIF 
;  set_plot, 'x' ; reset to 'x'
ENDELSE
  message,  "plot_traj: completed ", /continue, /info

END

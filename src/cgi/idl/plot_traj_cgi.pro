PRO plot_traj_cgi
;print, 'hello in idl'
!QUIET=0
; 1. get the parameters from the environment
catch, ierr
IF (ierr ne 0) THEN BEGIN
  print, 'Error : ', !error_state.msg
  exit, status=1
ENDIF

get_params, names, values, nparams
convert_values, values
file=getenv('PATH_INFO')
;
; 2. Extract the variables expected
types=find_var('types',names,values)
trajs=find_var('trajs',names,values,/long)
vars=find_var('vars',names,values)
ps=find_var('ps',names,values)
cgm=find_var('cgm',names,values)
markers=find_var('markers',names,values,/long)

IF (N_Elements(markers) GT 0) THEN markers=markers[0]
var_lims=make_limits(vars,names,values)

message, "File: "+file, /continue, /Info ; print to stderr
lps=(ps[0] eq 'yes')
lcgm=(cgm[0] eq 'yes')
IF (lps) THEN BEGIN
  output='ps'
  psfile='/tmp/f'+string(1000*(1+randomu(seedundef)),format='(f7.2)')+'.ps'
  message, "psfile: "+psfile, /continue, /info
ENDIF ELSE BEGIN
  IF (lcgm) THEN BEGIN
    output='cgm'
    cgmfile='/tmp/f'+string(1000*(1+randomu(seedundef)),format='(f7.2)')+'.cgm'
    message, "cgmfile: "+cgmfile, /continue, /info
  ENDIF ELSE BEGIN
    output='pngstdo'
    print, 'Content-Type: image/png'
    print, ''
  ENDELSE
ENDELSE

;
; Plot
;message, "calling plot_traj: "+markers, /continue, /info
plot_traj, file, var_lims=var_lims,$
     types, trajs, output=output, ps=psfile, cgm=cgmfile,$
     markers=markers ; could use _extras for markers passing?

;
; Deal with ps file (slightly messy)
IF (lps) THEN BEGIN
  line=''
  openr, unit, psfile, /get_lun
  print, 'Content-Type: application/postscript'
  print, ''
  WHILE (not eof(unit)) DO BEGIN
    readf, unit, line
    print, line
  ENDWHILE
  spawn, 'rm '+psfile
ENDIF

IF (lcgm) THEN BEGIN
  line=''
  openr, unit, cgmfile, /get_lun
  print, 'Content-Type: application/cgm'
  print, ''
  WHILE (not eof(unit)) DO BEGIN
    readf, unit, line
    print, line
  ENDWHILE
;  spawn, 'rm '+cgmfile
ENDIF

END

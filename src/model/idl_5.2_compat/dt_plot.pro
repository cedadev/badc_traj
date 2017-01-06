; $Id: dt_plot.pro 888 2005-08-17 10:52:08Z badc $
;
; Copyright (c) 1998, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.

Function DT_PLOT_FORMAT, axis, index, value
; Function to format date/time axis values.

                                ;Convert to Julian date/time value.
x = value / !DT_PLOT_STATE.mult + !DT_PLOT_STATE.trange[0]

secs = (x-long(x)+1.0d-8) * 86400d0 ;Round to nearest second, + a little fudge
                                ; ~ 1 millisecond, to prevent rounding errors.
x = long(x) + secs / 86400d0

val = strarr(6)
for i=0,5 do $                  ;We have to all, to properly suppress
  val[i] = string(x, format='(C(' + !DT_PLOT_STATE.format[i] + '))')

if !DT_PLOT_STATE.suppress then begin         ;Suppress identical fields
    for i=5,0,-1 do $
      if (i ge 0) and (!DT_PLOT_STATE.tlast[i] eq val[i]) then val[i] = '' $
    else goto, no_breaks_in_idl
    no_breaks_in_idl: for i=i,0,-1 do !DT_PLOT_STATE.tlast[i] = val[i]
endif

out = strarr(6)
for i=!DT_PLOT_STATE.start_level, !DT_PLOT_STATE.last_level do begin
    i1 = !DT_PLOT_STATE.order[i]
    out[i1] = val[i]
    if !DT_PLOT_STATE.wave then begin
        out[i1] = strtrim(out[i1] + !DT_PLOT_STATE.fmt_sep[i],2) + '!C'
    endif else begin
        if val[i] ne '' and $   ;Insert separator? 
          ((i ne !DT_PLOT_STATE.start_level) or $
           (!DT_PLOT_STATE.fmt_sep[i] eq ' ')) then $
          out[i1] = out[i1] + !DT_PLOT_STATE.fmt_sep[i]
    endelse
endfor

result = out[5]+out[4]+out[3]+out[2]+out[1]+out[0]

;Search for a place to break string (!C)?
if (!DT_PLOT_STATE.wave eq 0) and (strlen(result) ge 5) then begin 
    blanks = where(byte(result) eq 32b, count)
    if count gt 0 then begin    ;Find blank closest to middle
        dummy = min(abs(strlen(result)/2 - blanks),i)
        pos = blanks[i]
        result = strmid(result,0,pos) + '!C' + strmid(result, pos+1, 1000)
    endif
endif

; print, "'",result, "'"
return, result
end


Pro DT_OPLOT, p1, p2, _EXTRA=e
; Overplot onto a previously established DT_PLOT.
; Parameters are identical to OPLOT.
;

t = !DT_PLOT_STATE.ytime ? p2 : p1 ;Get the time variable
tmp = size(t)
struct = tmp[tmp[0]+1] eq 8    ;True if a structure
n = n_elements(t)

if struct then begin            ;Passed date/time structures?  Extract Julian.
    for i=0,n-1 do if t[i].recalc then $ ;Recalculate?
      t[i] = VAR_TO_DT(t[i].year, t[i].month, t[i].day, $
                       t[i].hour, t[i].minute, t[i].second)
    t = t.julian
endif                           ;Struct

; Scale Julian to plot scale:
t = (t - !DT_PLOT_STATE.trange[0]) * !DT_PLOT_STATE.mult 

if !DT_PLOT_STATE.ytime then oplot, p1, t, _EXTRA=e $ ;Oplot it....
else oplot, t, p2, _EXTRA=e
end

Pro DT_PLOT, p1, p2, $
      START_LEVEL=start_level, $ ;1st level to label, 0=secs, 1=minutes, 2=hr,
      $                         ; 3=day, 4=month, 5=year 
      NUM_LEVELS=num_levels, $  ;# of levels to label (default is usually 3)
      MAX_LEVELS=max_levels, $    ;Highest level to label
      NUMERIC_MONTH=numeric_month, $ ;If set, label months numerically
      TWO_DIGIT_YEAR=two_digit_year,$ ;If set, write years as two digit numbers
      MDY=mdy, YMD=ymd, DMY=dmy, $ ;If set write dates in designated order
      SUPPRESS = suppress, $    ;Suppress redundant labels
      DATE_SEPARATOR=date_sep, $ ;Date separator for numeric dates, default='/'.
      TIME_SEPARATOR = time_sep, $ ;Time separator, default=':'
      YTIME=ytime, $          ;If set, label Y axis with time (X = default)
      XMARGIN=xmargin, YMARGIN=ymargin, $ ;Standard margin keywords
      WAVE_FORMAT=wave_format, $
      XRANGE=xrange, YRANGE=yrange, $ ;Explicit ranges for axes.
      $                         ;The following are unsupported Wave keywords
      COMPRESS=compress, EXCLUDE_HOLIDAY=exclude_holiday, $
      MONTH_ABBREV=month_abbrev, WEEK_BOUNDARY=week_boundary, $
      BOX=box, $
      _EXTRA=extra

;+
; NAME:
;	DT_PLOT
;
; PURPOSE:
;	Simplified interface for plotting with date/time axes.  Also,
;	use DT_OPLOT, for overplotting.
;
; CATEGORY:
;	Plotting.
;
; CALLING SEQUENCE:
;	DT_PLOT, P1, P2
; INPUTS:
;	P1 = variable for abcissa. Normally this is the time axis,
;	 unless the YTIME keyword is set.  The variable containing the
;	 time values may be either an array of julian date/times, or an
;	 array of date/time structures, see {IDLDT}.
;	P2 = variable for the ordinate.  If YTIME is set, time is
;	 plotted on the ordinate, and P1 is plotted on the abcissa.
;;	
; KEYWORD PARAMETERS:
;	Note: keywords accepted by the PLOT procedure are also
;	accepted and passed along to PLOT by DT_PLOT.
;
;	DATE_SEPARATOR: a string containing the separator for the date
;	 units.  The default value is loaded from the system variable
;	 !DATE_SEPARATOR, which is defined by {IDLDT}, and is usually '/'.
;	DMY: if set, label dates in the order of day, month, year.
;	MDY: if set, label dates in the order of month, day, year (the
;	 default).
;	NUMERIC_MONTH: if set, label months numerically, otherwise use
;	 month names.
;	NUM_LEVELS: number of levels of time units to label.  Default
;	 is three.  See also START_LEVEL.
;	START_LEVEL: least significant time unit to label: 0=secs,
;	 1=minutes, 2=hr, 3=day, 4=month, 5=year.  Default is
;	 determined automatically from the time range.  See also
;	 NUM_LEVELS. 
;	SUPPRESS: if set, suppress label elements that are identical to
;	 the previous tick mark's time/date label.
;	TIME_SEPARATOR: a string containing the separator for the time
;	 units.  The default value is loaded from the system variable
;	 !TIME_SEPARATOR, which is defined by {IDLDT}, and is usually ':'.
;	TWO_DIGIT_YEAR: if set, label years using only two digits.
;	 Default is to write years with 4 digits.
;	WAVE_FORMAT: If set, date/time labels are written vertically
;	 with one date/time unit per line.  Otherwise combine multiple
;	 units into one or more lines. Default = set.
;	XMARGIN: explicit margins for the X axis.  Same meaning as in PLOT.
;	XRANGE: explicit X axis range, with the same meaning as in PLOT.
;	YMARGIN: explicit margins for the Y axis.  Same meaning as in PLOT.
;	YMD: if set, label dates in the order of year, month, day.
;	YRANGE: explicit Y axis range, with the same meaning as in PLOT.
;	YTIME: if set, the second parameter is in time units which are
;	 plotted on the ordinate, otherwise time units are plotted
;	 along the abcissa. 
;
; OUTPUTS:
;	A plot with one of the axes labelled with date/times.
;
; RESTRICTIONS:
;	To preserve accuracy, the time axis scaling established by
;	DT_PLOT contains both an offset and a factor.  To convert plot data
;	values to date/time values:
;	    DateTimeValue = PlotDataValue / !DT_PLOT_STATE.mult +
;		!DT_PLOT_STATE.trange[0]
; 	To convert date/time values to plot data values:
;	    PlotDataValue = (DateTimeValue - !DT_PLOT_STATE.trange[0])
;                           * !DT_PLOT_STATE.mult
;
;	For example, to place the string "Now" at the current time
;	position on a plot produced by DT_PLOT:
;	    XYOUTS, (SYSTIME(/JULIAN)-!DT_PLOT_STATE.trange[0]) *
; 		!DT_PLOT_STATE.mult, 0, "Now"
;	For example, to read the cursor position and convert to a
;	julian date/time:
;	    CURSOR, X, Y, /DATA
;	    Julian = X / !DT_PLOT_STATE.mult + !DT_PLOT_STATE.trange[0]
;
;	This convenience routine is intended to provide a simplified
;	interface to date/time formats and tick formatting.
;	For complete control of date/time formatting and tick
;	placement, you must write your own tick formatting routine,
;	similar to DT_PLOT_FORMAT, or LABEL_DATE.
;
;	This routine is not recommended for sub-second intervals.
;
;	Use DT_OPLOT to overplot on DT_PLOT axes.  Same call and
;	parameters as OPLOT.
;
; PROCEDURE:
;	Straightforward.
;
; EXAMPLE:
;	Plot an array versus the current 7 days:
;	  Now = FLOOR(SYSTIME(/JULIAN)+0.5)   ;Julian days start at noon.
;	  DT_PLOT, Now + INDGEN(7), INDGEN(7)
;	Plot the same data, labelling hours, days, and months:
;	  DT_PLOT, Now + INDGEN(7), INDGEN(7), START_LEVEL=2, NUM_LEVELS=3
;	Plot the same data, labelling hours, minutes and days:
;	  DT_PLOT, Now + INDGEN(7), INDGEN(7), START_LEVEL=1
;	Plot the same data, labelling hours, minutes and days and months:
;	  DT_PLOT, Now + INDGEN(7), INDGEN(7), START_LEVEL=1, NUM_LEVELS=4
;  To overplot, use DT_OPLOT, contained in this module:
;	  DT_OPLOT, Now + INDGEN(7), INDGEN(7), PSYM=4
;
; MODIFICATION HISTORY:
; 	Written by:	Your name here, Date.
;	DMS		September, 1998
;-


if keyword_set(compress) or $   ;Unsupported keywords
  keyword_set(exclude_holiday) or $
  keyword_set(month_abbrev) or $
  keyword_set(box) or $
  keyword_set(week_boundary) then message, /INFO, $
  'COMPRESS, EXCLUDE_HOLIDAY, MONTH_ABBREV, WEEK_BOUNDARY Keywords not implemented'

dummy = {IDLDT}                 ;Define date/time system variables

if keyword_set(ytime) then begin
    t = p2
    if keyword_set(yrange) then dt_range = yrange
endif else begin
    t = p1                      ;Copy time axis
    if keyword_set(xrange) then dt_range = xrange
endelse

tmp = size(t)
struct = tmp[tmp[0]+1] eq 8    ;True if a structure
n = n_elements(t)

if struct then begin            ;Passed date/time structures?  Extract Julian.
    for i=0,n-1 do if t[i].recalc then $ ;Recalculate?
      t[i] = VAR_TO_DT(t[i].year, t[i].month, t[i].day, $
                       t[i].hour, t[i].minute, t[i].second)
    t = t.julian
endif                           ;Struct

if n_elements(dt_range) ne 2 then $ ;Obtain time axis range
  dt_range = double([min(t, MAX=tmax), tmax]) $
else dt_range = double(dt_range)

;         0         1         2        3      4       5
;         secs       Mins     Hrs      Days   Months  Years
;         10 mins    6 hrs,   2dys
levels = [10./1440., 6./24.,  2.0,     365,   12*365, 1.0e10] ;range to show
fmts =   ['CSI2.2',  'CMI2.2','CHI2.2','CDI0','CMoA', 'CYI' ]
seps =   ['',        ':',     ':',     ' ',   ' ',    ' ']
len =    [2,         3,       3,       3,     4,      4]
;

; Printing order.... Print from lowest order[] to highest
if keyword_set(dmy) or keyword_set(dny) then order = [5,4,3,0,1,2] $
else if keyword_set(ymd) or keyword_set(ynd) then order = [5,4,3,2,1,0] $
else order = [5,4,3,1,0,2]      ;Default = mdy or ndy

if keyword_set(numeric_month) then fmts[4] = 'CMoI2.2' ;Numeric month format?
if keyword_set(two_digit_year) then begin ;2 Digit year format
    fmts[5] = 'CYI2.2' 
    len[5] = 2
endif

date_sep1 = keyword_set(date_sep) ? date_sep : !date_separator
time_sep1 = keyword_set(time_sep) ? time_sep : !time_separator
if keyword_set(numeric_month) or $
  keyword_set(date_sep) then seps[[4,5]] = date_sep1 ;Date separator

seps[1] = time_sep1             ;Local time separator
seps[2] = seps[1]

if n_elements(start_level) eq 0 then slevel = -1 else slevel = start_level

if slevel lt 0 then begin       ;Determine start level...
    slevel = 0             ;Assume seconds are displayed
    tspan = dt_range[1] - dt_range[0] ;Delta time
    while tspan ge levels[slevel] and slevel lt 3 do $
      slevel = slevel + 1
    if n_elements(num_levels) eq 0 then begin
        num_levels = (6-slevel) < 3
        if slevel eq 2 then begin ;If doing hours, also do minutes
            slevel = 1
            num_levels = num_levels + 1
        endif
    endif
endif
    
slevel = slevel > 0 < 5
if n_elements(num_levels) eq 0 then begin
    num_levels = (6-slevel) < 3
endif

if n_elements(max_levels) ne 0 then num_levels = num_levels < max_levels

;;;; help,slevel, num_levels

; Round to the nearest second.  For sub-second intervals, this is not
; apprpriate. 

day = floor(dt_range)
frac_day = round((dt_range - day) * 86400) / 86400.d0 ;Round to seconds
carry = where(frac_day eq 1.0, count) ;Carry to next day?
if count ne 0 then begin
    day[carry] = day[carry] + 1L
    frac_day[carry] = 0.0
endif
                                ;Multiplier for days to try to make axes even.
mult = ([86400, 1440, 24, 1, 1,1])[slevel]
                                ;Cvt time scale to units of start_level
frac_day = [floor(frac_day[0] * mult), ceil(frac_day[1] * mult)] / double(mult)
dt_range = day + frac_day
t0 = dt_range[0]
; print, day, frac_day

wave = n_elements(wave_format) eq 1 ? fix(wave_format) : 1 ;Default = set....

defsysv, '!DT_PLOT_STATE', { PDT_STRUCT, $
                   trange: dt_range, $
                   mult: double(mult), $
                   format : fmts, $
                   fmt_sep: seps, $
                   order: order, $
                   wave: wave, $
                   suppress : keyword_set(suppress), $
                   start_level: slevel, $
                   last_level : (slevel+num_levels-1) < 5, $
                   tlast : strarr(6), $
                   ytime: keyword_set(ytime)}                             

xx_margin = arg_present(xmargin) ? xmargin : !x.margin ;Obtain the margins
yy_margin = arg_present(ymargin) ? ymargin : !y.margin
;; print,'Dt_range: ',dt_range-t0

t = (t-t0) * mult
dt_range = (dt_range - t0) * mult ;Scale to proper units

if keyword_set(ytime) then begin ;Dates along the Y axis??
;     xx_margin[0] = xx_margin[0] > maxlen ;Room along left X axis
    if n_elements(xrange) eq 0 then xrange = [min(p2, MAX=tmp, /NAN), tmp]
    plot, p1, t, ytickformat='DT_PLOT_FORMAT', $
      XMARGIN=xx_margin, YRANGE=dt_range, $
      XRANGE=xrange, _EXTRA=extra

endif else begin                ;Dates along the X axis
;    xx_margin[1] = yy_margin[1] > (maxlen+1)/2 ;Room along right Y axis
    if !DT_PLOT_STATE.wave then $         ;Increase bottom margin?
      yy_margin[0] = (!DT_PLOT_STATE.last_level - slevel + 2) > yy_margin[0]
    if n_elements(yrange) eq 0 then yrange = [min(p2, MAX=tmp, /NAN), tmp]
    plot, t, p2, xtickformat='DT_PLOT_FORMAT', $
      XMARGIN=xx_margin, YMARGIN=yy_margin, XRANGE=dt_range, $
      YRANGE=yrange, _EXTRA=extra
endelse

end

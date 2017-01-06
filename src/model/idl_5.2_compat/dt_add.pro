; $Id: dt_add.pro 888 2005-08-17 10:52:08Z badc $
;
; Copyright (c) 1997-1998, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.

;+
; NAME:
;   DT_ADD
;
;
; PURPOSE:
;   Increments an array of IDLDT date/time variables by a constant amount.
;
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;   result = DT_ADD( dt_var )
;
; 
; INPUTS:
;   dt_var: An array of IDLDT date/time structures
;
;
; OPTIONAL INPUTS:
;
;
;	
; KEYWORD PARAMETERS:
;    Day: The offset in days
;    Hour: The offset in hours
;    Minute: The offset in minutes
;    Second: The offset in seconds
;    Year: The offset in years
;
;
; OUTPUTS:
;    The result is an array of IDLDT date/time structures, incremented
;    by the desired amount
;
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	Year, Month, Days must be integers for correct operation.
;	DT_ADD and DT_SUBTRACT are not necessarily commutative when
;	the YEAR or MONTH keyword are used because these time units do
;	not have a constant number of days.  For example, ((Jan 31 + 1
;	Month) - 1 Month) is NOT Jan 31.  One month
;	after January 31st is March 3rd, unless its a leap year.
;	One month before March 3rd is February 3rd.
;
;
; PROCEDURE:
;    Each value is added to the corresponding component, then the
;    result is renormalized with JUL_TO_DT.
;
;
; EXAMPLE:
;    print, DT_ADD( TODAY(), day=1, year=3) ; Adds 1 to today's day
;                                           ; and 3 to today's year.
;
;
; MODIFICATION HISTORY:
;    Dec 1997, Dr. G. Scott Lett, RSI, Written
;    Jun 1998, DMS, Corrected.
;-
FUNCTION DT_ADD, dt_var, Day=day, Hour=hour, $
           Minute=minute, Month=month, Second=second, Year=year, $
           Round=iround

tyear = keyword_set(year) ? long(year) : 0
tmonth = keyword_set(month) ? long(month) : 0
tday = keyword_set(day) ? day : 0
thour = keyword_set(hour) ? hour : 0
tminute = keyword_set(minute) ? minute : 0
tsecond = keyword_set(second) ? second : 0

; Change in time, in days and fractions.
delta = tday + (thour / 24.0d0) + (tminute/1440.0d0) + (tsecond / 86400.0d0)

if abs(tmonth) gt 12 then begin ;Reduce month modulo 12
    tyear = tyear + long(tmonth)/12
    tmonth = tmonth mod 12
endif

retVal = dt_var
FOR i = 0L, n_elements(dt_var)-1 DO BEGIN
    
    ;; First do calculations based in variable length units,
    ;; e.g. those other than days & secs
    
    if dt_var[i].recalc then $
      dt_var[i] = VAR_TO_DT(dt_var[i].year, dt_var[i].month, dt_var[i].day, $
                            dt_var[i].hour, dt_var[i].minute, dt_var[i].second)

    if tyear ne 0 or tmonth ne 0 then begin
        Nyear = dt_var[i].year + tyear
        Nmonth = dt_var[i].month + tmonth
        if Nmonth lt 1 then begin
            Nmonth = Nmonth + 12
            Nyear = Nyear - 1
        endif else if Nmonth gt 12 then begin
            Nmonth = Nmonth - 12
            Nyear = Nyear + 1
        endif
; Because adding months and years can cause an invalid date, start at
; the first of the month.
        julian = julday(Nmonth, 1, Nyear, $ ;Always use 1st of month
                        dt_var[i].Hour, dt_var[i].Minute, dt_var[i].Second)
        julian = julian + (dt_var[i].day-1) ;Then add day....
    endif else julian = dt_var[i].julian
    
    julian = julian + delta

    if keyword_set(iround) then begin
        days = long(julian)
        julian = days + Round((julian - days) * 86400d0) / 86400d0
    endif

    retVal[i] = JUL_TO_DT(julian)
ENDFOR
RETURN, retVal
END

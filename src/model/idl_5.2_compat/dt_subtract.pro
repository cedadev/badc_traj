; $Id: dt_subtract.pro 888 2005-08-17 10:52:08Z badc $
;
; Copyright (c) 1997-1998, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.

;+
; NAME:
;   DT_SUBTRACT
;
;
; PURPOSE:
;   Decrements an array of IDLDT date/time variables by a constant amount.
;
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;   result = DT_SUBTRACT( dt_var )
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
;    Compress: Eliminate holidays
;    Day: The offset in days
;    Hour: The offset in hours
;    Minute: The offset in minutes
;    Second: The offset in seconds
;    Month: The offset in months.
;    Year: The offset in years
;
;
; OUTPUTS:
;    The result is an array of IDLDT date/time structures, decremented
;    by the desired amount
;
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	Year, Month, Days must be integers for correct operation.
;
;	DT_ADD and DT_SUBTRACT are not necessarily commutative when
;	the YEAR or MONTH keyword are used because these time units do
;	not have a constant number of days.  For example, ((Jan 31 + 1
;	Month) - 1 Month) is NOT Jan 31.  One month
;	after January 31st is March 3rd, unless its a leap year.
;	One month before March 3rd is February 3rd.
;
; PROCEDURE:
;    Each value is added to the corresponding component, then the
;    result is renormalized with JUL_TO_DT.
;
; PROCEDURE:
;    Each value is subtracted from the corresponding component, then the
;    result is renormalized with JUL_TO_DT.
;
;
; EXAMPLE:
;    print, DT_SUBTRACT( TODAY(), day=1, year=3) ; Takes 1 from today's day
;                                                ; and 3 from today's year.
;
;
; MODIFICATION HISTORY:
;    Dec 1997, Dr. G. Scott Lett, RSI, Written
;    Jun 1998, DMS, Corrected.
;-
FUNCTION DT_SUBTRACT, dt_var, Compress=compress, Day=day, Hour=hour, $
           Minute=minute, Month=month, Second=second, Year=year

tyear = keyword_set(year) ? year : 0
tmonth = keyword_set(month) ? month : 0
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

; This must be reverse process of dt_add.  Otherwise, we get errors
; involving leap-years and February.

    if delta ne 0 then begin
        tmp = JUL_TO_DT(dt_var[i].julian - delta)
    endif else tmp = dt_var[i]

    if tyear ne 0 or tmonth ne 0 then begin
        Nyear = tmp.year - tyear
        Nmonth = tmp.month - tmonth
        if Nmonth lt 1 then begin
            Nmonth = Nmonth + 12
            Nyear = Nyear - 1
        endif else if Nmonth gt 12 then begin
            Nmonth = Nmonth - 12
            Nyear = Nyear + 1
        endif
        tmp = VAR_TO_DT(Nyear, Nmonth, tmp.day, $
                        Tmp.hour, Tmp.minute, Tmp.second)
    endif
    retVal[i] = tmp
ENDFOR
RETURN, retVal
END

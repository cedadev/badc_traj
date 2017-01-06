; $Id: var_to_dt.pro 888 2005-08-17 10:52:08Z badc $
;
; Copyright (c) 1997-1998, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.

;+
; NAME:
;      VAR_TO_DT
;
;
; PURPOSE:
;      Convert values representing date and time to an IDLDT date/time
;      variable.
;
;
; CATEGORY:
;      Misc
;
;
; CALLING SEQUENCE:
;      dtvar = VAR_TO_DT( yyyy, mm, dd, hh, min, ss)
;
; 
; INPUTS:
;
; OPTIONAL INPUTS:
;      yyyy: A scalar or array containing integer year(s).
;      mm:   A scalar or array containing integer month(s)
;      dd:   A scalar or array containing integer day(s) of the month.
;      hh:   A scalar or array containing integer hour(s) of the day.
;      min:  A scalar or array containing integer minute(s).
;      ss:   A scalar or array containing the float seconds.
;
;	
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;      VAR_TO_DT returns the IDLDT date/time structure containing the
;      converted date(s).
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;      NONE
;
;
; SIDE EFFECTS:
;      The result is a named IDLDT date/time structure.  If the
;      structure named IDLDT has not been defined, IDL invokes the
;      IDLDT__DEFINE procedure to create it.  IDLDT__DEFINE creates
;      a number of system variables.
;
;
;
; RESTRICTIONS:
;      If any of the inputs are arrays, all of the inputs, if present,
;      must be arrays of the same dimension.
;
;
;
; PROCEDURE:
;
;
; EXAMPLES:
;      date = VAR_TO_DT( 1997, 12, 21 ) ; Create an IDLDT with default
;                                       ; Values for hh, mm, ss.
;      PRINT, date
;
; The result is
;      { 1997 12 21 0 0.00000 0.0000000 0 }
;
;
;      date = VAR_TO_DT( [1997, 1998], [12, 1], [1, 1] ) 
;
; MODIFICATION HISTORY:
;	DMS, June, 1998, Cleaned-up logic.
;
;-

FUNCTION VAR_TO_DT, yyyy, mm, dd, hh, min, ss

nElements = n_elements( yyyy )

if nElements eq 0 then begin
    dummy = {IDLDT}             ;Define date structs
    retVal = !dt_base
endif else begin
    retVal = replicate({IDLDT}, nElements)
    for i=0L, nElements-1 do begin
        retVal[i] = { IDLDT, yyyy[i], $ ;Supply defaults if necessary
                      (n_elements(mm) gt i ? mm[i] : 1) < 12 > 1, $
                      (n_elements(dd) gt i ? dd[i] : 1) < 31 > 1, $
                      (n_elements(hh) gt i ? hh[i] : 0) < 23 > 0, $
                      (n_elements(min) gt i ? min[i] : 0) < 59 > 0, $
                      (n_elements(ss) gt i ? ss[i] : 0) < 60 > 0, $
                      0.d, 0b}
                                ;Obtain Julian date
        retVal[i].Julian = julday( retVal[i].month, retVal[i].day, $
                                   retVal[i].year, retVal[i].hour, $
                                   retVal[i].minute, retVal[i].second)
    endfor
endelse
return, retVal
end

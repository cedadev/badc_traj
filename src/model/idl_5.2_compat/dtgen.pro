; $Id: dtgen.pro 888 2005-08-17 10:52:08Z badc $
;
; Copyright (c) 1998, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;   DTGEN
;
;
; PURPOSE:
;   Generates an array of IDLDT date/time variables
;   incremented by the values input by the user.
;
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;   result = DTGEN( dt_var, number )
;
;
; INPUTS:
;   dt_var: A variable of IDLDT date/time structure
;	number: The number of IDLDT vars to create.
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;    Day: The increment in days
;    Hour: The increment in hours
;    Minute: The increment in minutes
;    Second: The increment in seconds
;    Year: The increment in years
;
;
; OUTPUTS:
;    The result is an array of IDLDT date/time structures, incremented
;    by the desired amount
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;    Each element in the array is added to the corresponding component, then the
;    result is renormalized with JUL_TO_DT.
;
;
; EXAMPLE:
;    myvar = DTGEN( TODAY(), 10, DAY =1) ;  Creates 10 dt's in a var
;                                        ; and Adds 1 to each succeeding
;                                        ; day.  Uses today() as the base
;                                        ; date.
;
;
; MODIFICATION HISTORY:
;    March 1998, RSI, Written
;-
FUNCTION DTGEN, dt_var, Number, Day=day, Hour=hour, $
                 Minute=minute, Month=month, Second=second, Year=year

if not(keyword_set( month)) THEN month = 0
if not(keyword_set( day)) THEN day = 0
if not(keyword_set( hour)) THEN hour = 0
if not(keyword_set( minute)) THEN minute = 0
if not(keyword_set( second)) THEN second = 0.0
if not(keyword_set( year)) THEN year = 0
nElements = n_elements( dt_var )

if nElements GT 1 THEN BEGIN
	Message,"Invalid IDLDT variable. Input should be a scalar IDLDT variable"
    RETURN, -1
endif

If (Number LT 1) THEN BEGIN
     Message,"Invalid number of IDLDT elements.  You must request 1 or more to create"
     RETURN, -1
ENDIF

; Create an array of the requested number of DT variables
retVal = replicate(dt_var, Number)

; Add the appropriate increment to each field.
FOR i = 0, (Number-1) DO BEGIN
   tmp = julday( (retVal[i]).month + (month*i), $
                   (retVal[i]).day + (day*i), $
                   (retVal[i]).year + (year*i), $
                   (retVal[i]).hour + (hour*i), $
                   (retVal[i]).minute + (minute*i), $
                   (retVal[i]).second + (second*i))
    retVal[i] = jul_to_dt( tmp)
ENDFOR

RETURN,retVal
END

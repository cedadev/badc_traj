; $Id: day_of_week.pro 888 2005-08-17 10:52:08Z badc $
;
; Copyright (c) 1997-1998, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;     DAY_OF_WEEK
;
;
; PURPOSE:
;     Returns an array of integers, containing the day of the week for
;     each element of the input IDLDT date/time variable.
;
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;     result = DAY_OF_WEEK( dt_days )
;
; 
; INPUTS:
;     dt_days: an array of type IDLDT (the IDL date/time structure)
;
;
; OPTIONAL INPUTS:
;
;
;	
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;     Returns an array of integers in the range 0-6, with 0 indicating
;     Sunday and 6 indicating Saturday.
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
;     See IDLDT__DEFINE.
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;     Uses JULDAY.
;
;
; EXAMPLE:
;     print, DAY_OF_WEEK( TODAY() )
;
;
; MODIFICATION HISTORY:
;
;       Dec 1997, Dr. G. Scott Lett, RSI, Written
;
;		
;
;-
FUNCTION DAY_OF_WEEK, dt_days

nElements = n_elements( dt_days )
IF nElements GT 0 THEN BEGIN
    retVal = INTARR( nElements )
    FOR i = 0, nElements-1 DO BEGIN
        tmp = dt_days[i]
        retVal[i] = ( JULDAY( tmp.month, tmp.day, tmp.year ) + 1) mod 7
    ENDFOR
ENDIF
RETURN, retVal
END

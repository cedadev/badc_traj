; $Id: day_of_year.pro 888 2005-08-17 10:52:08Z badc $
;
; Copyright (c) 1997-1998, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;     DAY_OF_YEAR
;
;
; PURPOSE:
;     Returns an array of integers, containing the day of the year for
;     each element of an array of IDLDT date/time variables.
;
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;     result = DAY_OF_YEAR( dt_days )
;
; 
; INPUTS:
;     dt_days: An array of type IDLDT (the IDL date/time structure)
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
;     Returns an array of integers in the range 1-365 (1-366 for leap years)
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
;
;
;
; EXAMPLE:
;     print, DAY_OF_YEAR( TODAY() )
;
;
; MODIFICATION HISTORY:
;
;       Dec 1998, Dr. G. Scott Lett, RSI, Written
;		
;
;-
FUNCTION DAY_OF_YEAR, dt_days

nElements = n_elements( dt_days )

retVal = intarr( nElements )

FOR i = 0, nElements-1 DO BEGIN
    tmp = dt_days[i]
    yearStart = JULDAY( 1, 1, tmp.year )
    now = JULDAY( tmp.month, tmp.day, tmp.year )
    retVal[i] = now - yearStart + 1
ENDFOR
RETURN, retVal
END

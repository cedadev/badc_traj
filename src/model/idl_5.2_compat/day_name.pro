; $Id: day_name.pro 888 2005-08-17 10:52:08Z badc $
;
; Copyright (c) 1997-1998, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;     DAY_NAME
;
;
; PURPOSE:
;     Returns a string array containing the names of the days of the
;     week represented by an IDLDT date/time variable.
;
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;     result = DAY_NAME( dt_days)
;
; 
; INPUTS:
;     dt_days: An array of IDLDT date/time structures.
;
;
; OPTIONAL INPUTS:
;     NONE
;
;	
; KEYWORD PARAMETERS:
;     NONE
;
;
; OUTPUTS:
;     Returns and array of names of days.  The names are controlled by
;     the system variable !Day_Names, which is created by IDLDT__DEFINE.
;
;
; OPTIONAL OUTPUTS:
;     NONE
;
;
; COMMON BLOCKS:
;     NONE
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
;     DAY_NAME uses the system variable !Day_Names, which is created
;     by IDLDT__DEFINE and can be user-modified.
;
;
; EXAMPLE:
;     print, DAY_NAME( TODAY() )
;
;
; MODIFICATION HISTORY:
;
;       Dec 1997, Dr. G. Scott Lett, RSI, Written
;
;		
;
;-
FUNCTION DAY_NAME, dt_days

nElements = n_elements( dt_days )

IF nElements GT 0 THEN BEGIN
    retVal = strarr( nElements )
    FOR i = 0, nElements-1 DO $
        retVal[i] = !Day_names[ DAY_OF_WEEK( dt_days[i] ) ]
ENDIF
RETURN, retVal
END

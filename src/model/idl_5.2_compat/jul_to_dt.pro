; $Id: jul_to_dt.pro 888 2005-08-17 10:52:08Z badc $
;
; Copyright (c) 1997-1998, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;     JUL_TO_DT
;
;
; PURPOSE:
;     Converts julian day to IDLDT date/time structure.
;
;
; CATEGORY:
;     Misc
;
;
; CALLING SEQUENCE:
;     result = JUL_TO_DT( julian )
;
; 
; INPUTS:
;     julian: A scalar or array of Julian days.
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
;     Returns an array of IDLDT date/time variables
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
;     See IDLDT__DEFINE
;
;
; RESTRICTIONS:
; 
;
;
; PROCEDURE:
;     JUL_TO_DT uses CALDAT and VAR_TO_DT.
;
;
; EXAMPLE:
;     print, JUL_TO_DT( JULDAY( 1, 1, 1982 ) )
;
;
; MODIFICATION HISTORY:
;     Dec 1997, Scott Lett, RSI, Written
;
;-

FUNCTION JUL_TO_DT, julian

CALDAT, julian, month, day, year, hour, minute, seconds
RETURN, VAR_TO_DT( year, month, day, hour, minute, seconds )

END

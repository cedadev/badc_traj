; $Id: dt_to_var.pro 888 2005-08-17 10:52:08Z badc $
;
; Copyright (c) 1997-1998, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;     DT_TO_VAR
;
;
; PURPOSE:
;     Converts IDLDT date/time variables into numerical data.
;
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;     DT_TO_VAR, dt_dates
;
; 
; INPUTS:
;     dt_dates:  A scalar or array of IDLDT date/time structures.
;
;
; OPTIONAL INPUTS:
;     NONE
;
;	
; KEYWORD PARAMETERS:
;     Year: Integer variable to contain years
;     Month: Byte variable to contain months.
;     Day: Byte variable to contain days of the month.
;     Hour: Byte veriable to contain hours of the day.
;     Minute: Byte variable to contain minutes.
;     Second: Float variable to contain seconds.
;
;
; OUTPUTS:
;     NONE
;
;
; OPTIONAL OUTPUTS:
;     See the keywords
;
;
; COMMON BLOCKS:
;     NONE
;
;
; SIDE EFFECTS:
;     None
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
;     DT_TO_VAR, TODAY(), Year=years, Day=days, Month=month
;
;
; MODIFICATION HISTORY:
;
;       Dec 1997, Dr. G. Scott Lett
;
;		
;
;-
PRO DT_TO_VAR, dt_days, YEAR=years, MONTH=months, DAY=days, HOUR=hours, $
               MINUTE=minutes, SECOND=seconds

IF n_elements( dt_days) GT 0 THEN BEGIN
    IF ARG_PRESENT( years ) THEN years = dt_days.year
    IF ARG_PRESENT( months ) THEN months = dt_days.month
    IF ARG_PRESENT( days ) THEN days = dt_days.day
    IF ARG_PRESENT( hours ) THEN hours = dt_days.hour
    IF ARG_PRESENT( minutes ) THEN minutes = dt_days.minute
    IF ARG_PRESENT( seconds ) THEN seconds = dt_days.second
ENDIF
END

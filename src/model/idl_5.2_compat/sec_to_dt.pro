;$Id: sec_to_dt.pro 888 2005-08-17 10:52:08Z badc $
;
; Copyright (c) 1997-1998, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
FUNCTION SEC_TO_DT, seconds, Base=base, Date_fmt=date_fmt
;+
; NAME:
;    SEC_TO_DT
;
;
; PURPOSE:
;    Convert time from seconds-since-base to an IDLDT structure
;
;
; CATEGORY:
;    MISC
;
;
; CALLING SEQUENCE:
;    result = SEC_TO_DT( seconds)
;
; 
; INPUTS:
;    seconds:  a floating point scalar, giving the number of seconds
;    elapsed from the base date.
;
;
; OPTIONAL INPUTS:
;
;
;	
; KEYWORD PARAMETERS:
;    BASE:  A keyword in string format (see STR_TO_DT), giving the
;    base date.  The default value is calculated from !DT_BASE.
;    (Please see IDLDT__DEFINE for a description of !DT_BASE)
;
;    Date_Fmt: An integer indicating the format of the BASE keyword.
;    (Please see STR_TO_DT for an explanation of Date_Fmt)
; OUTPUTS:
;    Returns an IDLDT structure with the date corresponding to
;    the base date plus the seconds parameter.
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
;    PRINT, SEC_TO_DT( 245004.0)
;
;
; MODIFICATION HISTORY:
;
;-

   tmp = {idldt}

   IF keyword_set( base ) THEN BEGIN
      ret_val = STR_TO_DT( base, Date_fmt=date_fmt )
   ENDIF ELSE BEGIN
      ret_val = !dt_base
   ENDELSE

   ret_val = DT_ADD( ret_val, second = seconds)

   RETURN, ret_val
END

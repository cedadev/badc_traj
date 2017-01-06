; $Id: today.pro 888 2005-08-17 10:52:08Z badc $
;
; Copyright (c) 1997-1998, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;        TODAY
;
;
; PURPOSE:
;        Returns an IDLDT date/time structure containing the current
;        system date and time.
;
;
; CATEGORY:
;        Misc
;
;
; CALLING SEQUENCE:
;        result = TODAY()
;
; 
; INPUTS:
;        NONE
;
;
; OPTIONAL INPUTS:
;        NONE
;
;	
; KEYWORD PARAMETERS:
;        NONE
;
;
; OUTPUTS:
;        returns an IDLDT date/time structure containing the current
;        system date and time.
;
;
; OPTIONAL OUTPUTS:
;        NONE
;
;
; COMMON BLOCKS:
;        NONE
;
;
; SIDE EFFECTS:
;        The named structure IDLDT is defined and the named common
;        block IDLDT_COMMON is created.
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;        Uses VAR_TO_DT and SYSTIME.
;
;
; EXAMPLE:
;        PRINT, TODAY()
; resulted in
;       {    1997  12   7  12  10      25.0000       2450790.0   0}
; on the day this was written.
;
;
; MODIFICATION HISTORY:
;
;       Dec 1997, Dr. G. Scott Lett, RSI, Written
;
;		
;
;-
FUNCTION TODAY

curBinTime = bin_date( systime() )
return, VAR_TO_DT( curBinTime[0], curBinTime[1], curBinTime[2], $
                   curBinTime[3], curBinTime[4], curBinTime[5] )
END

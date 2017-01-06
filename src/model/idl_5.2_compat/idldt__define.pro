; $Id: idldt__define.pro 888 2005-08-17 10:52:08Z badc $
;
; Copyright (c) 1997-1998, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:  
;             IDLDT__DEFINE
;
;
; PURPOSE:
;           Automatic definition procedure for the IDLDT date/time structure
;  
;
; COMMON BLOCKS:
;
;
; SIDE EFFECTS:
;           Creates the named structure, IDLDT, and a number of system
;           variables.
;
; MODIFICATION HISTORY:
;
;      Dec 1997, Dr. G. Scott Lett, RSI, Written
;		
;
;-
PRO IDLDT__DEFINE


tmp = {IDLDT, Year:0, Month:0B, Day:0B, $
       Hour:0B, Minute:0B, Second:0., $
       Julian:0.D, $
       Recalc:0B $
       }

; Initialize Date/Time system variables

if n_elements( idldt_date_base ) eq 0 then begin
   DEFSYSV, '!dt_base', {IDLDT, -4713, 1, 1, 12, 0, 0, 0.d, 0}
   DEFSYSV, '!date_separator', '/'
   DEFSYSV, '!time_separator', ':'
   DEFSYSV, '!holidays', replicate( {IDLDT}, 50 )
   DEFSYSV, '!weekend_list', replicate( 0B, 7 )
   DEFSYSV, '!day_names', ['Sunday', 'Monday', 'Tuesday', $ 
                      'Wednesday', 'Thursday', 'Friday', 'Saturday' ]
   DEFSYSV, '!month_names',['January','February','March','April','May', $
                            'June', 'July', 'August', 'September', $
                            'October', 'November', 'December' ]
endif

END

; $Id: str_to_dt.pro 888 2005-08-17 10:52:08Z badc $
;
; Copyright (c) 1997-1998, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.

;+
; NAME:
;    STR_TO_DT
;
;
; PURPOSE:
;    Converts date and time string arrays to IDLDT date/time structures
;
;
; CATEGORY:
;    Misc
;
;
; CALLING SEQUENCE:
;     result = STR_TO_DT( date_strings[, time_strings]) 
;
; 
; INPUTS:
;     date_strings: string or array of strings containing dates
;
; OPTIONAL INPUTS:
;     time_strings: string or array of strings containing times (if
;                   it exists, it must be of the same dimension as 
;                   date_strings)
;
;	
; KEYWORD PARAMETERS:
;     date_fmt: Integer date format specifier
;     time_fmt: Integer time format specifier
;
;
; OUTPUTS:
;     returns an array of IDLDT date/time structure
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
;     Invokes IDLDT__DEFINE, if the IDLDT structure hasn't already
;     been defined.
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
;     print, STR_TO_DT( '05/01/92' )
;
;
; MODIFICATION HISTORY:
;     Dec 1997, Dr. G. Scott Lett, RSI, Written
;-
FUNCTION STR_TO_DT, date_strings, time_strings, date_fmt=date_fmt, $
                    time_fmt=time_fmt

nElements = n_elements( date_strings )
retVal = {IDLDT}

separators = ['-', '/', ',', ':', '.'] ;Possible separators
date_sep = '/'                  ;Common default values
time_sep = ':'
date_fmtt = keyword_set(date_fmt) ? date_fmt : 1
time_fmtt = keyword_set(time_fmt) ? time_fmt : -1

if nElements GT 0 THEN BEGIN
    retVal = replicate( {IDLDT}, nElements )

    FOR i = 0, nElements-1 DO BEGIN
        temp = date_strings[i]
        if strpos(temp, date_sep) lt 0 then begin ;Find new separator?
            date_sep = ''
            for j=0, n_elements(separators)-1 do $
              if strpos(temp, separators[j]) ge 0 then date_sep = separators[j]
            if date_sep eq '' then $
              message, "Date string contains no legitimate separators. ->" + $
              temp
        endif
        s = str_sep( date_strings[i], date_sep)

        CASE date_fmtt OF
            1: BEGIN
                month = fix(s[0])
                day = fix(s[1])
                year = fix(s[2])
            END
            2: BEGIN
                month = fix(s[1])
                day = fix(s[0])
                year = fix(s[2])
            END
            3: BEGIN
                day = fix(s[0])
                year = fix(s[1])
                j = julday( 1, day, year )
                caldat, j, month, day, year
            END
            4: BEGIN
                day = fix(s[0])
                year = fix(s[2])
                if n_elements(upcaseNames) eq 0 then $ ;Ignore case in match
                  upcaseNames = strupcase(!month_names)
                month = 1+(where(strpos(upcaseNames, strupcase(s[1])) eq 0))[0]
            END
            5: BEGIN
                year = fix(s[0])
                month = fix(s[1])
                day = fix(s[2])
            END
        ENDCASE

        if year lt 100 then year = year + 1900 ;Not Y2K compatible!!!

        minute = 0
        hour = 0
        second = 0.0d

        if n_elements( time_strings) GT 0 THEN BEGIN
            temp = time_strings[i]
            CASE time_fmtt OF
                -1: BEGIN
                    if strpos(temp, time_sep) lt 0 then begin ;new separator?
                        time_sep = ''
                        for j=0, n_elements(separators)-1 do $
                          if strpos(temp, separators[j]) ge 0 then $
                          time_sep = separators[j]
                        if time_sep eq '' then $
                          message, "Time string contains no legitimate separators. ->" + $
                          temp
                    endif       ;New separator
                    s = str_sep(temp, time_sep)
                    hour = fix(s[0])
                    minute = fix(s[1])
                    second = double(s[2])
                END
                -2: BEGIN
                    it = fix(temp)
                    minute = it mod 100
                    hour = it / 100
                END
            ENDCASE
        ENDIF                   ;Time present

        retval[i] = var_to_dt( year, month, day, hour, minute, second)
    ENDFOR
ENDIF
RETURN, retVal
END

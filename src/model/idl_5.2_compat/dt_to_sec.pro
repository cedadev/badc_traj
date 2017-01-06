; $Id: dt_to_sec.pro 888 2005-08-17 10:52:08Z badc $
;
; Copyright (c) 1998, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;     DT_TO_SEC
;
; PURPOSE:
;     Converts IDLDT date/time variables into seconds-since-base value.
;
; CATEGORY:
;     MISC
;
; CALLING SEQUENCE:
;     Result = DT_TO_SEC( dt_dates )
;
; INPUTS:
;     dt_dates:  A scalar or array of IDLDT date/time structures.
;
; KEYWORD PARAMETERS:
;     BASE - Set this keyword to a string containing the base date, such as
;         '3-27-92', from which the number of elapsed seconds is to be
;         calculated.  By default, the value in !DT_BASE is used.
;
;     DATE_FMT - Set this keyword to one of the following values to select
;         the format of the date string provided via the BASE keyword:
;
;         Value   Format                   Example
;         -------------------------------------------
;           1     MM*DD*[YY]YY             05/01/92         
;           2     DD*MM*[YY]YY             01/05/92
;           3     ddd*[YY]YY               122/1992
;           4     DD*mmm[mmmmmm]*[YY]YY    01/May/92
;           5     [YY]YY*MM*DD             1992/05/01
;
;          where the asterisk, '*', may be replaced by any of the following
;          separators: - / , . : (dash, slash, comma, period, or colon).
;
; OUTPUTS:
;     A scalar or array of floating point values (one for each input
;     date/time structure) representing the number of seconds elapsed
;     since the base date.
;
; COMMON BLOCKS:
;     NONE
;
; SIDE EFFECTS:
;     None
;
; EXAMPLE:
;     PRINT, DT_TO_SEC(TODAY())
;
; MODIFICATION HISTORY:
;       Aug 1998, DLD: Initial version.
;
;-
FUNCTION DT_TO_SEC, dt_days, Base=base, Date_fmt=date_fmt

    nDates = N_ELEMENTS( dt_days )
    IF nDates GT 0 THEN BEGIN

        ; Convert the base date from string to a date/time structure.         
        IF KEYWORD_SET(base) THEN $
            baseDate = STR_TO_DT( base, DATE_FMT=date_fmt ) $
        ELSE $
            baseDate = !DT_BASE

        seconds = FLTARR(nDates)

        FOR i=0,nDates-1 DO BEGIN
            ; Compute difference (in days) for day, month, and year only.
            dayDelta = JULDAY(dt_days[i].month, dt_days[i].day, $
                              dt_days[i].year) - $
                       JULDAY(baseDate.month, baseDate.day, baseDate.year)
 
            ; Compute difference (in seconds) for hour, minute and second.
            secDelta = (dt_days[i].hour * 3600.0d0 + $
                        dt_days[i].minute * 60.0d0 + $ 
                        dt_days[i].second) -         $ 
                       (baseDate.hour * 3600.0d0 +   $
                        baseDate.minute * 60.0d0 +   $
                        baseDate.second)

            ; If difference for minutes/seconds is negative, borrow from
            ; a day.
            IF (secDelta < 0) THEN BEGIN
                dayDelta = dayDelta - 1
                secDelta = secDelta + 86400.0d0
            ENDIF

            ; Convert all to seconds, and combine.
            seconds[i] = dayDelta * 86400.0d0 + secDelta
        ENDFOR

    ENDIF ELSE $
        MESSAGE, 'Incorrect number of arguments.'

    RETURN, seconds
END

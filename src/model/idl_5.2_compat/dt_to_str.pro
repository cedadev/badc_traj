; $Id: dt_to_str.pro 888 2005-08-17 10:52:08Z badc $
;
; Copyright (c) 1998, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;     DT_TO_STR
;
; PURPOSE:
;     Converts IDLDT date/time variables into strings.
;
; CATEGORY:
;     MISC
;
; CALLING SEQUENCE:
;     DT_TO_STR, dt_dates[, Dates][, Times]
;
; INPUTS:
;     dt_dates:  A single (or array of) IDLDT date/time structure(s) that
;         are to be converted to strings.
;
; KEYWORD PARAMETERS:
;     DATE_FMT - Set this keyword to one of the following values to select
;         the format of the date string(s) returned via the Dates argument.
;
;         Value   Format                   Example
;         -------------------------------------------
;           1     MM*DD*[YY]YY             05/01/92         
;           2     DD*MM*[YY]YY             01/05/92
;           3     ddd*[YY]YY               122/1992    (ddd is day in year)
;           4     DD*mmm[mmmmmm]*[YY]YY    01/May/92
;           5     [YY]YY*MM*DD             1992/05/01
;
;          where the asterisk, '*', may be replaced by any of the following
;          separators: - / , . : (dash, slash, comma, period, or colon).
;
;     TIME_FMT - Set this keyword to one of the following values to select
;         the format of the time string(s) returned via the time argument.
;
;         Value   Format                   Example
;         -------------------------------------------
;          -1     HH*Mn*SS.ss              13:30:35.25
;          -2     HHMn                     1330
;
;          where the asterisk, '*', may be replaced by any of the following
;          separators: - / , : (dash, slash, comma, or colon).
;
; OUTPUTS:
;     NONE
;
; OPTIONAL OUTPUTS:
;     Dates: A single (or array of) strings that will be set according to
;            the corresponding input date(s).
;     Times: A single (or array of) strings that will be set according to
;            the corresponding input date(s). 
;
; COMMON BLOCKS:
;     NONE
;
; SIDE EFFECTS:
;     None
;
; EXAMPLE:
;     DT_TO_STR, TODAY(), dateStr, TimeStr
;     PRINT, dateStr + ' ' + TimeStr
;
; MODIFICATION HISTORY:
;       Aug 1998, DLD: Initial version.
;
;-
PRO DT_TO_STR, dt_days, date_strs, time_strs, $
                    DATE_FMT=user_date_fmt, TIME_FMT=user_time_fmt

    date_fmt = KEYWORD_SET(user_date_fmt) ? user_date_fmt : 1;
    time_fmt = KEYWORD_SET(user_time_fmt) ? user_time_fmt : -1;

    nDates = N_ELEMENTS( dt_days )
    date_strs = STRARR(nDates)

    IF nDates GT 0 THEN BEGIN
        CASE date_fmt OF
            1: BEGIN  ; MM*DD*[YY]YY
                dfmt = '(C(CMOI2.2, "' + $
                           !DATE_SEPARATOR + $
                           '", CDI2.2, "' + $
                           !DATE_SEPARATOR + $
                           '", CYI))'
            END
            2: BEGIN  ; DD*MM*[YY]YY
                dfmt = '(C(CDI2.2, "' + $
                           !DATE_SEPARATOR + $
                           '", CMOI2.2, "' + $
                           !DATE_SEPARATOR + $
                           '", CYI))'
            END
            3: BEGIN  ; ddd*[YY]YY
                FOR i=0,nDates-1 DO BEGIN
                    julDay0 = JULDAY(1, 1, dt_days[i].year)
                    julDay1 = JULDAY(dt_days[i].month, $
                                     dt_days[i].day, dt_days[i].year)
                    date_strs[i] = $
                        STRING((julDay1-julDay0), FORMAT='(I3.3)') + $
                        !DATE_SEPARATOR
                ENDFOR
                dfmt = '(C(CYI))'
            END
            4: BEGIN  ; DD*mmm[mmmmmm]*[YY]YY
                dfmt = '(C(CDI2.2, "' + $
                           !DATE_SEPARATOR + $
                           '", CMOA0, "' + $
                           !DATE_SEPARATOR + $
                           '", CYI))'
            END
            5: BEGIN  ; [YY]YY*MM*DD
                dfmt = '(C(CYI, "' + $
                           !DATE_SEPARATOR + $
                           '", CMOI2.2, "' + $
                           !DATE_SEPARATOR + $
                           '", CDI2.2))'
            END
            ELSE: BEGIN
                MESSAGE, 'Illegal keyword value for DATE_FMT.'
                RETURN
            END
        ENDCASE
        date_strs = date_strs + STRING(dt_days.julian, FORMAT=dfmt)

        CASE time_fmt OF
            -1: BEGIN  ; HH*Mn*SS.ss
                tfmt = '(C(CHI2.2, "' + $
                           !TIME_SEPARATOR + $
                           '", CMI2.2, "' + $
                           !TIME_SEPARATOR + $
                           '", CSF5.2))'
                time_strs = STRING(dt_days.julian, FORMAT=tfmt)
            END
            -2: BEGIN  ; HHMn
                tfmt = '(C(CDI2.2, CMOI2.2))'
                time_strs = STRING(dt_days.julian, FORMAT=tfmt)
            END
            ELSE: BEGIN
                MESSAGE, 'Illegal keyword value for TIME_FMT.'
                RETURN
            END
        ENDCASE

    ENDIF ELSE $
        MESSAGE, 'Incorrect number of arguments.'        
END

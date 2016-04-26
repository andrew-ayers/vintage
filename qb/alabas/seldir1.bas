DECLARE SUB SelectDir (CursorForeColor%, CursorBackColor%, PrevForeColor%, PrevBackColor%, TextForeColor%, TextBackColor%, xpos%, ypos%, leng%, widt%, select$)
DECLARE SUB ReadDir (dir$, widt%, NumEntries%)
'
CLS
'
DO
  SHELL "cd > temp.dir"
  OPEN "temp.dir" FOR INPUT AS #1
  LINE INPUT #1, curdir$
  LOCATE 20, 1: PRINT SPACE$(80);
  LOCATE 20, 10: PRINT curdir$;
  IF RIGHT$(curdir$, 1) <> "\" THEN PRINT "\";
  CLOSE #1
  SHELL "del temp.dir"
  '
  CALL SelectDir(14, 4, 11, 0, 7, 0, 10, 5, 10, 18, select$)
  '
  IF select$ = "*&*" THEN EXIT DO
  '
  SHELL "cd " + select$
  '
LOOP


SUB ReadDir (dir$, widt%, NumEntries%)
  '
  SHELL "dir > " + "temp.dir"
  '
  OPEN "temp.dir" FOR INPUT AS #1
  '
  temp1$ = "": temp2$ = ""
  '
  DO
    LINE INPUT #1, line$
    line$ = LEFT$(line$, 20)
    IF LEFT$(line$, 1) <> " " AND LEFT$(line$, 2) <> ". " THEN
      IF LEFT$(line$, 2) = ".." THEN MID$(line$, 1, 12) = "<-- Prev Dir"
      IF RIGHT$(line$, 5) = "<DIR>" THEN
        temp1$ = temp1$ + LEFT$(line$, widt% - 6) + " <DIR>"
        NumEntries% = NumEntries% + 1
      END IF
    END IF
  LOOP UNTIL EOF(1)
  '
  dir$ = temp1$
  '
  CLOSE #1
  '
  SHELL "del " + "temp.dir"
  '
END SUB

SUB SelectDir (CursorForeColor%, CursorBackColor%, PrevForeColor%, PrevBackColor%, TextForeColor%, TextBackColor%, xpos%, ypos%, leng%, widt%, select$)
  '
  dir$ = "": CALL ReadDir(dir$, widt%, NumEntries%)
  '
  start% = 1: cpos% = 0
  '
  DO
    v% = 0
    '
    FOR t% = start% TO start% + leng% - 1
      '
      check$ = RTRIM$(MID$(dir$, (t% * widt%) - (widt% - 1), widt% - 5))
      '
      IF v% = cpos% THEN
        COLOR CursorForeColor%, CursorBackColor%
        select$ = RTRIM$(MID$(dir$, (t% * widt%) - (widt% - 1), widt% - 5))
        IF select$ = "<-- Prev Dir" THEN select$ = ".."
      ELSE
        COLOR TextForeColor%, TextBackColor%
        IF check$ = "<-- Prev Dir" THEN
          COLOR PrevForeColor%, PrevBackColor%
        END IF
      END IF
      '
      LOCATE ypos% + v%, xpos%
      '
      IF t% <= NumEntries% THEN
        PRINT MID$(dir$, (t% * widt%) - (widt% - 1), widt%);
      ELSE
        PRINT SPACE$(widt%);
      END IF
      '
      v% = v% + 1
      '
    NEXT
    '
    DO: key$ = INKEY$: LOOP UNTIL key$ <> ""
    '
    SELECT CASE key$
      CASE CHR$(0) + CHR$(&H48)
        cpos% = cpos% - 1
        IF cpos% < 0 THEN
          cpos% = 0
          start% = start% - 1: IF start% < 1 THEN start% = 1
        END IF
      CASE CHR$(0) + CHR$(&H50)
        cpos% = cpos% + 1
        '
        IF RTRIM$(MID$(dir$, ((start + cpos% + 1) * widt%) - (widt% - 1), widt% - 5)) = "" THEN
          cpos% = cpos% - 1
        END IF
        '
        IF cpos% > leng% - 1 THEN
          cpos% = leng% - 1
          start% = start% + 1: IF start% + leng% > NumEntries% THEN start% = NumEntries% - leng%
        END IF
      CASE CHR$(13)
        EXIT DO
      CASE CHR$(27)
        select$ = "*&*"
        EXIT DO
    END SELECT
    '
  LOOP
  '
  COLOR TextForeColor%, TextBackColor%
  '
END SUB


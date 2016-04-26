SCREEN 0, , 1, 0: WIDTH 80: CLS 0: COLOR 7, 0
'
file$ = "readme.txt"
'
IF COMMAND$ <> "" THEN file$ = COMMAND$
'
up$ = CHR$(0) + CHR$(72)
pup$ = CHR$(0) + CHR$(73)
dn$ = CHR$(0) + CHR$(80)
pdn$ = CHR$(0) + CHR$(81)
'
top% = 1: bot% = 22
'
DO
  '
  COLOR 7, 0: CLS
  '
  max% = 0: cnt% = 0: LOCATE 2, 1
  '
  OPEN file$ FOR INPUT AS 1
  '
  WHILE NOT (EOF(1))
    '
    LINE INPUT #1, line$
    '
    cnt% = cnt% + 1: max% = max% + 1
    '
    IF cnt% >= top% AND cnt% <= bot% THEN PRINT line$
    '
  WEND
  '
  CLOSE #1
  '
  LOCATE 1, 1: COLOR 15, 1: PRINT "   README! - README.TXT File Reader - Copyright (C) 1997 by Andrew L. Ayers   "
  LOCATE 24, 1: COLOR 15, 1: PRINT "         Use the Arrow and PgUp/PgDn to scroll up/down - [ESC] exits          ";
  '
  PCOPY 1, 0
  '
  DO: key$ = INKEY$: LOOP UNTIL key$ <> ""
  '
  SELECT CASE key$
    CASE CHR$(27)
      EXIT DO
    CASE dn$
      top% = top% + 1: IF top% + 21 > max% THEN top% = max% - 21
      bot% = top% + 21
    CASE up$
      top% = top% - 1: IF top% < 1 THEN top% = 1
      bot% = top% + 21
    CASE pdn$
      top% = top% + 22: IF top% + 21 > max% THEN top% = max% - 21
      bot% = top% + 21
    CASE pup$
      top% = top% - 22: IF top% < 1 THEN top% = 1
      bot% = top% + 21
  END SELECT
  '
LOOP
'
SCREEN 0, , 0, 0: WIDTH 80: CLS 0: COLOR 7, 0


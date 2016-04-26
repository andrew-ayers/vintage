'
' Description : PsychoPrint! - Custom text print subroutine for
'               VGA Mode 13
' Written by  : Andrew L. Ayers
' Date        : 08/13/96
'
' What can I say? I can't seem to get enough of custom text!
' Well, anyhow - this routine needs to be played with. It allows
' you to create text that flashes (techno/house/rave style),
' text that fades away, dot by dot, and random snow text (certain
' combos of which look like flowing puke), and even rainbow flash
' text. Give it a shot!
'
' BTW: You may use this routine in any manner you like, as long
'      as you give credit in an appropriate manner.
'
DECLARE SUB PsychoPrint (x%, y%, strg$, fclr%, bclr%, range1%, range2%, factor%, special%)
'
SCREEN 13
'
DO
  '
  special% = 1
  '
  CALL PsychoPrint(6, 12, "PsychoPrint! by Andrew Ayers", 3, 0, 0, 15, 4, special%)
  '
LOOP UNTIL special% = 999 OR INKEY$ <> ""

SUB PsychoPrint (x%, y%, strg$, fclr%, bclr%, range1%, range2%, factor%, special%)
  '
  STATIC FirstTime AS INTEGER
  STATIC colr AS INTEGER
  '
  IF strg$ = "" THEN FirstTime% = 0: EXIT SUB
  '
  xpos% = x% * 8 - 8: ypos% = y% * 8 - 8
  xend% = xpos% + (LEN(strg$) * 8): yend% = ypos% + 8
  '
  IF FirstTime% = 0 THEN
    COLOR 255: LOCATE y%, x%: PRINT strg$: FirstTime% = 1
    COLOR 15
    colr% = fclr%
    FOR y% = ypos% TO yend%
      FOR x% = xpos% TO xend%
        IF POINT(x%, y%) <> 255 THEN
          PSET (x%, y%), bclr%
        ELSE
          PSET (x%, y%), fclr%
        END IF
      NEXT x%
    NEXT y%
  END IF
  '
  '***********************************************************
  '
  flag% = 999
  '
  FOR y% = ypos% TO yend%
    FOR x% = xpos% TO xend%
      IF POINT(x%, y%) <> bclr% THEN
        flag% = 0
        PSET (x%, y%), colr%
        '
        SELECT CASE special%
          CASE 3 ' Regular Fade
            IF INT(RND * 2) = 1 THEN
              colr% = bclr%
            ELSE
              colr% = fclr%
            END IF
          CASE 4 ' Psycho Snow
            colr% = INT(RND * factor%)
            IF colr% = bclr% THEN colr% = colr% + 1
          CASE 5 ' Psycho Snow Fade
            colr% = INT(RND * factor%)
        END SELECT
        '
      END IF
      '
      SELECT CASE special%
        CASE 1 ' Psycho Cycle
          colr% = colr% + factor%
          IF colr% = bclr% THEN colr% = colr% + 1
          IF colr% >= range2% THEN colr% = range1%
          IF colr% = bclr% THEN colr% = colr% + 1
        CASE 2 ' Psycho Fade
          colr% = colr% + 1
          IF colr% > range2% THEN colr% = range1%
      END SELECT
      '
    NEXT x%
    '
    SELECT CASE special%
      CASE 6 ' Psycho Rainbow
        colr% = colr% + factor%
        IF colr% = bclr% THEN colr% = colr% + 1
        IF colr% >= range2% THEN colr% = range1%
        IF colr% = bclr% THEN colr% = colr% + 1
      CASE 7 ' Psycho Rainbow Fade
        colr% = colr% + 1
        IF colr% > range2% THEN colr% = range1%
      CASE 8 ' Regular Line Fade
        IF INT(RND * 2) = 1 THEN
          colr% = bclr%
        ELSE
          colr% = fclr%
        END IF
      CASE 9 ' Psycho Line Snow
        colr% = INT(RND * factor%)
        IF colr% = bclr% THEN colr% = colr% + 1
      CASE 10 ' Psycho Line Snow Fade
        colr% = INT(RND * factor%)
    END SELECT
    '
  NEXT y%
  '
  FOR dlay = 1 TO 10000: NEXT dlay' Adjust this to your computer
  '
  special% = flag%
  '
END SUB


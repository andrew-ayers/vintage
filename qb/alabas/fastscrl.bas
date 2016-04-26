'
' Description : FastScroll! - VGA Mode 13 Scrolling Routine
' Written by  : Andrew L. Ayers
' Date        : 08/02/96
'
' This little routine allows you to scroll the ENTIRE mode 13
' screen ANY number of pixels up, down, left and right. It uses
' GET/PUT to accomplish this, but the GET/PUT is tiled around
' the screen, so that the buffer used only needs to be about
' 1000 bytes! When you use this routine, don't pass in both
' x and y offsets at one time (don't try to go diagonal) - the
' program will bomb. Pass one, then the other to move diagonally.
' I know this isn't the best way (jumps a bit), but it does work.
' I made this routine for a game, and I only needed the four
' cardinal directions. When scrolling, be aware of the fact that
' if any graphics are on the edges of the scroll region (one pixel
' "in" if offset is 1, two if offset is 2, 4 if offset is four,
'  etc.), when the scroll is performed, "droppings" will be left
' and will need to be cleaned up. I know I could have did this
' myself, but I felt that some people may have wanted droppings
' left (I don't know why...), so I left it like it is.
'
' You may use this routine in any manner you like, as long
' as you give credit in an appropriate manner. Have phun!
'
DECLARE SUB FastScroll (XSpeed%, YSpeed%)
'
SCREEN 13
'
' Set up a Demo Graphic
'
FOR T% = 0 TO 500
  X1% = INT(RND * 260) + 10
  Y1% = INT(RND * 140) + 10
  X2% = INT(RND * 260) + 10
  Y2% = INT(RND * 140) + 10
  C% = INT(RND * 16)
  LINE (X1%, Y1%)-(X2%, Y2%), C%
NEXT T%
'
LOCATE 11, 4: PRINT "FastScroll! by Andrew L. Ayers"
'
' Show off scrolling!
'
count% = 0: x% = 1: y% = 0
'
DO
  count% = count% + 1
  IF count% = 10 THEN x% = 0: y% = 1
  IF count% = 20 THEN x% = -1: y% = 0
  IF count% = 30 THEN x% = 0: y% = -1
  IF count% = 40 THEN x% = 1: y% = 0: count% = 0
  '
  CALL FastScroll(x% * 4, y% * 4)
LOOP UNTIL INKEY$ <> ""

SUB FastScroll (XSpeed%, YSpeed%)
  '
  DIM Temp%(502)
  '
  XStep% = 40: YStep% = 25
  '
  IF XSpeed% < 0 OR YSpeed% < 0 THEN
    FOR y% = 0 TO 199 STEP YStep%
      FOR x% = 0 TO 319 STEP XStep%
        IF (XSpeed% <> 0 AND x% = 0) OR (YSpeed% <> 0 AND y% = 0) THEN
          GET (x% - XSpeed%, y% - YSpeed%)-(x% + XStep% - 1, y% + YStep% - 1), Temp%
          PUT (x%, y%), Temp%, PSET
        ELSE
          GET (x%, y%)-(x% + XStep% - 1, y% + YStep% - 1), Temp%
          PUT (x% + XSpeed%, y% + YSpeed%), Temp%, PSET
        END IF
      NEXT x%
    NEXT y%
  ELSE
    FOR y% = 199 TO 0 STEP -YStep%
      FOR x% = 319 TO 0 STEP -XStep%
        IF (XSpeed% <> 0 AND x% = 319) OR (YSpeed% <> 0 AND y% = 199) THEN
          GET (x% - (XStep% - 1), y% - (YStep% - 1))-(x% - XSpeed%, y% - YSpeed%), Temp%
          PUT (x% - (XStep% - 1) + XSpeed%, y% - (YStep% - 1) + YSpeed%), Temp%, PSET
        ELSE
          GET (x% - (XStep% - 1), y% - (YStep% - 1))-(x%, y%), Temp%
          PUT (x% - (XStep% - 1) + XSpeed%, y% - (YStep% - 1) + YSpeed%), Temp%, PSET
        END IF
      NEXT x%
    NEXT y%
  END IF
  '
END SUB


'
' Put screen and page numbers in integer vars for speed
'
SCR% = 7: PG0% = 0: PG1% = 1: PG2% = 2
'
' Set up background page (2)
'
SCREEN SCR%, , PG2%, PG0%
'
' Draw object on background page
'
FOR t = 0 TO 14
  LINE (t, 0)-(0, 14 - t), t
  LINE (t, 14)-(14, 14 - t), t
  'CIRCLE (7, 7), t, t * 2
NEXT t
'
' Switch to work page (1)
'
SCREEN SCR%, , PG1%, PG0%
'
' Set up zoom square size
'
H1% = 0: V1% = 0
H2% = 14: V2% = 14
'
' Half height/width values
'
HH% = (H2% - H1%) / 2
HV% = (V2% - V1%) / 2
'
' Destination position (center of object)
'
DX% = 160: DY% = 100
'
' Scale to 8X!
'
s% = 1: e% = 8: sp% = 1
DO
  FOR SCALE% = s% TO e% STEP sp%
    '
    ' Set up and clear work page (1)
    '
    SCREEN SCR%, , PG1%, PG0%
    CLS
    '
    ' Set up plus/minus value, plus double...
    '
    SC% = SCALE% - 1
    SC2% = SC% * 2
    DXC% = DX% - SC%
    DYC% = DY% - SC%
    '
    ' Set up destination step rate
    '
    stp% = SCALE% + (SCALE% - 1)
    '
    ' Scan thru source square, zoom on only
    ' colored pixels...
    '
    FOR V% = V1% TO V2%
      FOR H% = H1% TO H2%
        '
        ' Get color of pixel on work page (2)
        '
        SCREEN SCR%, , PG2%, PG0%
        COLR% = POINT(H%, V%)
        IF COLR% THEN
          '
          ' Has a color, so zoom in on it
          '
          X1% = DXC% + H% * stp% - HH% * stp%
          Y1% = DYC% + V% * stp% - HV% * stp%
          X2% = X1% + SC2%
          Y2% = Y1% + SC2%
          '
          ' Plot zoomed pixel on work page (1) at destination
          ' centerpoint...
          '
          SCREEN SCR%, , PG1%, PG0%
          LINE (X1%, Y1%)-(X2%, Y2%), COLR%, BF
        END IF
      NEXT H%
    NEXT V%
    '
    ' Copy work page onto visible page (1 to 0)
    '
    LOCATE 12, 16: PRINT "8X Zoom!"
    '
    PCOPY PG1%, PG0%
    '
  NEXT SCALE%
  aa% = s%: s% = e%: e% = aa%: sp% = -sp%
LOOP UNTIL INKEY$ <> ""


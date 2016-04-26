DECLARE SUB NewCircle (xp%, yp%, xr%, yr%, c%)
DECLARE SUB NewPaint (xp%, yp%, pc%, bc%)
DECLARE SUB NewPaint2 (xp%, yp%, pc%, bc%, quad%)
'
SCREEN 13
'
o = 0
FOR t% = 14 TO 3 STEP -1
  c% = 34 - t%
  CALL NewCircle(15 + o, 15 + o, t%, t%, c%)
  CALL NewPaint(15 + o, 15 + o, c%, c%)
  o = o - .4
NEXT t%

SUB NewCircle (xp%, yp%, xr%, yr%, c%)
  '
  ' Note: This circle routine is not part of the Blast! Library, and is only
  ' included in this demo to allow the creation of the "bouncing spheres"
  ' used in this demo. This routine is *not* perfect, and will "blow up" if
  ' used in certain situations. As such, I do not endorse use of this rou-
  ' tine in any way outside of this demo, and suggest for those in need of
  ' a general purpose circle routine to continue their search elsewhere...
  '
  x% = xp% + SIN(0) * xr%
  y% = yp% + COS(0) * (yr% - 1)
  '
  LINE (x%, y%)-(x%, y%), c%
  '
  FOR t! = 0 TO 6.28 STEP .1
    x% = xp% + SIN(t!) * xr%
    y% = yp% + COS(t!) * (yr% - 1)
    LINE -(x%, y%), c%
  NEXT
  '
  x% = xp% + SIN(0) * xr%
  y% = yp% + COS(0) * (yr% - 1)
  '
  LINE -(x%, y%), c%
  '
END SUB

SUB NewPaint (xp%, yp%, pc%, bc%)
  '
  ' Note: This paint routine is not part of the Blast! Library, and is only
  ' included in this demo to allow the creation of the "bouncing spheres"
  ' used in this demo. This routine is *not* perfect, and will "blow up" if
  ' used in certain situations. As such, I do not endorse use of this rou-
  ' tine in any way outside of this demo, and suggest for those in need of
  ' a general purpose paint routine to continue their search elsewhere...
  '
  FOR t% = 0 TO 3
    SELECT CASE t%
      CASE 0
        ox% = 0: oy% = 0
      CASE 1
        ox% = 1: oy% = 0
      CASE 2
        ox% = 0: oy% = 1
      CASE 3
        ox% = -1: oy% = 1
    END SELECT
    '
    CALL NewPaint2(xp% + ox%, yp% + oy%, pc%, bc%, t%)
  NEXT
  '
END SUB

SUB NewPaint2 (xp%, yp%, pc%, bc%, quad%)
  '
  IF xp% < 0 OR xp% > 319 OR yp% < 0 OR yp% > 199 THEN EXIT SUB
  '
  IF POINT(xp%, yp%) = bc% OR POINT(xp%, yp%) = pc% THEN EXIT SUB
  '
  PSET (xp%, yp%), pc%
  '
  SELECT CASE quad%
    CASE 0
      CALL NewPaint2(xp% - 1, yp%, pc%, bc%, quad%)
      CALL NewPaint2(xp%, yp% - 1, pc%, bc%, quad%)
    CASE 1
      CALL NewPaint2(xp%, yp% - 1, pc%, bc%, quad%)
      CALL NewPaint2(xp% + 1, yp%, pc%, bc%, quad%)
    CASE 2
      CALL NewPaint2(xp% + 1, yp%, pc%, bc%, quad%)
      CALL NewPaint2(xp%, yp% + 1, pc%, bc%, quad%)
    CASE 3
      CALL NewPaint2(xp% - 1, yp%, pc%, bc%, quad%)
      CALL NewPaint2(xp%, yp% + 1, pc%, bc%, quad%)
  END SELECT
  '
END SUB


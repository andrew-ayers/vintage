DECLARE SUB BuildTables ()
DECLARE SUB Translate3D (x%, y%, z%, px%, py%, pz%, mx%, my%, mz%)
DECLARE SUB Rotate3D (x!, y!, z!, tx%, ty%, tz%, yaw%)
DECLARE SUB Project3D (sx%, sy%, x!, y!, z!, vis%)
'
TYPE pnt
  px AS INTEGER
  py AS INTEGER
  pz AS INTEGER
  '
  sx AS INTEGER
  sy AS INTEGER
  '
  vis AS INTEGER
END TYPE
'
TYPE walls
  pnt0 AS pnt
  pnt1 AS pnt
  pnt2 AS pnt
  pnt3 AS pnt
  '
  pnt4 AS pnt
END TYPE
'
DIM wall(100) AS walls
DIM SHARED stab(359), ctab(359)
'
fw$ = CHR$(0) + CHR$(72)
bk$ = CHR$(0) + CHR$(80)
lt$ = CHR$(0) + CHR$(75)
rt$ = CHR$(0) + CHR$(77)
'
' Define a couple of walls...
'
wall(0).pnt0.px = -50: wall(0).pnt0.py = -50: wall(0).pnt0.pz = 50
wall(0).pnt1.px = 50: wall(0).pnt1.py = -50: wall(0).pnt1.pz = 50
wall(0).pnt2.px = 50: wall(0).pnt2.py = 50: wall(0).pnt2.pz = 50
wall(0).pnt3.px = -50: wall(0).pnt3.py = 50: wall(0).pnt3.pz = 50
'
wall(1).pnt0.px = -50: wall(1).pnt0.py = -50: wall(1).pnt0.pz = 50
wall(1).pnt1.px = -50: wall(1).pnt1.py = 50: wall(1).pnt1.pz = 50
wall(1).pnt2.px = -50: wall(1).pnt2.py = 50: wall(1).pnt2.pz = -50
wall(1).pnt3.px = -50: wall(1).pnt3.py = -50: wall(1).pnt3.pz = -50
'
' Build SIN/COS tables to speed up processing
'
CALL BuildTables
'
SCREEN 7, , 1, 0
'
mx% = 0: my% = 0: mz% = 350: hed% = 180
'
done% = 0: DO
  '
  ' Get user input
  '
  key$ = INKEY$
  '
  SELECT CASE key$
    CASE rt$
      hed% = hed% + 5
      IF hed% > 359 THEN hed% = hed% - 359
    CASE lt$
      hed% = hed% - 5
      IF hed% < 0 THEN hed% = 359 + hed%
    CASE fw$
      mx% = mx% + stab(hed%) * 10
      mz% = mz% + ctab(hed%) * 10
    CASE bk$
      mx% = mx% - stab(hed%) * 10
      mz% = mz% - ctab(hed%) * 10
    CASE "q", "Q"
      done% = 1
    CASE ELSE
      '
      ' Do other stuff here (with GOSUBs/CALLs)
      '
  END SELECT
  '
  ' Clear off page (this method is FAST!)
  '
  PCOPY 2, 1
  '
  ' Calculate position of each wall
  '
  '
  FOR wn% = 0 TO 1
    '
    ' Translate, then rotate each point for wall
    '
    yaw%(wn%) = hed%
    '
    CALL Translate3D(tx%, ty%, tz%, wall(wn%).pnt0.px, wall(wn%).pnt0.py, wall(wn%).pnt0.pz, mx%, my%, mz%)
    CALL Rotate3D(rx, ry, rz, tx%, ty%, tz%, yaw%(wn%))
    CALL Project3D(wall(wn%).pnt0.sx, wall(wn%).pnt0.sy, rx, ry, rz, wall(wn%).pnt0.vis)
    '
    CALL Translate3D(tx%, ty%, tz%, wall(wn%).pnt1.px, wall(wn%).pnt1.py, wall(wn%).pnt1.pz, mx%, my%, mz%)
    CALL Rotate3D(rx, ry, rz, tx%, ty%, tz%, yaw%(wn%))
    CALL Project3D(wall(wn%).pnt1.sx, wall(wn%).pnt1.sy, rx, ry, rz, wall(wn%).pnt1.vis)
    '
    CALL Translate3D(tx%, ty%, tz%, wall(wn%).pnt2.px, wall(wn%).pnt2.py, wall(wn%).pnt2.pz, mx%, my%, mz%)
    CALL Rotate3D(rx, ry, rz, tx%, ty%, tz%, yaw%(wn%))
    CALL Project3D(wall(wn%).pnt2.sx, wall(wn%).pnt2.sy, rx, ry, rz, wall(wn%).pnt2.vis)
    '
    CALL Translate3D(tx%, ty%, tz%, wall(wn%).pnt3.px, wall(wn%).pnt3.py, wall(wn%).pnt3.pz, mx%, my%, mz%)
    CALL Rotate3D(rx, ry, rz, tx%, ty%, tz%, yaw%(wn%))
    CALL Project3D(wall(wn%).pnt3.sx, wall(wn%).pnt3.sy, rx, ry, rz, wall(wn%).pnt3.vis)
    '
    ' Draw wall
    '
    IF wall(wn%).pnt0.vis AND wall(wn%).pnt1.vis THEN LINE (wall(wn%).pnt0.sx, wall(wn%).pnt0.sy)-(wall(wn%).pnt1.sx, wall(wn%).pnt1.sy), 2
    IF wall(wn%).pnt1.vis AND wall(wn%).pnt2.vis THEN LINE (wall(wn%).pnt1.sx, wall(wn%).pnt1.sy)-(wall(wn%).pnt2.sx, wall(wn%).pnt2.sy), 2
    IF wall(wn%).pnt2.vis AND wall(wn%).pnt3.vis THEN LINE (wall(wn%).pnt2.sx, wall(wn%).pnt2.sy)-(wall(wn%).pnt3.sx, wall(wn%).pnt3.sy), 2
    IF wall(wn%).pnt3.vis AND wall(wn%).pnt0.vis THEN LINE (wall(wn%).pnt3.sx, wall(wn%).pnt3.sy)-(wall(wn%).pnt0.sx, wall(wn%).pnt0.sy), 2
    '
  NEXT wn%
  '
  ' Copy new image to visible page
  '
  PCOPY 1, 0
  '
LOOP UNTIL done%
'
SCREEN 7, , 0, 0: CLS

SUB BuildTables
  '
  rad = 3.14159 / 180
  '
  FOR t% = 0 TO 359
    stab(t%) = SIN(rad * t%)
    ctab(t%) = COS(rad * t%)
  NEXT t%
  '
END SUB

SUB Project3D (sx%, sy%, x, y, z, vis%)
  '
  vd% = 250: dcor% = 30: vis% = 1
  '
  IF z > 0 THEN vis% = 0
  '
  sx% = 160 + ((vd% * x) / (z))
  sy% = 100 + ((vd% * y) / (z + dcor%))
  '
  IF sx% < 0 OR sx% > 319 OR sy% < 0 OR sy% > 199 THEN vis% = 0
  '
END SUB

SUB Rotate3D (x, y, z, tx%, ty%, tz%, yaw%)
  '
  x = tx% * ctab(yaw%) - tz% * stab(yaw%)
  y = -ty%
  z = tx% * stab(yaw%) + tz% * ctab(yaw%)
  '
END SUB

SUB Translate3D (x%, y%, z%, px%, py%, pz%, mx%, my%, mz%)
  '
  x% = px% + mx%
  y% = py% + my%
  z% = pz% + mz%
  '
END SUB


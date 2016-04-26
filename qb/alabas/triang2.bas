DECLARE SUB DrawHline (fromx%, tox%, yy%, col%)
DECLARE SUB DrawTriangle (x1%, y1%, x2%, y2%, x3%, y3%, col%)
'
' Demonstrates a routine to draw solid triangles in mode 13h
'
SCREEN 13
'
RANDOMIZE TIMER
'
t = TIMER
'
cnt% = 0: DO
  x1% = INT(RND * 320)
  x2% = INT(RND * 320)
  x3% = INT(RND * 320)
  y1% = INT(RND * 200)
  y2% = INT(RND * 200)
  y3% = INT(RND * 200)
  col% = INT(RND * 255) + 1
  '
  CALL DrawTriangle(x1%, y1%, x2%, y2%, x3%, y3%, col%)
  '
  cnt% = cnt% + 1
  '
LOOP UNTIL cnt% = 183
PRINT TIMER - t

SUB DrawHline (fromx%, tox%, yy%, col%)
  '
  'DEF SEG = &HA000
  '
  'IF fromx% > tox% THEN SWAP fromx%, tox%
  '
  'yyy& = yy%
  'sloc& = yyy& * 320 + fromx%
  'eloc& = sloc& + (tox% - fromx%)
  '
  'FOR t& = sloc& TO eloc&
  '  POKE t&, col%
  'NEXT
  '
  'DEF SEG
  LINE (fromx%, yy%)-(tox%, yy%), col%
  '
END SUB

SUB DrawTriangle (x1%, y1%, x2%, y2%, x3%, y3%, col%)
  '
  DO
    sflag% = 0
    IF y1% > y2% THEN
      sflag% = 1
      SWAP y1%, y2%
      SWAP x1%, x2%
    END IF
    IF y2% > y3% THEN
      sflag% = 1
      SWAP y2%, y3%
      SWAP x2%, x3%
    END IF
  LOOP UNTIL sflag% = 0
  '
  IF y2% = y3% THEN
    '
    ' Draw a flat bottomed triangle
    '
    ydiff1% = y2% - y1%
    ydiff2% = y3% - y1%
    '
    IF ydiff1% <> 0 THEN
      slope1! = (x2% - x1%) / ydiff1%
    ELSE
      slope1! = 0
    END IF
    '
    IF ydiff2% <> 0 THEN
      slope2! = (x3% - x1%) / ydiff2%
    ELSE
      slope2! = 0
    END IF
    '
    sx! = x1%: ex! = x1%
    '
    FOR y% = y1% TO y2%
      CALL DrawHline(INT(sx!), INT(ex!), y%, col%)
      sx! = sx! + slope1!
      ex! = ex! + slope2!
    NEXT
    '
    EXIT SUB
    '
  ELSE
    IF y1% = y2% THEN
      '
      ' Draw a flat topped triangle
      '
      ydiff1% = y3% - y1%
      ydiff2% = y3% - y2%
      '
      IF ydiff1% <> 0 THEN
        slope1! = (x3% - x1%) / ydiff1%
      ELSE
        slope1! = 0
      END IF
      '
      IF ydiff2% <> 0 THEN
        slope2! = (x3% - x2%) / ydiff2%
      ELSE
        slope2! = 0
      END IF
      '
      sx! = x1%: ex! = x2%
      '
      FOR y% = y1% TO y3%
        CALL DrawHline(INT(sx!), INT(ex!), y%, col%)
        sx! = sx! + slope1!
        ex! = ex! + slope2!
      NEXT
      '
      x1% = sx!: x2% = ex!
      '
      EXIT SUB
      '
    ELSE
      '
      ' Draw a general purpose triangle
      '
      '
      ' First draw the flat bottom portion (top half)
      '
      ydiff1% = y2% - y1%
      ydiff2% = y3% - y1%
      '
      IF ydiff1% <> 0 THEN
        slope1! = (x2% - x1%) / ydiff1%
      ELSE
        slope1! = 0
      END IF
      '
      IF ydiff2% <> 0 THEN
        slope2! = (x3% - x1%) / ydiff2%
      ELSE
        slope2! = 0
      END IF
      '
      sx! = x1%: ex! = x1%
      '
      FOR y% = y1% TO y2%
        CALL DrawHline(INT(sx!), INT(ex!), y%, col%)
        sx! = sx! + slope1!
        ex! = ex! + slope2!
      NEXT
      '
      ' Then draw the flat topped portion (bottom half)
      '
      x1% = x2%
      x2% = ex!
      y1% = y2%
      '
      ydiff1% = y3% - y1%
      ydiff2% = y3% - y2%
      '
      IF ydiff1% <> 0 THEN
        slope1! = (x3% - x1%) / ydiff1%
      ELSE
        slope1! = 0
      END IF
      '
      IF ydiff2% <> 0 THEN
        slope2! = (x3% - x2%) / ydiff2%
      ELSE
        slope2! = 0
      END IF
      '
      sx! = x1%: ex! = x2%
      '
      FOR y% = y1% TO y3%
        CALL DrawHline(INT(sx!), INT(ex!), y%, col%)
        sx! = sx! + slope1!
        ex! = ex! + slope2!
      NEXT
      '
      x1% = sx!: x2% = ex!
      '
    END IF
  END IF
  '
END SUB


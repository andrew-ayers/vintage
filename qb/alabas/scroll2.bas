DEFINT A-Z
'
SCREEN 13: CLS
'
TYPE tile
  tilenum AS INTEGER
  value AS INTEGER
END TYPE
'
DIM map(100, 100) AS tile
'
wid = 18: hgt = 14
'
GOSUB InitMap
'
GOSUB ShowMap
'
done = 0: DO
  a$ = INKEY$
  IF a$ <> "" THEN
    SELECT CASE a$
      CASE "6"
        x = x + 1
      CASE "4"
        x = x - 1
      CASE "8"
        y = y - 1
      CASE "2"
        y = y + 1
      CASE "7"
        x = x - 1: y = y - 1
      CASE "9"
        x = x + 1: y = y - 1
      CASE "3"
        x = x + 1: y = y + 1
      CASE "1"
        x = x - 1: y = y + 1
      CASE "q"
        done = 1
    END SELECT
    '
    IF x > 100 THEN x = 0
    IF x < 0 THEN x = 100
    IF y < 0 THEN y = 100
    IF y > 100 THEN y = 0
    '
    GOSUB ShowMap
  END IF
LOOP UNTIL done
'
STOP
'
InitMap:
  COLOR 15
  PRINT "Please wait, computing map";
  FOR yy% = 0 TO 100
    PRINT ".";
    FOR xx% = 0 TO 100
      map(xx%, yy%).tilenum = INT(RND * 2)
      map(xx%, yy%).value = INT(RND * 16)
    NEXT xx%
  NEXT yy%
  FOR xx% = 0 TO 100
    map(xx%, 0).tilenum = 2: map(xx%, 100).tilenum = 2
    map(xx%, 0).value = 15: map(xx%, 100).value = 15
  NEXT xx%
  FOR yy% = 0 TO 100
    map(0, yy%).tilenum = 2: map(100, yy%).tilenum = 2
    map(0, yy%).value = 15: map(100, yy%).value = 15
  NEXT yy%
  CLS
RETURN

ShowMap:
  v% = 0: vv% = 0
  FOR yy% = y TO y + hgt
    vv% = yy%
    IF vv% > 100 THEN vv% = vv% - 101
    IF vv% < 0 THEN vv% = vv% + 101
    h% = 0: hh% = 0
    FOR xx% = x TO x + wid
      hh% = xx%
      IF hh% > 100 THEN hh% = hh% - 101
      IF hh% < 0 THEN hh% = hh% + 101
      IF map(hh%, vv%).tilenum = 0 THEN ch$ = " "
      IF map(hh%, vv%).tilenum = 1 THEN ch$ = "Û"
      IF map(hh%, vv%).tilenum = 2 THEN ch$ = "X"
      COLOR map(hh%, vv%).value
      LOCATE v% + 1, h% + 1: PRINT ch$;
      h% = h% + 1
    NEXT xx%
    v% = v% + 1
  NEXT yy%
RETURN


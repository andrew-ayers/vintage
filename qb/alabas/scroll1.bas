WIDTH 40: SCREEN 0, , 1, 0: CLS
'
TYPE tile
  tilenum AS INTEGER
  value AS INTEGER
END TYPE
'
DIM map(100, 100) AS tile
'
wid = 20: hgt = 15
'
GOSUB InitMap
'
GOSUB ShowMap
'
done = 0: DO
  a$ = INKEY$
  IF a$ <> "" THEN
    SELECT CASE a$
      CASE "."
        x = x + 1: IF x + wid > 100 THEN x = 100 - wid
      CASE ","
        x = x - 1: IF x < 0 THEN x = 0
      CASE "a"
        y = y - 1: IF y < 0 THEN y = 0
      CASE "z"
        y = y + 1: IF y + hgt > 100 THEN y = 100 - hgt
      CASE "q"
        done = 1
    END SELECT
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
    PCOPY 1, 0
    FOR xx% = 0 TO 100
      map(xx%, yy%).tilenum = INT(RND * 2)
      map(xx%, yy%).value = INT(RND * 16)
    NEXT xx%
  NEXT yy%
  CLS
RETURN

ShowMap:
  v% = 0
  FOR yy% = y TO y + hgt
    h% = 0
    FOR xx% = x TO x + wid
      IF map(xx%, yy%).tilenum = 0 THEN ch$ = " " ELSE ch$ = "Û"
      COLOR map(xx%, yy%).value
      LOCATE v% + 1, h% + 1: PRINT ch$;
      h% = h% + 1
    NEXT xx%
    v% = v% + 1
  NEXT yy%
  PCOPY 1, 0
RETURN


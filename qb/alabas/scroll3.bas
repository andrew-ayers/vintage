SCREEN 13: CLS
'
TYPE tile
  tilenum AS INTEGER
  value AS INTEGER
END TYPE
'
wid = 18: hgt = 14
'
DIM map AS tile
'
GOSUB InitMap
'
OPEN "c:\temp\test.bin" FOR BINARY AS #1 LEN = LEN(map)
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
    IF x > 300 THEN x = 0
    IF x < 0 THEN x = 300
    IF y < 0 THEN y = 300
    IF y > 300 THEN y = 0
    '
    GOSUB ShowMap
  END IF
LOOP UNTIL done
'
CLOSE #1
'
STOP
'
InitMap:
  OPEN "c:\temp\test.bin" FOR BINARY AS #1 LEN = LEN(map)
  COLOR 15
  PRINT "Please wait, computing map";
  FOR yy& = 0 TO 300
    PRINT ".";
    FOR xx& = 0 TO 300
      map.tilenum = INT(RND * 2)
      map.value = INT(RND * 16)
      rec& = yy& * 300 + xx&
      PUT #1, rec& + 1, map
    NEXT xx&
  NEXT yy&
  CLOSE #1
  CLS
RETURN

ShowMap:
  v% = 0: vv% = 0
  FOR yy% = y TO y + hgt
    vv& = yy%
    IF vv& > 300 THEN vv& = vv& - 301
    IF vv& < 0 THEN vv& = vv& + 301
    h% = 0: hh& = 0
    FOR xx% = x TO x + wid
      hh& = xx%
      IF hh& > 300 THEN hh& = hh& - 301
      IF hh& < 0 THEN hh& = hh& + 301
      rec& = vv& * 300 + hh&
      GET #1, rec& + 1, map
      IF map.tilenum = 0 THEN ch$ = " "
      IF map.tilenum = 1 THEN ch$ = "Û"
      IF map.tilenum = 2 THEN ch$ = "X"
      COLOR map.value
      LOCATE v% + 1, h% + 1: PRINT ch$;
      h% = h% + 1
    NEXT xx%
    v% = v% + 1
  NEXT yy%
RETURN


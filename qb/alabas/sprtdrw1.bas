DECLARE SUB getfilename (filename$, title$)
DECLARE SUB loadsprites ()
DECLARE SUB savesprites ()
DECLARE SUB setsprite (x!, y!, col%)
DECLARE SUB choosecol (scr!)
DECLARE SUB setscreen (scr!)
DECLARE SUB setgrid ()
DECLARE SUB drawgrid ()
DECLARE SUB getkey (key$)
DECLARE SUB movecurs (dir$)
'
CONST num.sprites = 16
CONST sprite.size = 16
CONST grid.size = 8
CONST scr = 7
CONST invisible.color = 0
CONST hotx = 0
CONST hoty = 0
'
DIM temp.grid%(sprite.size - 1, sprite.size - 1, num.sprites - 1)
DIM sprite$(num.sprites - 1)
'
sprite.num = 0: x = 0: y = 0: col% = 5
'
up$ = CHR$(0) + CHR$(72)
dn$ = CHR$(0) + CHR$(80)
lt$ = CHR$(0) + CHR$(75)
rt$ = CHR$(0) + CHR$(77)
sp$ = " "
rn$ = CHR$(13)
'
CALL setscreen(scr)
'
CALL drawgrid
'
CALL movecurs(up$)
'
DO
  '
  CALL getkey(key$)
  '
  SELECT CASE key$
    '
    CASE up$
      CALL movecurs(up$)
    CASE dn$
      CALL movecurs(dn$)
    CASE rt$
      CALL movecurs(rt$)
    CASE lt$
      CALL movecurs(lt$)
    CASE sp$, rn$
      CALL setgrid
    CASE "c", "C"
      CALL choosecol(scr)
      CALL drawgrid
      CALL movecurs("")
    CASE ","
      sprite.num = sprite.num - 1: IF sprite.num < 0 THEN sprite.num = 0
      CALL drawgrid
      CALL movecurs("")
    CASE "."
      sprite.num = sprite.num + 1: IF sprite.num > num.sprites - 1 THEN sprite.num = num.sprites - 1
      CALL drawgrid
      CALL movecurs("")
    CASE "s", "S"
      CALL savesprites
      CALL drawgrid
      CALL movecurs("")
    CASE "l", "L"
      CALL loadsprites
      CALL drawgrid
      CALL movecurs("")
    CASE ELSE
    '
  END SELECT
  '
LOOP

SUB choosecol (scr)
  '
  SHARED up$
  SHARED dn$
  SHARED lt$
  SHARED rt$
  SHARED sp$
  SHARED rn$
  SHARED col%
  SHARED pcp
  '
  SELECT CASE scr
    CASE 1
    CASE 2
    CASE 3
    CASE 4
    CASE 5
    CASE 6
    CASE 7
      maxx = 319: maxy = 199
      stpx = 80: stpy = 50
    CASE 8
      maxx = 639: maxy = 199
      stpx = 160: stpy = 50
    CASE 9
      maxx = 639: maxy = 319
      stpx = 160: stpy = 80
    CASE 10
    CASE 11
    CASE 12
      maxx = 639: maxy = 319
      stpx = 160: stpy = 80
    CASE 13
      maxx = 319: maxy = 175
      stpx = 20: stpy = 11
    CASE ELSE
  END SELECT
  '
  CLS 0
  '
  col% = 0
  '
  FOR y = 0 TO maxy STEP stpy
    FOR x = 0 TO maxx STEP stpx
      LINE (x + 1, y + 1)-(x + stpx - 2, y + stpy - 2), col%, BF
      col% = col% + 1
    NEXT x
  NEXT y
  '
  curx = 0: cury = 0: done = 0
  '
  LINE (curx * stpx, cury * stpy)-(curx * stpx + stpx - 1, cury * stpy + stpy - 1), 15, B
  '
  IF pcp THEN PCOPY 1, 0
  '
  DO
    '
    CALL getkey(key$)
    '
    LINE (curx * stpx, cury * stpy)-(curx * stpx + stpx - 1, cury * stpy + stpy - 1), 0, B
    '
    SELECT CASE key$
      '
      CASE up$
        cury = cury - 1: IF cury < 0 THEN cury = 0
      CASE dn$
        cury = cury + 1: IF cury > INT((maxy + 1) / stpy) - 1 THEN cury = INT((maxy + 1) / stpy) - 1
      CASE rt$
        curx = curx + 1: IF curx > INT((maxx + 1) / stpx) - 1 THEN curx = INT((maxx + 1) / stpx) - 1
      CASE lt$
        curx = curx - 1: IF curx < 0 THEN curx = 0
      CASE sp$, rn$
        col% = POINT(curx * stpx + 5, cury * stpy + 5)
        done = 1
      CASE ELSE
      '
    END SELECT
    '
    LINE (curx * stpx, cury * stpy)-(curx * stpx + stpx - 1, cury * stpy + stpy - 1), 15, B
    '
    IF pcp THEN PCOPY 1, 0
    '
  LOOP UNTIL done
  '
END SUB

SUB drawgrid
  '
  SHARED col%
  SHARED pcp
  SHARED sprite.num
  SHARED temp.grid%()
  SHARED offset
  SHARED divd
  '
  CLS
  '
  locx = INT((sprite.size * grid.size + 8) / 8)
  locy = INT((sprite.size + offset) / divd)
  LOCATE locy, locx: PRINT "             "
  LOCATE locy, locx: PRINT sprite.num + 1; "of"; num.sprites
  '
  LINE (0, 0)-(grid.size * sprite.size, grid.size * sprite.size), 15, B
  '
  FOR t = 0 TO grid.size * sprite.size - 1 STEP grid.size
    '
    LINE (0, t)-(grid.size * sprite.size - 1, t), 15
    LINE (t, 0)-(t, grid.size * sprite.size - 1), 15
    '
  NEXT t
  '
  v = 0
  '
  FOR y = 0 TO grid.size * sprite.size - 1 STEP grid.size
    '
    h = 0
    '
    FOR x = 0 TO grid.size * sprite.size - 1 STEP grid.size
      '
      PAINT (x + 1, y + 1), temp.grid%(h, v, sprite.num), 15
      CALL setsprite(h, v, temp.grid%(h, v, sprite.num))
      h = h + 1
      '
    NEXT x
    '
    v = v + 1
    '
  NEXT y
  '
  LINE (sprite.size * grid.size + 18, 100)-(sprite.size * grid.size + 38, 120), col, BF
  LINE (sprite.size * grid.size + 18, 100)-(sprite.size * grid.size + 38, 120), 15, B
  '
  IF pcp THEN PCOPY 1, 0
  '
END SUB

SUB getfilename (filename$, title$)
  '
  SHARED pcp
  '
  st1$ = "Ú" + STRING$(35, "Ä") + "¿"
  st2$ = "³" + STRING$(35, " ") + "³±"
  st3$ = "À" + STRING$(35, "Ä") + "Ù±"
  st4$ = STRING$(37, "±")
  '
  LOCATE 10, 2: PRINT st1$
  FOR t = 11 TO 13
    LOCATE t, 2: PRINT st2$
  NEXT t
  LOCATE t, 2: PRINT st3$
  LOCATE t + 1, 3: PRINT st4$
  '
  IF pcp THEN PCOPY 1, 0
  '
  SCREEN , , 0, 0
  '
  LOCATE 12, 4
  '
  IF title$ = "S" THEN
    LINE INPUT "Save as: "; filename$
  ELSE
    LINE INPUT "Load as: "; filename$
  END IF
  '
  SCREEN , , 1, 0
  '
END SUB

SUB getkey (key$)
  '
  DO
    '
    key$ = INKEY$
    '
  LOOP UNTIL LEN(key$)
  '
END SUB

SUB loadsprites
  '
  SHARED temp.grid%()
  '
  CALL getfilename(filename$, "L")
  '
  ' Load sprite grid
  '
  offset = VARPTR(temp.grid%(0, 0, 0))
  DEF SEG = VARSEG(temp.grid%(0, 0, 0))
  length = ((sprite.size - 1) * 2) * ((sprite.size - 1) * 2) * ((num.sprites - 1) * 2)
  '
  BLOAD filename$ + ".grd", offset
  '
END SUB

SUB movecurs (dir$)
  '
  SHARED up$
  SHARED dn$
  SHARED lt$
  SHARED rt$
  SHARED x
  SHARED y
  SHARED sprite.num
  SHARED temp.grid%()
  SHARED col%
  SHARED pcp
  '
  LINE (x * grid.size, y * grid.size)-(x * grid.size + grid.size, y * grid.size + grid.size), temp.grid%(x, y, sprite.num), BF
  '                                                 
  LINE (x * grid.size, y * grid.size)-(x * grid.size + grid.size, y * grid.size + grid.size), 15, B
  '
  SELECT CASE dir$
    CASE up$
      y = y - 1: IF y < 0 THEN y = 0
    CASE dn$
      y = y + 1: IF y > sprite.size - 1 THEN y = sprite.size - 1
    CASE lt$
      x = x - 1: IF x < 0 THEN x = 0
    CASE rt$
      x = x + 1: IF x > sprite.size - 1 THEN x = sprite.size - 1
    CASE ELSE
  END SELECT
  '
  LINE (x * grid.size + 2, y * grid.size + 2)-(x * grid.size + grid.size - 2, y * grid.size + grid.size - 2), col%, BF
  '
  LINE (x * grid.size + 2, y * grid.size + 2)-(x * grid.size + grid.size - 2, y * grid.size + grid.size - 2), 15, B
  '
  IF pcp THEN PCOPY 1, 0
  '
END SUB

SUB savesprites
  '
  SHARED temp.grid%()
  SHARED sprite$()
  '
  CALL getfilename(filename$, "S")
  '
  ' Convert sprite grid data into BASIC DRAW statements...
  '
  FOR num = 0 TO num.sprites - 1
    '
    IF hotx <> 0 THEN
      sprite$(num) = "bm-" + STR$(hotx) + ",-" + STR$(hoty)
    ELSE
      sprite$(num) = ""
    END IF
    '
    oldx = 0: oldy = 0
    '
    FOR y = 0 TO sprite.size - 1
      '
      color1 = temp.grid%(0, y, num)
      '
      FOR x = 0 TO sprite.size - 1
        '
        color2 = temp.grid%(x, y, num)
        '
        IF color2 <> color1 THEN
          '
          dx = x - oldx: dy = y - oldy
          oldx = x: oldy = y
          '
          IF color1 = invisible.color THEN
            '
            sprite$(num) = sprite$(num) + "bm+" + STR$(dx) + ",+" + STR$(dy)
            '
          ELSE
            '
            sprite$(num) = sprite$(num) + "c" + STR$(color1) + "m+" + STR$(dx) + ",+" + STR$(dy)
            '
          END IF
          '
          color1 = color2
          '
        END IF
        '
      NEXT x
      '
      dx = x - oldx: dy = y - oldy
      oldx = 1: oldy = y + 1
      '
      IF color1 = invisible.color THEN
        '
        sprite$(num) = sprite$(num) + "bm+" + STR$(dx) + ",+" + STR$(dy)
        '
      ELSE
        '
        sprite$(num) = sprite$(num) + "c" + STR$(color1) + "m+" + STR$(dx) + ",+" + STR$(dy)
        '
      END IF
      '
      IF y < sprite.size - 1 THEN
        '
        sprite$(num) = sprite$(num) + "bm-" + STR$(sprite.size - 1) + ",+1"
        '
      END IF
      '
    NEXT y
    '
    strg.len = LEN(sprite$(num))
    temp.strg$ = ""
    FOR tt = 1 TO strg.len
      char$ = MID$(sprite$(num), tt, 1)
      IF char$ <> " " THEN
        temp.strg$ = temp.strg$ + char$
      END IF
    NEXT tt
    sprite$(num) = temp.strg$
  NEXT num
  '
  ' Save sprite grid
  '
  offset = VARPTR(temp.grid%(0, 0, 0))
  DEF SEG = VARSEG(temp.grid%(0, 0, 0))
  length = ((sprite.size - 1) * 2) * ((sprite.size - 1) * 2) * ((num.sprites - 1) * 2)
  '
  BSAVE filename$ + ".grd", offset, length
  '
  ' Save sprite DRAW statements
  '
  OPEN filename$ + ".drw" FOR OUTPUT AS #1
  '
  FOR num = 0 TO num.sprites - 1
    '
    WRITE #1, sprite$(num)
    '
  NEXT num
  '
  CLOSE #1
  '
END SUB

SUB setgrid
  '
  SHARED x
  SHARED y
  SHARED col%
  SHARED sprite.num
  SHARED temp.grid%()
  SHARED pcp
  '
  temp.grid%(x, y, sprite.num) = col%
  '
  PAINT (x * grid.size + 1, y * grid.size + 1), col%, 15
  CALL setsprite(x, y, col%)
  '
  IF pcp THEN PCOPY 1, 0
  '
END SUB

SUB setscreen (scr)
  '
  SHARED pcp
  SHARED offset
  SHARED divd
  '
  SELECT CASE scr
    CASE 1
    CASE 2
    CASE 3
    CASE 4
    CASE 5
    CASE 6
    CASE 7
      SCREEN 7, , 1, 0: pcp = 1: REM 320 x 200, 16 color
      offset = 32: divd = 8
    CASE 8
      SCREEN 8, , 1, 0: pcp = 1: REM 640 x 200, 16 color
      offset = 32: divd = 8
    CASE 9
      SCREEN 9, , 1, 0: pcp = 1: REM 640 x 350, 16 color
      offset = 16: divd = 8
    CASE 10
    CASE 11
    CASE 12
      SCREEN 12: pcp = 0: REM 640 x 480, 16 color
      offset = 48: divd = 16
    CASE 13
      SCREEN 13: pcp = 0: REM 320 x 200, 256 color
      offset = 32: divd = 8
    CASE ELSE
  END SELECT
  '
END SUB

SUB setsprite (x, y, col%)
  '
  PSET (sprite.size * grid.size + 20 + x, 20 + y), col%
  '
  LINE (sprite.size * grid.size + 20 - 2, 18)-(sprite.size * grid.size + 20 + sprite.size + 1, 20 + sprite.size + 1), 15, B
  '
END SUB


' Phobia here... This is my first "contribution" to ABC so ... errr...
' well... something... I'll just comment the source and hopefully
' most of you will understand it... There shouldn't be any problems
' if you've made any kind of demoeffects before.
' This is a little source I made in school... the computerlessons
' really suck!
' Feel free to comment or critisize (the source, not my bad english!),
' my email is:
'
' phobia2@hotmail.com
'
' I don't want anyone to "clean up" the source like someone (won't
' mention any names!) did with the flameeffect by Martin Lindhe...
' If you can't write new Putpixel and Getpixel routines,
' then you can't make it noticeable faster...
' One thing that could be made is a precalculated COS and SIN table...
' but since that's not the slow part I decided to use realtime COS and SIN...
'
' Check out my coming demo (it's made in C++... but check it out anyway!)...
' Send me an email and I'll send it to you as soon as it's finished.
'
' See ya in another source!
'
DECLARE SUB pal (c%, r%, g%, b%)
DECLARE SUB flam ()
SCREEN 13
xoff = 160              ' Center of the cube in x
yoff = 100              ' Same but in y
zoff = 200              ' Same but in z
xang = .05              ' Don't change these... they control the rotation.
yang = .05
zang = .15
points = 8              ' Amount of vertexes in the cube...
DIM cube(points * 3)    ' All coordinates of the cube
DIM scr(points * 2)     ' Coordinates of where the vertexes should be on the screen
PAINT (10, 10), 0       ' Errrr...
FOR a = 1 TO points * 3
   READ cube(a)
NEXT a

FOR a% = 0 TO 63               ' Change the palette, colors 0-48
   pal a% / 4, a%, 0, 0
   pal a% / 4 + 16, 63, a%, 0
   pal a% / 4 + 32, 63, 63, a%
NEXT

inc = 1    ' This is the current cube() pos the calculate


DO
' The following lines are matrixes (misspelled?), do NOT try to understand them...
REM ********** Rotate around z-axis **********
a = cube(inc)
b = cube(inc + 1)
a2 = COS(zang) * a - SIN(zang) * b
b2 = SIN(zang) * a + COS(zang) * b
cube(inc) = a2
cube(inc + 1) = b2
REM ********** Rotate around y-axis **********
a = cube(inc)
b = cube(inc + 2)
a2 = COS(yang) * a - SIN(yang) * b
b2 = SIN(yang) * a + COS(yang) * b
cube(inc) = a2
cube(inc + 2) = b2
REM ********** Rotate around x-axis **********
a = cube(inc + 1)
b = cube(inc + 2)
a2 = COS(xang) * a - SIN(xang) * b
b2 = SIN(xang) * a + COS(xang) * b
cube(inc + 1) = a2
cube(inc + 2) = b2
REM ***********************************

inc = inc + 3       ' Go to next x position

IF inc > points * 3 THEN
   inc = 1
   inc2 = 1
  
   WAIT &H3DA, 8        ' Wait retrace
   flam                 ' Make the flameeffect
   DO
      ' This do-loop calculates the screen coordinates of the cube...
      scr(inc2) = (cube(inc) * 256) / (cube(inc + 2) - zoff) + xoff
      scr(inc2 + 1) = (cube(inc + 1) * 256) / (cube(inc + 2) - zoff) + yoff
     
      vx = (cube(inc) * 256) / (cube(inc + 2) - zoff) + xoff
      vy = (cube(inc + 1) * 256) / (cube(inc + 2) - zoff) + yoff
     
      inc2 = inc2 + 2
      inc = inc + 3
   LOOP WHILE inc < (points * 3) + 1
   LINE (scr(1), scr(2))-(scr(3), scr(4)), 48
   LINE (scr(1), scr(2))-(scr(5), scr(6)), 48
   LINE (scr(13), scr(14))-(scr(3), scr(4)), 48
   LINE (scr(13), scr(14))-(scr(5), scr(6)), 48
   LINE (scr(5), scr(6))-(scr(9), scr(10)), 48
   LINE (scr(1), scr(2))-(scr(7), scr(8)), 48
   LINE (scr(11), scr(12))-(scr(7), scr(8)), 48
   LINE (scr(15), scr(16))-(scr(11), scr(12)), 48
   LINE (scr(15), scr(16))-(scr(13), scr(14)), 48
   LINE (scr(15), scr(16))-(scr(9), scr(10)), 48
   LINE (scr(3), scr(4))-(scr(11), scr(12)), 48
   LINE (scr(7), scr(8))-(scr(9), scr(10)), 48
   inc = 1
END IF
SELECT CASE INKEY$
   CASE CHR$(27)
      SCREEN 0: WIDTH 80, 25
      PRINT "Burning vector by Phobia 1996!"
      END
END SELECT
LOOP

REM *** Coordinates of the cube ***
REM  x   y  z
DATA 10,10,10
DATA 10,10,-10
DATA 10,-10,10
DATA -10,10,10
DATA -10,-10,10
DATA -10,10,-10
DATA 10,-10,-10
DATA -10,-10,-10

SUB flam
' This is how you make a REAL flameeffect ...
' (the one by Martin Lindhe is FAKE :-) (he's my friend, I wouldn't
' mention his flame if his wasn't) )
' It simply adds all colors around x,y into a variable
' and calculates average color, dec it by one and pset (x,y-1)
' Kinda simple...
FOR p3 = 50 TO 125
   FOR px3 = 138 TO 183
      pol3 = POINT(px3, p3)
      pol3 = pol3 + POINT(px3 + 1, p3)
      pol3 = pol3 + POINT(px3, p3 + 1)
      pol3 = pol3 + POINT(px3 + 1, p3 + 1)
      pol3 = pol3 + POINT(px3 - 1, p3)
      pol3 = pol3 + POINT(px3, p3 - 1)
      pol3 = pol3 + POINT(px3 - 1, p3 - 1)
      pol3 = pol3 + POINT(px3 + 1, p3 - 1)
      pol3 = pol3 + POINT(px3 - 1, p3 + 1)
      pol3 = INT(pol3 / 9 - 1)
      IF pol3 > 48 THEN pol3 = 48
      IF pol3 < 0 THEN pol3 = 0
      PSET (px3, p3 - 3), pol3
      PSET (px3 + 50, p3 - 3), pol3
  NEXT
NEXT
END SUB

SUB pal (c%, r%, g%, b%)
' This routine is MUCH faster than palette (like... 10000000 times faster :-)
' It uses the BIOS instead of whatever palette uses...
' I don't understand how they made all routines so f****** slow...
' Use it if you want to... give me credits if you want to!
' Lamers don't!
OUT &H3C7, c%
OUT &H3C9, r%
OUT &H3C9, g%
OUT &H3C9, b%
END SUB

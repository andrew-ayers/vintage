'*****************************************************************************
'*                                                                           *
'*  Vector Cube Rehash - Originally done by Phobia in 1996... Rehashed for   *
'*  speed and clarity by Andrew L. Ayers in 1997. Phobia, if you don't like  *
'*  it, well tough cookies!                                                  *
'*                                                                           *
'*****************************************************************************
'
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
'
' >> Why not? Afraid of retribution? Well here it is! I know I am kinda late
' >> with this version, but hey - I don't look at EVERY ABC packet, just most
' >> of them. And I gave credit where credit was due - to Martin Lindhe. My
' >> flametext routine was MUCH, much cleaner than Lindhe's, and fully
' >> commented. That was the routine that inspired me to do better with
' >> QuickBASIC and programming in general - The Blast! Library PROVES that I
' >> have come a long way. So, Phobia, you have given me a challenge - I WILL
' >> CLEAN THIS CODE - fully commented - so everyone can see YOU for the
' >> lamer you really are. Any comments I make will be prefixed by ">>".
' >> Here goes...
'
' If you can't write new Putpixel and Getpixel routines,
' then you can't make it noticeable faster...
'
' >> But it does help if you use INTEGER variables where possible!
'
' One thing that could be made is a precalculated COS and SIN table...
' but since that's not the slow part I decided to use realtime COS and SIN...
'
' Check out my coming demo (it's made in C++... but check it out anyway!)...
' Send me an email and I'll send it to you as soon as it's finished.
'
' See ya in another source!
'
DECLARE SUB pal (c%, r%, g%, B%)
DECLARE SUB flam ()
'
SCREEN 13
'
xoff% = 160              ' Center of the cube in x
yoff% = 100             ' Same but in y
zoff% = 200              ' Same but in z
'
xang = .15              ' Don't change these... they control the rotation.
yang = .1
zang = .05
'
points% = 8              ' Amount of vertexes in the cube...
points3% = points% * 3   '>> This is to eliminate the multiply
'
DIM cube(points% * 3)    ' All coordinates of the cube
DIM scr(points% * 2)     ' Coordinates of where the vertexes should be
                         ' on the screen
'
PAINT (10, 10), 0        ' Errrr...
'
FOR a% = 1 TO points3%
   READ value%
   cube(a%) = value% * 2
NEXT
'
' >> Hey look everyone! He got the integer portion right! Too bad it isn't in
' >> the flame rendering code...
'
FOR a% = 0 TO 63               ' Change the palette, colors 0-48
   pal a% / 4, a%, 0, 0
   pal a% / 4 + 16, 63, a%, 0
   pal a% / 4 + 32, 63, 63, a%
NEXT

inc% = 1    ' This is the current cube() pos the calculate >> Damn!

DO
  '
  ' The following lines are matrixes (misspelled?), do NOT try to
  ' understand them...
  '
  ' >> Why not? This is simple code after all... Actually the following code
  ' >> are not matrices - but matrix calculations for the X, Y, and Z coord-
  ' >> inate rotations - I will clean this code to make it easier to under-
  ' >> stand, for those of you afraid of 3D math...
  '
  xp = cube(inc%)
  yp = cube(inc% + 1)
  zp = cube(inc% + 2)
  '
  ' ********** Rotate around z-axis **********
  '
  rxp = COS(zang) * xp - SIN(zang) * yp
  ryp = SIN(zang) * xp + COS(zang) * yp
  '
  xp = rxp
  yp = ryp
  '
  ' ********** Rotate around y-axis **********
  '
  rxp = COS(yang) * xp - SIN(yang) * zp
  rzp = SIN(yang) * xp + COS(yang) * zp
  '
  xp = rxp
  zp = rzp
  '
  ' ********** Rotate around x-axis **********
  '
  ryp = COS(xang) * yp - SIN(xang) * zp
  rzp = SIN(xang) * yp + COS(xang) * zp
  '
  cube(inc%) = rxp
  cube(inc% + 1) = ryp
  cube(inc% + 2) = rzp
  '
  ' ***********************************
  '
  inc% = inc% + 3       ' Go to next x position >> (actually skip to next triple)
  '
  IF inc% > points3% THEN
    '
    inc% = 1: inc2% = 1
    '
    WAIT &H3DA, 8        ' Wait retrace
    '
    flam                 ' Make the flameeffect
    '
    DO
      '
      ' This do-loop calculates the screen coordinates of the cube...
      '
      ' >> Yes, that is what it does, but it actually does two things - a
      ' >> perspective to screen transform, then a screen translation to
      ' >> center the cube...
      '
      scr(inc2%) = (cube(inc%) * 256) / (cube(inc% + 2) - zoff%) + xoff%
      scr(inc2% + 1) = (cube(inc% + 1) * 256) / (cube(inc% + 2) - zoff%) + yoff%
      '
      '>> There was some lines here doing the same above and assigning the
      '>> result to vx and vy, but nothing was ever done with vx and vy...
      '
      inc2% = inc2% + 2
      inc% = inc% + 3
      '
    LOOP WHILE inc% < points3% + 1
    '
    '>> Draw Top
    '
    LINE (scr(1), scr(2))-(scr(3), scr(4)), 48
    LINE -(scr(5), scr(6)), 48
    LINE -(scr(7), scr(8)), 48
    LINE -(scr(1), scr(2)), 48
    '
    '>> Draw Bottom
    '
    LINE (scr(9), scr(10))-(scr(11), scr(12)), 48
    LINE -(scr(13), scr(14)), 48
    LINE -(scr(15), scr(16)), 48
    LINE -(scr(9), scr(10)), 48
    '
    '>> Draw Sides
    '
    LINE (scr(1), scr(2))-(scr(9), scr(10)), 48
    LINE (scr(3), scr(4))-(scr(11), scr(12)), 48
    LINE (scr(5), scr(6))-(scr(13), scr(14)), 48
    LINE (scr(7), scr(8))-(scr(15), scr(16)), 48
    '
    inc% = 1
    '
  END IF
  '
  IF INKEY$ = CHR$(27) THEN
    '
    SCREEN 0: WIDTH 80, 25
    '
    PRINT
    PRINT
    PRINT
    '
    FOR t% = 1 TO 8
      COLOR 15, 4: PRINT SPACE$(80)
    NEXT
    COLOR 15, 4: PRINT "                       Burning Vector Cube by Phobia 1996                       "
    COLOR 15, 4: PRINT SPACE$(80)
    COLOR 12, 4: PRINT "         Customized, rehashed, and IMPROVED - By Andrew L. Ayers 1997!          "
    FOR t% = 1 TO 8
      COLOR 15, 4: PRINT SPACE$(80)
    NEXT
    '
    COLOR 7, 0
    '
    a$ = INPUT$(1)
    '
    END
  END IF
  '
LOOP
'
' >> I reorganized the vertex data to make it more logical. Points are
' >> arranges in a counterclockwise manner, starting from rear left.
'
' *** Coordinates of the cube ***
' >> TOP
'      x   y   z
DATA -10, 10, 10
DATA -10, 10,-10
DATA  10, 10,-10
DATA  10, 10, 10
'>> BOTTOM
DATA -10,-10, 10
DATA -10,-10,-10
DATA  10,-10,-10
DATA  10,-10, 10

'
' >> Ok, well, that is about it - I was thinking about doing an assembler
' >> flame rendering routine for this, but I think that would be unfair to
' >> Phobia - even so, I have managed to speed it up by a great percentage,
' >> and it could probably be made even faster. An assembler flame renderer
' >> would do the most though, since most of the time is spent in that
' >> inner loop (perhaps just a horizontal renderer would do)... Phobia, I
' >> hope you aren't too angry with the liberties I have taken with your
' >> code - I would love to see your C++ demo, send me a copy, if you wish!
' >> As you can see, most of the changes I made with your code are integer
' >> related - on a fast machine (esp. one with a coprocessor), using floats
' >> probably doesn't matter. I know that more and more games (like DN3D and
' >> Quake) require you to have one. But in certain situations, integers are
' >> best (if only for memory usage reasons). So I hope there aren't any hard
' >> feelings. BTW, I hoped you learned something here about integers. I must
' >> say I learned some things from the 3D code you used that I didn't know
' >> before...
' >>
' >> L8r!

SUB flam
  ' This is how you make a REAL flameeffect ...
  ' (the one by Martin Lindhe is FAKE :-) (he's my friend, I wouldn't
  ' mention his flame if his wasn't) )
  '
  ' It simply adds all colors around x,y into a variable
  ' and calculates average color, dec it by one and pset (x,y-1)
  ' Kinda simple...
  '
  ' >> Actually, Martin's did very much the same thing, but added a little
  ' >> noise (albeit with RND, which is sorta slow) to make things interesting.
  ' >> I will concede that most of the flame effects I have read about use
  ' >> a pixel interpolation method like below. I have commented things out to
  ' >> do a 5 pixel interpolate, rather than an 8 pixel, to speed things up a
  ' >> bit more. To see the original 8 pixel effect, uncomment  the lines that
  ' >> are commented out below...
  '
  FOR py3% = 10 TO 148
    FOR px3% = 115 TO 206
      pol3% = POINT(px3%, py3%)
      pol3% = pol3% + POINT(px3% + 1, py3%)
      pol3% = pol3% + POINT(px3%, py3% + 1)
      pol3% = pol3% + POINT(px3% - 1, py3%)
      pol3% = pol3% + POINT(px3%, py3% - 1)
      '
      'pol3% = pol3% + POINT(px3% + 1, py3% + 1)
      'pol3% = pol3% + POINT(px3% - 1, py3% - 1)
      'pol3% = pol3% + POINT(px3% + 1, py3% - 1)
      'pol3% = pol3% + POINT(px3% - 1, py3% + 1)
      '
      'pol3% = pol3% \ 9 - 1 ' >> Using an INTEGER divide (rather than INT()) is MUCH faster - 9 pixel
      pol3% = pol3% \ 5 - 1 ' >> Using an INTEGER divide (rather than INT()) is MUCH faster - 5 pixel
      '
      IF pol3% > 48 THEN
        pol3% = 48
      ELSE
        IF pol3% < 0 THEN pol3% = 0
      END IF
      '
      PSET (px3%, py3% - 3), pol3%
      '
    NEXT
  NEXT
  '
END SUB

DEFINT A-Z
           ' >> WOW! A DEFINT to speed this routine along...
SUB pal (c%, r%, g%, B%)
  '
  ' This routine is MUCH faster than palette (like... 10000000 times faster :-)
  '
  ' It uses the BIOS instead of whatever palette uses...
  '
  ' >> No. It uses direct VGA port writes to shuffle the palette. NOT THE BIOS!
  '
  ' I don't understand how they made all routines so f****** slow...
  '
  ' >> Because they probably used the f****** BIOS!
  '
  ' Use it if you want to... give me credits if you want to!
  ' Lamers don't!
  '
  ' >> Are you claiming that you invented direct video port palette writing
  ' >> routines all by your lonesome? Because of the DEFINT as well as the
  ' >> use of integers for the palette routines, but NO WHERE ELSE - I think
  ' >> you may be appropriating code - I don't have a problem with this -
  ' >> just LEARN FROM IT - USE INTEGERS WHEREVER, WHENEVER POSSIBLE!
  '
  OUT &H3C7, c%
  OUT &H3C9, r%
  OUT &H3C9, g%
  OUT &H3C9, B%
  '
END SUB


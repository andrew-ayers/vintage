'
' Description : Color Bars - Pseudo 256 Color for Screen 12
' Written by  : Andrew L. Ayers
' Date        : 03/18/97
'
' You may use this routine in any manner you like, as long
' as you give credit in an appropriate manner. Have phun!
'
DECLARE SUB ColrBar (y1%, y2%, pal1%, pal2%)
'
SCREEN 12
'
LOCATE 1, 16: PRINT "Mode 12 - 640x480x16 - Pseudo 256 Color Display"
'
' Draw color bars - Intensity 0
'
start1% = 32: LINE (0, start1%)-(300, start1% + 63), 0, BF
'
CALL ColrBar(start1%, start1% + 15, &H8888, NOT (&H2222))
CALL ColrBar(start1% + 16, start1% + 31, &HAAAA, &HAAAA)
CALL ColrBar(start1% + 32, start1% + 47, NOT (&H8888), &H2222)
CALL ColrBar(start1% + 48, start1% + 63, &HFFFF, 0)
'
LOCATE 3, 42: PRINT "Shade0"
LOCATE 4, 42: PRINT "Shade1           Intensity"
LOCATE 5, 42: PRINT "Shade2               0"
LOCATE 6, 42: PRINT "Shade3"
'
' Draw color bars - Intensity 1
'
start1% = 112: LINE (0, start1%)-(300, start1% + 63), 8, BF
'
CALL ColrBar(start1%, start1% + 15, &H8888, NOT (&H2222))
CALL ColrBar(start1% + 16, start1% + 31, &HAAAA, &HAAAA)
CALL ColrBar(start1% + 32, start1% + 47, NOT (&H8888), &H2222)
CALL ColrBar(start1% + 48, start1% + 63, &HFFFF, 0)
'
LOCATE 8, 42: PRINT "Shade0"
LOCATE 9, 42: PRINT "Shade1           Intensity"
LOCATE 10, 42: PRINT "Shade2               1"
LOCATE 11, 42: PRINT "Shade3"
'
' Draw color bars - Intensity 2
'
start1% = 192: LINE (0, start1%)-(300, start1% + 63), 7, BF
'
CALL ColrBar(start1%, start1% + 15, &H8888, NOT (&H2222))
CALL ColrBar(start1% + 16, start1% + 31, &HAAAA, &HAAAA)
CALL ColrBar(start1% + 32, start1% + 47, NOT (&H8888), &H2222)
CALL ColrBar(start1% + 48, start1% + 63, &HFFFF, 0)
'
LOCATE 13, 42: PRINT "Shade0"
LOCATE 14, 42: PRINT "Shade1           Intensity"
LOCATE 15, 42: PRINT "Shade2               2"
LOCATE 16, 42: PRINT "Shade3"
'
' Draw color bars - Intensity 3
'
start1% = 272: LINE (0, start1%)-(300, start1% + 63), 15, BF
'
CALL ColrBar(start1%, start1% + 15, &H8888, NOT (&H2222))
CALL ColrBar(start1% + 16, start1% + 31, &HAAAA, &HAAAA)
CALL ColrBar(start1% + 32, start1% + 47, NOT (&H8888), &H2222)
CALL ColrBar(start1% + 48, start1% + 63, &HFFFF, 0)
'
LOCATE 18, 42: PRINT "Shade0"
LOCATE 19, 42: PRINT "Shade1           Intensity"
LOCATE 20, 42: PRINT "Shade2               3"
LOCATE 21, 42: PRINT "Shade3"
'
LOCATE 23, 1: PRINT "This is a demonstration of how to achieve a pseudo 256 color palette in the"
LOCATE 24, 1: PRINT "640x480x16 color (mode 12) screen. The concept used is called dithering, and"
LOCATE 25, 1: PRINT "can allow the user to create new colors by mixing existing colors together,"
LOCATE 26, 1: PRINT "using closely spaced pixels. I have chosen to show the dithering using the"
LOCATE 27, 1: PRINT "system colors grey scale (0,8,7,15), but nothing prevents you from mixing any"
LOCATE 28, 1: PRINT "other two colors, like red and yellow (for orange), or even using a totally"
LOCATE 29, 1: PRINT "different palette to create even more variations. Press any key to exit.";
'
a$ = INPUT$(1)
'
SCREEN 0: WIDTH 80: CLS

SUB ColrBar (y1%, y2%, pal1%, pal2%)
  '
  FOR t1% = 0 TO 15
    FOR t2% = t1% * 20 TO t1% * 20 + 20 STEP 2
      LINE (t2%, y1%)-(t2%, y2%), t1%, , pal1%
      LINE (t2% + 1, y1%)-(t2% + 1, y2%), t1%, , NOT (pal2%)
    NEXT
  NEXT
  '
END SUB


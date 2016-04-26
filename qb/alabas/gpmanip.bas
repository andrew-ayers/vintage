'
' Description : GET/PUT Manipulation Routines
' Written by  : Andrew L. Ayers
' Date        : 10/22/96
'
' These routines show how you can *directly* manipulate the GET/PUT buffer
' structure to build/destroy/read/write a sprite at any moment. You could
' display a sprite, modify the buffer, then display it again - changes and
' all! These routines are powerful! For a great demonstration, see the
' GetShift demo, which uses the same techniques to achieve a full (almost)
' screen scroll, with little overhead! For more info, read the comments
' within each routine...
'
' You may use this routine in any manner you like, as long
' as you give credit in an appropriate manner. Have phun!
'
DECLARE SUB GetSpriteData (xsize%, ysize%, x%, y%, num%, colr%)
DECLARE SUB SetSpriteData (xsize%, ysize%, x%, y%, num%, colr%)
'
DIM SHARED buffer%(1000)
'
SCREEN 13
'
' Play with these lines...
'
'LINE (1, 1)-(14, 14), 12, BF
'LINE (4, 4)-(11, 11), 4, BF
'GET (0, 0)-(15, 15), buffer%(0)
'
'LINE (1, 1)-(14, 14), 15, BF
'LINE (4, 4)-(11, 11), 3, BF
'LINE (6, 6)-(9, 9), 235, BF
'GET (0, 0)-(15, 15), buffer%(130)
'
'PUT (0, 0), buffer%(0), PSET
'CALL GetSpriteData(16, 16, 6, 6, 1, colr%) ' Get a color
'PRINT colr%
'
CLS
'
' Building a sprite from scratch!
'
CALL SetSpriteData(16, 16, -1, 0, 1, 128) ' Set the Width (16 x 8 = 128)
CALL SetSpriteData(16, 16, -3, 0, 1, 16)  ' Set the Height
'
FOR y% = 0 TO 15
  FOR x% = 0 TO 15
    CALL SetSpriteData(16, 16, x%, y%, 1, 12)  ' Data
  NEXT x%
NEXT y%
'
FOR y% = 5 TO 10
  FOR x% = 5 TO 10
    CALL SetSpriteData(16, 16, x%, y%, 1, 4)  ' Data
  NEXT x%
NEXT y%
'
' Use the *real* PUT command to place our sprite!
'
PUT (50, 50), buffer%(130), PSET

SUB GetSpriteData (xsize%, ysize%, x%, y%, num%, colr%)
  '
  ' xsize% = Width of sprite in pixels
  ' ysize% = Height of sprite in lines
  ' x%     = X position to "get", must be 0-(xsize%-1) (see note below)
  ' y%     = Y position to "get", must be 0-(ysize%-1) (see note below)
  ' num%   = Sprite number offset (0 if buffer only contains one image)
  ' colr%  = Color of pixel retrieved
  '
  ' **NOTE**
  ' Both x% and y% must be set to values between 0 and "size" - 1. The only
  ' time this rule is not enforced is for the following:
  '
  '   If x% is less than zero (0), and y% equals zero (0), then the following
  '   information may be obtained for values of x% :
  '
  '     Value  Information
  '     -----  -----------
  '      -1    Low byte of width information (divide by 8 to get true width)
  '      -2    High byte of width information
  '      -3    Low byte of height information (true height)
  '      -4    High byte of height information
  '
  '   I believe for large sprites (larger than 32 x 256 pixels), both high
  '   and low bytes will have info in them for width and height, and there-
  '   fore are true integer values (16 bit). In these cases, you will need
  '   to multiply the byte values out to get the 16 bit real value.
  '
  DEF SEG = VARSEG(buffer%(0))
  '
  spritewords% = ((xsize% * ysize%) / 2) + 2
  '
  IF x% < 0 AND y% = 0 THEN
    offset% = ABS(x%) - 1
  ELSE
    offset% = ABS(y%) * ysize% + ABS(x%) + 4
  END IF
  '
  colr% = PEEK(VARPTR(buffer%(num% * spritewords%)) + offset%)
  '
  DEF SEG
  '
END SUB

SUB SetSpriteData (xsize%, ysize%, x%, y%, num%, colr%)
  '
  ' xsize% = Width of sprite in pixels
  ' ysize% = Height of sprite in lines
  ' x%     = X position to "set", must be 0-(xsize%-1) (see note below)
  ' y%     = Y position to "set", must be 0-(ysize%-1) (see note below)
  ' num%   = Sprite number offset (0 if buffer only contains one image)
  ' colr%  = Color of pixel to set
  '
  ' **NOTE**
  ' Both x% and y% must be set to values between 0 and "size" - 1. The only
  ' time this rule is not enforced is for the following:
  '
  '   If x% is less than zero (0), and y% equals zero (0), then the following
  '   information may be modified for values of x% :
  '
  '     Value  Information
  '     -----  -----------
  '      -1    Low byte of width information (multiply by 8 to set true width)
  '      -2    High byte of width information
  '      -3    Low byte of height information (true height)
  '      -4    High byte of height information
  '
  '   I believe for large sprites (larger than 32 x 256 pixels), both high
  '   and low bytes will need to have info in them for width and height, and
  '   therefore the width/height values will need to be broken down from the
  '   integer value (16 bit)  to high and low byte values, then sent to this
  '   routine.
  '
  DEF SEG = VARSEG(buffer%(0))
  '
  spritewords% = ((xsize% * ysize%) / 2) + 2
  '
  IF x% < 0 AND y% = 0 THEN
    offset% = ABS(x%) - 1
  ELSE
    offset% = ABS(y%) * ysize% + ABS(x%) + 4
  END IF
  '
  POKE (VARPTR(buffer%(num% * spritewords%)) + offset%), colr%
  '
  DEF SEG
  '
END SUB


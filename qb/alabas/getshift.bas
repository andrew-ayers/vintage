'
' Description : GetShift - Mode 13 VGA Special Effect Routine
' Written by  : Andrew L. Ayers
' Date        : 09/10/96
'
' This shows off one hell of a way to achieve a very smooth and fast "scroll"
' type effect which may be useful in a game or demo. The technique I use to
' do the effect is to shift the data within the GET buffer, re-displaying it
' each time using the PSET operator. By keeping the buffer small, I can do
' the shifting relatively fast, then copy the shifted version over and over
' again on the screen to create a full screen scroll type effect!
'
' As always, if you use this in any of your creations, please consider your
' source and mention my name. Thanx, and have phun!
'
DIM a%(451), save%(24)
'
SCREEN 13
'
' Build our image
'
FOR t% = 15 TO 0 STEP -1
  '
  col% = 31 - (t% * 2)
  CIRCLE (15, 0), t%, col%
  PAINT (15, 0), col%
  '
  CIRCLE (15, 29), t%, col%
  PAINT (15, 29), col%
  '
  CIRCLE (0, 15), t%, col%
  PAINT (0, 15), col%
  '
  CIRCLE (29, 15), t%, col%
  PAINT (29, 15), col%
  '
NEXT
'
' Put it in the buffer
'
GET (0, 0)-(29, 29), a%
'
' The following is info you might find helpful
'
dbyte% = a%(0)
highbyte% = INT(dbyte% MOD 256)
lowbyte% = INT(dbyte% / 256)
wid% = INT(highbyte% / 8) ' Width of GET image in pixels
'
dbyte% = a%(1)
highbyte% = INT(dbyte% MOD 256)
lowbyte% = INT(dbyte% / 256)
hgt% = highbyte% ' Height of GET image in lines
'
' Clear off the screen
'
CLS
'
LOCATE 1, 3: PRINT "Smooth as Silk! - By Andrew L. Ayers"
'
DO
  '
  ' Shift the buffer up
  '
  FOR t% = 0 TO 14: save%(t%) = a%(2 + t%): NEXT ' Top line
  '
  ' Middle lines
  '
  FOR t% = 17 TO 451
    '
    a%(t% - 15) = a%(t%)
    '
  NEXT
  '
  FOR t% = 0 TO 14: a%(437 + t%) = save%(t%): NEXT ' Bottom line
  '
  ' Display it!
  '
  FOR y% = 0 TO 5
    '
    FOR x% = 0 TO 9
      '
      PUT (7 + x% * 30, 10 + y% * 30), a%, PSET
      '
    NEXT
    '
  NEXT
  '
LOOP UNTIL INKEY$ <> ""


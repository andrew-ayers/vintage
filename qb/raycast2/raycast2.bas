'****************************************************************************
'
' Program Name : RAYCAST2.BAS - Ray Casting with Texture Mapping for QB
' Developed By : Original PowerBasic Development by Wolfgang Bruske and
'                Thomas Gohel. QBASIC/QuickBasic 4.5 conversion by
'                Andrew L. Ayers.
' Date         : 04/23/97
' Comments     : I wish to thank Wolfgang Bruske and Thomas Gohel for their
'                support and advice, and especially for providing me with an
'                English translation for the PowerBasic Ray Casting clone.
'                Thomas - I am sorry none of your assembler code survived the
'                conversion - it wasn't needed due to the intrinsic routines
'                found within QB. I will say this, however: This conversion
'                doesn't come close to matching the speed and power of the
'                original PowerBasic Ray Caster. In the PB version, even the
'                texture mapping engine was done in BASIC! I had to code mine
'                in assembler in order to gain enough speed back! It presents
'                an excellent argument for moving over to PowerBasic. I plan
'                on doing so myself if I can get the funds.
'
'                Well, that is it! I hope you enjoy this program. Be sure to
'                check out the original code also!
'
'                ************************************************************
'                ! UPDATE ! UPDATE ! UPDATE ! UPDATE ! UPDATE ! UPDATE !
'                ************************************************************
'
'                August 20, 1998 - I have updated this code for the following
'                reasons:
'
'                      1) Added a way to save the view of the player to a PCX
'                         format file.
'
'                      2) Corrected the 7 pixel offset problem.
'
'                      3) Added two new programs to convert from PCX->DAT and
'                         from DAT->PCX, so that others may be able to modify
'                         the texture maps.
'
'                That is all - I hope you enjoy this new, "improved" version!
'
'****************************************************************************
'
' Declare subroutines
'
DECLARE SUB RayCaster (ViewAngle%)
DECLARE SUB InitAsm ()
DECLARE SUB TextMap ()
DECLARE SUB VLine (y1%, y2%, colour%)
DECLARE SUB LoadTextures (file$)
DECLARE SUB TableBuild ()
DECLARE SUB CreatePalette (Filename$)
DECLARE SUB LoadWorld (Filename$)
DECLARE SUB WriteScrn (row%, column%, colour%, Text$)
'
' Declare PCX Encode decode routines (by Andrew L. Ayers)
'
DECLARE SUB LoadPcx (tsegment%, file$)
DECLARE SUB SavePCX (fsegment%, file$)
'
TYPE PCXHeader
  Manufacturer AS STRING * 1    ' Set to 10
  Version      AS STRING * 1    ' Set to 5
  Encoding     AS STRING * 1    ' Set to 1
  BitsPerPixel AS STRING * 1    ' Set to 8
  XMin         AS INTEGER       ' Set to 0
  YMin         AS INTEGER       ' Set to 0
  XMax         AS INTEGER       ' Set to 319
  YMax         AS INTEGER       ' Set to 199
  HRes         AS INTEGER       ' Set to 320
  VRes         AS INTEGER       ' Set to 200
  ColorMap     AS STRING * 48   ' Set to NULL
  Reserved1    AS STRING * 1    ' Set to NULL
  NumPlanes    AS STRING * 1    ' Set to 1
  NumBytesLine AS INTEGER       ' Set to 320
  PaletteType  AS INTEGER       ' Set to 0
  Reserved2    AS STRING * 58   ' Set to NULL
END TYPE
'
' Define Global Variables
'
DIM SHARED Angle0%, Angle6%, Angle30%, Angle90%, Angle180%, Angle270%, ViewAngle%
DIM SHARED Angle360%, WorldRow%, WorldColumn%, CellXsize%, CellYsize%, InvCellXsize!, InvCellYsize!
DIM SHARED x%, y%, MaXX%, MaXY%
DIM SHARED StartX%, StartY%, TextureNumber%, TextureColumn%
DIM SHARED Ray%, Scale%, UpperEnd%, LowerEnd%
'
' Define global arrays
'
REDIM SHARED World%(16, 16)
REDIM SHARED texture%(32766)
REDIM SHARED TanTable!(1920)
REDIM SHARED InvTanTable!(1920)
REDIM SHARED YStep!(1920)
REDIM SHARED XStep!(1920)
REDIM SHARED CosTable!(1920)
REDIM SHARED InvCosTable!(1920)
REDIM SHARED InvSinTable!(1920)
REDIM SHARED code%(56)
'
MinDistance% = 32
Angle0% = 0
Angle6% = 30
Angle30% = 160
Angle90% = 480
Angle180% = 960
Angle270% = 1440
Angle360% = 1920
WorldRow% = 16
WorldColumn% = 16
CellXsize% = 64
CellYsize% = 64
'
' Initialize maximum world boundries (1024 x 1024 units)
'
MaXX% = (WorldColumn% * CellXsize%)
MaXY% = (WorldRow% * CellYsize%)
'
InvCellXsize! = 1 / CellXsize%
InvCellYsize! = 1 / CellYsize%
'
' Start *---> MAIN <---* loop
'
SCREEN 13
'
CALL InitAsm
'
CALL WriteScrn(1, 1, 160, "PB RayCasting Engine by Wolfgang Bruske")
CALL WriteScrn(2, 5, 120, "QB Conversion by Andrew L. Ayers")
CALL WriteScrn(24, 1, 160, "NumPad Moves, S=Snapshot, [ESC] to Exit")
'
CALL LoadWorld("MAP.DAT")
CALL LoadTextures("TEXTURE.DAT")
CALL TableBuild
'
' Assign player view location (in world coordinates)
'
x% = (StartX% * 64) + 32: y% = (StartY% * 64) + 32: ViewAngle% = Angle90%
'
DO
  DX! = 0: DY! = 0
  '
  ' Draw the world!
  '
  CALL RayCaster(ViewAngle%)
  '
  ' Get user input
  '
  DO: key$ = INKEY$: LOOP UNTIL key$ <> ""
  '
  SELECT CASE key$
    CASE "4"
      '
      ' Rotating left
      '
      ViewAngle% = ViewAngle% - Angle6%
      IF ViewAngle% < Angle0% THEN ViewAngle% = Angle360% + ViewAngle%
    CASE "6"
      '
      ' Rotating right
      '
      ViewAngle% = ViewAngle% + Angle6%
      IF ViewAngle% > Angle360% THEN ViewAngle% = ViewAngle% - Angle360%
    CASE "8"
      '
      ' Moving forward
      '
      DX! = COS(6.28 * ViewAngle% / Angle360%) * 15
      DY! = SIN(6.28 * ViewAngle% / Angle360%) * 15
    CASE "2"
      '
      ' Moving backward
      '
      DX! = -(COS(6.28 * ViewAngle% / Angle360%) * 15)
      DY! = -(SIN(6.28 * ViewAngle% / Angle360%) * 15)
    CASE CHR$(27)
      '
      ' Player has quit - exit
      '
      SCREEN 0: WIDTH 80: CLS : EXIT DO
    CASE "S", "s"
      '
      ' Player wants to take a "snapshot" of screen
      '
      CALL SavePCX(&HA000, "snapshot.pcx")
      BEEP
  END SELECT
  '
  ' Move the player
  '
  x% = x% + DX!
  y% = y% + DY!
  '
  ' Do collision detection with walls
  '
  XCell% = INT(x% / CellXsize%)
  YCell% = INT(y% / CellYsize%)
  XSubCell% = x% AND CellXsize% - 1
  YSubCell% = y% AND CellYsize% - 1
  '
  IF DX! > 0 THEN
    IF World%(XCell% + 1, YCell%) <> 0 AND XSubCell% > (CellXsize% - MinDistance%) THEN
      x% = x% - (XSubCell% - (CellXsize% - MinDistance%))
    END IF
  ELSE
    IF World%(XCell% - 1, YCell%) <> 0 AND XSubCell% < MinDistance% THEN
      x% = x% + (MinDistance% - XSubCell%)
    END IF
  END IF
  '
  IF DY! > 0 THEN
    IF World%(XCell%, (YCell% + 1)) <> 0 AND YSubCell% > (CellYsize% - MinDistance%) THEN
      y% = y% - (YSubCell% - (CellYsize% - MinDistance%))
    END IF
  ELSE
    IF World%(XCell%, (YCell% - 1)) <> 0 AND YSubCell% < MinDistance% THEN
      y% = y% + (MinDistance% - YSubCell%)
    END IF
  END IF
  '
LOOP
'
' Deallocate global arrays
'
REDIM SHARED World%(0, 0)
REDIM SHARED texture%(0)
REDIM SHARED TanTable!(0)
REDIM SHARED InvTanTable!(0)
REDIM SHARED YStep!(0)
REDIM SHARED XStep!(0)
REDIM SHARED CosTable!(0)
REDIM SHARED InvCosTable!(0)
REDIM SHARED InvSinTable!(0)
REDIM SHARED code%(0)
'
END

SUB InitAsm
  '
  ' Initialize Texture Mapping (TEXTMAP1.ASM) Assembler Routine
  '
  code$ = "1E5589E58B760CB106D3E689F3B102D3E601DE8B5E0E01DE8B5E1801DE8B4612"
  code$ = code$ + "BB0010F7E389C38B7E10B106D3E701DF037E1483EF0031DB8B4E0C83F914"
  code$ = code$ + "7C1481F9B4007F0E8B46168ED88A158B461A8ED8881483C3403B5E087E0647"
  code$ = code$ + "2B5E08EBF581C64001413B4E0A7ECF5D1FCA1400"
  '
  DEF SEG = VARSEG(code%(0))
  '
  FOR I% = 0 TO 112
    d% = VAL("&h" + MID$(code$, I% * 2 + 1, 2))
    POKE VARPTR(code%(0)) + I%, d%
  NEXT I%
  '
  DEF SEG
  '
END SUB

SUB LoadPcx (tsegment%, file$)
  '
  DIM Header AS PCXHeader, byte AS STRING * 1
  '
  filenum% = FREEFILE: OPEN file$ FOR BINARY AS filenum%
  '
  GET filenum%, , Header ' Get the header information
  '
  IF ASC(Header.Manufacturer) <> 10 AND ASC(Header.Version) <> 5 THEN
    PRINT "Not a valid PCX file!"
    EXIT SUB
  END IF
  '
  Wid = Header.XMax - Header.XMin + 1
  Dep = Header.YMax - Header.YMin + 1
  MaxCols% = 2 ^ ASC(Header.BitsPerPixel)
  '
  IF Wid <> 320 OR Dep <> 200 OR MaxCols% <> 256 THEN
    PRINT "Not a valid 320x200x256 color PCX file!"
    EXIT SUB
  END IF
  '
  SEEK 1, LOF(filenum%) - 767 ' Move to the start of the palette data
  '
  ' Load palette
  '
  FOR I% = 0 TO 255
    GET filenum%, , byte: red% = ASC(byte) \ 4
    GET filenum%, , byte: grn% = ASC(byte) \ 4
    GET filenum%, , byte: blu% = ASC(byte) \ 4
    '
    OUT &H3C8, I% ' Set to palette slot
    OUT &H3C9, red%
    OUT &H3C9, grn%
    OUT &H3C9, blu%
  NEXT
  '
  SEEK filenum%, 129 ' Skip the header information
  '
  DEF SEG = tsegment%
  '
  FOR y& = 0 TO 199
    FOR x& = 0 TO 319
      GET filenum%, , byte: ByteVal% = ASC(byte)
      '
      IF ByteVal% > 192 THEN
	'
	Rep% = ByteVal% - 193
	'
	GET filenum%, , byte: ByteVal% = ASC(byte)
	'
	FOR I% = 0 TO Rep%
	  POKE (y& * 320 + (x& + I%)), ByteVal%
	NEXT
	'
	x& = x& + Rep%
      ELSE
	POKE (y& * 320 + x&), ByteVal%
      END IF
    NEXT
  NEXT
  '
  DEF SEG
  '
  CLOSE filenum%
  '
END SUB

SUB LoadTextures (file$)
  '
  ' Load in texture maps
  '
  DEF SEG = VARSEG(texture%(0))
  '
  BLOAD file$, 0
  '
  ' Read the palette in
  '
  ii% = 0
  '
  FOR I% = 0 TO 255
    byte% = PEEK(64766 + ii%): red% = byte%: ii% = ii% + 1
    byte% = PEEK(64766 + ii%): grn% = byte%: ii% = ii% + 1
    byte% = PEEK(64766 + ii%): blu% = byte%: ii% = ii% + 1
    '
    OUT &H3C8, I%' Set to palette slot
    OUT &H3C9, red%
    OUT &H3C9, grn%
    OUT &H3C9, blu%
  NEXT
  '
  DEF SEG
  '
END SUB

SUB LoadWorld (Filename$)
  '
  ' Read in World Data Map
  '
  OPEN Filename$ FOR INPUT AS #1
  '
  FOR row% = 1 TO WorldRow%
    '
    LINE INPUT #1, Buffer$
    '
    FOR column% = 1 TO WorldColumn%
      '
      World%(column%, row%) = VAL(MID$(Buffer$, column%, 1))
      '
      IF MID$(Buffer$, column%, 1) = "A" THEN
	'
	' Set player starting location
	'
	StartX% = column%
	StartY% = row%
      END IF
      '
    NEXT
    '
  NEXT
  '
  CLOSE #1
  '
END SUB

SUB RayCaster (ViewAngle%)
  '
  ' Raycasting Routine - First define some local variables
  '
  DIM CellX%
  DIM CellY%
  DIM XonHorizontal AS SINGLE
  DIM YonVertical AS SINGLE
  DIM DistTOHorizontal AS SINGLE
  DIM DistTOVertical AS SINGLE
  '
  ' Define start sweep angle (View angle - 30 degrees)
  '
  VAngle% = ViewAngle% - Angle30%
  IF VAngle% < 0 THEN VAngle% = Angle360% + VAngle%
  '
  ' Find first horizontal grid lines
  '
  TempHorizontal% = INT(y% / CellYsize%) * CellYsize%
  TempHorizontal1% = INT(y% / CellYsize%) * CellYsize% + CellYsize%
  '
  ' Find first vertical grid lines
  '
  TempVertical% = INT(x% / CellXsize%) * CellXsize%
  TempVertical1% = INT(x% / CellXsize%) * CellXsize% + CellXsize%
  '
  ' Find distance to both horizontal grid lines
  '
  DiffTOHorizontal% = TempHorizontal% - y%
  DiffTOHorizontal1% = TempHorizontal1% - y%
  '
  ' Find distance to both vertical grid lines
  '
  DiffTOVertical% = TempVertical% - x%
  DiffTOVertical1% = TempVertical1% - x%
  '
  ' Cast out 320 rays (one for each vertical screen line)
  '
  FOR Ray% = 0 TO 319
    '
    ' Find horizontal/vertical intercepts based on which quadrant of a unit
    ' circle the ray is in...
    '
    IF VAngle% < Angle180% THEN
      Horizontal% = TempHorizontal1%
      XonHorizontal = InvTanTable!(VAngle%) * DiffTOHorizontal1% + x%
      NextHorizontal% = CellYsize%
      NextY% = 0
    ELSE
      Horizontal% = TempHorizontal%
      XonHorizontal = InvTanTable!(VAngle%) * DiffTOHorizontal% + x%
      NextHorizontal% = -CellYsize%
      NextY% = -1
    END IF
    '
    IF VAngle% < Angle90% OR VAngle% >= Angle270% THEN
      Vertical% = TempVertical1%
      YonVertical = TanTable!(VAngle%) * DiffTOVertical1% + y%
      NextVertical% = CellXsize%
      NextX% = 0
    ELSE
      Vertical% = TempVertical%
      YonVertical = TanTable!(VAngle%) * DiffTOVertical% + y%
      NextVertical% = -CellXsize%
      NextX% = -1
    END IF
    '
    ' Step thru horizontal intercepts until a wall is hit, or ray escapes
    '
    DO
      IF XonHorizontal > MaXX% OR XonHorizontal < 0 THEN
	DistTOHorizontal = 1000000!
	EXIT DO
      END IF
      CellX% = INT(XonHorizontal * InvCellXsize!)
      CellY% = INT(Horizontal% * InvCellYsize!) + NextY%
      IF World%(CellX%, CellY%) THEN
	DistTOHorizontal = (XonHorizontal - x%) * InvCosTable!(VAngle%)
	TextureNumberHorz% = World%(CellX%, CellY%)
	EXIT DO
      END IF
      XonHorizontal = XonHorizontal + XStep!(VAngle%)
      Horizontal% = Horizontal% + NextHorizontal%
    LOOP
    '
    ' Step thru vertical intercepts until a wall is hit, or ray escapes
    '
    DO
      IF YonVertical > MaXY% OR YonVertical < 0 THEN
	DistTOVertical = 1000000!
	EXIT DO
      END IF
      CellX% = INT(Vertical% * InvCellXsize!) + NextX%
      CellY% = INT(YonVertical * InvCellYsize!)
      IF World%(CellX%, CellY%) THEN
	TextureNumberVert% = World%(CellX%, CellY%) + 7
	DistTOVertical = (YonVertical - y%) * InvSinTable!(VAngle%)
	EXIT DO
      END IF
      YonVertical = YonVertical + YStep!(VAngle%)
      Vertical% = Vertical% + NextVertical%
    LOOP
    '
    ' Draw using closest intercept only
    '
    IF DistTOHorizontal < DistTOVertical THEN
      '
      ' Find correct texture strip scale
      '
      TextureNumber% = TextureNumberHorz%
      TextureColumn% = INT(XonHorizontal) MOD CellYsize%
      Scale% = CosTable!(Ray%) / DistTOHorizontal
      Scale% = Scale% - Scale% MOD 2
      '
      UpperEnd% = 100 - (Scale% \ 2)
      LowerEnd% = UpperEnd% + Scale%
      '
      ' Draw ceiling strip
      '
      IF UpperEnd% > 20 THEN CALL VLine(20, UpperEnd%, 36)
      '
      ' Draw wall strip
      '
      CALL TextMap
      '
      ' Draw floor strip
      '
      IF LowerEnd% < 180 THEN CALL VLine(LowerEnd%, 180, 215)
    ELSE
      '
      ' Find correct texture strip scale
      '
      TextureNumber% = TextureNumberVert%
      TextureColumn% = INT(YonVertical) MOD CellXsize%
      Scale% = CosTable!(Ray%) / DistTOVertical
      Scale% = Scale% - Scale% MOD 2
      '
      UpperEnd% = 100 - (Scale% \ 2)
      LowerEnd% = UpperEnd% + Scale%
      '
      ' Draw ceiling strip
      '
      IF UpperEnd% > 20 THEN CALL VLine(20, UpperEnd%, 36)
      '
      ' Draw wall strip
      '
      CALL TextMap
      '
      ' Draw floor strip
      '
      IF LowerEnd% < 180 THEN CALL VLine(LowerEnd%, 180, 215)
    END IF
    '
    ' Next angle for ray
    '
    VAngle% = VAngle% + 1
    '
    IF VAngle% >= Angle360% THEN VAngle% = 0
    '
  NEXT
  '
END SUB

SUB SavePCX (fsegment%, file$)
  '
  DIM Header AS PCXHeader, byte AS STRING * 1, LastByte AS STRING * 1
  '
  filenum% = FREEFILE: OPEN file$ FOR BINARY AS filenum%
  '
  ' Build the header and write it out
  '
  Header.Manufacturer = CHR$(10)
  Header.Version = CHR$(5)
  Header.Encoding = CHR$(1)
  Header.BitsPerPixel = CHR$(8)
  Header.XMin = 0
  Header.YMin = 0
  Header.XMax = 319
  Header.YMax = 199
  Header.HRes = 320
  Header.VRes = 200
  Header.ColorMap = STRING$(48, CHR$(0))
  Header.Reserved1 = CHR$(0)
  Header.NumPlanes = CHR$(1)
  Header.NumBytesLine = 320
  Header.PaletteType = 0
  Header.Reserved2 = STRING$(58, CHR$(0))
  '
  PUT filenum%, 1, Header
  '
  ' Write out the picture data
  '
  DEF SEG = fsegment%
  '
  pixel& = 0
  '
  WHILE pixel& < 64000
    '
    ' Get a run of pixels (all same color - maximum of 63 pixels in length)
    '
    count% = 0
    '
    WHILE (PEEK(pixel& + count%) = PEEK(pixel& + count% + 1)) AND ((pixel& + count%) < 320) AND (count% < 63)
      '
      count% = count% + 1
      '
    WEND
    '
    ' Do we have a run or a single pixel?
    '
    IF count% > 0 THEN
      '
      ' Byte run - write out number of bytes
      '
      byte = CHR$(count% OR 192)
      PUT filenum%, , byte
      '
      ' Write out color for bytes
      '
      byte = CHR$(PEEK(pixel&))
      PUT filenum%, , byte
      '
      ' Move the pointer forward
      '
      pixel& = pixel& + count%
      '
    ELSE
      '
      ' Single pixel
      '
      ' Color value checking/flag (so it doesn't look like a run to the
      ' PCX decoder)
      '
      IF (PEEK(pixel&) AND 192) = 192 THEN
	 '
	 byte = CHR$(193)
	 PUT filenum%, , byte
	 '
      END IF
      '
      ' Write out the byte
      '
      byte = CHR$(PEEK(pixel&))                ' Write Pixel colour.
      PUT filenum%, , byte
      '
      ' Move the pointer forward
      '
      pixel& = pixel& + 1
      '
    END IF
    '
  WEND
  '
  byte = CHR$(12): PUT filenum%, , byte
  '
  ' Write the palette out
  '
  FOR I% = 0 TO 255
    OUT &H3C7, I% ' Read the RGB values from slot
    '
    byte = CHR$(INP(&H3C9) * 4): PUT filenum%, , byte
    byte = CHR$(INP(&H3C9) * 4): PUT filenum%, , byte
    byte = CHR$(INP(&H3C9) * 4): PUT filenum%, , byte
    '
  NEXT
  '
  CLOSE filenum%
  '
  DEF SEG
  '
END SUB

SUB TableBuild
  '
  ' Precalculate global trig tables for later use to speed up processing
  '
  CALL WriteScrn(12, 3, 161, "Please Wait - Building Trig Tables...")
  '
  DIM RadAngle AS DOUBLE
  '
  FOR I% = Angle0% TO Angle360%
    '
    RadAngle = .0003272 + I% * 3.27249234791667D-03
    '
    TanTable!(I%) = TAN(RadAngle)
    '
    InvTanTable!(I%) = 1 / TanTable!(I%)
    '
    IF I% >= Angle0% AND I% < Angle180% THEN
      YStep!(I%) = ABS(TanTable!(I%) * CellYsize%)
    ELSE
      YStep!(I%) = -ABS(TanTable!(I%) * CellYsize%)
    END IF
    '
    IF I% >= Angle90% AND I% < Angle270% THEN
      XStep!(I%) = -ABS(InvTanTable!(I%) * CellXsize%)
    ELSE
      XStep!(I%) = ABS(InvTanTable!(I%) * CellXsize%)
    END IF
    '
    InvCosTable!(I%) = 1 / COS(RadAngle)
    InvSinTable!(I%) = 1 / SIN(RadAngle)
    '
  NEXT
  '
  FOR I% = -Angle30% TO Angle30%
    RadAngle = .0003272 + I% * 3.27249234791667D-03
    CosTable!(I% + Angle30%) = 1 / COS(RadAngle) * 12000
  NEXT
  '
END SUB

SUB TextMap
  '
  ' Call assembly raycasting routine
  '
  DEF SEG = VARSEG(code%(0))
  '
  CALL ABSOLUTE(BYVAL &HA000, BYVAL 0, BYVAL VARSEG(texture%(0)), BYVAL VARPTR(texture%(0)), BYVAL TextureNumber% - 1, BYVAL TextureColumn%, BYVAL Ray%, BYVAL UpperEnd%, BYVAL LowerEnd%, BYVAL Scale%, VARPTR(code%(0)))
  '
  DEF SEG
  '
END SUB

SUB VLine (y1%, y2%, col%)
  '
  ' Draw a vertical line using endpoints, ray number, and color
  '
  LINE (Ray%, y1%)-(Ray%, y2%), col%
  '
END SUB

SUB WriteScrn (row%, column%, colour%, Text$)
  '
  ' Write a string using color specified on screen
  '
  COLOR colour%: LOCATE row%, column%: PRINT Text$;
  '
END SUB


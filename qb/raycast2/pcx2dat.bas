'
' Convert a PCX file to a DAT file for the Raycaster Conversion
' by Andrew L. Ayers
'
DECLARE SUB LoadPCX (tsegment%, file$)
DECLARE SUB Convert2DAT (fsegment%, tsegment%, file$)
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
  PaletteType  AS INTEGER       ' Set to 1
  HScreenSize  AS INTEGER       ' Set to 320
  VScreenSize  AS INTEGER       ' Set to 200
  Reserved2    AS STRING * 54   ' Set to NULL
END TYPE
'
' Get file info from user
'
CLS
PRINT "Please input texture PCX image filename, leaving out extension."
PRINT "For example, if your file was TEXTURE.PCX, you would enter TEXTURE"
PRINT "instead..."
PRINT
PRINT "Input filename ([RETURN]=Exit) : ";
LINE INPUT file$: IF file$ = "" THEN END
'
' Allocate texture memory
'
REDIM SHARED texture%(32766)
'
SCREEN 13
'
CALL LoadPCX(&HA000, file$ + ".pcx")
'
CALL Convert2DAT(&HA000, VARSEG(texture%(0)), file$ + ".dat")
'
' Deallocate texture memory
'
REDIM SHARED texture%(0)
'
BEEP
'
SCREEN 13: SCREEN 0: WIDTH 80: CLS

SUB Convert2DAT (fsegment%, tsegment%, file$)
  '
  DIM byte AS STRING * 1
  '
  ' Convert from screen image (320 x 200) into a DAT image
  ' consisting of fifteen 64 x 64 textures...
  '
  FOR n& = 0 TO 14
    FOR x& = 0 TO 63
      FOR y& = 0 TO 63
        DEF SEG = fsegment%
        byte = CHR$(POINT((x& + ((n& MOD 5&) * 64&)), (y& + (INT(n& / 5) * 64&))))
        DEF SEG = tsegment%
        POKE (n& * 4096&) + (x& * 64&) + y&, ASC(byte)
        PSET ((x& + ((n& MOD 5&) * 64&)), (y& + (INT(n& / 5) * 64&))), 0
      NEXT
    NEXT
  NEXT
  '
  ' Write the palette out
  '
  ii% = 0
  '
  FOR i% = 0 TO 255
    OUT &H3C7, i% ' Read the RGB values from slot
    '
    byte = CHR$(INP(&H3C9)): POKE 64766 + ii%, ASC(byte): ii% = ii% + 1
    byte = CHR$(INP(&H3C9)): POKE 64766 + ii%, ASC(byte): ii% = ii% + 1
    byte = CHR$(INP(&H3C9)): POKE 64766 + ii%, ASC(byte): ii% = ii% + 1
    '
  NEXT
  '
  ' Save out DAT image
  '
  DEF SEG = tsegment%
  BSAVE file$, 0, 65535
  '
  DEF SEG
  '
END SUB

SUB LoadPCX (tsegment%, file$)
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
  FOR i% = 0 TO 255
    GET filenum%, , byte: red% = ASC(byte) \ 4
    GET filenum%, , byte: grn% = ASC(byte) \ 4
    GET filenum%, , byte: blu% = ASC(byte) \ 4
    '
    OUT &H3C8, i% ' Set to palette slot
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
        FOR i% = 0 TO Rep%
          POKE (y& * 320 + (x& + i%)), ByteVal%
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


'
' Convert a DAT file to a PCX file for the Raycaster Conversion
' by Andrew L. Ayers
'
DECLARE SUB LoadDAT (tsegment%, file$)
DECLARE SUB SavePCX (fsegment%, file$)
DECLARE SUB Convert2PCX (fsegment%, tsegment%, file$)
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
PRINT "Please input texture DAT image filename, leaving out extension."
PRINT "For example, if your file was TEXTURE.DAT, you would enter TEXTURE"
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
CALL LoadDAT(VARSEG(texture%(0)), file$ + ".dat")
'
CALL Convert2PCX(VARSEG(texture%(0)), &HA000, file$ + ".pcx")
'
' Deallocate texture memory
'
REDIM SHARED texture%(0)
'
BEEP
'
a$ = INPUT$(1)
'
SCREEN 13: SCREEN 0: WIDTH 80: CLS

SUB Convert2PCX (fsegment%, tsegment%, file$)
  '
  ' Convert from DAT image of fifteen 64 x 64 textures into a
  ' screen image of 320 x 200)...
  '
  DEF SEG = fsegment%
  '
  FOR n& = 0 TO 14
    FOR x& = 0 TO 63
      FOR y& = 0 TO 63
        colr% = PEEK((n& * 4096&) + (x& * 64&) + y&)
        PSET ((x& + ((n& MOD 5&) * 64&)), (y& + (INT(n& / 5) * 64&))), colr%
      NEXT
    NEXT
  NEXT
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
  ' Save as PCX image
  '
  CALL SavePCX(tsegment%, file$)
  '
END SUB

SUB LoadDAT (tsegment%, file$)
  '
  DEF SEG = tsegment%
  BLOAD file$, 0
  DEF SEG
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


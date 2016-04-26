'
' Decode PCX files by Andrew L. Ayers
'
DECLARE SUB LoadPCX (tsegment%, file$)
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
  PaletteType  AS INTEGER       ' Set to 1
  HScreenSize  AS INTEGER       ' Set to 320
  VScreenSize  AS INTEGER       ' Set to 200
  Reserved2    AS STRING * 54   ' Set to NULL
END TYPE
'
SCREEN 13: CLS
'
CALL LoadPCX(&HA000, "test1.pcx")
'
CALL SavePCX(&HA000, "test2.pcx")
'
CLS
'
CALL LoadPCX(&HA000, "test2.pcx")
'
a$ = INPUT$(1)
'
SCREEN 0: WIDTH 80: CLS

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


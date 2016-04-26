'$DYNAMIC
'
CLS
'
Avail = FRE(-1): PRINT "Memory Available : "; Avail; "Bytes"
'
SpriteSize% = 16: NumSprites% = 128
'
DIM SpriteBuffer1%(((((SpriteSize% + 1) * (SpriteSize% + 1)) / 2) + 2) * NumSprites%)
DIM SpriteBuffer2%(((((SpriteSize% + 1) * (SpriteSize% + 1)) / 2) + 2) * NumSprites%)
'
PRINT "Memory Allocated : "; Avail - FRE(-1); "Bytes"
PRINT "Memory Available : "; FRE(-1); "Bytes"


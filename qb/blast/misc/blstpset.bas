DIM buffer%(31999), code1%(29), code2%(43)
'
' Assembler code (code1) is as follows:
'
' 1E            PUSH    DS              ' Save the Data Segment
' 55            PUSH    BP              ' Save the Base Pointer
' 89E5          MOV     BP,SP           ' Get the Stack Pointer
' 8B460A        MOV     AX,[BP+0A]      ' Let AX=Buffer Segment Address
' 8ED8          MOV     DS,AX           ' Set the Data Segment=AX
' 8B7608        MOV     SI,[BP+08]      ' Let Source Index(SI)=Buffer Offset
' B800A0        MOV     AX,A000         ' Set AX=Start of Video (13h)
' 8EC0          MOV     ES,AX           ' Set the Extra Segment
' BF0000        MOV     DI,0000         ' Set the Destination Index to 0
' B9007D        MOV     CX,7D00         ' Number of words to copy (32000)
' F3A5          REP     MOVSW           ' Move the words!
' 5D            POP     BP              ' Reset the Base Pointer
' 1F            POP     DS              ' Reset the Data Segment
' CA0400        RETF    0004            ' Return to BASIC Program, clean up
'                                         stack...
'
' Assembler code (code2) is as follows:
'
code1$ = "1E5589E58B460A8ED88B7608B800A08EC0BF0000B9007DF3A55D1FCA0400"
code2$ = "1E5589E58B46108ED88B760AB106D3E689F3B102D3E601DE8B5E0C01DE8B5E0E01DE8A460888045D1FCA0A00"
'
' Now we poke the code into the memory reserved for it:
'
DEF SEG = VARSEG(code1%(0))
'
FOR i% = 0 TO 29
  d% = VAL("&h" + MID$(code1$, i% * 2 + 1, 2))
  POKE VARPTR(code1%(0)) + i%, d%
NEXT i%
'
DEF SEG
'
DEF SEG = VARSEG(code2%(0))
'
FOR i% = 0 TO 43
  d% = VAL("&h" + MID$(code2$, i% * 2 + 1, 2))
  POKE VARPTR(code2%(0)) + i%, d%
NEXT i%
'
DEF SEG
'
SCREEN 13
'
DO
  '
  ' First, clear out our buffer (kinda like cls 0)
  '
  ERASE buffer%
  '
  ' Set some random pixels using our new routine...
  '
  DEF SEG = VARSEG(code2%(0))
  FOR t% = 1 TO 300
    x% = INT(RND * 320)
    y% = INT(RND * 200)
    c% = INT(RND * 256)
    CALL ABSOLUTE(BYVAL VARSEG(buffer%(0)), BYVAL VARPTR(buffer%(0)), BYVAL x%, BYVAL y%, BYVAL c%, VARPTR(code2%(0)))
  NEXT t%
  DEF SEG
  '
  ' Blast it to screen!
  '
  DEF SEG = VARSEG(code1%(0))
  CALL ABSOLUTE(BYVAL VARSEG(buffer%(0)), BYVAL VARPTR(buffer%(0)), VARPTR(code1%(0)))
  DEF SEG
  '
LOOP UNTIL INKEY$ <> ""


DECLARE SUB WavePCSpeaker (file$, Volume%)
DECLARE SUB InitPCSpeaker ()
DECLARE SUB PlayPCSpeaker (Tone%, Length%, Volume%)
DECLARE SUB ResetPCSpeaker ()
'
' This must be called FIRST!
'
CALL InitPCSpeaker
'
' Note% and Length% must be between 0 and 32767. Volume must
' be between 0 and 63.
'
Note% = 32000: Length% = 100
'
'FOR Volume% = 0 TO 63
'  CALL PlayPCSpeaker(Note%, Length%, Volume%)
'NEXT
CALL WavePCSpeaker("c:\sb16\vocutil\glass.voc", 12)
'
' Call this when you are done to avoid strange side
' effects (like sounds playing continuously)...
'
CALL ResetPCSpeaker

SUB InitPCSpeaker
  '
  ' Reset Speaker
  '
  OUT &H43, &HB6
  OUT &H42, &HFF
  OUT &H42, 0
  '
  ' Place in "One Shot" Mode
  '
  OUT &H43, &H90
  OUT &H61, INP(&H61) OR 3
  '
END SUB

SUB PlayPCSpeaker (Tone%, Length%, Volume%)
  '
  ' Do error checking
  '
  IF Tone% < 0 THEN Tone% = 0
  IF Length% < 0 THEN Length% = 0
  IF Volume% < 0 THEN Volume% = 0
  IF Volume% > 63 THEN Volume% = 63
  '
  ' Toggle speaker at specified volume level
  '
  Port% = &H42: delay% = 32767 - Tone%
  '
  FOR t% = 0 TO Length%
    OUT Port%, Volume%
    FOR d% = 0 TO delay%: NEXT
  NEXT
  '
END SUB

SUB ResetPCSpeaker
  '
  ' Reset Speaker
  '
  OUT &H43, &HB6
  OUT &H61, INP(&H61) AND &HFC
  '
END SUB

SUB WavePCSpeaker (file$, Volume%)
  '
  OPEN file$ FOR BINARY AS #1
  '
  NumBytes& = LOF(1) \ 2                   ' Get size of WAV file in bytes
  '
  valu$ = SPACE$(&H7F00)
  '
  GET #1, , valu$
  '
  DEF SEG = VARSEG(valu$)
  offset% = SADD(valu$)
  '
  FOR t& = 1 TO &H7F00
    OUT &H42, (PEEK(offset% + t&) - 64) \ 8
    FOR dlay% = 1 TO 40: NEXT
  NEXT
  '
  CLOSE #1
  '
  DEF SEG
  '
END SUB


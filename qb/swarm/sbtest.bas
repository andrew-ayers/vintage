'
' Define SoundBlaster Subroutines
'
DECLARE SUB InitBlaster (eflag%)
DECLARE SUB WriteDSP (byte%)
DECLARE SUB SpeakerState (OnOff%)
DECLARE SUB DMAPlay (Segment&, Offset&, Length&, Freq&, eflag%)
DECLARE SUB GetBLASTER (DMA%, BasePort%, IRQ%, eflag%)
DECLARE SUB PlayWAV (filename AS STRING, Freq&, eflag%)
DECLARE SUB SetMasterVolume (Right%, Left%, Getvol%)
DECLARE SUB SetDMAState (StopGo%)
DECLARE SUB SetStereo (OnOff%)
DECLARE SUB WriteDAC (byte%)
'
' Define SoundBlaster Functions
'
DECLARE FUNCTION GetDMAState% ()
DECLARE FUNCTION GetDSPVersion! ()
DECLARE FUNCTION ReadDAC% ()
DECLARE FUNCTION ReadDSP% ()
DECLARE FUNCTION ResetDSP% ()
'
COMMON SHARED BasePort%, LenPort%, Channel%
'
SCREEN 0: WIDTH 80: CLS
'
PRINT "SoundBlaster Test Utility - Version 1.0"
PRINT "Copyright (C) 1997 by Andrew L. Ayers"
PRINT
'
' Initialize SoundBlaster - if there are any errors, beeps will sound
' indicating type of error:
'
'   1 Beep  - BLASTER environment variable not set
'   2 Beeps - Invalid DMA channel assignment (0-3 only)
'
' eflag% is set to a number 1 or 2 to indicate error type.
'
PRINT "Initializing SoundBlaster..."
PRINT
'
CALL InitBlaster(eflag%)
'
IF eflag% = 0 THEN
  '
  ' Everything is ok, so play a sound!
  '
  PRINT "Testing 1...2...3..."
  PRINT
  '
  CALL PlayWAV("test123.wav", 8000, eflag%)
  '
  SLEEP 3
  '
END IF
'
SELECT CASE eflag%
  CASE 1
    PRINT "BLASTER environment variable is not set up in AUTOEXEC.BAT."
    PRINT "Please correct and rerun this program..."
    PRINT
  CASE 2
    PRINT "DMA channel setting invalid in BLASTER environment variable."
    PRINT "Please correct and rerun this program..."
    PRINT
  CASE 3
    PRINT "Invalid DSP Version - Need DSP version 3.x or higher in order"
    PRINT "to play WAV files with sample rates greater than 23000 Hz..."
    PRINT
END SELECT
'
PRINT "Done!"
PRINT
PRINT "If you have any error messages showing, please correct and rerun"
PRINT "this program to test again."
PRINT

SUB DMAPlay (Segment&, Offset&, Length&, Freq&, eflag%)
  '
  DO: LOOP UNTIL ResetDSP%
  '
  CALL SpeakerState(1) 'Turn the speaker on
  '
  ' Transfers and plays the contents of the buffer.
  '
  eflag% = 0
  Length& = Length& - 1
  Page% = 0
  MemLoc& = Segment& * 16 + Offset&
  '
  SELECT CASE Channel%
    CASE 0
      PgPort% = &H87
      AddPort% = &H0
      LenPort% = &H1
      ModeReg% = &H48
    CASE 1
      PgPort% = &H83
      AddPort% = &H2
      LenPort% = &H3
      ModeReg% = &H49
    CASE 2
      PgPort% = &H81
      AddPort% = &H4
      LenPort% = &H5
      ModeReg% = &H4A
    CASE 3
      PgPort% = &H82
      AddPort% = &H6
      LenPort% = &H7
      ModeReg% = &H4B
    CASE ELSE
      EXIT SUB
  END SELECT
  '
  OUT &HA, &H4 + Channel%
  OUT &HC, &H0
  OUT &HB, ModeReg%
  OUT AddPort%, MemLoc& AND &HFF
  OUT AddPort%, (MemLoc& AND &HFFFF&) \ &H100
  '
  IF (MemLoc& AND 65536) THEN Page% = Page% + 1
  IF (MemLoc& AND 131072) THEN Page% = Page% + 2
  IF (MemLoc& AND 262144) THEN Page% = Page% + 4
  IF (MemLoc& AND 524288) THEN Page% = Page% + 8
  '
  OUT PgPort%, Page%
  OUT LenPort%, Length& AND &HFF
  OUT LenPort%, (Length& AND &HFFFF&) \ &H100
  OUT &HA, Channel%
  '
  IF Freq& < 23000 THEN
    TimeConst% = 256 - 1000000 \ Freq&
    WriteDSP &H40
    WriteDSP TimeConst%
    WriteDSP &H14
    WriteDSP (Length& AND &HFF)
    WriteDSP ((Length& AND &HFFFF&) \ &H100)
  ELSE
    IF GetDSPVersion! >= 3 THEN
      '
      ' Need DSP to be 3.x or higher for high sample rate
      '
      TimeConst% = ((65536 - 256000000 \ Freq&) AND &HFFFF&) \ &H100
      '
      WriteDSP &H40
      WriteDSP TimeConst%
      WriteDSP (Length& AND &HFF)
      WriteDSP ((Length& AND &HFFFF&) \ &H100)
      WriteDSP &H91
    ELSE
      eflag% = 3
    END IF
  END IF
  '
END SUB

SUB GetBLASTER (DMA%, BasePort%, IRQ%, eflag%)
  '
  ' This subroutine parses the BLASTER environment string and returns settings.
  '
  IF LEN(ENVIRON$("BLASTER")) = 0 THEN eflag% = 1: EXIT SUB
  '
  FOR Length% = 1 TO LEN(ENVIRON$("BLASTER"))
    '
    SELECT CASE MID$(ENVIRON$("BLASTER"), Length%, 1)
      CASE "A"
        BasePort% = VAL("&H" + MID$(ENVIRON$("BLASTER"), Length% + 1, 3))
      CASE "I"
        IRQ% = VAL(MID$(ENVIRON$("BLASTER"), Length% + 1, 1))
      CASE "D"
        DMA% = VAL(MID$(ENVIRON$("BLASTER"), Length% + 1, 1))
        '
        IF DMA% < 0 OR DMA% > 3 THEN eflag% = 2
        '
    END SELECT
    '
  NEXT
  '
END SUB

FUNCTION GetDMAState%
  '
  Count% = INP(LenPort%)
  Count2% = INP(LenPort%)
  Count& = CLNG(Count% + 1) * CLNG(Count2% + 1)
  '
  IF (Count& - 1) >= &HFFFF& THEN
    junk% = INP(DSPDataAvail%): GetDMAState% = -1
  END IF
  '
END FUNCTION

FUNCTION GetDSPVersion!
  '
  ' Gets the DSP version.
  '
  WriteDSP &HE1
  '
  Temp% = ReadDSP%
  '
  Temp2% = ReadDSP%
  '
  GetDSPVersion! = VAL(STR$(Temp%) + "." + STR$(Temp2%))
  '
END FUNCTION

SUB InitBlaster (eflag%)
  '
  eflag% = 0
  '
  CALL GetBLASTER(Channel%, BasePort%, IRQ%, eflag%) ' Parses BLASTER environment
  '
  IF eflag% = 0 THEN
    '
    DO: LOOP UNTIL ResetDSP%
    '
    CALL SetMasterVolume(15, 15, 0) ' This cranks the master volume all the way up.
    '
  END IF
  '
  FOR t% = 1 TO eflag%: BEEP: NEXT
  '
END SUB

SUB PlayWAV (filename AS STRING, Freq&, eflag%)
  '
  REDIM WavBuffer(1 TO 1) AS STRING * 16383 ' Make a 16k buffer for file.
  '
  f% = FREEFILE
  '
  OPEN filename FOR BINARY AS f%
  '
  GET #1, 64, WavBuffer(1) ' Get 16k from file (skip header on WAV)
  '
  Length& = LOF(1) - 64
  '
  IF Length& > 16383 THEN Length& = 16383 ' Adjust length if needed to 16k
  '
  DMAPlay VARSEG(WavBuffer(1)), VARPTR(WavBuffer(1)), Length&, Freq&, eflag%
  '
  CLOSE f%
  '
END SUB

FUNCTION ReadDAC%
  '
  ' Reads a byte from the DAC.
  '
  WriteDSP &H20
  ReadDAC% = ReadDSP%
  '
END FUNCTION

FUNCTION ReadDSP%
  '
  ' Reads a byte from the DSP
  '
  DO: LOOP UNTIL INP(BasePort% + 14) AND &H80
  '
  ReadDSP% = INP(BasePort% + 10)
  '
END FUNCTION

FUNCTION ResetDSP%
  '
  ' Resets the DSP
  '
  OUT BasePort% + 6, 1
  '
  FOR Count% = 1 TO 4
    junk% = INP(BasePort% + 6)
  NEXT
  '
  OUT BasePort% + 6, 0
  '
  IF INP(BasePort% + 14) AND &H80 = &H80 AND INP(BasePort% + 10) = &HAA THEN
    ResetDSP% = -1
  ELSE
    ResetDSP% = 0
  END IF
  '
END FUNCTION

SUB SetDMAState (StopGo%)
  '
  ' Stops or continues DMA play.
  '
  IF StopGo% THEN WriteDSP &HD4 ELSE WriteDSP &HD0
  '
END SUB

SUB SetMasterVolume (Right%, Left%, Getvol%)
  '
  OUT BasePort% + 4, &H22
  '
  IF Getvol% THEN
    Left% = INP(BasePort% + 5) \ 16
    Right% = INP(BasePort% + 5) AND &HF
  ELSE
    OUT BasePort% + 5, (Right% + Left% * 16) AND &HFF
  END IF
  '
END SUB

SUB SetStereo (OnOff%)
  '
  OUT BasePort% + 4, &HE
  '
  IF OnOff% THEN OUT BasePort% + 5, 2 ELSE OUT BasePort% + 5, 0
  '
END SUB

SUB SpeakerState (OnOff%)
  '
  ' Turns speaker on or off.
  '
  IF OnOff% THEN WriteDSP &HD1 ELSE WriteDSP &HD3
  '
END SUB

SUB WriteDAC (byte%)
  '
  ' Writes a byte to the DAC.
  '
  WriteDSP &H10
  WriteDSP byte%
  '
END SUB

SUB WriteDSP (byte%)
  '
  ' Writes a byte to the DSP
  '
  DO: LOOP WHILE INP(BasePort% + 12) AND &H80
  '
  OUT BasePort% + 12, byte%
  '
END SUB


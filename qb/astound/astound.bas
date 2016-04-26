'****************************************************************************
'
' Description : Astound! Library - Custom sound routines for the PC speaker,
'               LPT DAC, and SoundBlaster.
' Written by  : Copyright (c) 1997 by Andrew L. Ayers
' Date        : 02/25/97
' Comments    : These routines were devised for the purpose of providing an
'               easy to use interface to a variety of sound devices on the PC
'               for QBasic and QuickBasic programmers. Of particular interest
'               is the SoundBlaster routine, which uses a buffer and DMA
'               transfer to play the sound in the background. All of the rou-
'               tines involved in the SoundBlaster portion were pulled from
'               Wormhole by Brent P. Newhall. I don't know where he got them,
'               but I assume because he put them in the PD, they are PD rou-
'               tines. They have only been modified a bit by myself, mostly
'               to homogenize them with the other routines for the PC speaker
'               and LPT DAC.
'
'****************************************************************************
'
' Define general routines
'
DECLARE SUB InitSoundDevice (device%, eflag%)
DECLARE SUB PlaySoundDevice (device%, note%, length!, lvolume%, rvolume%)
DECLARE SUB WaveSoundDevice (device%, file$, lvolume%, rvolume%)
DECLARE SUB ResetSoundDevice (device%)
'
' Define general PC speaker routines
'
DECLARE SUB InitPCSpeaker (eflag%)
DECLARE SUB GetSPEAKER (eflag%)
DECLARE SUB PlayPCSpeaker (note%, length&, volume%)
DECLARE SUB WavePCSpeaker (file$)
DECLARE SUB ResetPCSpeaker ()
'
' Define general LPT DAC routines
'
DECLARE SUB InitDAC (eflag%)
DECLARE SUB GetDACLPT (eflag%)
DECLARE SUB PlayDAC (note%, length&, volume%)
DECLARE SUB WaveDAC (file$, volume%)
DECLARE SUB ResetDAC ()
'
' Define general SoundBlaster routines
'
DECLARE SUB InitBlaster (eflag%)
DECLARE SUB GetBLASTER (eflag%)
DECLARE SUB PlayBlaster (note%, length&, lvolume%, rvolume%)
DECLARE SUB WaveBlaster (file$, lvolume%, rvolume%)
DECLARE SUB ResetBlaster ()
'
' Define SoundBlaster support routines
'
DECLARE SUB WriteDSP (byte%)
DECLARE SUB SpeakerState (OnOff%)
DECLARE SUB DMAPlay (Segment&, Offset&, length&)
DECLARE SUB DMAWave (Segment&, Offset&, length&)
DECLARE SUB SetMasterVolume (Right%, Left%, Getvol%)
DECLARE SUB SetDMAState (StopGo%)
DECLARE SUB SetStereo (OnOff%)
DECLARE SUB WriteSBDAC (byte%)
'
' Define SoundBlaster support functions
'
DECLARE FUNCTION GetDMAState% ()
DECLARE FUNCTION GetDSPVersion! ()
DECLARE FUNCTION ReadSBDAC% ()
DECLARE FUNCTION ReadDSP% ()
DECLARE FUNCTION ResetDSP% ()
'
' Define common global variables
'
COMMON SHARED BasePort%, LenPort%, Channel%, bias%, IRQ%
COMMON SHARED WaveBufferSeg&, WaveBufferOff&, WaveBufferLen&
'
' SoundBlaster Constants
'
CONST BaseWaveFreq& = 8000
CONST BasePlayFreq& = 22050
'
'****************************************************************************
'
' Before using Astound!, the following environment variables must be set by
' your AUTOEXEC.BAT at bootup time:
'
'   BLASTER - Environment variable for the SoundBlaster.
'   SPEAKER - Environment variable for the 6 bit PC speaker.
'   DACLPT  - Environment variable for the 8 bit printer port DAC.
'
' Each of these must be set properly by AUTOEXEC.BAT in order for the sound
' devices referenced by them to work properly.
'
'   BLASTER - Refer to the manual that came with your SoundBlaster card for
'             complete info. Essentially you will need to have the Base Port
'             address set, the IRQ set, and the DMA channel set (0-3).
'
'   SPEAKER - Needs to be set as follows:
'
'               SET SPEAKER = A(address) B(bias)
'
'               address should be set to 42, while bias may be any number
'               between 0-32767. The bias is set for each particular machine
'               and is used to slow down data being sent to the PC speaker,
'               so that WAV files and other sounds are in "tune".
'
'               Example:
'
'                 SET SPEAKER = A42 B1200
'
'   LPTDAC  - Needs to be set as follows:
'
'               SET LPTDAC = A(address) B(bias)
'
'               address should be set to a valid LPT base port address. The
'               following base ports can be used. These are the standard LPT
'               ports, but you may have others:
'
'               LPT  Port Base Address
'               ---  -----------------
'                1   &H378
'                2   &H278
'                3   &H3BC
'                4   &H37B
'
'                The bias may be any number between 0-32767. The bias is set
'                for each particular machine and is used to slow down data
'                being sent to the LPT DAC, so that WAV files and other
'                sounds are in "tune".
'
'                  SET LPTDAC = A378 B1200
'
'****************************************************************************
'
' First, call the initialization procedure:
'
' The parameter passed (device%) must be one of
' the following:
'
'   0 = No Sound
'   1 = PC Speaker (Normal)
'   2 = LPT DAC (Printer D/A Convertor)
'   3 = SoundBlaster (DSP Version 3.x+)
'   4 = PC Speaker (6 Bit Mode)
'
device% = 3
'
CALL InitSoundDevice(device%, eflag%)
'
' When InitSoundDevice is executed, the Sound Device will either be set
' up properly based on which Sound Device it is, as well as the settings
' defined in the environment variables (see above), or an error will have
' occurred, in which case beeps will sound, the number of which can be
' used to determine what the error is, based on the Sound Device being
' initialized. A call to the routine ShowError can be made to show a
' text description of what the error was also. If an error occurs - do NOT
' proceed any further with the routines - eflag% will be set to a number
' greater than 0 for the error that has occurred, and can be used to cause
' your program to branch accordingly.
'
' The following table lists the error numbers and what they mean, as returned
' by eflag% and sounded in the beeps:
'
' Beeps/eflag%  Description
' ------------  -----------------------------------------------------------
'      0        No error has occurred (all devices).
'      1        Missing environment variable in AUTOEXEC.BAT (all devices).
'               AUTOEXEC.BAT must set the following environment variables
'               in order to fully support all devices (see above):
'
'                 BLASTER, SPEAKER, and DACLPT
'
'      2        Invalid DMA channel (Astound! supports DMA channels 0-3
'               only).
'      3        Invalid DSP version (Astound! supports DSP 3.x+ only).
'
IF eflag% = 0 THEN
  '
  ' The following routine allows you to play a note, similar
  ' to the SOUND statement:
  '
  ' device% must be one of the following:
  '
  '   0 = No Sound
  '   1 = PC Speaker (Normal)
  '   2 = LPT DAC (Printer D/A Convertor)
  '   3 = SoundBlaster (DSP Version 3.x+)
  '   4 = PC Speaker (6 Bit Mode)
  '
  ' note% must be between 37 and 32767.
  ' length& must be between 0 and 32767.
  '
  ' For the LPT DAC, lvolume% must be between 0 and 255.
  ' For the SoundBlaster, lvolume%/rvolume% must be between 0 and 15.
  ' For the PC speaker (6 bit), lvolume% must be between 0 and 63.
  '
  ' If the sound device is the LPT DAC (2) or the 6 bit PC speaker (4),
  ' only lvolume% needs to be set. If the sound device is the normal PC
  ' speaker (1), neither need to be set, as it isn't used.
  '
  ' Helpful tip: Setting length! to a small decimal can yield suprising
  '              results if the decimal is small enough and your machine is
  '              fast enough. This only works with the normal PC speaker (1)
  '              sound device though. Stick the call in a loop. Try a length
  '              of (.1). Experiment... Everything from sirens to water drip
  '              sounds can be made!
  '
  device% = 3: note% = 1537: length! = 12228: lvolume% = 15: rvolume% = 15
  '
  CALL PlaySoundDevice(device%, note%, length!, lvolume%, rvolume%)
  '
  ' The following routine allows you to play a WAV file:
  '
  ' device% must be one of the following:
  '
  '   0 = No Sound
  '   1 = PC Speaker (Normal)
  '   2 = LPT DAC (Printer D/A Convertor)
  '   3 = SoundBlaster (DSP Version 3.x+)
  '   4 = PC Speaker (6 Bit Mode)
  '
  ' file$ must be a WAV file in the current directory.
  '
  ' For the LPT DAC, lvolume% must be between 0 and 255.
  ' For the SoundBlaster, lvolume%/rvolume% must be between 0 and 15.
  '
  ' For the PC speaker (normal or 6 bit), lvolume% does not need to be set.
  ' The WAV file will be played at its maximum volume level. This is because
  ' the PC speaker has problems playing a WAV file at a lower volume level,
  ' distorting it. If anyone knows a way around this (I suspect an assembler
  ' routine might fix it), go for it, and let me know. Thanks!
  '
  'device% = 3: file$ = "test123.wav": lvolume% = 15: rvolume% = 15
  '
  'CALL WaveSoundDevice(device%, file$, lvolume%, rvolume%)
  '
END IF
'
' After you are finished using a Sound Device (generally at the end of your
' program, NOT after playing the sound), you need to call the reset routine.
' The reset routine is mainly for the 6 bit PC speaker Sound Device, due to
' its nature to sometimes lock and play a sound continuously. It is the only
' Sound Device you will want to reset AFTER playing the sound, due to this
' problem. The problem occurs only occasionally, due to reasons involving
' the hardware.
'
' device% must be one of the following:
'
'   0 = No Sound
'   1 = PC Speaker (Normal)
'   2 = LPT DAC (Printer D/A Convertor)
'   3 = SoundBlaster (DSP Version 3.x+)
'   4 = PC Speaker (6 Bit Mode)
'
device% = 3
'
CALL ResetSoundDevice(device%)

SUB DMAPlay (Segment&, Offset&, length&)
  '
  ' Transfers and plays the contents of the buffer (play frequency base).
  '
  length& = length& - 1
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
  OUT LenPort%, length& AND &HFF
  OUT LenPort%, (length& AND &HFFFF&) \ &H100
  OUT &HA, Channel%
  '
  IF FREQ& < 23000 THEN
    TimeConst% = 256 - 1000000 \ BasePlayFreq&
    CALL WriteDSP(&H40)
    CALL WriteDSP(TimeConst%)
    CALL WriteDSP(&H14)
    CALL WriteDSP(length& AND &HFF)
    CALL WriteDSP((length& AND &HFFFF&) \ &H100)
  ELSE
    TimeConst% = ((65536 - 256000000 \ BasePlayFreq&) AND &HFFFF&) \ &H100
    '
    CALL WriteDSP(&H40)
    CALL WriteDSP(TimeConst%)
    CALL WriteDSP(length& AND &HFF)
    CALL WriteDSP((length& AND &HFFFF&) \ &H100)
    CALL WriteDSP(&H91)
  END IF
  '
END SUB

SUB DMAWave (Segment&, Offset&, length&)
  '
  ' Transfers and plays the contents of the buffer (wave frequency base).
  '
  length& = length& - 1
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
  OUT LenPort%, length& AND &HFF
  OUT LenPort%, (length& AND &HFFFF&) \ &H100
  OUT &HA, Channel%
  '
  IF FREQ& < 23000 THEN
    TimeConst% = 256 - 1000000 \ BaseWaveFreq&
    CALL WriteDSP(&H40)
    CALL WriteDSP(TimeConst%)
    CALL WriteDSP(&H14)
    CALL WriteDSP(length& AND &HFF)
    CALL WriteDSP((length& AND &HFFFF&) \ &H100)
  ELSE
    TimeConst% = ((65536 - 256000000 \ BaseWaveFreq&) AND &HFFFF&) \ &H100
    '
    CALL WriteDSP(&H40)
    CALL WriteDSP(TimeConst%)
    CALL WriteDSP(length& AND &HFF)
    CALL WriteDSP((length& AND &HFFFF&) \ &H100)
    CALL WriteDSP(&H91)
  END IF
  '
END SUB

SUB GetBLASTER (eflag%)
  '
  ' This subroutine parses the BLASTER environment string and returns
  ' settings.
  '
  IF LEN(ENVIRON$("BLASTER")) = 0 THEN eflag% = 1: EXIT SUB
  '
  FOR length% = 1 TO LEN(ENVIRON$("BLASTER"))
    '
    SELECT CASE MID$(ENVIRON$("BLASTER"), length%, 1)
      CASE "A"
        BasePort% = VAL("&H" + MID$(ENVIRON$("BLASTER"), length% + 1, 3))
      CASE "I"
        IRQ% = VAL(MID$(ENVIRON$("BLASTER"), length% + 1, 1))
      CASE "D"
        Channel% = VAL(MID$(ENVIRON$("BLASTER"), length% + 1, 1))
        '
        IF Channel% < 0 OR Channel% > 3 THEN eflag% = 2
        '
    END SELECT
    '
  NEXT
  '
END SUB

SUB GetDACLPT (eflag%)
  '
  ' This subroutine parses the DACLPT environment string and returns
  ' settings.
  '
  IF LEN(ENVIRON$("DACLPT")) = 0 THEN eflag% = 1: EXIT SUB
  '
  FOR length% = 1 TO LEN(ENVIRON$("DACLPT"))
    '
    SELECT CASE MID$(ENVIRON$("DACLPT"), length%, 1)
      CASE "A"
        BasePort% = VAL("&H" + MID$(ENVIRON$("DACLPT"), length% + 1, 3))
      CASE "B"
        bias% = VAL(MID$(ENVIRON$("DACLPT"), length% + 1, 5))
        '
        IF bias% < 0 THEN bias% = 0
        IF bias% > 32767 THEN bias% = 32767
        '
    END SELECT
    '
  NEXT
  '
END SUB

FUNCTION GetDMAState%
  '
  ' Gets the status of the SoundBlaster DMA.
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
  CALL WriteDSP(&HE1)
  '
  Temp% = ReadDSP%
  '
  Temp2% = ReadDSP%
  '
  GetDSPVersion! = VAL(STR$(Temp%) + "." + STR$(Temp2%))
  '
END FUNCTION

SUB GetSPEAKER (eflag%)
  '
  ' This subroutine parses the SPEAKER environment string and returns
  ' settings.
  '
  IF LEN(ENVIRON$("SPEAKER")) = 0 THEN eflag% = 1: EXIT SUB
  '
  FOR length% = 1 TO LEN(ENVIRON$("SPEAKER"))
    '
    SELECT CASE MID$(ENVIRON$("SPEAKER"), length%, 1)
      CASE "A"
        BasePort% = VAL("&H" + MID$(ENVIRON$("SPEAKER"), length% + 1, 3))
      CASE "B"
        bias% = VAL(MID$(ENVIRON$("SPEAKER"), length% + 1, 5))
        '
        IF bias% < 0 THEN bias% = 0
        IF bias% > 32767 THEN bias% = 32767
        '
    END SELECT
    '
  NEXT
  '
END SUB

SUB InitBlaster (eflag%)
  '
  ' Initialize SoundBlaster
  '
  eflag% = 0
  '
  CALL GetBLASTER(eflag%)  ' Parses BLASTER environment
  '
  IF eflag% = 0 THEN
    '
    IF GetDSPVersion! < 3 THEN eflag% = 3
    '
  END IF
  '
END SUB

SUB InitDAC (eflag%)
  '
  ' Initialize LPT DAC
  '
  ' This routine isn't really needed, but I am including it for
  ' the purpose that there may be a DAC out there that really does
  ' need to be initialized prior to being used...
  '
  eflag% = 0
  '
  CALL GetDACLPT(eflag%)
  '
END SUB

SUB InitPCSpeaker (eflag%)
  '
  ' Initialize PC Speaker
  '
  ' eflag% is not really needed for this routine, but I have added
  ' it in case at some future date (ha!) checks can be put in to determine
  ' if the PC speaker is installed...
  '
  eflag% = 0
  '
  CALL GetSPEAKER(eflag%)
  '
  IF eflag% = 0 THEN
    '
    ' Reset Speaker
    '
    OUT BasePort% + 1, &HB6
    OUT BasePort%, &HFF
    OUT BasePort%, 0
    '
    ' Place in "One Shot" Mode
    '
    OUT BasePort% + 1, &H90
    OUT BasePort% + 31, INP(BasePort% + 31) OR 3
  END IF
  '
END SUB

SUB InitSoundDevice (device%, eflag%)
  '
  ' Initialize a Sound Device
  '
  SELECT CASE device%
    CASE 0 ' No Sound
      '
      ' No initialization needed
      '
    CASE 1 ' PC Speaker (Normal)
      '
      ' No initialization needed
      '
    CASE 2 ' LPT DAC
      '
      CALL InitDAC(eflag%)
      '
    CASE 3 ' SoundBlaster
      '
      CALL InitBlaster(eflag%)
      '
    CASE 4 ' PC Speaker (6 Bit Mode)
      '
      CALL InitPCSpeaker(eflag%)
      '
  END SELECT
  '
  FOR t% = 1 TO eflag%: BEEP: NEXT
  '
END SUB

SUB PlayBlaster (note%, length&, lvolume%, rvolume%)
  '
  ' Play a note on the SoundBlaster (similar to SOUND)
  '
  CALL SpeakerState(1) ' Turn the speaker on
  '
  CALL SetMasterVolume(lvolume%, rvolume%, 0) ' Set the volume
  '
  REDIM WavBuffer(0 TO 0) AS STRING * 32767 ' Make a 32k buffer for sound.
  '
  DEF SEG = VARSEG(WavBuffer(0))
  '
  thresh% = ((32767 - note%) \ note%) + 1 ' Frequency threshold
  '
  tt% = 0
  '
  ' Build up a square wave (change for whatever wave you would like)
  '
  FOR t% = 0 TO length& - 1
    IF tt% >= thresh% THEN
      POKE t%, 0
      tt% = 0
    ELSE
      POKE t%, 255
      tt% = tt% + 1
    END IF
  NEXT
  '
  DEF SEG
  '
  ' Play the sound
  '
  Lng& = length&
  '
  CALL DMAPlay(VARSEG(WavBuffer(0)), VARPTR(WavBuffer(0)), Lng&)
  '
  CALL SpeakerState(0) ' Turn the speaker off
  '
END SUB

SUB PlayDAC (note%, length&, volume%)
  '
  ' Play a note on the LPT DAC (similar to SOUND)
  '
  delay% = bias% - note%: IF delay% < 0 THEN delay% = 0
  '
  FOR t% = 0 TO length&
    OUT BasePort%, volume%
    OUT BasePort%, 0
    FOR d% = 0 TO delay%: NEXT
  NEXT
  '
END SUB

SUB PlayPCSpeaker (note%, length&, volume%)
  '
  ' Play a note on the PC speaker (similar to SOUND)
  '
  delay% = 32767 - note% - bias%: IF delay% < 0 THEN delay% = 0
  '
  FOR t% = 0 TO length& * 20
    OUT BasePort%, volume%
    FOR d% = 0 TO delay%: NEXT
  NEXT
  '
END SUB

SUB PlaySoundDevice (device%, note%, length!, lvolume%, rvolume%)
  '
  ' Play a note on a Sound Device (similar to SOUND)
  '
  ' Do error checking
  '
  IF note% < 37 THEN note% = 37
  IF note% > 32767 THEN note% = 32767
  '
  IF length! < 0 THEN length! = 0
  IF length! > 32767 THEN length! = 32767
  '
  SELECT CASE device%
    CASE 0 ' No Sound
    CASE 1 ' PC Speaker (Normal)
      '
      ' Gee, does this look familiar...?
      '
      SOUND note%, length!
      '
    CASE 2 ' LPT1 DAC
      '
      IF lvolume% < 0 THEN lvolume% = 0
      IF lvolume% > 255 THEN lvolume% = 255
      '
      CALL PlayDAC(note%, INT(length!), lvolume%)
      '
    CASE 3 ' SoundBlaster
      '
      IF lvolume% < 0 THEN lvolume% = 0
      IF lvolume% > 15 THEN lvolume% = 15
      '
      IF rvolume% < 0 THEN rvolume% = 0
      IF rvolume% > 15 THEN rvolume% = 15
      '
      CALL PlayBlaster(note%, INT(length!), lvolume%, rvolume%)
      '
    CASE 4 ' PC Speaker (6 Bit Mode)
      '
      IF lvolume% < 0 THEN lvolume% = 0
      IF lvolume% > 63 THEN lvolume% = 63
      '
      CALL PlayPCSpeaker(note%, INT(length!), lvolume%)
      '
  END SELECT
  '
END SUB

FUNCTION ReadDSP%
  '
  ' Reads a byte from the DSP
  '
  DO: LOOP UNTIL INP(BasePort% + 14) AND &H80
  '
  ReadDSP% = INP(BasePort% + 10)
  '
END FUNCTION

FUNCTION ReadSBDAC%
  '
  ' Reads a byte from the SoundBlaster DAC.
  '
  CALL WriteDSP(&H20)
  '
  ReadSBDAC% = ReadDSP%
  '
END FUNCTION

SUB ResetDAC
  '
  ' This routine isn't really needed, but I am including it for
  ' the purpose that there may be a DAC out there that really does
  ' need to be reset after the host program finishes...
  '
END SUB

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

SUB ResetPCSpeaker
  '
  ' Reset Speaker
  '
  OUT BasePort% + 1, &HB6
  OUT BasePort% + 31, INP(BasePort% + 31) AND &HFC
  '
END SUB

SUB ResetSoundDevice (device%)
  '
  SELECT CASE device%
    CASE 0 ' No Sound
      '
      ' No reset needed
      '
    CASE 1 ' PC Speaker (Normal)
      '
      CALL ResetPCSpeaker
      '
    CASE 2 ' LPT1 DAC
      '
      CALL ResetDAC
      '
    CASE 3 ' SoundBlaster
      '
      ' No reset needed
      '
    CASE 4 ' PC Speaker (6 Bit Mode)
      '
      CALL ResetPCSpeaker
      '
  END SELECT
  '
END SUB

SUB SetDMAState (StopGo%)
  '
  ' Stops or continues DMA play.
  '
  IF StopGo% THEN
    CALL WriteDSP(&HD4)
  ELSE
    CALL WriteDSP(&HD0)
  END IF
  '
END SUB

SUB SetMasterVolume (Right%, Left%, Getvol%)
  '
  ' Sets the Left/Right stereo volume of the SoundBlaster
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
  ' Sets Stereo or Mono mode on the SoundBlaster
  '
  OUT BasePort% + 4, &HE
  '
  IF OnOff% THEN OUT BasePort% + 5, 2 ELSE OUT BasePort% + 5, 0
  '
END SUB

SUB SpeakerState (OnOff%)
  '
  ' Turns sound to SoundBlaster speaker output on or off.
  '
  IF OnOff% THEN
    CALL WriteDSP(&HD1)
  ELSE
    CALL WriteDSP(&HD3)
  END IF
  '
END SUB

SUB WaveBlaster (file$, lvolume%, rvolume%)
  '
  ' Play a WAV file on the SoundBlaster
  '
  CALL SpeakerState(1) ' Turn the speaker on
  '
  CALL SetMasterVolume(lvolume%, rvolume%, 0) ' Set the volume
  '
  CALL DMAWave(WaveBufferSeg&, WaveBufferOff&, WaveBufferLen&)
  '
  CALL SpeakerState(0) ' Turn the speaker off
  '
END SUB

SUB WaveDAC (file$, volume%)
  '
  ' Play a WAV file on the LPT DAC
  '
  DEF SEG = WaveBufferSeg&
  '
  st& = WaveBufferOff&: ed& = WaveBufferLen&
  '
  FOR t& = st& TO ed&
    OUT BasePort%, PEEK(t&) * 2
    FOR dlay% = 1 TO bias%: NEXT
  NEXT
  '
  DEF SEG
  '
END SUB

SUB WavePCSpeaker (file$)
  '
  ' Play a WAV file on the PC Speaker (!)
  '
  DEF SEG = WaveBufferSeg&
  '
  st& = WaveBufferOff&: ed& = WaveBufferLen&
  '
  FOR t& = st& TO ed&
    OUT BasePort%, PEEK(t&) * 2
    FOR dlay% = 1 TO bias%: NEXT
  NEXT
  '
  DEF SEG
  '
END SUB

SUB WaveSoundDevice (device%, file$, lvolume%, rvolume%)
  '
  ' Play a WAV file on a Sound Device
  '
  REDIM WavBuffer(0 TO 0) AS STRING * 32767 ' Make a 32k buffer for file.
  '
  f% = FREEFILE
  '
  OPEN file$ FOR BINARY AS f%
  '
  GET #1, 64, WavBuffer(0) ' Get 32k from file (skip header on WAV)
  '
  length& = LOF(1) - 64
  '
  IF length& > 32767 THEN length& = 32767 ' Adjust length if needed to 32k
  '
  CLOSE f%
  '
  WaveBufferSeg& = VARSEG(WavBuffer(0))
  WaveBufferOff& = VARPTR(WavBuffer(0)) + 64
  WaveBufferLen& = length&
  '
  SELECT CASE device%
    CASE 0 ' No Sound
    CASE 1 ' PC Speaker (Normal)
      '
      CALL InitPCSpeaker(eflag%)
      '
      IF eflag% = 0 THEN CALL WavePCSpeaker(file$)
      '
      CALL ResetPCSpeaker
      '
    CASE 2 ' LPT1 DAC
      '
      IF lvolume% < 0 THEN lvolume% = 0
      IF lvolume% > 255 THEN lvolume% = 255
      '
      CALL WaveDAC(file$, lvolume%)
      '
    CASE 3 ' SoundBlaster
      '
      IF lvolume% < 0 THEN lvolume% = 0
      IF lvolume% > 15 THEN lvolume% = 15
      '
      IF rvolume% < 0 THEN rvolume% = 0
      IF rvolume% > 15 THEN rvolume% = 15
      '
      CALL WaveBlaster(file$, lvolume%, rvolume%)
      '
    CASE 4 ' PC Speaker (6 Bit Mode)
      '
      CALL WavePCSpeaker(file$)
      '
  END SELECT
  '
  ERASE WavBuffer
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

SUB WriteSBDAC (byte%)
  '
  ' Writes a byte to the DAC.
  '
  CALL WriteDSP(&H10)
  CALL WriteDSP(byte%)
  '
END SUB


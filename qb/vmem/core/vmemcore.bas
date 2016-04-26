'****************************************************************************
'
' Description : VMEM Library - Virtual Memory Management Library - Core
' Written by  : Copyright (c) 1997 by Andrew L. Ayers
' Date        : 05/07/97
'
'****************************************************************************
'
DECLARE SUB Deallocate (handle$)
DECLARE SUB CopyFromVMem (handle$, address&, numbytes&, tsegment%, toffset%)
DECLARE SUB CopyToVMem (fsegment%, foffset%, handle$, address&, numbytes&)
DECLARE SUB VPokeByte (handle$, address&, value%)
DECLARE SUB VPokeLong (handle$, address&, value&)
DECLARE SUB VPokeStrg (handle$, address&, value$)
DECLARE SUB VPokeWord (handle$, address&, value%)
'
DECLARE FUNCTION VPeekStrg$ (handle$, address&, bytes%)
DECLARE FUNCTION VPeekLong& (handle$, address&)
DECLARE FUNCTION VPeekWord% (handle$, address&)
DECLARE FUNCTION VPeekByte% (handle$, address&)
DECLARE FUNCTION GetNewHandle$ ()
DECLARE FUNCTION Allocate$ ()
'
CONST TempDir$ = "C:\TEMP\" ' Temporary File Directory
CONST BufferSize% = 2048    ' Memory buffer size
'
ON ERROR GOTO ErrorHandler
'
'****************************************************************************
'
' Your stuff goes here!
'
'****************************************************************************
'
END
'
ErrorHandler:
'
IF ERR = 61 THEN
  PRINT "No free disk space - unable to proceed..."
ELSE
  PRINT "Unrecoverable error #"; ERR
END IF
'
ON ERROR GOTO 0

FUNCTION Allocate$
  '
  ' Check to see if handle is currently reserved
  '
  filenum% = FREEFILE
  '
  cnt& = 0
  '
  DO
    '
    cnt& = cnt& + 1: IF cnt& > 1000000 THEN handle$ = "": EXIT DO
    '
    handle$ = GetNewHandle$
    '
    SHELL "DIR " + TempDir$ + handle$ + " /B > " + TempDir$ + "VMEM.TMP"
    '
    OPEN TempDir$ + "VMEM.TMP" FOR INPUT AS filenum%
    '
    IF LOF(filenum%) = 0 THEN CLOSE filenum%: EXIT DO
    '
    CLOSE filenum%
    '
  LOOP
  '
  ' Remove temporary file
  '
  SHELL "DEL " + TempDir$ + "VMEM.TMP"
  '
  ' Pass back handle
  '
  Allocate$ = handle$
  '
  IF handle$ <> "" THEN
    PRINT "Allocated memory handle " + handle$ + " sucessfully!"
  ELSE
    PRINT CHR$(7) + "Error - Unable to allocate memory handle!"
  END IF
  '
END FUNCTION

SUB CopyFromVMem (handle$, address&, numbytes&, tsegment%, toffset%)
  '
  value$ = SPACE$(BufferSize%): filenum% = FREEFILE
  '
  OPEN TempDir$ + handle$ FOR BINARY AS filenum%
  '
  DEF SEG = tsegment%
  '
  numblocks& = numbytes& \ BufferSize%: leftover% = numbytes& - (numblocks& * BufferSize%)
  '
  FOR t& = 0 TO numblocks& - 1
    GET filenum%, address& + t& * BufferSize%, value$
    FOR tt% = 0 TO BufferSize% - 1
      POKE toffset% + (t& * BufferSize%) + tt%, ASC(MID$(value$, tt% + 1, 1))
    NEXT
  NEXT
  '
  value$ = SPACE$(leftover%)
  '
  GET filenum%, address& + (t& * BufferSize%), value$
  '
  FOR tt% = 0 TO leftover% - 1
    POKE toffset% + (t& * BufferSize%) + tt%, ASC(MID$(value$, tt% + 1, 1))
  NEXT
  '
  DEF SEG
  '
  CLOSE filenum%
  '
END SUB

SUB CopyToVMem (fsegment%, foffset%, handle$, address&, numbytes&)
  '
  filenum% = FREEFILE: value$ = ""
  '
  OPEN TempDir$ + handle$ FOR BINARY AS filenum%
  '
  DEF SEG = fsegment%
  '
  FOR t& = 0 TO numbytes& - 1
    value$ = value$ + CHR$(PEEK(foffset% + t&))
    IF LEN(value$) = BufferSize% THEN
      PUT filenum%, address& + t& - (BufferSize% - 1), value$
      value$ = ""
    END IF
  NEXT
  '
  IF value$ <> "" THEN
    PUT filenum%, address& + t& - LEN(value$), value$
    value$ = ""
  END IF
  '
  DEF SEG
  '
  CLOSE filenum%
  '
END SUB

SUB Deallocate (handle$)
  '
  ' Make sure handle exists
  '
  filenum% = FREEFILE
  '
  SHELL "DIR " + TempDir$ + handle$ + " /B > " + TempDir$ + "VMEM.TMP"
  '
  OPEN TempDir$ + "VMEM.TMP" FOR INPUT AS filenum%
  '
  IF LOF(filenum%) <> 0 THEN
    '
    SHELL "DEL " + TempDir$ + handle$
    '
    PRINT "Deallocated memory handle " + handle$ + " successfully!"
  ELSE
    PRINT CHR$(7) + "Unable to deallocate memory handle " + handle$ + "!"
  END IF
  '
  CLOSE filenum%
  '
  ' Remove temporary file
  '
  SHELL "DEL " + TempDir$ + "VMEM.TMP"
  '
END SUB

FUNCTION GetNewHandle$
  '
  ' See if handle file exists (just in case any other VMEM aware
  ' applications are running)
  '
  filenum% = FREEFILE
  '
  SHELL "DIR " + TempDir$ + "VMEM.HND /B > " + TempDir$ + "VMEM.TMP"
  '
  OPEN TempDir$ + "VMEM.TMP" FOR INPUT AS filenum%
  '
  IF LOF(filenum%) <> 0 THEN
    '
    ' Ok, a prior handle has been created, so get next handle
    '
    CLOSE filenum%
    '
    OPEN TempDir$ + "VMEM.HND" FOR INPUT AS filenum%
    '
    INPUT #filenum%, hnd$
    '
    hnd$ = LTRIM$(RIGHT$(STR$(VAL(MID$(hnd$, 3, 6)) + 1), 6))
    hnd$ = "VM" + STRING$(6 - LEN(hnd$), "0") + hnd$ + ".DAT"
    '
  ELSE
    '
    ' This is the first time VMEM has been used, so create a brand new one.
    '
    hnd$ = "VM000000.DAT"
    '
  END IF
  '
  CLOSE filenum%
  '
  ' Remove temporary file
  '
  SHELL "DEL " + TempDir$ + "VMEM.TMP"
  '
  ' Update handle file
  '
  OPEN TempDir$ + "VMEM.HND" FOR OUTPUT AS filenum%
  '
  PRINT #filenum%, hnd$
  '
  CLOSE filenum%
  '
  GetNewHandle$ = hnd$
  '
END FUNCTION

FUNCTION VPeekByte% (handle$, address&)
  '
  IF address& < 0 OR address& > 2147483647 THEN
    PRINT CHR$(7) + "Error - invalid address (" + LTRIM$(STR$(address&)) + ")..."
    STOP
  END IF
  '
  value$ = " ": filenum% = FREEFILE
  '
  OPEN TempDir$ + handle$ FOR BINARY AS filenum%
  '
  GET filenum%, address&, value$
  '
  CLOSE filenum%
  '
  VPeekByte% = ASC(value$)
  '
END FUNCTION

FUNCTION VPeekLong& (handle$, address&)
  '
  IF address& < 0 OR address& > 2147483644 THEN
    PRINT CHR$(7) + "Error - invalid address (" + LTRIM$(STR$(address&)) + ")..."
    STOP
  END IF
  '
  value& = 0: filenum% = FREEFILE
  '
  OPEN TempDir$ + handle$ FOR BINARY AS filenum%
  '
  GET filenum%, address&, value&
  '
  CLOSE filenum%
  '
  VPeekLong& = value&
  '
END FUNCTION

FUNCTION VPeekStrg$ (handle$, address&, numbytes%)
  '
  IF numbytes% <= 0 THEN EXIT FUNCTION
  '
  IF address& < 0 OR address& > 2147483648# - numbytes% THEN
    PRINT CHR$(7) + "Error - invalid address (" + LTRIM$(STR$(address&)) + ")..."
    STOP
  END IF
  '
  value$ = SPACE$(numbytes%): filenum% = FREEFILE
  '
  OPEN TempDir$ + handle$ FOR BINARY AS filenum%
  '
  GET filenum%, address&, value$
  '
  CLOSE filenum%
  '
  VPeekStrg$ = value$
  '
END FUNCTION

FUNCTION VPeekWord% (handle$, address&)
  '
  IF address& < 0 OR address& > 2147483646 THEN
    PRINT CHR$(7) + "Error - invalid address (" + LTRIM$(STR$(address&)) + ")..."
    STOP
  END IF
  '
  value% = 0: filenum% = FREEFILE
  '
  OPEN TempDir$ + handle$ FOR BINARY AS filenum%
  '
  GET filenum%, address&, value%
  '
  CLOSE filenum%
  '
  VPeekWord% = value%
  '
END FUNCTION

SUB VPokeByte (handle$, address&, value%)
  '
  IF address& < 0 OR address& > 2147483647 THEN
    PRINT CHR$(7) + "Error - invalid address (" + LTRIM$(STR$(address&)) + ")..."
    STOP
  END IF
  '
  IF value% < 0 OR value% > 255 THEN
    PRINT CHR$(7) + "Error - invalid BYTE value (" + LTRIM$(STR$(value%)) + ")..."
    STOP
  END IF
  '
  value$ = CHR$(value%): filenum% = FREEFILE
  '
  OPEN TempDir$ + handle$ FOR BINARY AS filenum%
  '
  PUT filenum%, address&, value$
  '
  CLOSE filenum%
  '
END SUB

SUB VPokeLong (handle$, address&, value&)
  '
  IF address& < 0 OR address& > 2147483644 THEN
    PRINT CHR$(7) + "Error - invalid address (" + LTRIM$(STR$(address&)) + ")..."
    STOP
  END IF
  '
  IF value& < -2147483648# OR value& > 2147483647 THEN
    PRINT CHR$(7) + "Error - invalid LONG value (" + LTRIM$(STR$(value&)) + ")..."
    STOP
  END IF
  '
  filenum% = FREEFILE
  '
  OPEN TempDir$ + handle$ FOR BINARY AS filenum%
  '
  PUT filenum%, address&, value&
  '
  CLOSE filenum%
  '
END SUB

SUB VPokeStrg (handle$, address&, value$)
  '
  IF value$ = "" THEN EXIT SUB
  '
  IF address& < 0 OR address& > 2147483648# - LEN(value$) THEN
    PRINT CHR$(7) + "Error - invalid address (" + LTRIM$(STR$(address&)) + ")..."
    STOP
  END IF
  '
  filenum% = FREEFILE
  '
  OPEN TempDir$ + handle$ FOR BINARY AS filenum%
  '
  PUT filenum%, address&, value$
  '
  CLOSE filenum%
  '
END SUB

SUB VPokeWord (handle$, address&, value%)
  '
  IF address& < 0 OR address& > 2147483646 THEN
    PRINT CHR$(7) + "Error - invalid address (" + LTRIM$(STR$(address&)) + ")..."
    STOP
  END IF
  '
  IF value% < -32768 OR value% > 32767 THEN
    PRINT CHR$(7) + "Error - invalid WORD value (" + LTRIM$(STR$(value%)) + ")..."
    STOP
  END IF
  '
  filenum% = FREEFILE
  '
  OPEN TempDir$ + handle$ FOR BINARY AS filenum%
  '
  PUT filenum%, address&, value%
  '
  CLOSE filenum%
  '
END SUB


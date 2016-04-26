'****************************************************************************
'
' Description : VMEM Library - Virtual Memory Management Library - Demo
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
SCREEN 0: WIDTH 80: CLS
'
PRINT "Introduction"
PRINT "------------"
PRINT "VMEM - Copyright (c) 1997 by Andrew L. Ayers, All Rights Reserved"
PRINT "I am retaining copyright on this work, but I am releasing it as freeware."
PRINT "Basically, you can use any part of this in your own work, as long as you"
PRINT "give me credit where it is due. You may also make copies of this library,"
PRINT "as long as no fee is charged and it is given in its original form. Thank"
PRINT "you for your cooperation."
PRINT "-------------------------------------------------------------------------"
PRINT "Have you ever needed more memory for a project, but there just wasn't"
PRINT "enough? Did you try to create a memory swapping system, and found that"
PRINT "there was way too much to keep track of? Did you try one of the free"
PRINT "EMS/XMS libraries, only to find out how buggy they were, and were too"
PRINT "much of a hassle to maintain? Well, look no further! VMEM is here! This"
PRINT "library will allow any programmer to set up, maintain, and remove virtual"
PRINT "memory segments, that can be accessed as easy as base memory. Standard"
PRINT "integer and string data types are supported, as well as a block memory"
PRINT "transfer system for really *huge* constructs (such as graphics). Still"
PRINT "don't believe me? Well, continue on..."
PRINT
PRINT "Press any key to continue :"
'
a$ = INPUT$(1)
'
CLS
'
PRINT "Handle Allocation"
PRINT "-----------------"
PRINT "First, we must allocate a handle for the virtual memory. This is *NOT*"
PRINT "the same as allocating the memory. Since virtual memory is near to un-"
PRINT "limited, there is no need to declare the maximum size..."
PRINT
'
handle$ = Allocate$
'
PRINT
PRINT "Press any key to continue :"
'
a$ = INPUT$(1)
'
CLS
'
PRINT "Demonstration #1"
PRINT "----------------"
PRINT "We will first demonstrate a simple BYTE copy to virtual memory and then"
PRINT "back. We'll copy the value 105 to address 2000 :"
PRINT
'
CALL VPokeByte(handle$, 2000, 105)
'
PRINT "Copied!"
PRINT
PRINT "And now we will bring it back!"
PRINT
PRINT "The value stored at address 2000 is :" + STR$(VPeekByte%(handle$, 2000))
PRINT
PRINT "See how easy that was?"
PRINT
PRINT "Press any key to continue :"
'
a$ = INPUT$(1)
'
CLS
'
PRINT "Demonstration #2"
PRINT "----------------"
PRINT "Now, we will demonstrate a WORD copy to virtual memory and then"
PRINT "back. We'll copy the value 1005 to address 1000 :"
PRINT
'
CALL VPokeWord(handle$, 1000, 1005)
'
PRINT "Copied!"
PRINT
PRINT "And now we will bring it back!"
PRINT
PRINT "The value stored at address 1000 is :" + STR$(VPeekWord%(handle$, 1000))
PRINT
PRINT "Press any key to continue :"
'
a$ = INPUT$(1)
'
CLS
'
PRINT "Demonstration #3"
PRINT "----------------"
PRINT "Next, we will demonstrate a LONG copy to virtual memory and then"
PRINT "back. We'll copy the value 120005 to address 450 :"
PRINT
'
CALL VPokeLong(handle$, 450, 120005)
'
PRINT "Copied!"
PRINT
PRINT "And now we will bring it back!"
PRINT
PRINT "The value stored at address 450 is :" + STR$(VPeekLong&(handle$, 450))
PRINT
PRINT "Press any key to continue :"
'
a$ = INPUT$(1)
'
CLS
'
PRINT "Demonstration #4"
PRINT "----------------"
PRINT "This demonstration will be more practical - we will store a STRING to"
PRINT "virtual memory and then bring it back. We'll copy the standard " + CHR$(34) + "Hello"
PRINT "World!" + CHR$(34) + " to address 3000 :"
PRINT
'
CALL VPokeStrg(handle$, 3000, "Hello World!")
'
PRINT "Copied!"
PRINT
PRINT "And now we will bring it back!"
PRINT
PRINT "The string stored at address 3000 is : " + VPeekStrg$(handle$, 3000, 12)
PRINT
PRINT "We can also only bring back part of it : " + VPeekStrg$(handle$, 3000, 5)
PRINT "Or we can bring back the middle : " + VPeekStrg$(handle$, 3003, 5)
PRINT "Or just the last part of it : " + VPeekStrg$(handle$, 3006, 6)
PRINT
PRINT "Press any key to continue :"
'
a$ = INPUT$(1)
'
CLS
'
PRINT "Demonstration #5"
PRINT "----------------"
PRINT "Now, here is one that everyone will love - we will draw an image onto"
PRINT "the mode 13 (320x200x256) screen, and save it into virtual memory. Be"
PRINT "patient on this one, it takes a little while to copy 64000 bytes!"
PRINT
PRINT "Press any key to continue :"
'
a$ = INPUT$(1)
'
SCREEN 13: CLS
'
CIRCLE (159, 99), 50, 10
PAINT (159, 99), 5, 10
LINE (0, 0)-(319, 199), 15
LINE (319, 0)-(0, 199), 14
'
CALL CopyToVMem(&HA000, 0, handle$, 2000, 64000)
'
SCREEN 0: WIDTH 80: CLS
'
PRINT "Demonstration #5 (cont.)"
PRINT "------------------------"
PRINT "Now, the screen is stored in virtual memory (I told you it would take"
PRINT "a bit). Now we have to copy it back - so get ready!"
PRINT
PRINT "Press any key to continue :"
'
a$ = INPUT$(1)
'
SCREEN 13: CLS
'
CALL CopyFromVMem(handle$, 2000, 64000, &HA000, 0)
'
PRINT "Press any key to continue :"
'
a$ = INPUT$(1)
'
SCREEN 0: WIDTH 80: CLS
'
PRINT "Demonstration #5 (cont.)"
PRINT "------------------------"
PRINT "Pretty cool, huh? How many screens can be stored on your drive? Or sprites?"
PRINT "Or anything? You could use this copy routine, and one of the byte read"
PRINT "routines to pull individual pixels, even! The possibilites are endless!"
PRINT "These routines could also be used on text screens, or even data arrays,"
PRINT "so you can create extremely flexible applications!"
PRINT
PRINT "Press any key to continue :"
'
a$ = INPUT$(1)
'
CLS
'
PRINT "Handle Deallocation"
PRINT "-------------------"
PRINT "Lastly, at the end of all processing (or whenever you need to), the"
PRINT "handle must be deallocated. When this is done, the virtual memory is"
PRINT "cleared, releasing the handle..."
PRINT
'
CALL Deallocate(handle$)
'
PRINT
PRINT "Press any key to continue :"
'
a$ = INPUT$(1)
'
CLS
'
PRINT "Conclusion"
PRINT "----------"
PRINT "That concludes the VMEM demo - I hope this collection of routines can"
PRINT "fill a void that has existed. Many in the QuickBasic/QBasic community"
PRINT "have needed the ability to create huge arrays without using up their"
PRINT "precious base memory area. EMS/XMS routines fill this void, but none"
PRINT "are crash proof (though a few a pretty well stable) - due to the com-"
PRINT "plexity of dealing with EMS/XMS. Even these routines wouldn't be able"
PRINT "to handle super huge arrays, due to memory constraints. The VMEM rou-"
PRINT "tines, however, can handle near infinite amounts of virtual memory."
PRINT "Using VMEM, coupled with an EMS/XMS system, could open to the program-"
PRINT "mer new possibilities in QuickBasic/QBasic development. Good luck, and"
PRINT "have phun!"
PRINT
PRINT "Andrew L. Ayers - 5/8/97"
PRINT
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


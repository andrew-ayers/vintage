-----------------------------------------------------------------------------

    Name: BlastCopy! - Offscreen Buffer/Video Memory Copy
    Date: October 16, 1996
  Author: Andrew L. Ayers

-----------------------------------------------------------------------------
BASIC Calling Procedure:
-----------------------------------------------------------------------------

DIM code%(14) ' 15 Words Allocated
'
' Load hex codes for routine into memory here
'
DEF SEG = VARSEG(code%(0)) ' Get code segement
'
' Call our routine
'
CALL ABSOLUTE(BYVAL VARSEG(fbuffer%(0)), BYVAL VARPTR(fbuffer%(0)), --->
	BYVAL VARSEG(tbuffer%(0)), BYVAL VARPTR(tbuffer%(0)),       --->
        VARPTR(code%(0)))

DEF SEG ' Reset to default segment

-----------------------------------------------------------------------------
Stack Descriptor:
-----------------------------------------------------------------------------

The following stack layout is only valid after all values are place onto the
stack for operation. In this routine, the layout is not valid until after
byte 0x0002 is executed (label Init:), due to prior stack manipulation by
both BASIC and the routine.

SP Offset Descriptions
--------- ------------------------------------- 
==[Pushed on by call to routine]=============
   0E	  From Buffer Segment
   0C	  From Buffer Offset
   0A	  To Buffer Segment
   08	  To Buffer Offset
==[Pushed on by BASIC]=======================
   06	  BASIC Return Segment
   04	  BASIC Return Offset
==[Pushed on by routine]=====================
   02	  DS Register
   00	  BP Register

------- ------- ------  --------------- -------------------------------------
HexCode Label   OpCode  Operands        Remarks
------- ------- ------  --------------- -------------------------------------
1E	Start:	PUSH    DS              ;Save the Destination Segment
55		PUSH    BP              ;Save the Base Pointer

89E5	Init:	MOV     BP,SP           ;Get the Stack Pointer
8B460E		MOV     AX,[BP+0E]      ;Get the from buffer segment
8ED8		MOV     DS,AX           ;and set DS to it.
8B760C		MOV     SI,[BP+0C]      ;Set SI to the from buffer offset.
8B460A		MOV     AX,[BP+0A]      ;Get the to buffer segment.
8EC0		MOV     ES,AX           ;and set ES to it.
8B7E08		MOV     DI,[BP+08]      ;Set DI to the to buffer offset.

B9007D	Plot:	MOV     CX,7D00         ;Number of words to copy (32000).
F3A5		REP     MOVSW           ;Move the words!

5D	Exit:	POP     BP              ;Reset the Base Pointer
1F		POP     DS              ;Reset the Destination Segment
CA0800		RETF    0008            ;Return to BASIC Program, clean up

-----------------------------------------------------------------------------
Length : 30 Bytes (15 Words)
-----------------------------------------------------------------------------
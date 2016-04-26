-----------------------------------------------------------------------------

    Name: BlastScroll! - Offscreen Buffer/Video Memory Copy/Scroll
          *Note* - This routine is very similar to the BlastCopy routine, in
          that it performs a block memory transfer of data. It differs, how-
          ever, in that BlastCopy moves WORDs, whereas this routine moves
          BYTES. I chose to use bytes over words in order to allow the end
          user to scroll in pixel increments left/right (the BlastCopy rou-
          tine would have only allowed 2 pixel scrolls at minimum).
    Date: April 29, 1997
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
        BYVAL NumBytes%, VARPTR(code%(0)))

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
   10	  From Buffer Segment
   0E	  From Buffer Offset
   0C	  To Buffer Segment
   0A	  To Buffer Offset
   08     Number of bytes to move
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
8B4610		MOV     AX,[BP+10]      ;Get the from buffer segment
8ED8		MOV     DS,AX           ;and set DS to it.
8B760E		MOV     SI,[BP+0E]      ;Set SI to the from buffer offset.
8B460C		MOV     AX,[BP+0C]      ;Get the to buffer segment.
8EC0		MOV     ES,AX           ;and set ES to it.
8B7E0A		MOV     DI,[BP+0A]      ;Set DI to the to buffer offset.

8B4E08	Plot:	MOV     CX,[BP+08]      ;Number of bytes to copy.
F3		REPZ
A4		MOVSB		        ;Move the bytes!

5D	Exit:	POP     BP              ;Reset the Base Pointer
1F		POP     DS              ;Reset the Destination Segment
CA0A00		RETF    000A            ;Return to BASIC Program, clean up

-----------------------------------------------------------------------------
Length : 30 Bytes (15 Words)
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

    Name: BlastCLS! - Offscreen Buffer/Video Memory Clear Screen
    Date: January 17, 1997
  Author: Andrew L. Ayers

-----------------------------------------------------------------------------
BASIC Calling Procedure:
-----------------------------------------------------------------------------

DIM code%(17) ' 18 Words Allocated
'
' Load hex codes for routine into memory here
'
DEF SEG = VARSEG(code%(0)) ' Get code segement
'
' Call our routine
'
CALL ABSOLUTE(BYVAL VARSEG(tbuffer%(0)), BYVAL VARPTR(tbuffer%(0)), --->
	BYVAL colr%, VARPTR(code%(0)))

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
   0C	  To Buffer Segment
   0A	  To Buffer Offset
   08	  Color to clear to
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

89E5	Init:	MOV     BP, SP          ;Get the Stack Pointer
8B460C		MOV     AX, [BP+0C]     ;Get the to buffer segment
8ED8		MOV     DS, AX          ;and set DS to it.
8B760A		MOV     SI, [BP+0A]     ;Set SI to the to buffer offset.
8B4608		MOV     AX, [BP+08]     ;Get the color.
88C4		MOV	AH, AL		;Duplicate the color byte.
B900FA		MOV	CX, FA00	;Set counter to 64000 bytes to move.

8904	Plot:	MOV	[SI], AX	;Plot the pixels @ position SI.
83C602		ADD	SI, 2		;Move ahead 2 bytes.
83E902		SUB	CX, 2		;Subtract 2 from counter.
75F6		JNZ	Plot:		;Loop until no more to plot.

5D	Exit:	POP     BP              ;Reset the Base Pointer
1F		POP     DS              ;Reset the Destination Segment
CA0600		RETF    0006            ;Return to BASIC Program, clean up

-----------------------------------------------------------------------------
Length : 35 Bytes (18 Words)
-----------------------------------------------------------------------------
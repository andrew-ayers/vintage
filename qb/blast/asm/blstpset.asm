-----------------------------------------------------------------------------

    Name: BlastPset! - Offscreen Buffer/Video Memory Pixel Plot Routine
    Date: October 16, 1996
  Author: Andrew L. Ayers

-----------------------------------------------------------------------------
BASIC Calling Procedure:
-----------------------------------------------------------------------------

DIM code%(21) ' 22 Words Allocated
'
' Load hex codes for routine into memory here
'
DEF SEG = VARSEG(code%(0)) ' Get code segement
'
' Call our routine
'
CALL ABSOLUTE(BYVAL VARSEG(tbuffer%(0)), BYVAL VARPTR(tbuffer%(0)), --->
	BYVAL xpos%, BYVAL ypos%, BYVAL colr%, VARPTR(code%(0)))

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
   10	  To Buffer Segment
   0E	  To Buffer Offset
   0C	  X Position
   0A	  Y Position
   08	  Color
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
8B4610		MOV     AX,[BP+10]      ;Get the to buffer segment
8ED8		MOV     DS,AX           ;and set DS to it.
8B760A		MOV     SI,[BP+0A]      ;Get the Y position.

B106	Plot:	MOV	CL,06		;Multiply it by 64 by using a Shift
D3E6		SHL	SI,CL		;Left (SHL) for speed.
89F3		MOV	BX,SI		;Save the result temporarily.
B102		MOV	CL,02		;Shift left again to multiply the
D3E6		SHL	SI,CL		;Y position by 256, then add that
01DE		ADD	SI,BX		;value to our saved result.
8B5E0C		MOV	BX,[BP+0C]	;Now get the X position and add it
01DE		ADD	SI,BX		;to the result to get our final
8B5E0E		MOV	BX,[BP+0E]	;offset. Then get the To buffer
01DE		ADD	SI,BX		;offset and add the pixel offset.
8A4608		MOV	AL,[BP+08]	;Get the pixel color,
8804		MOV	[SI],AL		;and plot it.

5D	Exit:	POP     BP              ;Reset the Base Pointer
1F		POP     DS              ;Reset the Destination Segment
CA0A00		RETF    000A            ;Return to BASIC Program, clean up

-----------------------------------------------------------------------------
Length : 44 Bytes (22 Words)
-----------------------------------------------------------------------------
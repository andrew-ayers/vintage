-----------------------------------------------------------------------------

    Name: Raycast Vertical Line Strip Drawing Routine
    Date: January 15, 1997
  Author: Andrew L. Ayers

-----------------------------------------------------------------------------
BASIC Calling Procedure:
-----------------------------------------------------------------------------

DIM code%(47) ' 48 Words Allocated
'
' Load hex codes for routine into memory here
'
DEF SEG = VARSEG(code%(0)) ' Get code segement
'
' Call our routine
'
CALL ABSOLUTE(BYVAL VARSEG(tbuffer%(0)), BYVAL VARPTR(tbuffer%(0)), --->
	BYVAL xpos%, BYVAL ypos%, BYVAL len1%, BYVAL colr1%, --->
	BYVAL len2%, BYVAL colr2%, BYVAL len3%, BYVAL colr3%, --->
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
   1A 	  To Buffer Segment
   18     To Buffer Offset
   16	  X position
   14	  Y position
   12	  Length 1 (Sky)
   10	  Color 1  (Sky)
   0E	  Length 2 (Walls)
   0C	  Color 2  (Walls)
   0A	  Length 3 (Ground)
   08	  Color 3  (Ground)
==[Pushed on by BASIC]=======================
   06	  BASIC Return Segment
   04 	  BASIC Return Offset
==[Pushed on by routine]=====================
   02	  DS
   00	  BP

------- ------- ------  --------------- -------------------------------------
HexCode Label   OpCode  Operands        Remarks
------- ------- ------  --------------- -------------------------------------

1E	Start:	PUSH	DS		;Save the Destination Segment
55		PUSH	BP		;Save the Base Pointer

89E5	Init:	MOV	BP, SP		;Get the Stack Pointer
8B461A		MOV	AX, [BP+1A]	;Get the to buffer segment
8ED8		MOV	DS, AX		;and set DS to it.
8B7614		MOV	SI, [BP+14]	;Get the Y position.

B106	SetPos:	MOV	CL, 06		;Multiply it by 64 by using a Shift
D3E6		SHL	SI, CL		;Left (SHL) for speed.
89F3		MOV	BX, SI		;Save the result temporarily.
B102		MOV	CL, 02		;Shift left again to multiply the
D3E6		SHL	SI, CL		;Y position by 256, then add that
01DE		ADD	SI, BX		;value to our saved result.
8B5E16		MOV	BX, [BP+16]	;Now get the X position and add it
01DE		ADD	SI, BX		;to the result to get our final
8B5E18		MOV	BX, [BP+18]	;offset. Then get the To buffer
01DE		ADD	SI, BX		;offset and add the pixel offset.
		
31C9	IntP1:	XOR	CX, CX		;Clear out the counter.
8A4610		MOV	AL, [BP+10]	;Get the color for the sky.

3B4E12	Plot1:	CMP	CX, [BP+12]	;Have we reached the length?
7409		JE	IntP2:		;Yes? Then move to next run.
8804		MOV	[SI], AL	;No? Then plot a pixel.
81C64001	ADD	SI, 140		;Move to the next scanline.
41		INC	CX		;Increment our counter.
EBF2		JMP	Plot1:		;And loop back to do it all again!

31C9	IntP2:	XOR	CX, CX		;Clear out the counter.
8A460C		MOV	AL, [BP+0C]	;Get the color for the wall.

3B4E12	Plot2:	CMP	CX, [BP+0E]	;Have we reached the length?
7409		JE	IntP3:		;Yes? Then move to next run.
8804		MOV	[SI], AL	;No? Then plot a pixel.
81C64001	ADD	SI, 140		;Move to the next scanline.
41		INC	CX		;Increment our counter.
EBF2		JMP	Plot2:		;And loop back to do it all again!

31C9	IntP3:	XOR	CX, CX		;Clear out the counter
8A4608		MOV	AL, [BP+08]	;Get the color for the floor.

3B4E12	Plot3:	CMP	CX, [BP+0A]	;Have we reached the length?
7409		JE	Exit:		;Yes? Then move to next run.
8804		MOV	[SI], AL	;No? Then plot a pixel.
81C64001	ADD	SI, 140		;Move to the next scanline.
41		INC	CX		;Increment our counter.
EBF2		JMP	Plot3:		;And loop back to do it all again!

5D	Exit:	POP	BP		;Reset the Base Pointer
1F		POP	DS		;Reset the Destination Segment
CA1400		RETF	0014		;Return to BASIC Program, clean up

-----------------------------------------------------------------------------
Length : 96 Bytes (48 Words)
-----------------------------------------------------------------------------
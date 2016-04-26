//
// Blast! Library Demo Program for Mode 13h (320x200x256)
//

#include <stdio.h>
#include <dos.h>
#include <conio.h>
#include <stdlib.h>
#include <alloc.h>
#include <math.h>

// Defines for BlastMode Constants //

#define MODE_13  0x13
#define TEXT_80  0x03

// Prototypes for Blast! Library //

void BlastMode(int mode);
char far *BlastAlloc(unsigned int NumBytes);
void BlastPset(char far *tbuffer, int x, int y, unsigned char c);
void BlastCopy(char far *fbuffer, char far *tbuffer);
void BlastCLS(char far *tbuffer, unsigned char c);
unsigned char BlastPoint(char far *fbuffer, int x, int y);
void BlastLine(char far *tbuffer, int x1, int y1, int x2, int y2, char color);
void BlastBox(char far *tbuffer, int x1, int y1, int x2, int y2, char color);
void BlastCircle(char far *tbuffer, int x1, int y1, int r1, int r2, char color);

// Prototypes for demo specific subroutines //

int InitMem(void);
int Init(void);

// Initialize buffer pointers //

char far *screen=(char *)MK_FP(0xa000,0); // Set up global pointer to VGA memory
char far *buffer1=NULL;

// M A I N L I N E //

void main()
{

  if (!Init()) return;

  BlastMode(MODE_13); // Set screen to 320x200x256 color mode

  do
  {
    BlastCLS(buffer1,0); // Clear off hidden buffer

    for (int i = 0; i < 100; i++) // Lets plot 100 random lines
    {
      int x1 = rand() % 320;
      int y1 = rand() % 200;
      int x2 = rand() % 320;
      int y2 = rand() % 200;

      unsigned char c = rand() % 256;

      BlastLine(buffer1,x1,y1,x2,y2,c);

      //BlastPset(buffer1,x,y,c); // Plot the pixels on the hidden buffer
    }

    BlastCopy(buffer1,screen); // Copy the hidden buffer to the visible screen

  }

  while (!kbhit()); // Pause for keypress

  farfree(buffer1); // Deallocate buffer

  BlastMode(TEXT_80); // Set screen to 80x24 text mode

 }

/////////////////////////////////////////////////////////////////////////////
//                                                                         //
//		       	 Demo Specific Subroutines                         //
//                                                                         //
/////////////////////////////////////////////////////////////////////////////


int InitMem(void)
{
  cprintf("InitMem::Total available far heap memory is: %lu bytes\n\r", farcoreleft());

  if ((buffer1 = BlastAlloc(64000))==NULL) return(0);

  return(1);
}

int Init(void)
{

  clrscr(); // Clear the screen

  textcolor(15);
  textbackground(4);
  cprintf("                Blast! Library for Borland C 3.0 - Demo Program                \n\n\r");

  textcolor(7);
  textbackground(0);
  cprintf("Init::Initializing Process...\n\r");

  int value=InitMem();

  cprintf("\nPress any key to continue...");

  while (!kbhit());

  getch();

  return(value);
}

/////////////////////////////////////////////////////////////////////////////
//                                                                         //
//			Blast! Library Subroutines                         //
//                                                                         //
/////////////////////////////////////////////////////////////////////////////

void BlastMode(int mode)
{
  _AH = 0;
  _AL = mode;

  geninterrupt(0x10);
}

void BlastCLS(char far *tbuffer, unsigned char c)
{
  for (unsigned int ii=0; ii<64000; ii++)
  {
    tbuffer[ii] = c;
  }
}

void BlastPset(char far *tbuffer, int x, int y, unsigned char c)
{
  int ToDataSeg=FP_SEG(tbuffer);
  int ToDataOff=FP_OFF(tbuffer);

  asm{

    PUSH  DS           ;// Save the Destination Segment

    MOV   AX,ToDataSeg ;// Get the to buffer segment
    MOV   DS,AX        ;// and set DS to it.
    MOV   SI,y         ;// Get the Y position.

    MOV   CL,0x06      ;// Multiply it by 64 by using a Shift
    SHL   SI,CL        ;// Left (SHL) for speed.
    MOV	  BX,SI        ;// Save the result temporarily.
    MOV	  CL,0x02      ;// Shift left again to multiply the
    SHL	  SI,CL        ;// Y position by 256, then add that
    ADD	  SI,BX        ;// value to our saved result.
    MOV	  BX,x         ;// Now get the X position and add it
    ADD	  SI,BX        ;// to the result to get our final
    MOV	  BX,ToDataOff ;// offset. Then get the To buffer
    ADD	  SI,BX        ;// offset and add the pixel offset.
    MOV	  AL,c         ;// Get the pixel color,
    MOV   [SI],AL      ;// and plot it.

    POP   DS           ;// Reset the Destination Segment

  }
}

void BlastCopy(char far *fbuffer, char far *tbuffer)
{
  int FromDataSeg=FP_SEG(fbuffer);
  int FromDataOff=FP_OFF(fbuffer);
  int ToDataSeg=FP_SEG(tbuffer);
  int ToDataOff=FP_OFF(tbuffer);

  asm{

    PUSH    DS              ;// Save the Destination Segment

    MOV     AX,FromDataSeg  ;// Get the from buffer segment
    MOV     DS,AX           ;// and set DS to it.
    MOV     SI,FromDataOff  ;// Set SI to the from buffer offset.
    MOV     AX,ToDataSeg    ;// Get the to buffer segment.
    MOV     ES,AX           ;// and set ES to it.
    MOV     DI,ToDataOff    ;// Set DI to the to buffer offset.
    MOV     CX,0x7D00       ;// Number of words to copy (32000).
    REP     MOVSW           ;// Move the words!

    POP     DS              ;// Reset the Destination Segment
  }
}

unsigned char BlastPoint(char far *fbuffer, int x, int y)
{
  return (fbuffer[y*320+x]);
}

void BlastLine(char far *tbuffer, int x1, int y1, int x2, int y2, char color)
{
  int offset = (y1 << 8) + (y1 << 6) + x1;

  int xdiff = x2 - x1;
  int ydiff = y2 - y1;

  int xunit;
  int yunit;

  if (xdiff < 0)
  {
    xdiff = -xdiff;
    xunit = -1;
  }
  else
  {
    xunit = 1;
  }

  if (ydiff < 0)
  {
    ydiff = -ydiff;
    yunit = -320;
  }
  else
  {
    yunit = 320;
  }

  int errorterm = 0;

  if (xdiff > ydiff)
  {
    int length = xdiff + 1;

    for (int i = 0; i < length; i++)
    {
      tbuffer[offset] = color;
      offset += xunit;
      errorterm += ydiff;

      if (errorterm > xdiff)
      {
	offset += yunit;
	errorterm -= xdiff;
      }
    }
  }
  else
  {
    int length = ydiff + 1;

    for (int i = 0; i < length; i++)
    {
      tbuffer[offset] = color;
      offset += yunit;
      errorterm += xdiff;

      if (errorterm > 0)
      {
	offset += xunit;
	errorterm -= ydiff;
      }
    }
  }
}

void BlastBox(char far *tbuffer, int x1, int y1, int x2, int y2, char color)
{
  for (int i = y1; i <= y2; i++)
  {
    BlastLine(tbuffer, x1, i, x2, i, color);
  }
}

void BlastCircle(char far *tbuffer, int x1, int y1, int r1, int r2, char color)
{
  int xp, yp, oxp, oyp;

  oxp=x1+sin(0)*r1;
  oyp=y1+cos(0)*r2;

  for (float i=0; i < 6.28; i += .25)
  {
    xp=x1+sin(i)*r1;
    yp=y1+cos(i)*r2;

    BlastLine(tbuffer, oxp, oyp, xp, yp, color);

    oxp = xp;
    oyp = yp;
  }
}






char far *BlastAlloc(unsigned int NumBytes)
{
  char far *buffer=NULL; // Temporary pointer for memory buffer

  cprintf("BlastAlloc::Allocating %u bytes from far heap...\n\r",NumBytes);

  if ((buffer = (char far *)farmalloc(NumBytes))==NULL)
  {
    cprintf("Not enough memory - Couldn't allocate buffer.\n\r");
    return 0;
  }

  return buffer;
}


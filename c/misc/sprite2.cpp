// Program to show simple sprite animation
// Useful functions included!

#include <stdio.h>
#include <dos.h>
#include <conio.h>
#include <stdlib.h>
#include <mem.h>

// Our "handy-dandy" function prototypes

void setmode(int mode);
void pcopy(char far *source, char far *dest, int lines);
void pclr(char far *buffer, int lines, char color);
void putsprite(int xpos, int ypos, int num, char far *source, char far *dest);

// Declare pointers to various buffers
// for hidden pages and sprites

unsigned char far *buffer1;
unsigned char far *buffer2;
unsigned char far *sbuffer;
unsigned char far *vbuffer;

void main()
{
  // Initialize a pointer for the old mode

  int oldmode = *(int *)MK_FP(0x40,0x49); // Keep a pointer for the old mode

  // Set up a 320 x 200 x 256 color screen mode (13h)

  vbuffer = (unsigned char far *)MK_FP(0xa000,0);
  setmode(0x13);

  // Allocate buffers for hidden pages and sprites

  buffer1 = new unsigned char [64000];
  buffer2 = new unsigned char [64000];
  sbuffer = new unsigned char [64000];

  //
  // Beginning of REAL code
  //

  pclr(vbuffer, 200, 0); // Clear off visible screen,
  pclr(buffer1, 200, 0); // hidden page one, hidden
  pclr(buffer2, 200, 0); // page two, and the sprite
  pclr(sbuffer, 200, 0); // buffer...

  // Draw a sprite in the sprite buffer

  int x, y;

  for (y = 0; y < 32; y++)
  {
    for (x = 0; x < 32; x++)
    {
      sbuffer[(y << 5) + x] = 11;
    }
  }

  // Punch a hole in the sprite

  for (y = 7; y < 25; y++)
  {
    for (x = 7; x < 25; x++)
    {
      sbuffer[(y << 5) + x] = 0;
    }
  }

  // Draw a background on second buffer

  for (int i = 0; i < 200; i++)
  {
    int xpos = rand() % 320;
    int ypos = rand() % 200;
    int color = rand() % 15;

    buffer2[ypos * 320 + xpos] = color;
  }

  // Loop until the user hits a key

  while (!kbhit())
  {
    for (int i = 0; i < 288; i += 2)
    {
      pcopy(buffer2, buffer1, 200);
      putsprite(i, 100, 0, sbuffer, buffer1);
      pcopy(buffer1, vbuffer, 200);
    }
  }

  // De-allocate buffers

  delete buffer1;
  delete buffer2;
  delete sbuffer;

  // Reset screen mode

  setmode(oldmode);
}

void setmode(int mode)
{
  // Set up a graphics mode, using a
  // BIOS interrupt

  _AH = 0;
  _AL = mode;

  geninterrupt(0x10);
}

void pcopy(char far *source, char far *dest, int lines)
{
  // Copy one buffer to another (320 x 200)

  memmove(dest,source, (lines << 8) + (lines << 6));
}

void pclr(char far *buffer, int lines, char color)
{
  // Clear out buffer with specified color

  for (int y = 0; y < lines; y++)
  {
    for (int x = 0; x < 320; x++)
    {
      buffer[(y << 8) + (y << 6) + x] = color;
    }
  }
}

void putsprite(int xpos, int ypos, int num, char far *source, char far *dest)
{
  // Put a sprite from the sprite buffer to a
  // destination buffer. *NOTE* Sprites must
  // be on 32 x 32 pixel boundries!

  for (int j = 0; j < 32; j++)
  {
    for (int i = 0; i < 32; i++)
    {
      char color = source[(num << 10) + (j << 5) + i];

      if (color) dest[((ypos + j) << 8) + ((ypos + j) << 6) + xpos + i] = color;
    }
  }

}

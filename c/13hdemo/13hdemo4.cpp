//
// Simple program to set up and show off Mode 13h (320x200x256)
//

#include <stdio.h>
#include <dos.h>
#include <conio.h>
#include <stdlib.h>

void setmode(int mode); // Prototype for screen mode setup routine

void main()
  {
  int oldmode=*(int *)MK_FP(0x40,0x49); // Save the old mode

  char far *screen=(char *)MK_FP(0xa000,0); // Set up pointer to VGA memory

  setmode(0x13); // Set to 320x200x256 color mode

  // Plot Equation of a Circle

  for (int y = 0; y < 200; y++)
    {
    for (int x = 0; x < 320; x++)
      {

      // In the next line, (y << 8) + (y << 6) + x is
      // equal to y * 320 + x - The bit shift operator is
      // used as a faster way to multiply

      screen[(y << 8) + (y << 6) + x] = ((x * x) + (y * y)) % 256;
      }
    }

  while (!kbhit()); // Pause for keypress

  // Pseudo Fade-To-Black

  for (long i = 0; i < 300000; i++)
    {
    unsigned int pos = (rand() + rand()) % 64000;
    screen[pos] = 0;
    }

  setmode(oldmode); // Reset display to old mode

  }

void setmode(int mode)
  {
  _AH = 0;
  _AL = mode;

  geninterrupt(0x10);
  }

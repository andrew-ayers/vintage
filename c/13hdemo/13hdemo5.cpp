//
// Simple program to set up and show off Mode 13h (320x200x256)
// Has a few graphic primitives to plot points, lines and boxes
// Need to add more, such as circle, paint, etc.
// Then the fun part - Make it into a library!
//

#include <stdio.h>
#include <dos.h>
#include <conio.h>
#include <stdlib.h>
#include <math.h>
#include <mem.h>

// Function Prototypes

void setmode(int mode); // Screen mode setup

// Graphic Primitives

void plot(int x, int y, char color, char far *screen);
void line(int x1, int y1, int x2, int y2, char color, char far *screen);
void box(int x1, int y1, int x2, int y2, char color, char far *screen);
void circle(int x1, int y1, int r1, int r2, char color, char far *screen);
void pcopy(char far *source, char far *dest, int lines);

// M A I N   F U N C T I O N

void main()
{
  int x, y;
  char color;

  int oldmode = *(int *)MK_FP(0x40,0x49); // Save the old mode

  char far *screen = (char *)MK_FP(0xa000,0); // Set up pointer to VGA memory
  char far *buffer = new char [64000];

  setmode(0x13); // Set to 320x200x256 color mode

  // Plot Equation of a Circle on hidden buffer

  for (y = 0; y < 200; y++)
  {
    for (x = 0; x < 320; x++)
    {
      color = ((x * x) + (y * y)) % 256;
      plot(x, y, color, buffer);
    }
  }

  // Copy from hidden buffer to video ram

  while (!kbhit()); // Wait until a keypress...
  getch();

  pcopy(buffer, screen, 200);

  while (!kbhit()); // Wait until a keypress...
  getch();

  while (!kbhit())
  {
    int xpos = rand() % 288;
    int ypos = rand() % 180;

    for (y = 0; y < 20; y++)
    {
      for (x = 0; x < 32; x++)
      {
	color = screen[(y << 8) + (y << 6) + x];
	plot(x + xpos, y + ypos, color, screen);
      }
    }
  }

  while (!kbhit()); // Wait until a keypress...
  getch();

  // Draw MANY Random boxes and lines!

  while (!kbhit()) // Wait until a keypress
  {
    int x1 = rand() % 320;
    int x2 = rand() % 320;
    int y1 = rand() % 200;
    int y2 = rand() % 200;
    int color = rand() % 16;

    line(x1, y1, x2, y2, color, screen);

    x1 = rand() % 320;
    x2 = rand() % 320;
    y1 = rand() % 200;
    y2 = rand() % 200;
    color = rand() % 16;

    box(x1, y1, x2, y2, color, screen);

  }

  while (!kbhit()); // Wait until a keypress...
  getch();

  // Draw MANY Random circles!

  while (!kbhit()) // Wait until a keypress
  {
    int x1 = 159;
    int y1 = 99;
    int r1 = rand() % 100;
    int r2 = rand() % 100;
    int color = rand() % 16;

    circle(x1, y1, r1, r2, color, screen);

  }

  for (long t=0; t<300000; t++)
  {
    screen[((rand()+rand()) % 64000)]=0;
  }

  delete buffer;

  setmode(oldmode); // Reset display to old mode
}

void setmode(int mode)
{
  _AH = 0;
  _AL = mode;

  geninterrupt(0x10);
}

void plot(int x, int y, char color, char far *screen)
{
  screen[(y << 8) + (y << 6) + x] = color;
}

void line(int x1, int y1, int x2, int y2, char color, char far *screen)
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
      screen[offset] = color;
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
      screen[offset] = color;
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

void box(int x1, int y1, int x2, int y2, char color, char far *screen)
{
  for (int i = y1; i <= y2; i++)
  {
    line(x1, i, x2, i, color, screen);
  }
}

void circle(int x1, int y1, int r1, int r2, char color, char far *screen)
{
  int xp, yp, oxp, oyp;

  oxp=x1+sin(0)*r1;
  oyp=y1+cos(0)*r2;

  for (float i=0; i < 6.28; i += .25)
  {
    xp=x1+sin(i)*r1;
    yp=y1+cos(i)*r2;

    line(oxp, oyp, xp, yp, color, screen);

    oxp = xp;
    oyp = yp;
  }
}

void pcopy(char far *source, char far *dest, int lines)
{
  // Copy one buffer to another (320 x 200)

  memmove(dest,source, (lines << 8) + (lines << 6));
}

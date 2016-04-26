//
// Simple program to set up and show off Mode 13h (320x200x256)
//

#include <stdio.h>
#include <dos.h>
#include <conio.h>
#include <stdlib.h>
#include <alloc.h>

#define MODE_13  0x13
#define TEXT_80  0x03

void setmode(int mode); // Prototype for screen mode setup routine
void PlotPixel(int x,int y,int c);
void PlotPixel1(int x,int y,int c);
int GetPixel(int x,int y);
int GetPixel1(int x,int y);

char far *screen=(char *)MK_FP(0xa000,0); // Set up global pointer to VGA memory
unsigned char far *buffer1=NULL;


void main()
  {

  printf("Total available far heap memory is: %lu bytes\n", farcoreleft());

  if ((buffer1 = (unsigned char far *)farmalloc(64000))==NULL)
  {
    printf("\n\nNot enough memory - Couldn't allocate buffer.\n");
    return;
  }

  sleep(5);

  setmode(MODE_13); // Set to 320x200x256 color mode

  // Plot Equation of a Circle


  int y,x,i;
  unsigned int ii;

  do
  {
    for (ii=0; ii<64000; ii++)
    {
      buffer1[ii] = 0;
    }

    for (i = 0; i < 300; i++)
      {
      int x = rand() % 320;
      int y = rand() % 200;
      int c = rand() % 256;
      PlotPixel1(x,y,c);
      }

    for (ii=0; ii<64000; ii++)
    {
      screen[ii]=buffer1[ii];
    }
  }
  while (!kbhit()); // Pause for keypress


  setmode(TEXT_80); // Set to 80x24 text mode

  }

void setmode(int mode)
  {
  _AH = 0;
  _AL = mode;

  geninterrupt(0x10);
  }


void PlotPixel(int x, int y, int c)
{
    screen[y * 320 + x] = (unsigned char) c;

}

void PlotPixel1(int x, int y, int c)
{
    buffer1[y * 320 + x] = (unsigned char) c;

}

int GetPixel(int x, int y)
{
  return (screen[y*320+x]);
}

int GetPixel1(int x, int y)
{
  return (buffer1[y*320+x]);
}
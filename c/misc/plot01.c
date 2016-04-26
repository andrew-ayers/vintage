/*************************************************************************/
/* The following program will display a pretty cool bit of eye candy     */
/* to amaze the user. It does this by simply using the x and y values    */
/* in screen memory to evaluate the equation of a circle, and then using */
/* the result to compute a color.                                        */
/*************************************************************************/

/* I N C L U D E S */

#include <stdio.h>
#include <conio.h>
#include <dos.h>

/* D E F I N E S */

#define TEXTMODE 0x03
#define VGA256 0x13

/* G L O B A L S */

unsigned char far *vid_buff=(char far *)0xA0000000L;

/* P R O T O T Y P E S */

int calc_circ(int x,int y);
void set_point(int x,int y,int color);
void set_res(int res);

/* M A I N   P R O G R A M */

void main()
  {

  /* V A R I A B L E S */

  int x;
  int y;
  int c;
  int res;
  int h;
  int v;
  int hinc;
  int vinc;

  /* Now start the fun! */

  /* Set resolution to 320x200x256 mode*/
  set_res(VGA256);

  /* Set the increment for function */
  hinc=1;
  vinc=1;

  v=0;

  /* Loop thru calculating the color of each point */
  for (y=0; y<199; y++)
    {
    h=0;

    for (x=0; x<319; x++)
      {
      h+=hinc;
      c=calc_circ(h,v);
      /* Now set the point */
      set_point(x,y,c);
      }
    v+=vinc;
    }

  /* Display the eye candy until someone hits the keyboard */
  while (!kbhit());

  /* Now reset the video mode to boring text */
  set_res(TEXTMODE);
  }

void set_res(int mode)
  {
  _AH=0x00;
  _AL=mode;
  geninterrupt(0x10);
  }

int calc_circ(int x,int y)
  {
  long int c;

  /* Evaluate equation of a circle, ANDing result to bring range */
  /* in between 0 and 255, and returning result to main() */
  c=(x*x+y*y);

  return(c);
  }

void set_point(int x,int y,int color)
  {
  /* Plot a point in video memory by using a calculated offset into */
  /* the array */
  vid_buff[((y<<8)+(y<<6))+x]=color;
  }
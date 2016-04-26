/********************************************************/
/* A simple little program to draw a fake 3-D rotating  */
/* wireframe cube. Works well, has page flipping. Would */
/* be interesting to do in mode 13, or mode X, and also */
/* add code to make solid and/or shading...             */
/* Written by Andrew L. Ayers, on 05-09-1995, as one of */
/* the my first projects in attempting to learn 'C'     */
/********************************************************/

/* I N C L U D E S */

#include <conio.h>
#include <math.h>
#include <graphics.h>
#include <stdlib.h>

/* M A I N   P R O G R A M */

void main()
  {

  /* Cube definition */

  int rx=100;           /* X radius */
  int ry=20;            /* Y radius */
  int xc=320;           /* X center */
  int yc=200;           /* Y center */
  int height=100;       /* Height of cube */
  int diff=(yc+height); /* Used by drawing code */

  /* Code variables */

  int page=0;           /* Points to current drawing page */
  int points;           /* Loop counter */
  float stp=M_PI_2;     /* PI divided by 2 */
  float t;              /* Loop counter */

  int x[4]={0,0,0,0};   /* Array to hold X coords of lines */
  int y[4]={0,0,0,0};   /* Array to hold Y coords of lines */
  int graphmode=VGAMED; /* Resolution of graphics screen */
  int graphdriver=VGA;  /* Mode of graphics */

  initgraph(&graphdriver,&graphmode,""); /* Set video mode */

  setgraphmode(graphmode); /* Set resolution to 640 x 350 x 16 color mode */

  setcolor(15);
  setfillstyle(SOLID_FILL,0); /* Solid to blank out screen */

  while (!kbhit())
    {
    for (t=0;t<stp;t=t+.01)
      {
      setvisualpage(page);
      setactivepage(1-page);
      bar(0,0,640,350); /* Clear off old stuff */

      for (points=0;points<4;points=points+1)
	{

	x[points]=xc+sin(t+(stp*points))*rx; /* Compute four points in a circle */
	y[points]=yc+cos(t+(stp*points))*ry; /* and put into coordinate arrays */

	/* Draw sides */

	line(x[points],y[points],x[points],diff-y[points]);

	}/* Next points */

      /* Draw bottom */

      moveto(x[0],y[0]);
      lineto(x[1],y[1]);
      lineto(x[2],y[2]);
      lineto(x[3],y[3]);
      lineto(x[0],y[0]);

      /* Draw top */

      moveto(x[0],diff-y[0]);
      lineto(x[1],diff-y[1]);
      lineto(x[2],diff-y[2]);
      lineto(x[3],diff-y[3]);
      lineto(x[0],diff-y[0]);

      page=1-page; /* Switch to a new page */

      }/* Next t */

    }/* Loop until someone presses a key... */

  closegraph();/* Reset all of the graphic stuff to old values */

  }/* End of Main() */



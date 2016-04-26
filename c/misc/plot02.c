/********************************************************************/
/* This is a simple moronic program to draw random shapes and lines */
/* in different colors on the screen. It operates in the high-res   */
/* 16 color VGA mode (640x480x16). Not good eye-candy, but okay...  */
/********************************************************************/

/* I N C L U D E S */

#include <conio.h>
#include <math.h>
#include <bios.h>
#include <graphics.h>
#include <stdlib.h>

/* M A I N   P R O G R A M */

void main()
  {
  int x1,y1;
  int x2,y2;
  int radius;
  int color;
  int shape;

  int graphmode=VGAHI, graphdriver=VGA;
  initgraph(&graphdriver,&graphmode,"");

  /*setgraphmode(VGAHIGH);/* Set resolution to 640x480x16 color mode */

  /* Draw random shapes on screen until the user hits a key */

  while (!kbhit())
    {

    /* Get various random elements to make up shape on screen */

    x1=random(640);
    y1=random(480);
    x2=random(640);
    y2=random(480);
    radius=random(100);
    color=random(16);
    shape=random(5);

    setcolor(color);/* Set the color of the shape */
    setfillstyle(SOLID_FILL, color);/* Set the fill color */

    switch(shape)
      {
      case 0:
	line(x1,y1,x2,y2);     /* Draw simple line       */
      case 1:
	rectangle(x1,y1,x2,y2);/* Draw empty box         */
      case 2:
	circle(x1,y1,radius);  /* Draw empty circle      */
      case 3:
	bar(x1,y1,x2,y2);      /* Paint in filled box    */
      case 4:
	circle(x1,y1,radius);
	floodfill(x1,y1,color);/* Paint in filled circle */
      }
    }
  closegraph();
  }
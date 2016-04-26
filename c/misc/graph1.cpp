#include <conio.h>
#include <stdlib.h>
#include <bios.h>
#include <graphics.h>
//
void DrawLine(int x,int y);
//
int main()
{
  int graphmode=VGALO, graphdriver=VGA;
  initgraph(&graphdriver,&graphmode,"");

  int key=0;

  while (key==0)
  {
    key=bioskey(1);
    for (int x=0; x<640; x++)
      DrawLine(x,0);
    for (int y=0; y<200; y++)
      DrawLine(0,199-y);
  }
  closegraph();
  return 0;
}
void DrawLine(int x,int y)
{
  setcolor(random(16));
  line(x,y,639-x,199-y);
}
#include <conio.h>
#include <stdlib.h>
#include <bios.h>
#include <graphics.h>
//
void DrawLine(void);
//
int main()
{
  int graphmode=VGALO, graphdriver=VGA;
  initgraph(&graphdriver,&graphmode,"");

  int key=0;

  while (key==0)
  {
    key=bioskey(1);
    DrawLine();
  }
  closegraph();
  return 0;
}
void DrawLine(void)
{
  setcolor(random(16));
  line(random(640),random(200),random(640),random(200));
}
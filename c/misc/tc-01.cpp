#include <bios.h>
#include <stdio.h>
#include <graphics.h>
#include <process.h>
//
int c0=0,c1=1,c2=2,c3=3,c4=4,c5=5,c6=6,c7=7,c8=8,
    c9=9,c10=10,c11=11,c12=12,c13=13,c14=14,c15=15,
    mode_flag=0;
float x_res,y_res;
float sx,sy;
int t1=1;
//
void keyboard(void);
void quit_pgm(void);
void graphics_setup(void);
//
main()
  {
  graphics_setup();
  for (t1=1;t1!=2;)
    {
    keyboard();
    }
  quit_pgm();
  }
void keyboard(void)
  {
  if (bioskey(1)==0) return; else quit_pgm();
  }
void quit_pgm(void)
  {
  cleardevice();
  restorecrtmode();
  exit(0);
  }
void graphics_setup(void)
  {
  char *BGI_dir="C:\\TC\\BGI";
  int graphics_adapter,graphics_mode;
  detectgraph(&graphics_adapter,&graphics_mode);
  mode_flag=0;
  if (graphics_adapter==MCGA | mode_flag==4) goto CGA_mode;
  if (graphics_adapter==CGA | mode_flag==4) goto CGA_mode;
  if (graphics_mode==EGALO | mode_flag==3) goto EGA_SCD_mode;
  if (graphics_mode==EGAHI | mode_flag==2) goto EGA_ECD_mode;
  if (graphics_adapter==VGA | mode_flag==1) goto VGA_mode;
  goto abort_message;

  VGA_mode:
    graphics_adapter=VGA;
    graphics_mode=VGAHI;
    initgraph(&graphics_adapter,&graphics_mode,BGI_dir);
    x_res=640;
    y_res=480;
    mode_flag=1;
    setcolor(c7);
    outtextxy(240,472,"640x480 16-color VGA mode");
  return;

  EGA_ECD_mode:
    graphics_adapter=EGA;
    graphics_mode=EGAHI;
    initgraph(&graphics_adapter,&graphics_mode,BGI_dir);
    x_res=640;
    y_res=350;
    mode_flag=2;
    setcolor(c7);
    outtextxy(240,342,"640x350 16-color EGA mode");
  return;

  EGA_SCD_mode:
    graphics_adapter=EGA;
    graphics_mode=EGALO;
    initgraph(&graphics_adapter,&graphics_mode,BGI_dir);
    x_res=640;
    y_res=200;
    mode_flag=3;
    setcolor(c7);
    outtextxy(240,192,"640x200 16-color EGA mode");
  return;

  CGA_mode:
    graphics_adapter=CGA;
    graphics_mode=CGAC3;
    initgraph(&graphics_adapter,&graphics_mode,BGI_dir);
    x_res=320;
    y_res=200;
    c0=0;c1=3;c2=3;c3=3;c4=3;c5=3;c6=3;c7=3;c8=2;
    c9=1;c10=3;c11=1;c12=3;c13=1;c14=13;c15=3;
    mode_flag=4;
    setcolor(c7);
    outtextxy(64,192,"320x200 4-color CGA mode");
  return;

  abort_message:
    printf("\n\nUnable to proceed.\n");
    printf("Requires VGA, EGA, MCGA, or CGA adaptor\n");
    printf("with an appropriate monitor.\n");
    printf("Please refer to the book.\n\n");
  exit(0);
  }

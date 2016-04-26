#define FAILURE		0
#define	SUCCESS		1

char get_sprites(char far *sbuffer, char far *gbuffer)
char put_sprite(char spritenum, int xpos, int ypos, char far *sbuffer, char far *gbuffer)

char load_pcx(char filepath[128], char far *gbuf)
char save_pcx(char filepath[128], char far *gbuf)

char set_gmode(char mode)
char copy_gbuf(char far *fromgbuf, char far *togbuf)
char clear_gbuf(char far *gbuf, char color)

void main()
{
  char far *sbuffer[64000];			// Sprite Buffer
  char far *buffer1[64000];			// Hidden Page 1
  char far *buffer2[64000];                     // Hidden Page 2
  char far *vbuffer = (char *)MK_FP(0xa000,0);	// Visible Page

  set_gmode(0x13); // Set Video Mode 13h 320 x 200 x 256

  load_pcx("pcxfile.pcx", *buffer1);
  
  get_sprites(*sbuffer, *buffer1);
}

char get_sprites(char far *sbuffer, char far *gbuffer)
{
  //
  // Sprite ripper for 32 x 32 pixel sprites.
  // Bit shifting used in favor of multiplication
  // for optimization reasons...
  //

  int x, buffer_offset;
  char h, v, y, sprite_num = 0;

  for (v = 0; v <= 5; v++)
  {
    for (h = 0; h <= 9; h++)
    {
       buffer_offset = 0;

       //
       // In the following line, << 5 = * 32. Shifting the
       // bits is faster than multiplication. Notice, if the
       // number you need to multiply by is a power of two,
       // simply use the bitshift operator (<<) with whatever
       // power of two the number is (in this case, 5). Division
       // may be done the same way, only use (>>, bitshift right)
       // operator.
       //
       
       for (y = v << 5; y <= v << 5 + 31; y++)
       {
         for (x = h << 5; x <= h << 5 + 31; x++)
         {
           sbuffer[(sprite_num << 10) + buffer_offset] = gbuffer[(y << 8) + (y << 6) + x];
           buffer_offset++;
         }
       }
    }
  }
}
return (SUCESS);

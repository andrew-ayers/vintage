/*
   DAMPmenu - Version 1.7
   Developed by Andrew L. Ayers

   LCD driver routines adapted from Scripted C driver source code
   by Eduardo Kurita and Andy Chandler (4 bit LCD Driver).

   The author of DAMPmenu may be contacted at: andrewa@indirect.com

   Development History:

   Date        Vers  Notes
   ----------  ----  -----------------------------------------------
   11.22.1999  1.0   Displays title text on LCD - Hello, World!
   11.23.1999  1.1   Allows scrolling and selection via keypad
   11.23.1999  1.2   Added audio feedback for sign in/out and keypad
   11.24.1999  1.3   Added command line execution of selection
   12.08.1999  1.4   Updated includes to use <> rather than ""
   12.08.1999  1.5   Updated delete option to allow for listing and
                     selection of MP3 files for deletion
   12.09.1999  1.6   Added to to delete files selected by the delete
                     menu option
   12.14.1999  1.7   Fixed problem with delete to use system calls
                     instead of program built batch file
*/

#include <dos.h>
#include <stdio.h>
#include <stdlib.h>
#include <conio.h>
#include <string.h>
#include <memory.h>

/* Define constants for LCD driver */

const short LCD_PORTADDRESS = 0x378;
const short LCD_DATA = 0x378;
const short LCD_STATUS = 0x379;
const short LCD_CONTROL = 0x37a;

/* Define structure for directory records */

struct DirData
{
  char filename[17];
  char filesize[17];
  int dflag;
}
dirlist[999];

/* Global variable to hold record count */

unsigned int dirlistcount;

/* Global variable to hold contents of file buffer read in */

char FBuffer[512];

/* Define functions for INI file processing */

void ReadINI(char *filepath)
{
  FILE *infile;

  /* Open file */

  infile=fopen(filepath,"r");

  /* Read chunk of file */

  fread(FBuffer,sizeof(FBuffer),1,infile);

  /* Close file */

  fclose(infile);
}

void ParseINI(char *varname, char *result)
{
  /* Initialize loop and flag variables */

  int t1=0;
  int t2=0;
  int t3=0;
  int getdata=0;

  /* Clear result string */

  for (t1=0; t1<256; t1++)
  {
    result[t1]=NULL;
  }

  /* Parse thru file buffer */

  for (t1=0; t1<512; t1++)
  {
    /* Current character in buffer = ";"? */

    if (FBuffer[t1]==59)
    {
      /*
         If yes, and flag is set, then we have
         the proper data, so exit
      */

      if (getdata==1)
      {
        result[t2]=NULL;
        return;
      }
      else
      {
        /* Otherwise skip the null and semicolon, and continue */
        t1+=2;
      }
      /* Clear the result string */

      for (t3=0; t3<256; t3++)
      {
        result[t3]=NULL;
      }

      /* Reset result string position */
      t2=0;
    }

    /* Is the current character in the file buffer = "="? */
    if (FBuffer[t1]==61)
    {
      /*
         If it is, then check to see if we have the
         variable we are looking for, and set the flag
         accordingly
      */

      if (*result==*varname)
      {
        getdata=1;
      }

      /* Skip over the equal sign */
      t1++;

      /* Clear the result string */
      for (t3=0; t3<256; t3++)
      {
        result[t3]=NULL;
      }
      t2=0;
    }

    /* Add the current character to the result string */
    result[t2]=FBuffer[t1];
    t2++;
  }
  return;
}

/* Define functions for directory processing */

int BuildDirList(char *path)
{
  struct find_t f;
  char filesize[16];
  int tt;

  dirlistcount=0;

  sprintf(path,"%s*.MP3",path);

  /* Read each directory entry */

  if (!_dos_findfirst(path, _A_NORMAL, &f))
  {

    /* Build list */

    do
    {
      /* Copy file name into list, breaking at null */

      for (tt = 0; tt < 12; tt++)
      {
        if (!f.name[tt]) break;
        dirlist[dirlistcount].filename[tt] = f.name[tt];
      }

      /* Clear remaining bytes */

      for (tt=tt; tt<16; tt++)
      {
        dirlist[dirlistcount].filename[tt] = 32;
      }

      dirlist[dirlistcount].filename[tt] = NULL;
      /* Convert file size field into a string */

      sprintf(filesize,"Size: %ld",f.size);

      /* Copy file size into list, breaking at null */

      for (tt = 0; tt < 16; tt++)
      {
        if (!filesize[tt]) break;
        dirlist[dirlistcount].filesize[tt] = filesize[tt];
      }

      /* Clear remaining bytes */

      for (tt=tt; tt<16; tt++)
      {
        dirlist[dirlistcount].filesize[tt] = 32;
      }

      dirlist[dirlistcount].filesize[tt] = NULL;

      /* Clear deletion flag */

      dirlist[dirlistcount].dflag=0;

      /* Move on to next record*/

      dirlistcount++;
    }
    
    /* Keep looping until no more files */

    while(!_dos_findnext(&f));

    return(1);
  }
  else
  {
    /* No files found, return error */

    return(0);
  }
}

void lcd_driver_command(char cmd)
{
  /* Reset Control Port - Parallel Port direction = Out */

  outportb(LCD_CONTROL, inportb(LCD_CONTROL) | 0x08);
  outportb(LCD_DATA, cmd);

  outportb(LCD_CONTROL,inportb(LCD_CONTROL) | 0x01);
  outportb(LCD_CONTROL,inportb(LCD_CONTROL) & 0xFE);
  outportb(LCD_CONTROL,inportb(LCD_CONTROL) & 0xF7);

  delay(5);
}

void lcd_driver_char(char chr)
{
  outportb(LCD_DATA, chr);
  outportb(LCD_CONTROL, inportb(LCD_CONTROL) | 0x01);

  delay(1);

  outportb(LCD_CONTROL,inportb(LCD_CONTROL) & 0xFE);
  outportb(LCD_CONTROL,inportb(LCD_CONTROL) & 0xF7);
}

void lcd_driver_init()
{
  int n;
  char command[4];

  clrscr();

  printf("HD44780 16x2 Menu System - V1.7\n\n");
  printf("Developed by Andrew L. Ayers, adapted from Scripted C driver source code\n");
  printf("by Eduardo Kurita and Andy Chandler (4 bit LCD Driver). The author of\n");
  printf("this software may be contacted at:\n\n");
  printf("andrewa@indirect.com\n");

  /* Now we'll initialise the LCD */

  command[0] = 48; 
  command[1] = 32; 
  command[2] = 192; 
  command[3] = 16; 

  outportb(LCD_CONTROL, inportb(LCD_CONTROL) & 0xDF); 

  for (n = 1; n < 4; n++)
  {
    lcd_driver_command(command[0]);
  }

  lcd_driver_command(command[1]);

  for (n = 2; n < 4; n++)
  {
    lcd_driver_command(0);
    lcd_driver_command(command[n]);
  }
}

void lcd_driver_clear()
{
  lcd_driver_command(0);
  lcd_driver_command(16);
}

void lcd_driver_home()
{
  lcd_driver_command(0);
  lcd_driver_command(32);
}

void lcd_driver_printf(char *lcd_line1, char *lcd_line2)
{
  int n;

  /* Set position in display, first line */

  lcd_driver_command(128);
  lcd_driver_command(0);

  /* Send MSB nibble (4-bit) and LSB nibble (4-bit) */

  for(n=0;n<16;n++)
  {
    lcd_driver_char(lcd_line1[n]);
    lcd_driver_char(lcd_line1[n]<<4);
  }

  /* Set position in display, second line */

  lcd_driver_command(192); /* 128+64 */
  lcd_driver_command(0);


  /* Send MSB nibble (4-bit) and LSB nibble (4-bit) */

  for(n=0;n<16;n++)
  {
    lcd_driver_char(lcd_line2[n]);
    lcd_driver_char(lcd_line2[n]<<4);
  }
}

void dampmenu_show_credits()
{
  lcd_driver_clear();
  lcd_driver_home();

  lcd_driver_printf("DAMPmenu Ver 1.7", "by Andrew Ayers ");

  delay(2000);
}

int dampmenu_getkey()
{
  int ch;

  do
  {
    ch = getch();
  }
  while (ch == 0);

  sound(2000);
  delay(25);
  sound(0);

  switch (ch)
  {
    case 47:
      return(47); /* "/" */
    case 42:
      return(42); /* "*" */
    case 45:
      return(45); /* "-" */
    case 43:
      return(43); /* "+" */
    case 13:
      return(13); /* "ENTER" */
    case 46:
    case 83:
      return(46); /* "." */
    case 48:
    case 82:
      return(48); /* "0" */
    case 49:
    case 79:
      return(49); /* "1" */
    case 50:
    case 80:
      return(50); /* "2" */
    case 51:
    case 81:
      return(51); /* "3" */
    case 52:
    case 75:
      return(52); /* "4" */
    case 53:
    case 76:
      return(53); /* "5" */
    case 54:
    case 77:
      return(54); /* "6" */
    case 55:
    case 71:
      return(55); /* "7" */
    case 56:
    case 72:
      return(56); /* "8" */
    case 57:
    case 73:
      return(57); /* "9" */
    default:
      return(0); 
  }
  return (0);
}

void dampmenu_delete_mp3s()
{
  char path[256];
  char temp[512];
  char disp1[17];
  char disp2[17];
  int tt;
  int dirpos=0;
  int offset=0;
  int ret;

  ParseINI("mp3dir",path);

  if (BuildDirList(path))
  {
    do
    {
      do
      {
        sprintf(disp1,"%s",dirlist[dirpos].filename);
        sprintf(disp2,"%s",dirlist[dirpos].filesize);

        if (dirpos > 0)
        {
          /* Show "prev" arrow */
          disp1[15]=127;
        }

        if (dirpos < dirlistcount-1)
        {
          /* Show "next" arrow */
          disp2[15]=126;
        }

        if (dirlist[dirpos].dflag==1)
        {
          /* Show flagged for deletion */
          disp1[14]=68;
        }
        else
        {
          /* Show flagged for keep */
          disp1[14]=32;
        }

        lcd_driver_printf(disp1,disp2);

        ret = dampmenu_getkey();

        switch (ret)
        {
          case 13: /* Hit return, flag for deletion */
            if (dirlist[dirpos].dflag==1)
            {
              dirlist[dirpos].dflag=0;
            }
            else
            {
              dirlist[dirpos].dflag=1;
            }
            break;
          case 48: /* Hit zero - exit to menu */
           lcd_driver_printf("Exiting to main ", "menu...         ");
           delay(2000);
            return;
          case 42: /* Hit splat - verify deletion */
            /* User is ready to delete files */
            ret = 0;
            break;
          case 56: /* Hit up arrow - scroll to previous */
            dirpos--;
            if (dirpos < 0)
            {
              dirpos = 0;
            }
            break;
          case 50: /* Hit down arrow - scroll to next */
            dirpos++;
            if (dirpos > dirlistcount-1)
            {
              dirpos = dirlistcount-1;
            }
            break;
        }
      }
      while (ret != 0);

      lcd_driver_printf("Delete MP3s?    ","[RETURN]=YES    ");

      ret = dampmenu_getkey();

      if (ret==13)
      {
        ParseINI("mp3dir",path);
        /* Delete files using system calls */
        for (tt=0; tt<dirlistcount; tt++)
        {
          if (dirlist[tt].dflag==1)
          {
            sprintf(temp,"DEL %s%s\n",path,dirlist[tt].filename);
            ret = system(temp);
          }
        }

        ret = system("DAMP_DEL.BAT");

        lcd_driver_printf("                ", "Deleted...      ");

        sound(800);
        delay(250);
        sound(500);
        delay(500);
        sound(0);

        delay(2000);

        ret = 13;
      }
      else
      {
        /* User has aborted delete */
        lcd_driver_printf("File delete     ","aborted...      ");
        delay(2000);
      }
    }
    while (ret != 13);
  }
  else
  {
    /* MP3 directory is empty, let user know */
    lcd_driver_printf("MP3 directory   ","empty - Aborting");
    delay(2000);
  }
}

int dampmenu_select(int choice)
{
  int ret;
  char *cmd;

  switch (choice)
  {
    case 0:
      lcd_driver_printf("<--Initiating-->", "<-----DAMP----->");
      delay(2000);
      ret = system("DAMP_RUN");
      return(1);
    case 1:
      lcd_driver_printf("Preparing system", " for upload...  ");
      delay(2000);
      ret = system("DAMP_UPL");
      lcd_driver_printf("<-Modification->", "<---COMPLETE--->");
      delay(2000);
      return(1);
    case 2:
      dampmenu_delete_mp3s();
      return(1);
    case 3:
      dampmenu_show_credits();
      return(1);
    case 4:
      lcd_driver_printf("Exiting to      ", "DOS...          ");
      delay(2000);
      return(0); /* Exit to System */
  }

  return(1);
}

void dampmenu_signon()
{
  int loopvar;

  for (loopvar=500; loopvar<=2000; loopvar+=500)
  {
    sound(loopvar);
    delay(50);
  }

  sound(0);
}

void dampmenu_signoff()
{
  int loopvar;

  for (loopvar=2000; loopvar>=500; loopvar-=500)
  {
    sound(loopvar);
    delay(50);
  }

  sound(0);
}

int main()
{
  int ret;
  int menupos = 0;
  int offset = 0;
  char *menu[5];

  menu[0] = " 1. Run DAMP    ";
  menu[1] = " 2. Upload MP3s ";
  menu[2] = " 3. Delete MP3s ";
  menu[3] = " 4. About Menu  ";
  menu[4] = " 5. Exit        ";

  ReadINI("DAMPMENU.INI");

  lcd_driver_init();

  dampmenu_signon();

  dampmenu_show_credits();

  do
  {
    menu[menupos+offset][0]  = 126; /* Right Pointing Arrow */
    menu[menupos+offset][15] = 127; /* Left Pointing Arrow */
    
    lcd_driver_printf(menu[menupos], menu[menupos+1]);

    menu[menupos+offset][0]  = 32;
    menu[menupos+offset][15] = 32;

    ret = dampmenu_getkey();

    switch (ret)
    {
      case 13:
        ret = dampmenu_select(menupos+offset);
        break;
      case 56:
        menupos--;
        if (menupos < 0)
        {
          menupos = 0;

          offset--;
          if (offset < 0) offset=0;
        }
        break;
      case 50:
        menupos++;
        if (menupos > 3)
        {
          menupos = 3;
          offset++;
          if (offset > 1) offset=1;
        }
        break;
    }
  }
  while (ret != 0);

  lcd_driver_clear();

  dampmenu_signoff();

  return(1);
}

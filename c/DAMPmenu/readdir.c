#include <dos.h>
#include <stdio.h>
#include <stdlib.h>
#include <conio.h>
#include <string.h>
#include <memory.h>

/* Define structure for directory records */

struct DirData
{
  char filename[13];
  char filesize[10];
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
  char filesize[10];
  int tt;

  dirlistcount=0;

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

      /* Convert file size field into a string */

      sprintf(filesize,"%ld",f.size);

      /* Copy file size into list, breaking at null */

      for (tt = 0; tt < 10; tt++)
      {
        if (!filesize[tt]) break;
        dirlist[dirlistcount].filesize[tt] = filesize[tt];
      }

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

void ShowDirList()
{
  /* Parse record structure extracting file information */

  int tt;

  printf("=-=-=-=-=-=-=-=-=\n\n");

  for (tt=0; tt<dirlistcount; tt++)
  {
    printf("%d. Filename: %s Filesize: %s\n",(tt+1), dirlist[tt].filename,dirlist[tt].filesize);
  }

  printf("\nTotal Number of Files: %d\n",dirlistcount);

  printf("\n=-=-=-=-=-=-=-=-=\n");
}

/* Beginning of "main" program */

int main()
{
  char path[256];

  ReadINI("DAMPMENU.INI");

  ParseINI("mp3dir",path);

  if (BuildDirList(path))
  {
    printf("Files matched in \"%s\" :\n",path);

    ShowDirList();

    return(1);
  }
  else
  {
    printf("No files matching %s found!\n",*path);
  }

  return(0);
}

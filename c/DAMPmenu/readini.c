#include <stdio.h>
#include <string.h>

char FBuffer[512];

void ReadINI(char *filepath)
{
  FILE *infile;

  infile=fopen(filepath,"r");

  fread(FBuffer,sizeof(FBuffer),1,infile);

  fclose(infile);
}

void ParseINI(char *varname, char *result)
{
  int t1=0;
  int t2=0;
  int t3=0;
  int getdata=0;

  for (t1=0; t1<256; t1++)
  {
    result[t1]=NULL;
  }

  for (t1=0; t1<512; t1++)
  {
    if (FBuffer[t1]==59)
    {
      if (getdata==1)
      {
        result[t2]=NULL;
        return;
      }
      else
      {
        t1+=2;
      }

      for (t3=0; t3<256; t3++)
      {
        result[t3]=NULL;
      }
      t2=0;
    }

    if (FBuffer[t1]==61)
    {
      if (*result==*varname)
      {
        getdata=1;
      }

      t1++;

      for (t3=0; t3<256; t3++)
      {
        result[t3]=NULL;
      }
      t2=0;
    }
    result[t2]=FBuffer[t1];
    t2++;
  }
}

int main()
{
  char temp[256];

  ReadINI("DAMPMENU.INI");

  ParseINI("dampdir",temp);
  printf("DAMP Directory : %s\n",temp);

  ParseINI("mp3dir",temp);
  printf("MP3 Directory  : %s\n",temp);

  return(1);
}

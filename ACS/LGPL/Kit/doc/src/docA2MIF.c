/*******************************************************************************
* E.S.O. - VLT project
*
* "$Id: docA2MIF.c,v 1.32 2008/06/09 06:52:09 bjeram Exp $" 
*
* who        when      what
* ---------  --------  ----------------------------------------------
* G.Filippi  28/02/94  First release
* G.Filippi  01/02/95  corrected compilation warnings
* F.Carbogn  24/07/97  Modified for Framemaker 4 to 5 update
*/

/************************************************************************
*   NAME
*   docA2MIF - convert ASCII into FrameMaker Interchange Format (MIF)
* 
*   SYNOPSIS
*   docA2MIF  <input_file> [<output_file>]
* 
*   DESCRIPTION
*   This utility converts an ASCII file into a MIF file suitable to be 
*   imported into a FM document.
*   
*   If not specified, output file is named adding the suffix ".mif" to the
*   input file name. 
*
*   Conversion rules:
*       - <TAB>s are expanded into blanks, before being interpreted, 
*         using a tabbing step of 8
*       - when imported in FM, the ASCII file is one paragraph of type
*         "Listing"
*
*              
*   <input_file>   READ  the ASCII file to be converted
*
*   <output_file>  WRITE the output in MIF format. By defalt <input_file>.mif
*
*   COMMANDS
*
*   RETURN VALUES
*
*   EXIT_SUCCESS if there are no errors 
*   EXIT_FAILURE for system or input format error
*   
*   Additional diagnostic messages are printed by the utility.
*
*
*   CAUTION
*
*   EXAMPLES
*   The command:   docA2MIF  myfile.c  
*
*                  will produce the output files  myfile.c.mif
*
*   SEE ALSO
*   docDoManPages
*   FrameMaker MIF Reference
*
*   BUGS
*
*------------------------------------------------------------------------
*/

#define _POSIX_SOURCE 1

#include <vltPort.h>
static char *rcsId="@(#) $Id: docA2MIF.c,v 1.32 2008/06/09 06:52:09 bjeram Exp $";
/*static void *use_rcsId = ((void)&use_rcsId, (void *) &rcsId);*/

/* 
 * System Headers 
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>             /* required by malloc */
#include <sys/types.h>          /* required by stat */
#include <sys/stat.h>           /* required by stat */
#include <time.h>               /* required by strftime, localtime, time*/

                                /* ADDED FOR COMPATIBILITY WITH SunOS 4.1.1 */
#define EXIT_FAILURE  1         /* CAUTION these two are POSIX macros and   */
#define EXIT_SUCCESS  0         /* should be already defined in <stdlib.h>  */
                                /* TO BE DELETED IN A POSIX SYSTEM          */

/*
 * Local Headers 
 */
/* none */

#define IN
#define OUT


/*
 * Constant and type definitions
 */
#define MAXCHAR   1000              /*maximum number of characters per line  */
#define MAXNAME    100              /*maximum number of characters per name  */
                                    /*MAXCHAR shall be greater than MAXNAME  */
/* 
 * Macros
 */

void panic(char *filename, int line)
{
fprintf(stderr, "\n?Panic in line %d of file %s\n", line, filename);
perror("Unespected UNIX error");
abort();
}

#define PANIC (panic(__FILE__, __LINE__))

/*
 *******************
 * Local functions *
 *******************
 */

/*
 *****************************************************************************
 */

int getLine (
    FILE IN  *filein, 
    char OUT *line)
/*
 * get characters from filein until '\n' or EOF are encountered,
 * Read characters, excluding terminators, expand <tab> into blanks and
 * stored into "line", then return:
 *     EOF    if no more tokens (EOF encountered)
 *     <>EOF  if token is not the last one
 */
{
int i=0;
int j=0;
int nextTab=0;
int c;

do
    {
    c=getc(filein);

    switch (c) 
        {
        case EOF : 
        case '\n':              /* end of line, close line */
            line[i]='\0';       
            i++;
            break; 

        case '\t':              /* expand <tab> into blanks */
            nextTab = ((((i / 8 ) + 1) * 8) - 1);
            for (j = i; j <= nextTab ; j++)   
                {
                line[i]=' ';
                i++;
                }
            break; 

        default:                /* insert the character into line */
            line[i]=c;
            i++;
            break;
        }

    if ( i >= MAXCHAR - 8 )     /* input line too long: */
        { 
        line[i]='\0';           /* close the line*/
        i++;
        }
    }
while ( line[i - 1] != '\0' );

/*
 * delete trailing blanks 
 */

i = i - 2;

while ( i >=0 )                
    {                         
    if ( line[i] == ' ' )
        {
        line[i] = '\0';
        i--;
        }
    else
        {
        break;
        }
    }

return c;
}



/*
 *****************************************************************************
 */

void initFMFile(
    FILE IN *MIFFile,
    char IN *source,
    char IN *formatterVersion)
/*
 *
 * - initialize the file with the prolog:  <MIFFile 5.0> ....
 *                                         #....
 *                                         
 */
{

fprintf(MIFFile, "<MIFFile 5.0> # This is a FRAMEMAKER 5.0 MIF File\n");
fprintf(MIFFile, "# -----------------------------------------------\n");
fprintf(MIFFile, "# This file has been automatically produced.");
fprintf(MIFFile, " DO NOT EDIT THIS FILE!!!!!!\n");
fprintf(MIFFile, "# Input file:  %s\n", source);
fprintf(MIFFile, "# Formatter :  %s\n", formatterVersion);
fprintf(MIFFile, "# -----------------------------------------------\n");

return;
}

/*
 *****************************************************************************
 */

void writeFMParaLine(
    FILE IN *MIFFile,
    char IN *string
    )
/*
 * - write into a FM file a ParaLine structure with HardReturn.
 *
 *                         <ParaLine
 *                          <String `s_t_r_i_n_g'>
 *                          <Char HardReturn >
 *                         > # end of ParaLine
 */
{
fprintf(MIFFile,"  <ParaLine \n");
fprintf(MIFFile,"   <String `%s'>\n", string);
fprintf(MIFFile,"   <Char HardReturn >\n");
fprintf(MIFFile,"  > # end of ParaLine\n");

return;
}

/*
 *****************************************************************************
 */

void  FMfilter(
    char OUT *destination,
    char IN  *source
    )
/*
 * - copy char by char from source to destination, filtering MIF special 
 *   chars (see MIF REFERENCE manual, pag. 2-5)
 */
{
int i = 0;                      /* index & counter */
int j = 0;                      /* index & counter */

for (i = 0; i <= strlen(source); i++)    
    {
    switch (source[i])
        {
        case '\t': 
            destination[j] = '\\'; j++; destination[j] = 't'; j++; break; 

        case '>': 
            destination[j] = '\\'; j++; destination[j] = '>'; j++; break; 

        case '\'': 
            destination[j] = '\\'; j++; destination[j] = '\''; j++; break; 

        case '`': 
            destination[j] = '\\'; j++; destination[j] = '`'; j++; break; 

        case '\\': 
            destination[j] = '\\'; j++; destination[j] = '\\'; j++; break; 

        default:                
            destination[j] = source[i]; j++; break;
        }
    }

return;
}

void getName(
    char IN  *file, 
    char OUT *name)

/*
 * extract the name from a filename.
 */

{
int i = 0;                      /* index & counter */
int j = 0;                      /* index & counter */

/*
 * find the position of the last "/", if any
 */

for (i = 0; i <= strlen(file); i++)    
    {
    if (file[i] == '/')
        {
        j = i + 1;
        }
    }

/*
 * get from the "/" to the end of the filename
 */
strcpy(name, &file[j]);

return;
}


/* 
 *****************************************************************************
 * Main
 *****************************************************************************
 */

int main (int argc, char *argv[])
{

char line[MAXCHAR + 1] = "";            /* working line buffer  */
char lastChange[MAXCHAR + 1] = "";      /* current date */

char ASCIIFileName[MAXNAME + 1] = "";   /* input file name (ASCII)*/
char MIFFileName[MAXNAME + 1] = "";     /* output file name (FrameMaker-MIF)*/
char MIFName[MAXNAME + 1] = "";         /* output file name-only*/

FILE *ASCIIfile = NULL;                 /* input file  */ 
FILE *MIFFile = NULL;                   /* output file */ 

char FMline[MAXCHAR + MAXCHAR] = "";    /* used to filter MIF character set*/

struct stat dirStatus;                  /* used to check man&doc directories*/

time_t now;                             /* used to build today date*/
struct tm *t;                           /* used to build today date*/

/* 
 *****************************************************************************
 * Program Overview
 * 
 *  - preliminary checks: correct input parameter number, accessibility of
 *                        input file etc.
 *  - output the MIF header
 *  - read line from input and convert into MIF format.
 *
 *****************************************************************************
 */

/* 
 *****************************************************************************
 *  - Welcome
 *****************************************************************************
 */
printf("----------------------------------------");
printf("----------------------------------------\n");
printf("%s \n\n", rcsId);

/* 
 *****************************************************************************
 *  - preliminary checks
 *****************************************************************************
 */

/* 
 * check input parameter number (it must be 1) 
 */

if ((argc <2) || (argc > 3))
    {
    printf("Usage:  docA2MIF  <input_file>  [<output_file>]\n\n");
    exit(EXIT_FAILURE);
    }

/* 
 * argv[1] ---> <input_file> and check its existence
 */
strcpy(ASCIIFileName, argv[1]);
if (stat(ASCIIFileName, &dirStatus) != 0 )
    {
    printf("ERROR: cannot access input file: %s\n", ASCIIFileName);
    perror("       system error");
    exit(EXIT_FAILURE); 
    }

/* 
 *  <output_file> =  argv[2]   or   <input_file>.mif
 */
if (argc == 3)
    {
    strcpy(MIFFileName, argv[2]);
    }
else
    {
    strcpy(MIFFileName, ASCIIFileName);
    strcat(MIFFileName, ".mif");            
    }

/* 
 * extract the filename only
 */
getName(MIFFileName, MIFName);

/* 
 * get current date (used to mark the imported file)
 */
now = time((time_t *)NULL);
t = localtime(&now);
sprintf(lastChange, "(%d/%d/%d %2d:%2d)",    /* I know that this is not */
                    t->tm_year,              /* the most elegant way of */
                    t->tm_mon + 1,           /* formatting the time, but*/
                    t->tm_mday,              /* I tried to use strftime, */
                    t->tm_hour,               /* and it did not work */ 
                    t->tm_min);

/* 
 *****************************************************************************
 *  - made the conversion
 *****************************************************************************
 */

/*
 * open input file
 */
ASCIIfile = fopen(ASCIIFileName, "r");
if (ASCIIfile == NULL)
    {
    printf("ERROR: cannot access source file: %s\n", ASCIIFileName);
    perror("       system error");
    exit(EXIT_FAILURE);
    }

printf("Input file: %s\n", ASCIIFileName);

/* 
 * open the output file
 */
MIFFile = fopen(MIFFileName, "w");
if (MIFFile == NULL)
    {
    printf("ERROR: cannot open output (MIF) file: %s\n", MIFFileName);
    perror("       system error");
    exit(EXIT_FAILURE);
    }
else 
    {
    printf("MIF   file: %s\n", MIFFileName);
    }

/* 
 *  - initialize output file with the prolog 
 */
initFMFile(MIFFile, ASCIIFileName, rcsId);   
fprintf(MIFFile," <Para \n");
fprintf(MIFFile,"  <PgfTag `Listing'>\n");

/* 
 *  - for each line in the input file create a paraline.
 */
while (getLine(ASCIIfile, line) != EOF )
    {
    FMfilter(FMline, line);
    writeFMParaLine(MIFFile, FMline);
    }

fprintf(MIFFile, " > # end of Para\n");

/* 
 *  - close the file
 */
fprintf(MIFFile,"# _____oOo_____\n");
fclose(MIFFile);

/*
 *****************************************************************************
 * - end of process
 *****************************************************************************
 */
printf("----------------------------------------");
printf("----------------------------------------\n\n");
exit(EXIT_SUCCESS);

}

/*___oOo___*/


/*******************************************************************************
* E.S.O. - VLT project
*
* $RCSfile: vltMakeSetPanelShell.c,v $
*
* who       when      what
* --------  --------  ----------------------------------------------
* gfilippi  04/07/95  created
* gfilippi  22/08/95  modified according to panel 2.41 and SPR950325
* eallaert 1999-03-17 -f option for Tcl/Tk shells has disappeared long ago
*/

/************************************************************************
#   NAME
#   vltMakeSetPanelShell - set the first 3 lines of each panel file
#
#   SYNOPSIS
#
#   vltMakeSetPanelShell <panelFile> 
# 
#   DESCRIPTION
#   Utility used by vltMakefile to set the shell that should execute
#   a panel. 
#
#   From version 1.6 on, sequencer has changed the name of the shell to be 
#   used for scripts using the graphical part: seqWish. From version 2.30,
#   panel editor does not write the first lines containing the call to the
#   right sequencer (seqWish) any longer, leaving such a task to the vltMake. 
#   Under user request (saved panels should be executable, this is good
#   for test panels) panel 2.40 re-established the call to the sequencer
#   (seqWish) into the *.pan.
#
#   To keep compatibility with previous versions, this utility:
#
#      - checks whether "exec <xxxx> -f" is in the panel source code
#        If any, the first 3 lines are removed from the input file.
#
#      - create the ../bin file by adding the standard lines to the
#        input panel file. The output file is created with +x.
#
#   It is not intended to be used as a standalone command.
#
#   <panelFile>   the name of the panel to be treated (no extension)
#
#   FILES
#   $VLTROOT/include/vltMakefile  uses this utility
#   ./<panel>.pan                 input file
#   ../bin/<panel>                output file
#
#   ENVIRONMENT
#
#   RETURN VALUES
#
#   SEE ALSO 
#   vltMakefile, Makefile, (GNU) make
#
#   BUGS    
# 
*------------------------------------------------------------------------
*/

#define _POSIX_SOURCE 1
#include "vltPort.h"

static char *rcsId="@(#) $Id: vltMakeSetPanelShell.c,v 1.2 2008/06/09 06:51:09 bjeram Exp $"; 

/* 
 * System Headers 
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>          
#include <sys/types.h>        /* chmod, stat */
#include <sys/stat.h>         /* chmod  */


/*
 * Local Headers 
 */

/* 
 * Signal catching functions  
 */


/* 
 *Local functions  
 */


/* 
 * Main
 */

int main (int argc, char *argv[])
{

#define VLTSCCM 131

char panel[100];                        /* argv[1] */

char panelInName[100];                  /* source panel code */
FILE *panelIn;                          

char panelOutName[100];                 /* executable panel code */
FILE *panelOut;                         /* */

struct stat dirStatus;                  /* used to check man&doc directories*/

#define MAXCHAR 1000

char line1[MAXCHAR];                      /* 1st line of the input file */
char line2[MAXCHAR];                      /* 2nd line of the input file */
char line3[MAXCHAR];                      /* 3rd line of the input file */
char line [MAXCHAR];                       /* any other line of the input file */

/* 
 * get&check input parameters:
 */

if (argc != 2)
    {
    printf("ERROR: too few arguments. \n");
    printf("Usage:\n     vltMakeSetPanelShell <panelFile>  \n");
    exit(EXIT_FAILURE);
    }

/* 
 * argv[1] ---> <panelFile> and check its existence
 */
strcpy(panel, argv[1]);

strcpy(panelInName, panel);
strcat(panelInName, ".pan");

if (stat(panelInName, &dirStatus) != 0 )
    {
    printf("ERROR: vltMakeSetPanelShell - unable to access %s file.\n", panelInName);
    exit(EXIT_FAILURE);
    }
else
    {
    panelIn = fopen(panelInName, "r");
    if (panelIn == NULL)
        {
        printf("ERROR: cannot open file: %s\n", panelInName);
        perror("       system error");
        exit(EXIT_FAILURE);
        }
    }
/*
 * read the first 3 lines from the panel source code and buffer them
 */

if (fgets(line1, MAXCHAR, panelIn) == NULL)
    {
    printf("ERROR: vltMakeSetPanelShell - unable to read 1st line from %s file.\n", panelInName);
    perror("       system error");
    exit(EXIT_FAILURE);
    }
    
if (fgets(line2, MAXCHAR, panelIn) == NULL)
    {
    printf("ERROR: vltMakeSetPanelShell - unable to read 2nd line from %s file.\n", panelInName);
    perror("       system error");
    exit(EXIT_FAILURE);
    }  
      
if (fgets(line3, MAXCHAR, panelIn) == NULL)
    {
    printf("ERROR: vltMakeSetPanelShell - unable to read 3rd line from %s file.\n", panelInName);
    perror("       system error");
    exit(EXIT_FAILURE);
    }
    
 
/*
 * Initialize the output file
 */

if (stat("../bin", &dirStatus) != 0 )
    {
    printf("ERROR: vltMakeSetPanelShell - unable to access ../bin\n");
    exit(EXIT_FAILURE);
    }
else
    strcpy(panelOutName, "../bin/");
    strcat(panelOutName, panel);
    {
    panelOut = fopen(panelOutName, "w");
    if (panelOut == NULL)
        {
        printf("ERROR: cannot open file: %s\n", panelOutName);
        perror("       system error");
        exit(EXIT_FAILURE);
        }
    } 
    
fprintf(panelOut, "#!/bin/sh\n");
fprintf(panelOut, "# Panel Executable file produced by $RCSfile: vltMakeSetPanelShell.c,v $ $Revision: 1.2 $\n");
fprintf(panelOut, "# Define which version of the sequencer to use \\\n");
fprintf(panelOut, "  exec seqWish \"$0\" ${1+\"$@\"}\n");
fprintf(panelOut, "#\n");


/*
 * check whether the 3rd line contains the call to sequencer (" -f").
 */
if (strstr(line3, " \"$0\" ") != NULL)
    {
    /*
     * panel < 2.30 and panel > 2.41 
     *    --> skip the current first 3 lines 
     */
    }
else
    {
    /*
     * 2.30 < panel < 2.41 
     *    --> copy the first 3 lines to output.
     */
    fprintf(panelOut, "%s", line1);
    fprintf(panelOut, "%s", line2);
    fprintf(panelOut, "%s", line3);
    }

/*
 * copy the remaining lines from input to output until EOF.
 */
while (fgets(line, MAXCHAR, panelIn) != NULL)
    {
    fprintf(panelOut, "%s", line);
    }

/* 
 * set executable output file
 */
if (chmod(panelOutName, S_IRUSR | S_IWUSR | S_IXUSR | S_IRGRP | S_IXGRP | S_IROTH  | S_IXOTH ) != 0)
    {
    perror("cannot set the output file to read only mode");
    exit(EXIT_FAILURE);
    }

/*
 * bye bye
 */
fclose(panelOut);
fclose(panelIn);

exit(EXIT_SUCCESS);

}

/*___oOo___*/

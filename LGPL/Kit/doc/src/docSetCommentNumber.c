/*******************************************************************************
* E.S.O. - VLT project
*
* docSetCommentNumber.c 
* 
* who           when            what
* ---------    ------------    ----------------------------------------------
* G.FILIPPI    18/06/93        First version (setCommentNumber.c)
* G.FILIPPI    24/11/93        corrected POSIX_SOURCE into  _POSIX_SOURCE
* P.FORSTMANN  09/09/93        Merged into 'doc' directory 
*                              (docSetCommentNumber.c)
* G.Filippi    01/02/95  corrected compilation warnings
*/

/*******************************************************************************
*   NAME
*   docSetCommentNumber - put a sequencial number to each comment in a list
* 
*   SYNOPSIS
*       docSetCommentNumber   <commentListFile>
* 
*   DESCRIPTION
*   
*   This program is part of software review documentation procedure.
*   
*   It changes <commentListFile> as follows:
*     - each line with at least six characters and beginning with "---"
*       is considered the separation line between two comments
*       and gets a number:
*             before          --->     after
* 
*         "---xxx........."           "---nnn----- ..up to column 80 ...----" 
*
*                                      where  nnn  is a running counter
*
*     - other lines are left unchanged.
*
*   The original input file is saved in a backup copy (.BAK suffix added).
*
*   <commentListFile>  the ASCII file containing comments to be numbered.
*
*   FILES
*   <commentListFile>.BAK   
*                  backup copy of <commentListFile> before being modified
*
*   RETURN VALUES
*   It returns always success. Warning messages, if any, are directed to
*   stdout.
*
*   CAUTIONS
*   Applying the command to a file that has been already numbered, produces
*   a new numbering sequence. 
*   So, do not apply this command to a list already numbered where you have
*   added by editor other comment and you want to keep the original numbers
*   (instead, use the editor to mark the added comment).
*   The typical case is the review report that is prepared starting from 
*   the (numbered) comment list with, often, the insertion of comments
*   rised during the meeting.
*
*   EXAMPLES
*   A typical sequence is:
*
*       - preparing a comment list file named "commentList"
*         ("docMergeComments" command can be the first step for that).
*
*       - number the list:
*                     docSetCommentNumber commentList
*
*       - editing "commentList" to insert last minute comments,
*
*       - update the number in the list, using again: 
*                     docSetCommentNumber commentList
*
*
*   SEE ALSO
*   docMergeComments
*
*   BUGS 
*----------------------------------------------------------------------
*/
#define _POSIX_SOURCE 1

#include <vltPort.h>
static char *rcsId="@(#) $Id: docSetCommentNumber.c,v 1.32 2008/06/09 06:52:09 bjeram Exp $";
/*static void *use_rcsId = ((void)&use_rcsId, (void *) &rcsId); */

/* 
 * System Headers 
 */
#include <stdio.h>
#include <string.h>
#include <unistd.h>             /* required by rename*/
#include <sys/types.h>          /* required by stat */
#include <sys/stat.h>           /* required by stat */

/*
 * Local Headers 
 */
/* none */

#define IN
#define OUT

                                /* ADDED FOR COMPATIBILITY WITH SunOS 4.1.1 */
#define EXIT_FAILURE  1         /* CAUTION these two are POSIX macros and   */
#define EXIT_SUCCESS  0         /* should be already defined in <stdlib.h>  */
                                /* TO BE DELETED IN A POSIX SYSTEM          */

/*
 * Constant and type definitions
 */
#define MAXCHAR  1000            /*maximum number of characters per line*/
#define MAXNAME  100            /*maximum number of characters per name*/

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
 *Local functions  
 */

/*
 *****************************************************************************
 */

int getLine (
    FILE IN  *filein, 
    char OUT *line)
/*
 * get characters from filein until '\n' or EOF are encountered,
 * Read characters, excluding terminators, are stored into "line".
 * the value returned is
 *     EOF    if no more tokens (EOF encountered)
 *     <>EOF  if token is not the last one
 */
{
int i=0;
int c;

while (1)
    {
    c=getc(filein);

    switch (c) 
        {
        case EOF : 
        case '\n':              /* end of line, close line and exit */
            line[i]='\0';       
            return c; 
            break; 

        default:                /* insert the character into line */
            line[i++]=c;
            break;
        }

    if ( i >= MAXCHAR )         /* input line too long: */
        { 
        line[i]='\0';         /* close the line*/
        return c;
        }
    }
}


/*
 *****************************************************************************
 */

/* 
 * Main
 */

int main (int argc, char *argv[])
{

char line [MAXCHAR];                    /* working line buffer  */
int n_comment = 0;                      /* comment counter */     

char inputFile[MAXNAME + 1] = "";       /* input file name*/
char savedFile[MAXNAME + 1] = "";       /* input file name*/

FILE *infile = NULL;                    /* input file */ 
FILE *outfile = NULL;                   /* output file (numbered) */ 

struct stat dirStatus;                  /* used to check input file exists */

/* 
 *****************************************************************************
 * Program Overview
 * 
 *  - perform initial checks.
 *  - rename input file into backup file (.BAK). Backup file is used as input.
 *  - for each line in input:
 *      - if the lines is a separator: set the number
 *      - copy to output the (modified) line.
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
 * in addition to the command name, there shall be one and no more parameter
 */

if (argc != 2)
    {
    printf("Usage:  docSetCommentNumber <input_file> \n");
    exit(EXIT_FAILURE);
    }

/* 
 * argv[1] ---> <input_file> and check its existence
 */
strcpy(inputFile, argv[1]);
if (stat(inputFile, &dirStatus) != 0 )
    {
    printf("ERROR: cannot access input file: %s\n", inputFile);
    perror("       system error");
    exit(EXIT_FAILURE); 
    }

/* 
 * make safe copy  <input_file>  --renamed-->  <input_file>.BAK
 */
strcpy(savedFile, inputFile);
strcat(savedFile, ".BAK");

if (rename(inputFile, savedFile) == -1)
    {
    printf("ERROR: cannot saved input file into: %s\n", inputFile);
    perror("       system error");
    exit(EXIT_FAILURE); 
    }

/* 
 * Open input and output streams.
 */
infile = fopen(savedFile, "r");
if (infile  == NULL)
    {
    printf("ERROR: cannot open for read: %s\n", savedFile);
    perror("       system error");
    exit(EXIT_FAILURE);
    }

outfile = fopen(inputFile, "w");
if (outfile  == NULL)
    {
    printf("ERROR: cannot open output for write: %s\n", inputFile);
    perror("       system error");
    exit(EXIT_FAILURE);
    }

/* 
 * Process all lines from input
 */
while (getLine(infile, line) != EOF )
    {
    if ((strlen(line) >= 6) &&      /* if it is a separator line */
        (line[0] == '-') && 
        (line[1] == '-') && 
        (line[2] == '-') ) 
        {
        n_comment=n_comment + 1;    /* sbstitute it with the comment number */
        fprintf(outfile, "---%3.3d-----------------------------", n_comment);
        fprintf(outfile, "---------------------------------------------\n");
        }
    else
        {
        fprintf(outfile, "%s\n", line);       /* else output line as it is */
        }
    }

/*
 *****************************************************************************
 * - end of process
 *****************************************************************************
 */
fclose(infile);
fclose(outfile);

printf("----------------------------------------");
printf("----------------------------------------\n\n");
exit(EXIT_SUCCESS);

}

/*___oOo___*/















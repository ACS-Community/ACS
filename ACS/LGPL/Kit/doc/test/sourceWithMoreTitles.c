/*******************************************************************************
* E.S.O. - VLT project
*
* sourceWithMoreTitles.c
*
* who        when      what
* ---------  --------  ----------------------------------------------
* G.Filippi  12/05/93  First implementation
*/

USED TO TEST docDoManPages.c

/************************************************************************
*   NAME
*   title1, tiTle2,title3 title4- to test the case of many title and format
* 
*   SYNOPSIS
*   docDoManPages  <input_file>  <section>
* 
*   DESCRIPTION
*   This utility is used in the preparation of the on-line Reference
*   Manual. The original information are extracted form a C source file  
*   and formatted into an nroff file readable by man and xman utilities.
*
*   The input file must be formatted according to the template as
*   described in the VLT Programming Standards 1.0 10/03/93.
*
*   Briefly the rules are the following:
*
*      - the program searches the NAME keyword: When found formats
*        up to the first C terminator (an * followed by a /).
*        The column position of NAME is assumed as left margin for the 
*        following line of text. Everything leftmore is ignored!!!
*

*
*   RETURN VALUES
*
*       EXIT_SUCCES  if there are no errors (header may be not found)
*
*       EXIT_FAILURE for system of format error
*   
*   Additional information are given by the utility itself.
*
*
*
*   BUGS
*   This is a preliminary version. Input parsing and error management
*   are very limited and efficiency is low.
*   Input file shall be strictly formatted according to the rules.
*----------------------------------------------------------------
*/

#define POSIX_SOURCE 1

/* 
 * System Headers 
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>             /* required by malloc */
#include <sys/types.h>          /* required by stat */
#include <sys/stat.h>           /* required by stat */


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
#define MAXCHAR  1000           /*maximum number of characters per line*/
#define MAXNAME   100           /*maximum number of characters per name*/
#define MAXTITLES 100           /*maximum number of titles (man pages)*/

typedef enum {
   START_PAGE = 1,
   START_SECTION,
   END_OF_COMMENT,
   TEXT
} LINE_ANALYSIS_RESULT;

/* 
 * Macros
 */

void panic(char *filename, int line)
{
(void)fprintf(stderr, "\n?Panic in line %d of file %s\n");
(void)perror("Unespected UNIX error");
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
    

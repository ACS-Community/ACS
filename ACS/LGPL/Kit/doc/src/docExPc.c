/************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: docExPc.c,v 1.32 2008/06/09 06:52:09 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* pforstma  17/08/94  created from N. Feibig source
* -1.9----
* pforstma  21/02/95  SPR 950034: added blank line between comment groups
*/

/************************************************************************
*   NAME
*   docExPc - extract pseudo-code from a C source file
* 
*   SYNOPSIS
*   docExPc <infile> [<outfile>]
* 
*   DESCRIPTION
*   DocExPc extracts pseudo code from a C source file comments which
*   have a '*# ' within a comment, but not after the comment begin asterik.
*   If pseudo-code comments are separated by other line (code or usual
*   comments), then a blank line is printed to reflect this separation.
*
*   The output file begins with the following line:
*   # Generated from <infile> (date)
*   The output line is build from the source line, subtracting the '*# ' 
*   pattern, keeping both indentations  before the comment block and inside 
*   the pseudo-code.

*   The output is written to the specified file <outfile> or to the standard 
*   output if omitted.
*   
*   <infile>  C input source file
*   <outfile> output file (standard output if not given).
*            
*   FILES
*
*   ENVIRONMENT
*
*   RETURN VALUES
*
*   CAUTIONS 
*
*   EXAMPLES
*
*   SEE ALSO
*
*   BUGS   
* 
*------------------------------------------------------------------------
*/

#define _POSIX_SOURCE 1
#include "vltPort.h"

static char *rcsId="@(#) $Id: docExPc.c,v 1.32 2008/06/09 06:52:09 bjeram Exp $";
/* static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId); */

#include <stdio.h>
#include <string.h>
#include <time.h>               /* required by strftime, localtime, time*/

int main (int argCount, char * arg[])
    {
    FILE *infile, *outfile;
    char str[256], tempstr[256], *pstr;
    char mark[4] = "*# ";
    int  comment = 0;
    int  nocomment = 0;
    char buf[256] = "";
 
    time_t now;                             /* used to build today date*/
    struct tm *t;                           /* used to build today date*/
   
    if (argCount < 2) 
        {
        printf("\nDocExPc <infile> [<outfile>]\n");
        return 0;
        }
      
    /*
     *# open input file
     */
    infile = fopen(arg[1],"r");
    if (infile == NULL) 
        {
        printf("\nDocExPc - Error in opening input file %s\n",arg[1]);
        return -1;
        }
    
    /*
     *# create output file
     */
    if (argCount == 3)
        {
        outfile = fopen(arg[2],"w");
        if (outfile == NULL) 
            {
            printf("\nDocExPc - Error in opening output file %s\n",arg[2]);
            return -1;
            }
        }
    else outfile = stdout;

   /* 
    * get current date 
   */
   now = time((time_t *)NULL);
   t = localtime(&now);
   sprintf(buf, "(%d/%d/%d %2d:%2d)",   /* I know that this is not */
                    t->tm_year,                /* the most elegant way of */
                    t->tm_mon + 1,             /* formatting the time, but*/
                    t->tm_mday,                /* I tried to use strftime, */
                    t->tm_hour,                /* and it did not work */ 
                    t->tm_min);


    fprintf(outfile, "\n# Generated from %s %s \n\n",arg[1], buf);

    /*
     *# through the whole input file
     */
    while (!feof(infile))
        {
        /*
         *# read the next input line
         *# get the comment in the line if any
         */
        fgets(str,256,infile);
        if (strstr(str,"/*") != NULL)
            {
            comment = 1;
            }
        if (strstr(str,"*/") != NULL)
            {
            comment = 0;
	    nocomment = 1;
            }
        
        /*
         *# look for the pseudo code mark
         *# if found then extract pseudo code line
         *# and write it to output file
         */
        if (comment)
            {
            int mark_index; 

            pstr = (char *)strstr(str,mark);
            if (pstr != NULL)
                {
                mark_index = pstr - str - 1;
                memcpy(tempstr, str, mark_index);
                memcpy(tempstr + mark_index, str + mark_index + 3, 
                       strlen(pstr));
		if (nocomment == 1 )
		    {
		    fprintf(outfile,"\n");
		    nocomment = 0;
		    }
                fprintf(outfile,tempstr);
                }
            }
        }
        
    /*
     *# close files
     */
    fclose(infile);
    fclose(outfile);
    return 0;
    }
    
    
    

























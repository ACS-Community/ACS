/*******************************************************************************
* E.S.O. - VLT project
*
* splitIntoSingleComments.c
* 
* who           when            what
* ---------    ------------    ----------------------------------------------
* G.FILIPPI    18/06/93        First version (splitIntoSingleComments.c)
* P.FORSTMANN  09/09/94        Merged into 'doc' directory (docSplitIntoSingle
*                              Comments.c)
* G.Filippi    01/02/95  corrected compilation warnings
*/

/*******************************************************************************
*   NAME
*       docSplitIntoSingleComments - break a comment list into single comments
* 
*   SYNOPSIS
*       docSplitIntoSingleComments   <input_file>
* 
*   DESCRIPTION
*   
*       This program is part of software review documentation procedure.
*   
*       The input list of comment is divided into several files, each
*       containing the comments that are referenced to the same page.
*
*       The beginning of a comment is represented by a line where the first 
*       non-blank char is a "p" or "P" followed by alphabetic chars and by a
*       number, optionally separated by "." or blanks.
*
*       The output files are named:
*       
*                    <ppp>-<author>.tmp_single_comments
*       
*       where: <ppp>    : the page number on 3 characters, filled by 0
*              <author> : the author name, from the input file
*              
*       Each comment in the output file is written as follows:
*         - a line of 80 "-"
*         - all the lines of the input file up to the next comment or the EOF,
*           with the following editing exceptions:
*            * input line beginning with "---" are not copied.
*            * sequential blank lines are copied only once. 
*        - a blank line (if not already present in the input file)
*
*
*   <input_file> the ASCII file containing comments. The file name shall be 
*                in the format:   <author>.comments
*
*   FILES
*       order_comments.sh       shell script calling this program
*
*   RETURN VALUES
*       It returns always success. Warning messages, if any, are directed to
*       stdout.
*
*   SEE ALSO
*       order_comments
*       number_comments
*
*   BUGS 
*       Possible improvements:
*       - instead of simply copy from the input file, a text justification 
*         filter can be added.
*       - a more sophysticated parsing of the comment, including the section
*         numbers (e.g., page 23 2.4.5 ....)
*----------------------------------------------------------------------
*/
#define POSIX_SOURCE 1
#include <vltPort.h>
static char *rcsId="@(#) $Id: docSplitIntoSingleComments.c,v 1.32 2008/06/09 06:52:09 bjeram Exp $";
/* static void *use_rcsId = ((void)&use_rcsId, (void *) &rcsId); */

/* 
 * System Headers 
 */
#include <stdio.h>
#include <string.h>

/*
 * Local Headers 
 */
/* none */

#define IN
#define OUT

/*
 * Constant and type definitions
 */
#define MAXCHAR  100            /*maximum number of characters per line*/
#define MAXNAME  100            /*maximum number of characters per name*/

typedef enum {
    START_COMMENT = 1,
    COPY,
    SKIP
} LINE_ANALYSIS_RESULT;

/* 
 *Local functions  
 */

/*
 *****************************************************************************
 */

int getLine (
    FILE *filein, 
    char *line)
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
int getPageNumber(
    char IN     *line, 
    int  IN     start)
/*
 * find out whether the line, starting from position start, contains a page
 * number:
 *
 * REMARK: In this implementation the parsing algorithm is very simple.
 *         First it look for the first separator (a blank, a tab, a dot, a 
 *         figure), then to the first following figures and to the next 
 *         not-figure character.
 *         when the limits of the number are found, it tryes to decode
 *         the number.
 *         The decoded page number is returned, -1 elsewhere.
 */
{
int i = 0;                      /* index & counter */
int j = 0;                      /* index & counter */
int k = 0;                      /* index & counter */
int page = -1;                  /* page number, -1 if not found */
char ppp[MAXCHAR + 1] = "";     /* buffer for page number manipulation */

i = start;

while ((line[i] != ' ' ) &&     /* the token is terminated by a blank,     */
       (line[i] != '\t' ) &&    /*  a tab, a dot, a figure, the end-of-line*/
       (line[i] != '.' ) &&
       (line[i] != '0' ) &&
       (line[i] != '1' ) &&
       (line[i] != '2' ) &&
       (line[i] != '3' ) &&
       (line[i] != '4' ) &&
       (line[i] != '5' ) &&
       (line[i] != '6' ) &&
       (line[i] != '7' ) &&
       (line[i] != '8' ) &&
       (line[i] != '9' ) &&
       (line[i] != '\0'))
    {
    i++;
    }

j = i;

while ((line[i] == ' ' ) ||     /* skip blanks, tabs, dots    */
       (line[i] == '\t' ) ||   
       (line[i] == '.' ))
    {
    i++;
    }
 
j = i;

while ((line[i] == '0' ) ||     /* now i search only for figures */
       (line[i] == '1' ) ||
       (line[i] == '2' ) ||
       (line[i] == '3' ) ||
       (line[i] == '4' ) ||
       (line[i] == '5' ) ||
       (line[i] == '6' ) ||
       (line[i] == '7' ) ||
       (line[i] == '8' ) ||
       (line[i] == '9' ))
    {
    i++;
    }

/* 
 * if figures are found convert them into a number 
 */           
if ( i > j)                   
    {
    k = 0;
    while (j<=i)
        {
        ppp[k] = line[j];
        k++;
        j++;
        }
    ppp[k - 1] = '\0';

    sscanf(ppp, "%d", &page);
    }

return page;
}

/*
 *****************************************************************************
 */

LINE_ANALYSIS_RESULT treatLine (
    char IN OUT *line, 
    char IN     *previousLine,
    int  OUT    *page)
/*
 * find out whether the line:
 *   - start a new comment (START_COMMENT): the p, page, etc. keyword are at
 *     the beginning
 *   - is a text line or a blank line that shall be copied to output (COPY),
 *   - is a "---..." line or an extra blank line and shall be not copied (SKIP)
 *
 * REMARK: input line can be modified (blank lines are purged of blanks 
 *         and tabs)
 */

/*
 * REMARK: This the routine has multiple exits!!!!
 */
{
int i = 0;                      /* index & counter */
int p = 0;                      /* page number */
int length = 0;                 /* input string length  */

/*
 * Is it a blank line following a previous blank line?
 * ---------------------------------------------------
 * To check this the line is first purged of blanks and tabs and 
 * if the resultant length is 0 the line is compared with the previous line
 */

length = strlen(line);
i = length;
while ( i >=0 )                 /* starting from the end,         */
    {                           /*    blanks and tabs are skipped */
    if ( (line[i] == ' ' ) ||
         (line[i] == '\t' ) )
        length--;
    i--;
    }
if (length == 0)
    line[0] = '\0';

if (length == 0)
    {
    if (strlen(previousLine) == 0)
        {
        return SKIP;
        }
    else
        {
        return COPY;
        }
    }
else
    {
                               /* perform the following checks */
    }

/*
 * Is it a "---..." line?
 * ----------------------
 * To check this the first three characters are checked.
 */

if ((line[0] == '-') && 
    (line[1] == '-') &&
    (line[2] == '-'))
    {
    line[0] = '\0';
    return SKIP;
    }
/*
 * Is it the start of a comment?
 * -----------------------------
 * A line is the beginning of a comment if the first non-blank char is a "p"
 * or "P" followed by alphabetic chars and by a number, optionally separated
 * by "." or blanks.
 * So, first the existence of "p" or "P" the beginning of the line is checked. 
 * If a keyword is found then the number is decoded, if possible. 
 */
length = strlen(line);
i = 0;
while ((line[i] == ' ' ) ||     /* skip heading blanks and tabs */
       (line[i] == '\t' )) 
    {
    i++;
    }

if ((line[i] == 'p' ) ||        /* to be a comment must have P or p */
    (line[i] == 'P' )) 
    {
    p = getPageNumber(line, i);      /* get page number, if any */
    if (p >=0)
        {
        *page = p;
        return START_COMMENT; 
        }
    }
else
    {
                               /* perform the following checks */
    }

/*
 * If none of the previous cases, copy it:
 */

return COPY; 
}

/*
 *****************************************************************************
 */

FILE *openOutput(
    int  IN  page, 
    char IN  *author)
/*
 * open a new output file named:  <ppp>-<author>.tmp_single_comment
 * and initialize it (separator line and author name).
 */
{
char fileoutName[MAXNAME + 1] = "";     /* used to build file name */
FILE *fp;                               /* file pointer */
 
sprintf(fileoutName, "%3.3d-%s.tmp_single_comment", page, author);

/*
 * the file is open in append mode. This is to manage the case of more
 * than one comment on the same page from the same author. 
 *
 * REMARK: this need that there are no *.tmp_single_comment from previous
 *         computations.
 */

fp = fopen(fileoutName, "a");

fprintf(fp, "----------------------------------------");
fprintf(fp, "----------------------------------------\n");
fprintf(fp, "(%s)\n", author);
fflush(fp);

return fp;
}


/*
 *****************************************************************************
 */

void closeOutput(
    FILE IN *fileout, 
    char IN *previousLine)
/*
 * add a blank line, if nedeed, and close output file
 */
{
if (previousLine[0] != '\0')
    fprintf(fileout, "\n");

fclose(fileout);

return;
}


/*
 *****************************************************************************
 */

/* 
 * Main
 */

int main (int argc, char *argv[])
{
int i = 0;                      /* index & counter */

char line[MAXCHAR + 1] = "";            /* working line buffer  */
char previousLine [MAXCHAR + 1] = "";   /* previous line buffer  */

char author[MAXNAME + 1] = "";          /* author of the comments name */
int page = 0;                           /* page number */

FILE *filein = NULL;                    /* input file (author's comments) */
FILE *fileout = NULL;                   /* current output file (one comment) */ 

LINE_ANALYSIS_RESULT status = START_COMMENT;    /* result of line analysis */

/* 
 * check input parameter
 */
if (argc != 2)
    {
    printf("Usage:  splitIntoSingleComments  <input_file>\n");
    exit(-1);
    }
else
    {
    printf("-->Processing: %s\n", argv[1]);
    }
/* 
 * open input file
 */
filein = fopen(argv[1], "r");

/* 
 * extract from input file name (<author>.comments) the <author> field.
 *  REMARK: the first encountered dot '.' is used as separator.
 */
strcpy(author, argv[1]);
i = 0;
while (author[i] != '.')
    {
    i++;
    }
author[i] = '\0';

/* 
 * open the first output file (000): it will contains everything between the
 * beginning of the file and the first comment.
 */
fileout = openOutput(page, author);

/* 
 * process all lines in the input file
 */
while (getLine(filein, line) != EOF )
    {
    status = treatLine(line, previousLine, &page);
    switch (status)
        {
        case START_COMMENT:
            /* 
             * - close current output
             * - open a new output file (<ppp>-<author>.tmp_single_comments)
             * - write the sepaartor line (80 "-")
             * - copy the line into current output file
             */
            if ( previousLine[0] != '\0')
                fprintf(fileout, "\n");
            closeOutput(fileout, previousLine);

            fileout = openOutput(page, author);

            fprintf(fileout, "%s\n", line);
            break;

        case COPY:
            /*
             * - copy the line into current output file
             */
            fprintf(fileout, "%s\n", line);
            fflush(fileout);
            break;

        case SKIP:
            /*
             * - skip the line
             */
            break;

        default:
            /*
             * PANIC ERROR
             */
            printf("PANIC ERROR treatLine return status unknown: %d\n", status);
            exit(-1);
            break;
        }
    strcpy(previousLine, line);
    }

closeOutput(fileout, previousLine);

fclose(filein);
exit(0);
}

/*___oOo___*/


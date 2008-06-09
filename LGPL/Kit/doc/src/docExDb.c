/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: docExDb.c,v 1.32 2008/06/09 06:52:09 bjeram Exp $" 
*
* who       when      what
* --------  --------  ----------------------------------------------
* pforstma  16/08/94  created from N. Fiebieg source.
* pforstma  03/11/94  Added '#A' & '#S' comment handling.
* -1.9-----
* pforstma  21/02/95  Initialized tcomments in GetInfo (SPR 940326)
* psivera   08/06/02  removed warnings (for APR2003)
*/

/************************************************************************
*   NAME
*   docExDb - extract RTAP database point configuration information
* 
*   SYNOPSIS
*   docExdb <infile> [ <outfile>]
* 
*   DESCRIPTION
*
*   See docDbInfo.
*
*   The output is written to <outfile>. If this parameter is missing,
*   the output file name is <infile>_db.
*
*   FILES
*
*   ENVIRONMENT
*
*   RETURN VALUES
*
*    0: program succeeded.
*   -1: program failed due to file access.
*
*   CAUTIONS 
*   Comments on point are truncated to 74 characters.
*   Comments on attributes and fields (lines starting with '#A' or '#S')
*   are truncated to 35 characters;
*   
* 
*   EXAMPLES
*
*   SEE ALSO
*   docDoDbInfo
*
*   BUGS   
*   No check is made wether <infile> is a valid RTAP database point
*   configuration file.
* 
*------------------------------------------------------------------------
*/

#define _POSIX_SOURCE 1
#include "vltPort.h"


static char *rcsId="@(#) $Id: docExDb.c,v 1.32 2008/06/09 06:52:09 bjeram Exp $";
/* static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId); */


#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* limited by the 'man' page paragraph */
#define POINT_COMMENT_LENGTH     74
#define ATTRIBUTE_COMMENT_LENGTH 35
#define ATTRIBUTE_COMMENT_NB   1000


void print_dash_line(FILE *outfile)
{ 
    int i;
 
    fprintf(outfile,"\n");
    for (i=0; i < POINT_COMMENT_LENGTH; i++) 
         fprintf(outfile, "=");
    fprintf(outfile,"\n\n");

    }

void getInfo (int *header, FILE *infile, FILE *outfile )
    {
    char str[256], tempstr[256], *pstr, pc[256], rpc[256];
    char name[20] = "", 
         deType[10] = "", 
         Residence[10] = "", 
         Type[10] = "", 
         Count[10] = "";
    int ready = 0,
        infield = 0,
        inattribute = 0,
        printinfo =0,
        ncomments = 0;
    int i = 0;
    char tcomments[ATTRIBUTE_COMMENT_NB][ATTRIBUTE_COMMENT_LENGTH];
         

    inattribute = 1;    
    for (i=0; i < ATTRIBUTE_COMMENT_NB; i++)
	tcomments[i][0]='\0';
    

    while (!ready && !feof(infile))
        {
        /*
         *# read next line
         *# skip leading blanks
         *# remove trailing newline char
         */
        printinfo = 0;
        fgets(str,256,infile);        
        pstr = str;
        while (*pstr == ' ' && *pstr != '\0') pstr++;
        if (str[strlen(str)-1] == '\n') str[strlen(str)-1] = '\0';
        
        /*
         *# if comment line
         *#  
         */
        if (*pstr == '#')
	    {
            if ( ( (infield || inattribute == 1 ) && (*(pstr+1) == 'A') )
		|| *(pstr+1) == 'S')
                {
		if (ncomments > ATTRIBUTE_COMMENT_NB) {
		    fprintf(stderr, "docDoExDb: Too Many comments \n");
		    exit(-1);
		    }
		    
		strncpy(tcomments[ncomments], pstr + 2, ATTRIBUTE_COMMENT_LENGTH -1);
		tcomments[ncomments][ATTRIBUTE_COMMENT_LENGTH] = '\0';
                ncomments++;
                }
            if (*header == 1)
               strcpy(pc, pstr);

	    }
        
        /*
         *# else it is an item line
         */
        else
            {
            /*
             *# remove all blanks
             */
            i = 0;
            while (*pstr != '\0') 
                {
                if (*pstr != ' ') tempstr[i++] = *pstr;
                pstr++;
                }
            tempstr[i] = '\0';
            
            /*
             *# analyze item name
             */
            if (strstr(tempstr,"END") != NULL) 
                {
                if (infield) 
                   {
                   infield--;
                   strcpy(Type,"");
                   strcpy(Count,"");
		   }
                else {
                    if (inattribute)
			{
		        inattribute=0;
		        }
                    ready=1;
                    /* for ATTRIBUTE without TABLE */
                    printinfo = 1;
		    }
                }
            else if (strstr(tempstr,"BEGINATTRIBUTE") != NULL) 
                {
                *header=0;

                print_dash_line(outfile);
                fprintf(outfile,"Point: %-19s Residence: %s\n\n",name,
                        Residence);     
                fprintf(outfile,"%s\n\n",rpc);
                
                ncomments=0;
            }
            else if (strstr(tempstr,"Name=") != NULL)
                {
                pstr = (char *)strstr(tempstr,"Name=") + strlen("Name=");
                strcpy(name,pstr);
                if (*header == 1)
                  strcpy(rpc, pc);
                }
            else if (strstr(tempstr,"DeType=") != NULL)
                {
                pstr = (char *)strstr(tempstr,"DeType=") + strlen("DeType=");
                strcpy(deType,pstr);
                }
            else if (strstr(tempstr,"Residence=") != NULL)
                {
                pstr = (char *)strstr(tempstr,"Residence=") + 
                       strlen("Residence=");
                strcpy(Residence,pstr);
                }
            else if (strstr(tempstr,"Type=") != NULL)
                {
                pstr = (char *)strstr(tempstr,"Type=") + strlen("Type=rt");
                strcpy(Type,pstr);
                }
            else if (strstr(tempstr,"RecordCount=") != NULL)
                {
                pstr = (char *)strstr(tempstr,"RecordCount=") + 
                       strlen("RecordCount=");
                strcpy(Count,pstr);
                }
            else if (strcmp(tempstr,"BEGINFIELD") == 0)
                {
                infield++;
                /* for TABLE and preceding FIELD */
                printinfo = 1;
                }

            }
        if (printinfo)
            {
            if (Type[0] == '\0')
                Type[0]=' ';

            fprintf(outfile,"%-19s %-8s %c %-4s ",name, deType, Type[0],Count);
	    
	    fprintf(outfile, "#%.*s \n", ATTRIBUTE_COMMENT_LENGTH+2, tcomments[0]);
	    for (i=1; i < ncomments; i++)
	         fprintf(outfile, "                                    #%.*s \n", ATTRIBUTE_COMMENT_LENGTH+2, tcomments[i]);

            ncomments=0;
            }

        }
    }
    
    
int main ( int argc, char * argv[] )
    {
    FILE *infile, *outfile;
    char str[256];
    char outname[256];
    int header = 1;
    
    if (argc < 2) 
        {
        printf("  docExDb <infile> [<outfile>]\n");
        printf("  <infile>  Name of point definition file\n");
        printf("  <outfile> Name of output file (default=<infile>_db)\n\n");
        return 0;
        }
      
    /*
     *# open input file
     */
    infile = fopen(argv[1],"r");
    if (infile == NULL) 
        {
        printf("\ndocExDb - Error in opening input file %s\n",argv[1]);
        return -1;
        }
    
    /*
     *# create output file
     */
    if (argc == 3) strcpy(outname,argv[2]);
    else sprintf(outname,"%s_db",argv[1]);
    outfile = fopen(outname,"w");
    if (outfile == NULL) 
        {
        printf("\ndocExDb - Error in opening output file %s\n",outname);
        fclose(infile);
        return -1;
        }

    /*
     *# through the whole input file
     */
    while (!feof(infile))
        {
        /*
         *# read the next input line
         */
        fgets(str,256,infile);
        /*
         *# if point header extract name and residence
         */
        /*
         *# if begin of attribute definition then extract information
         */
        if (((str[0] != '#') && (strstr(str,"BEGIN ATTRIBUTE") != NULL))
             || header)
            {
            getInfo(&header, infile, outfile);
            }
        }
        
    /*
     *# close files
     */
    fclose(infile);
    fclose(outfile);
    return 0;
    }
    
    
    

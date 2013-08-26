/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration)
*    and Cosylab 2002, All rights reserved
*
*    This library is free software; you can redistribute it and/or
*    modify it under the terms of the GNU Lesser General Public
*    License as published by the Free Software Foundation; either
*    version 2.1 of the License, or (at your option) any later version.
*
*    This library is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*    Lesser General Public License for more details.
*
*    You should have received a copy of the GNU Lesser General Public
*    License along with this library; if not, write to the Free Software
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
*
* who       when      what
* --------  --------  ----------------------------------------------
* gchiozzi 2002-11-25 Replaced VLTDATA with ACSDATA (but actually not used here)
* mcomin    08/06/95  created
* T.EBERT   27/06/95  changed _POSIX_SOURCE to _ALL_SOURCE to work on SUN
* mcomin    15/11/95  Search extended to VLTDATA directory
* ------------------
* mcomin    03/04/96  Use of function getcwd to get current directory
*/

//#define _ALL_SOURCE 1
#include "vltPort.h"

static char *rcsId="@(#) $Id: acsutilFindFile.cpp,v 1.6 2012/01/20 22:07:43 tstaig Exp $"; 
static void *use_rcsId = ((void)(void)&use_rcsId,(void *) &rcsId);

/* 
 * System Headers
 */
#include <stdlib.h>
#include <ace/OS_NS_string.h>
#include <unistd.h>
//#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>

/* 
 * Local Headers
 */

#include "acsutilFindFile.h"


int acsFindFile (const   char *fileName,
                           char         *filePath,
                           char      *mode,
                           int    *size,
                           char      *dirFlag)
{
    char myFile[1024];
    char cwd[1024];
    struct stat stbuf;
    char *pchar,*tok,*intRoot,*intList,*acsRoot;/*,*acsData;*/
    
    /* Initialization */
    
    strcpy(filePath,"");
    *mode = 0; *size = 0; *dirFlag = 0;
    
    if ( fileName == (char *) NULL ) return 0;
    if ( filePath == (char *) NULL ) return 0;

    /*
     *  Check if file exist in the current directory
     */    
      
    if ( (pchar = strpbrk(fileName,"/")) != (char *) NULL) 
       { 
       pchar ++ ; strcpy(myFile,pchar);
       }
    else
       strcpy(myFile,fileName);

    
    if (stat(myFile,&stbuf) == 0) { goto exit_success; }

    /*
     *  Check if file exist as current sub-directory
     */    
     
    if (getcwd(cwd,sizeof(cwd)) != (char *) NULL)
       sprintf(myFile,"%s/",cwd);
    else
       strcpy(myFile,"./");
     
    strcat(myFile,fileName);
    
    if (stat(myFile,&stbuf) == 0) { goto exit_success; }

    /*
     *  Check if file exist as sub-directory of current module
     */    
     
    strcpy(myFile,"../");
    strcat(myFile,fileName); 
    
    if (stat(myFile,&stbuf) == 0) { goto exit_success;}

    /*
     *  Check if file exist in INTROOT INTLIST ACSROOT ??? ACSDATA ???
     */    

    intRoot = getenv ("INTROOT");
    if (intRoot != (char *) NULL) 
	{
	sprintf(myFile,"%s/%s",intRoot,fileName);
	if (stat(myFile,&stbuf) == 0) { goto exit_success;}
	}

    intList = getenv ("INTLIST");
    if (intList!=NULL)
	{
	intList = ACE_OS::strdup(getenv ("INTLIST"));
	tok = strtok(intList,":");
	while ( tok != NULL )
	    {
	    sprintf(myFile,"%s/%s",tok,fileName);
	    if (stat(myFile,&stbuf) == 0) { goto exit_success;}
	    tok = strtok (NULL, ":");
	    }
	free(intList);
	}//if
    
    acsRoot = getenv ("ACSROOT");      
    if (acsRoot != (char *) NULL) 
	{
	sprintf(myFile,"%s/%s",acsRoot,fileName);
	if (stat(myFile,&stbuf) == 0) { goto exit_success;}
	}     
    
    /*
     *  File does not exist
     */   

    return 0;
    
exit_success:

    strcpy(filePath,myFile);
    *mode    = stbuf.st_mode;
    *size    = stbuf.st_size;
    
    if ((stbuf.st_mode & S_IFDIR) == S_IFDIR ) *dirFlag = 1;
    
    return 1;
}


int ccsFileExist (char *filePath )
{
    struct stat stbuf;
    
    if ( filePath  == (char *) NULL ) return 0;

    if (stat(filePath,&stbuf) == 0) {
    return 1;
    } else {
    return 0;
    }
}

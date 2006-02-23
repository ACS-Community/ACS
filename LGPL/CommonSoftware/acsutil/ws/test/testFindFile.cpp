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
* "@(#) $Id: testFindFile.cpp,v 1.1 2004/10/27 08:47:48 gchiozzi Exp $"
*/

#include <stdio.h>
#include <stdlib.h>
#include <acsutilFindFile.h>

#ifndef MAKE_VXWORKS
 int main(int argc, char *argv[])
#else
 int testFindFile (char *szCmdLn)
#endif
{
    char  mode, dirMode, filePath[256];
    int fileSize;

    if (!acsFindFile (argv[1], filePath, &mode, &fileSize, &dirMode))
	{
	printf("File %s could not be found\n", argv[1]);
	return -1;
	}
 
    printf("File %s found\n", argv[1]);
    printf("   Path: %s\n", filePath);
    printf("   Size: %d\n", fileSize);
    return 0;
}

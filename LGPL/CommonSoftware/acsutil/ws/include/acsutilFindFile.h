#ifndef ACS_FIND_FILE_H
#define ACS_FIND_FILE_H

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
*/

/** @file acsutilFindFile.h
 *  Header file for ACS file utility functions.
 */

/**
 * acsFindFile is a function which is quite literally used to find a file
 * in the ALMA Software Engineering prescribed file heirachy. You should 
 * provide the name of the file you're looking for (e.g., 
 * "lib/python/site-packages/myFile.py") and also pointers to memory addresses
 * where the results of this function will be stored. The only other thing
 * to mention is the file must be in INTROOT, INTLIST, or ACSROOT.
 * @param fileName Name of the file you are searching for.
 * @param filePath An empty pointer where this function will store the full
 * path name of the file you are searching for. If this is not found,
 * this pointer is set to "".
 * @param mode An empty pointer where the mode of the file you are 
 * searching for (file permissions) is stored.
 * @param size An empty pointer where the size of the file you are searching 
 * for in bytes is stored.
 * @param dirFlag An empty pointer where if this is a directory, 
 * the value is set to 1. 0 otherwise.
 * @returns 0 on an error condition and 1 otherwise.
 *
 * NOTES:
 * - should have a boolean return value
 * - dirFlag should be a boolean
 * - shouldn't size be a long or larger!?!
 */
int acsFindFile (const char *fileName,      
		 char       *filePath,      
		 char       *mode,          
		 int        *size,         
		 char       *dirFlag);

/**
 * Function which checks to see if a file exists.
 * @param filePath Full file path name of the file we're looking for.
 * @return 1 if the file exists and 0 otherwise.
 *
 * NOTES:
 * - return value should be boolean
 * - why is this called "ccs"FileExist?
 */
int ccsFileExist (char *filePath );

#endif

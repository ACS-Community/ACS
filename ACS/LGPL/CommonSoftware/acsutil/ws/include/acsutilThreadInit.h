#ifndef acsutilThreadInit_h
#define acsutilThreadInit_h

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
* "@(#) $Id: acsutilThreadInit.h,v 1.16 2005/08/26 22:22:54 dfugate Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2001/04/19  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

/** @file acsutilThreadInit.h
 * Header for thread initialization. Just includes a couple of 
 * function pointer typedefs.
 */

//
// Thread initializer functions
//

/** @defgroup InitThreadFuncTemplate InitThreadFunc 
 * Typedef for thread initialiation function. 
 * @{
 * The only parameter
 * to this void method is the name of the thread.
 */
typedef void (*InitThreadFunc)(const char * threadName);
/** @} */

/** @defgroup DoneThreadFuncTemplate DoneThreadFunc 
 * Typedef for thread destruction function. 
 * @{
 */
typedef void (*DoneThreadFunc)();
/** @} */


#endif   /* acsutilThreadInit_h */ 

// ************************************************************************
//
// REVISION HISTORY:
//
// $Log: acsutilThreadInit.h,v $
// Revision 1.16  2005/08/26 22:22:54  dfugate
// Improved inline documentation a great deal.
//
// Revision 1.15  2003/03/10 14:33:09  rgeorgie
// LGPL
//
// Revision 1.14  2002/12/05 12:31:54  vltsccm
// gchiozzi: Added proper GPL licence header to wildcard library
//
// ************************************************************************

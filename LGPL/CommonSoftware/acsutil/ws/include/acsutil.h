#ifndef acsutil_h
#define acsutil_h

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
* "@(#) $Id: acsutil.h,v 1.21 2012/01/20 22:07:43 tstaig Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram    2003/05/05  turnedon native namspeace since we are using tornado 2.2
* msekoran  2001/04/19  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

/** @file acsutil.h
 * Header file for lots of fun goodies probably of no interest 
 * to anyone outside of the ACS team.
 */

#include <vector>
#include <ace/OS.h>
#include <ace/Functor_String.h>


/**
 * Means ACS uses TAO.
 */
#define ACS_HAS_TAO			// use TAO 

/**
 * Means ???.
 */
#define CDB_HAS_ANY

/**
 * This old macro dealing with Orbacus can probably go.
 */
#undef ACS_HAS_OB			// do not use Orbacus
/**
 * Means we do not support the Windows OS.
 */
#undef ACS_HAS_WIN32		// we are not using windows

/**
 * Means ACS uses shared libraries.
 */
#define ACS_HAS_DLL
/**
 * Means ACS does not use static libraries.
 */
#undef  ACS_HAS_STATIC_LIBS

// automatically set WIN32 platform
#if defined (_MSC_VER)
#define ACS_HAS_WIN32
#endif

#ifdef MAKE_VXWORKS
#include <vsprintf.h>
#endif

/**
 * Some sort of export flag.
 */
#define ACS_DLL_EXPORT ACE_Proper_Export_Flag
/**
 * Some sort of export flag.
 */
#define ACS_DLL_IMPORT ACE_Proper_Import_Flag
/**
 * Some sort of export flag.
 */
#define ACS_DLL_UNMANGLED_EXPORT extern "C" ACS_DLL_EXPORT

//
// DESCRIPTION: Namespace support
//
/**
 * Defines we can use namespaces.
 * NOTE:
 * - why is this necessary any more?
 */
#define USING_NAMESPACES


#ifdef USING_NAMESPACES
#	define NAMESPACE_BEGIN(ns) namespace ns {
#	define NAMESPACE_END(ns) }
#	define NAMESPACE_USE(ns) using namespace ns;
#	define NAMESPACE_DIR(ns, code) ns##::##code 			// semicolon not set by macro
#else
#	define NAMESPACE_BEGIN(ns)
#	define NAMESPACE_END(ns)
#	define NAMESPACE_USE(ns)
#	define NAMESPACE_DIR(ns, code) code				// semicolon not set by macro
#endif

/**
 * DESCRIPTION: Allocate memory on the heap.
 *
 * Allocate memory on the heap using constructor CONSTRUCTOR and point
 * POINTER to it. In case of a failure, set *errno* to *ENOMEM* and
 * return from the current function with RET_VAL.
 *
 * NOTE: Do not use this macro when using placement *new*; rather use
 *       ACE_NEW_RETURN.
 *
 * Placement *new* allows you to to preallocate memory, and at a later time
 * instantiate an object into that memory. Of course, you can instantiate
 * objects in that same memory more than once, thereby reducing the number
 * of memory allocations.
 *
 * EXAMPLE:
 *
 *       MyClass* AttemptToAllocate()
 *       {
 *           MyClass *my_object;
 *
 *           ACS_NEW_RETURN(my_object, MyClass(), 0);
 *           return my_object;
 *       }
 */
#define ACS_NEW_RETURN(POINTER,CONSTRUCTOR,RET_VAL) ACE_NEW_RETURN(POINTER,CONSTRUCTOR,RET_VAL)

/**
 * DESCRIPTION: Allocate memory on the heap.
 *
 * Same as ACS_NEW_RETURN, but doesn't return a specific value from
 * the current function. Use this in functions returning void, and
 * ACS_NEW_RETURN in all other functions.
 *
 * SEE ALSO: ACE_NEW
 */
#define ACS_NEW(POINTER,CONSTRUCTOR) ACE_NEW(POINTER,CONSTRUCTOR)

/**
 * Vector of ACE_CString
 */
typedef std::vector<ACE_CString> ACE_CString_Vector;

#endif  /* acsutil_h */

// ************************************************************************
//
// REVISION HISTORY:
//
// $Log: acsutil.h,v $
// Revision 1.21  2012/01/20 22:07:43  tstaig
// Backport from branches ACS-9_0_0-windows-B and ACS-9_1_0-windows-B to support
// ACS on Windows under Cygwin. This commit corresponds to the folowing
// CommonSoftware modules:
// jacsutil acsEclipseUtils xmljbind xmlpybind acserridl acsidlcommon acsutil
// acsutilpy acsstartup loggingidl logging acserr acserrTypes acsQoS
// Along with adding dependencies for some libraries in acsdaemon and acstime
// modules so they would be built correctly.
//
// Revision 1.20  2008/07/28 07:01:40  cparedes
// removing the using namespace
//
// Revision 1.19  2005/08/26 22:22:54  dfugate
// Improved inline documentation a great deal.
//
// Revision 1.18  2005/04/26 08:03:46  bjeram
// added vsprintf include for VXWORKS
//
// Revision 1.17  2005/04/12 12:48:56  acaproni
// Added the following definition used in several modules:
// typedef std::vector<ACE_CString> ACE_CString_Vector;
//
// Revision 1.16  2003/05/06 13:26:10  bjeram
// porting to Tornado 2.2 (namespaces)
//
// Revision 1.15  2003/03/10 14:33:09  rgeorgie
// LGPL
//
// Revision 1.14  2002/12/05 12:31:53  vltsccm
// gchiozzi: Added proper GPL licence header to wildcard library
//
// ************************************************************************

#ifndef __cdb__Data_Types_h__
#define __cdb__Data_Types_h__
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2011
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: cdbData_Types.h,v 1.26 2011/10/28 15:05:05 hsommer Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* almadev  2011-10-28  created
*/

#include "acsutil.h"
#include "cdbExport.h"

#if defined(CDB_HAS_ANY)
// We need TAO to provide the definition of the Any type and of portable
// scalar data types.
#include <tao/corba.h>

#include <orbsvcs/orbsvcs/DsLogAdminC.h>   // DsLogAdmin::Anys support

#endif // defined(CDB_HAS_ANY)

#include <vector>
#include <ace/SString.h>

namespace cdb {

// ------------------------------------------------------------------------
// GROUP = Scalar data types
// ------------------------------------------------------------------------

//typedef std::string      String;
typedef ACE_CString String;

#if !defined(CDB_HAS_ANY)

// DESCRIPTION: An 8-bit unsigned integer.
typedef unsigned char    Octet;
// DESCRIPTION: A TRUE/FALSE boolean value.
typedef bool             Boolean;

// DESCRIPTION: A 16-bit signed integer.
typedef signed short     Short;
// DESCRIPTION: A 32-bit signed integer.
typedef signed long      Long;

// DESCRIPTION: A 16-bit unsigned integer.
typedef unsigned short   UShort;
// DESCRIPTION: A 32-bin unsigned integer.
typedef unsigned long    ULong;

#if defined(__GNUG__)
// DESCRIPTION: A 64-bin signed integer.
typedef signed long long   LongLong;
// DESCRIPTION: A 64-bin unsigned integer.
typedef unsigned long long ULongLong;
#elif _MSC_VER >= 1100
typedef signed __int64   LongLong;
typedef unsigned __int64 ULongLong;
#endif // _MSC_VER >= 1100

// DESCRIPTION: IEEE compliant 4-byte floating point (single precision).
typedef float  Float;
// DESCRIPTION: IEEE compliant 8-byte floating point (double precision).
typedef double Double;

#if !defined(FALSE)
//{partOf: Boolean}
#  define FALSE 0
#endif
#if !defined(TRUE)
//{partOf: Boolean}
#  define TRUE 1
#endif

#else

//
// If we are using the Any type, we must have included ACE. We therefore
// wrap ACE's datatypes to make use of its portability guarantees.
//
typedef DsLogAdmin::Anys Anys;

typedef CORBA::Any       Any;
typedef CORBA::Octet     Octet;
typedef CORBA::Boolean   Boolean;

typedef CORBA::Short     Short;
typedef CORBA::Long      Long;
typedef CORBA::LongLong  LongLong;

typedef CORBA::UShort    UShort;
typedef CORBA::ULong     ULong;
typedef CORBA::ULongLong ULongLong;

typedef CORBA::Float     Float;
typedef CORBA::Double    Double;

#if !defined(FALSE)
#  define FALSE 0
#  define TRUE 1
#endif

#endif // defined(CDB_HAS_ANY)

#if defined(__GNUG__)

//
// DESCRIPTION: Macro for specifying 64-bit integer literals.
//
// EXAMPLE:
//
//       ULongLong ull = CDB_ULONGLONG(0x8000000000000000);
//
#  define CDB_LONGLONG(n) n##LL

//{partOf:CDB_LONGLONG}
#  define CDB_ULONGLONG(n) n##ULL

#elif _MSC_VER >= 1100

#  define CDB_LONGLONG(n) n
#  define CDB_ULONGLONG(n) n

#endif // _MSC_VER >= 1100

// ------------------------------------------------------------------------
// GROUP = Arrays
// ------------------------------------------------------------------------

#if defined(CDB_HAS_ANY)
//{partOf:Arrays}
typedef std::vector<Any>       AnyArray;
#endif // defined(CDB_HAS_ANY)

//{partOf:Arrays}
typedef std::vector<String>    StringArray;
//{partOf:Arrays}
typedef std::vector<Octet>     OctetArray;

//{partOf:Arrays}
typedef std::vector<Short>     ShortArray;
//{partOf:Arrays}
typedef std::vector<Long>      LongArray;
//{partOf:Arrays}
typedef std::vector<LongLong>  LongLongArray;

typedef std::vector<UShort>    UShortArray;
typedef std::vector<ULong>     ULongArray;
typedef std::vector<ULongLong> ULongLongArray;

typedef std::vector<Float>     FloatArray;
typedef std::vector<Double>    DoubleArray;

 }; 

#endif // __cdb__Data_Types_h__

// ************************************************************************
//
// REVISION HISTORY:
//
//   $Log: cdbData_Types.h,v $
//   Revision 1.26  2011/10/28 15:05:05  hsommer
//   Manually fixed "no LGPL license text" issue reported by addCopyright.py
//
//   Revision 1.25  2006/09/01 02:20:54  cparedes
//   small change, NAMESPACE_BEGIN / NAMESPACE_END / NAMESPACE_USE macross to clean up a little the cpp code
//
//   Revision 1.24  2003/01/28 16:43:49  vltsccm
//   gchiozzi: patch for cdb module to create lib/endorsed directory, since CVS cannot restore empty directories
//
//   Revision 1.23  2003/01/24 10:44:03  vltsccm
//   cdb1.23
//
//   Revision 1.22  2003/01/20 15:12:19  vltsccm
//   cdb1.22
//
//   Revision 1.21  2003/01/20 10:45:53  vltsccm
//   cdb1.21
//
//   Revision 1.20  2002/12/05 16:03:58  vltsccm
//   cdb1.20
//
//   Revision 1.19  2002/11/25 16:04:49  vltsccm
//   cdb1.19
//
//   Revision 1.18  2002/11/13 14:53:04  vltsccm
//   cdb1.18
//
//   Revision 1.17  2002/11/13 10:22:30  vltsccm
//   cdb1.17
//
//   Revision 1.16  2002/11/06 08:37:04  vltsccm
//   cdb1.16
//
//   Revision 1.15.1.23  2002/11/05 16:05:13  vltsccm
//   cdb1.15.1.23
//
//   Revision 1.15.1.22  2002/11/05 13:46:30  vltsccm
//   cdb1.15.1.22
//
//   Revision 1.15.1.21  2002/11/05 10:41:14  vltsccm
//   cdb1.15.1.21
//
//   Revision 1.15.1.20  2002/11/01 12:49:02  vltsccm
//   cdb1.15.1.20
//
//   Revision 1.15.1.19  2002/10/30 07:56:43  vltsccm
//   cdb1.15.1.19
//
//   Revision 1.15.1.18  2002/10/25 12:44:23  vltsccm
//   cdb1.15.1.18
//
//   Revision 1.15.1.17  2002/10/24 13:08:43  vltsccm
//   cdb1.15.1.17
//
//   Revision 1.15.1.16  2002/10/16 11:43:44  vltsccm
//   cdb1.15.1.16
//
//   Revision 1.15.1.15  2002/10/14 22:26:09  vltsccm
//   cdb1.15.1.15
//
//   Revision 1.15.1.14  2002/10/14 12:18:32  vltsccm
//   cdb1.15.1.14
//
//   Revision 1.15.1.13  2002/10/04 16:20:23  vltsccm
//   cdb1.15.1.13
//
//   Revision 1.15.1.12  2002/10/02 12:54:14  vltsccm
//   cdb1.15.1.12
//
//   Revision 1.15.1.11  2002/10/01 10:33:25  vltsccm
//   cdb1.15.1.11
//
//   Revision 1.15.1.10  2002/09/30 13:56:51  vltsccm
//   cdb1.15.1.10
//
//   Revision 1.15.1.9  2002/09/26 14:13:10  vltsccm
//   cdb1.15.1.9
//
//   Revision 1.15.1.8  2002/09/26 07:45:46  vltsccm
//   cdb1.15.1.8
//
//   Revision 1.15.1.7  2002/09/17 16:19:22  vltsccm
//   cdb1.15.1.7
//
//   Revision 1.15.1.6  2002/09/17 11:15:47  vltsccm
//   cdb1.15.1.6
//
//   Revision 1.15.1.5  2002/09/02 09:37:06  vltsccm
//   cdb1.15.1.5
//
//   Revision 1.15.1.4  2002/08/09 09:35:22  vltsccm
//   cdb1.15.1.4
//
//   Revision 1.15.1.3  2002/07/24 07:29:10  vltsccm
//   cdb1.15.1.3
//
//   Revision 1.15.1.2  2002/07/12 09:58:16  vltsccm
//   cdb1.15.1.2
//
//   Revision 1.15+.1.1  2002/07/09 09:40:08  vltsccm
//   cdb1.15.1
//
//   Revision 1.15  2002/02/05 17:50:07  vltsccm
//   cdb1.15
//
//   Revision 1.14  2002/01/14 21:14:18  vltsccm
//   cdb1.14
//
//   Revision 1.13  2001/10/19 09:56:22  vltsccm
//   cdb1.13
//
//   Revision 1.12  2001/09/18 10:07:11  vltsccm
//   cdb1.12
//
//   Revision 1.11  2001/07/12 07:48:26  vltsccm
//   cdb1.11
//
//   Revision 1.10  2001/07/11 09:16:13  vltsccm
//   cdb1.10
//
//   Revision 1.6  2000/12/07 18:00:40  vltsccm
//   cdb1.6
//
//   Revision 1.5  2000/11/17 13:14:56  vltsccm
//   cdb1.5
//
//   Revision 1.4  2000/10/20 13:51:16  vltsccm
//   cdb1.4
//
//   Revision 1.3  2000/10/20 13:51:15  vltsccm
//   cdb1.3
//
//   Revision 1.2  2000/10/20 13:51:15  vltsccm
//   cdb1.2
//
//   Revision 1.1  2000/10/20 13:51:15  vltsccm
//   cdb1.1
//
//   Revision 1.0  2000/10/20 13:51:14  vltsccm
//   cdb1.0
//
//   Revision 1.3  2000/10/13 16:03:02  vltsccm
//   cdb1.3
//
//   Revision 1.2  2000/09/13 14:49:28  vltsccm
//   cdb1.2
//
//   Revision 1.1  2000/09/06 15:42:11  vltsccm
//   cdb1.1
//
//   Revision 1.1  2000/06/13 07:26:24  kzagar
//   CDB, initial commit. Documentation not yet finished.
//
// ************************************************************************

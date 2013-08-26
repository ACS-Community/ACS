/*
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2003 
*
*This library is free software; you can redistribute it and/or
*modify it under the terms of the GNU Lesser General Public
*License as published by the Free Software Foundation; either
*version 2.1 of the License, or (at your option) any later version.
*
*This library is distributed in the hope that it will be useful,
*but WITHOUT ANY WARRANTY; without even the implied warranty of
*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*Lesser General Public License for more details.
*
*You should have received a copy of the GNU Lesser General Public
*License along with this library; if not, write to the Free Software
*Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/

/** 
 * @file 
 * Header file BACI Export Macros.
 */


// -*- C++ -*-
// generate_export_file.pl, v1.0 Matej Sekoranja (adpated from ACE)
// Definition for Win32 Export directives.
// This file is generated automatically by generateExportFile.pl
// ------------------------------
#ifndef BACI_EXPORT_H
#define BACI_EXPORT_H

#include <acsutil.h>

#if defined (ACS_HAS_STATIC_LIBS) && !defined (BACI_HAS_DLL)
#  define BACI_HAS_DLL 0
#endif /* ACS_HAS_STATIC_LIBS && BACI_HAS_DLL */

#if !defined (BACI_HAS_DLL)
#  define BACI_HAS_DLL 1
#endif /* ! BACI_HAS_DLL */

#if defined (BACI_HAS_DLL) && (BACI_HAS_DLL == 1)
#  if defined (BACI_BUILD_DLL)
#    define baci_EXPORT ACS_DLL_EXPORT
#    define BACI_SINGLETON_DECLARATION(T) ACE_EXPORT_SINGLETON_DECLARATION (T)
#    define BACI_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK) ACE_EXPORT_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#  else /* BACI_BUILD_DLL */
#    define baci_EXPORT ACS_DLL_IMPORT
#    define BACI_SINGLETON_DECLARATION(T) ACE_IMPORT_SINGLETON_DECLARATION (T)
#    define BACI_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK) ACE_IMPORT_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#  endif /* BACI_BUILD_DLL */
#else /* BACI_HAS_DLL == 1 */
#  define baci_EXPORT
#  define BACI_SINGLETON_DECLARATION(T)
#  define BACI_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#endif /* BACI_HAS_DLL == 1 */

#endif /* BACI_EXPORT_H */

// End of auto generated file.

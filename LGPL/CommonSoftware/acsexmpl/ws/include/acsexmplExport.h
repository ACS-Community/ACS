/*******************************************************************************
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *   This library is free software; you can redistribute it and/or
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
 */
////////////////////////////////////////////////////////////////////////////////
// -*- C++ -*-
//
// generate_export_file.pl, v1.0 Matej Sekoranja (adpated from ACE)
// Definition for Win32 Export directives.
// This file is generated automatically by generateExportFile.pl
// ------------------------------
#ifndef ACSEXMPL_EXPORT_H
#define ACSEXMPL_EXPORT_H

#include <acsutil.h>


/** @file acsexmplExport.h
 *  Header file used for Win32 Export directives.
 */

#if !defined (ACSEXMPL_HAS_DLL)
#  define ACSEXMPL_HAS_DLL 1
#endif /* ! ACSEXMPL_HAS_DLL */

#if defined (ACSEXMPL_HAS_DLL) && (ACSEXMPL_HAS_DLL == 1)
#  if defined (ACSEXMPL_BUILD_DLL)
#    define acsexmpl_EXPORT ACS_DLL_EXPORT
#    define ACSEXMPL_SINGLETON_DECLARATION(T) ACE_EXPORT_SINGLETON_DECLARATION (T)
#    define ACSEXMPL_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK) ACE_EXPORT_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#  else /* ACSEXMPL_BUILD_DLL */
#    define acsexmpl_EXPORT ACS_DLL_IMPORT
#    define ACSEXMPL_SINGLETON_DECLARATION(T) ACE_IMPORT_SINGLETON_DECLARATION (T)
#    define ACSEXMPL_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK) ACE_IMPORT_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#  endif /* ACSEXMPL_BUILD_DLL */
#else /* ACSEXMPL_HAS_DLL == 1 */
#  define acsexmpl_EXPORT
#  define ACSEXMPL_SINGLETON_DECLARATION(T)
#  define ACSEXMPL_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#endif /* ACSEXMPL_HAS_DLL == 1 */

#endif /* ACSEXMPL_EXPORT_H */

// End of auto generated file.

// -*- C++ -*-
// generate_export_file.pl, v1.0 Matej Sekoranja (adpated from ACE)
// Definition for Win32 Export directives.
// This file is generated automatically by generateExportFile.pl
// ------------------------------

/** @file acsutilExport.h
 * Header file for macros dealing with ACE singletons. Since this file is generated
 * no other Doxygen documentation is available.
 */

#ifndef ACSUTIL_EXPORT_H
#define ACSUTIL_EXPORT_H

#include <acsutil.h>

#if !defined (ACSUTIL_HAS_DLL)
#  define ACSUTIL_HAS_DLL 1
#endif /* ! ACSUTIL_HAS_DLL */

#if defined (ACSUTIL_HAS_DLL) && (ACSUTIL_HAS_DLL == 1)
#  if defined (ACSUTIL_BUILD_DLL)
#    define acsutil_EXPORT ACS_DLL_EXPORT
#    define ACSUTIL_SINGLETON_DECLARATION(T) ACE_EXPORT_SINGLETON_DECLARATION (T)
#    define ACSUTIL_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK) ACE_EXPORT_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#  else /* ACSUTIL_BUILD_DLL */
#    define acsutil_EXPORT ACS_DLL_IMPORT
#    define ACSUTIL_SINGLETON_DECLARATION(T) ACE_IMPORT_SINGLETON_DECLARATION (T)
#    define ACSUTIL_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK) ACE_IMPORT_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#  endif /* ACSUTIL_BUILD_DLL */
#else /* ACSUTIL_HAS_DLL == 1 */
#  define acsutil_EXPORT
#  define ACSUTIL_SINGLETON_DECLARATION(T)
#  define ACSUTIL_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#endif /* ACSUTIL_HAS_DLL == 1 */

#endif /* ACSUTIL_EXPORT_H */

// End of auto generated file.

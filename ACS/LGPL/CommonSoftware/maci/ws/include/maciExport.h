
// -*- C++ -*-
// generate_export_file.pl, v1.0 Matej Sekoranja (adpated from ACE)
// Definition for Win32 Export directives.
// This file is generated automatically by generateExportFile.pl
// ------------------------------
#ifndef MACI_EXPORT_H
#define MACI_EXPORT_H

#include <acsutil.h>

#if defined (ACS_HAS_STATIC_LIBS) && !defined (MACI_HAS_DLL)
#  define MACI_HAS_DLL 0
#endif /* ACS_HAS_STATIC_LIBS && MACI_HAS_DLL */

#if !defined (MACI_HAS_DLL)
#  define MACI_HAS_DLL 1
#endif /* ! MACI_HAS_DLL */

#if defined (MACI_HAS_DLL) && (MACI_HAS_DLL == 1)
#  if defined (MACI_BUILD_DLL)
#    define maci_EXPORT ACS_DLL_EXPORT
#    define MACI_SINGLETON_DECLARATION(T) ACE_EXPORT_SINGLETON_DECLARATION (T)
#    define MACI_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK) ACE_EXPORT_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#  else /* MACI_BUILD_DLL */
#    define maci_EXPORT ACS_DLL_IMPORT
#    define MACI_SINGLETON_DECLARATION(T) ACE_IMPORT_SINGLETON_DECLARATION (T)
#    define MACI_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK) ACE_IMPORT_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#  endif /* MACI_BUILD_DLL */
#else /* MACI_HAS_DLL == 1 */
#  define maci_EXPORT
#  define MACI_SINGLETON_DECLARATION(T)
#  define MACI_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#endif /* MACI_HAS_DLL == 1 */

#endif /* MACI_EXPORT_H */

// End of auto generated file.

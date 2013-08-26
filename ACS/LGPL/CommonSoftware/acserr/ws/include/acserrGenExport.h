
// -*- C++ -*-
// generate_export_file.pl, v1.0 Matej Sekoranja (adpated from ACE)
// Definition for Win32 Export directives.
// This file is generated automatically by generateExportFile.pl
// ------------------------------
#ifndef ACSERRGEN_EXPORT_H
#define ACSERRGEN_EXPORT_H

#include <acsutil.h>

#if defined (ACS_HAS_STATIC_LIBS) && !defined (ACSERRGEN_HAS_DLL)
#  define ACSERRGEN_HAS_DLL 0
#endif /* ACS_HAS_STATIC_LIBS && ACSERRGEN_HAS_DLL */

#if !defined (ACSERRGEN_HAS_DLL)
#  define ACSERRGEN_HAS_DLL 1
#endif /* ! ACSERRGEN_HAS_DLL */

#if defined (ACSERRGEN_HAS_DLL) && (ACSERRGEN_HAS_DLL == 1)
#  if defined (ACSERRGEN_BUILD_DLL)
#    define acserrGen_EXPORT ACS_DLL_EXPORT
#    define ACSERRGEN_SINGLETON_DECLARATION(T) ACE_EXPORT_SINGLETON_DECLARATION (T)
#    define ACSERRGEN_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK) ACE_EXPORT_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#  else /* ACSERRGEN_BUILD_DLL */
#    define acserrGen_EXPORT ACS_DLL_IMPORT
#    define ACSERRGEN_SINGLETON_DECLARATION(T) ACE_IMPORT_SINGLETON_DECLARATION (T)
#    define ACSERRGEN_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK) ACE_IMPORT_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#  endif /* ACSERRGEN_BUILD_DLL */
#else /* ACSERRGEN_HAS_DLL == 1 */
#  define acserrGen_EXPORT
#  define ACSERRGEN_SINGLETON_DECLARATION(T)
#  define ACSERRGEN_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#endif /* ACSERRGEN_HAS_DLL == 1 */

#endif /* ACSERRGEN_EXPORT_H */

// End of auto generated file.

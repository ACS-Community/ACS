
// -*- C++ -*-
// generate_export_file.pl, v1.0 Matej Sekoranja (adpated from ACE)
// Definition for Win32 Export directives.
// This file is generated automatically by generateExportFile.pl
// ------------------------------
#ifndef LOGGING_EXPORT_H
#define LOGGING_EXPORT_H

#include <acsutil.h>

#if defined (ACS_HAS_STATIC_LIBS) && !defined (LOGGING_HAS_DLL)
#  define LOGGING_HAS_DLL 0
#endif /* ACS_HAS_STATIC_LIBS && LOGGING_HAS_DLL */

#if !defined (LOGGING_HAS_DLL)
#  define LOGGING_HAS_DLL 1
#endif /* ! LOGGING_HAS_DLL */

#if defined (LOGGING_HAS_DLL) && (LOGGING_HAS_DLL == 1)
#  if defined (LOGGING_BUILD_DLL)
#    define logging_EXPORT ACS_DLL_EXPORT
#    define LOGGING_SINGLETON_DECLARATION(T) ACE_EXPORT_SINGLETON_DECLARATION (T)
#    define LOGGING_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK) ACE_EXPORT_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#  else /* LOGGING_BUILD_DLL */
#    define logging_EXPORT ACS_DLL_IMPORT
#    define LOGGING_SINGLETON_DECLARATION(T) ACE_IMPORT_SINGLETON_DECLARATION (T)
#    define LOGGING_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK) ACE_IMPORT_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#  endif /* LOGGING_BUILD_DLL */
#else /* LOGGING_HAS_DLL == 1 */
#  define logging_EXPORT
#  define LOGGING_SINGLETON_DECLARATION(T)
#  define LOGGING_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#endif /* LOGGING_HAS_DLL == 1 */

#endif /* LOGGING_EXPORT_H */

// End of auto generated file.

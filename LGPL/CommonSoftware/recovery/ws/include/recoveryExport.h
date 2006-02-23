
// -*- C++ -*-
// generate_export_file.pl, v1.0 Matej Sekoranja (adpated from ACE)
// Definition for Win32 Export directives.
// This file is generated automatically by generateExportFile.pl
// ------------------------------
#ifndef RECOVERY_EXPORT_H
#define RECOVERY_EXPORT_H

#include <acsutil.h>

#if defined (ACS_HAS_STATIC_LIBS) && !defined (RECOVERY_HAS_DLL)
#  define RECOVERY_HAS_DLL 0
#endif /* ACS_HAS_STATIC_LIBS && RECOVERY_HAS_DLL */

#if !defined (RECOVERY_HAS_DLL)
#  define RECOVERY_HAS_DLL 1
#endif /* ! RECOVERY_HAS_DLL */

#if defined (RECOVERY_HAS_DLL) && (RECOVERY_HAS_DLL == 1)
#  if defined (RECOVERY_BUILD_DLL)
#    define recovery_EXPORT ACS_DLL_EXPORT
#    define RECOVERY_SINGLETON_DECLARATION(T) ACE_EXPORT_SINGLETON_DECLARATION (T)
#    define RECOVERY_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK) ACE_EXPORT_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#  else /* RECOVERY_BUILD_DLL */
#    define recovery_EXPORT ACS_DLL_IMPORT
#    define RECOVERY_SINGLETON_DECLARATION(T) ACE_IMPORT_SINGLETON_DECLARATION (T)
#    define RECOVERY_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK) ACE_IMPORT_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#  endif /* RECOVERY_BUILD_DLL */
#else /* RECOVERY_HAS_DLL == 1 */
#  define recovery_EXPORT
#  define RECOVERY_SINGLETON_DECLARATION(T)
#  define RECOVERY_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#endif /* RECOVERY_HAS_DLL == 1 */

#endif /* RECOVERY_EXPORT_H */

// End of auto generated file.

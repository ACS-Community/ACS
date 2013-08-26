
// -*- C++ -*-
// generate_export_file.pl, v1.0 Matej Sekoranja (adpated from ACE)
// Definition for Win32 Export directives.
// This file is generated automatically by generateExportFile.pl
// ------------------------------
#ifndef ACS_THREAD_EXPORT_H
#define ACS_THREAD_EXPORT_H

#include <acsutil.h>

#if defined (ACS_HAS_STATIC_LIBS) && !defined (ACS_THREAD_HAS_DLL)
#  define ACS_THREAD_HAS_DLL 0
#endif /* ACS_HAS_STATIC_LIBS && ACS_THREAD_HAS_DLL */

#if !defined (ACS_THREAD_HAS_DLL)
#  define ACS_THREAD_HAS_DLL 1
#endif /* ! ACS_THREAD_HAS_DLL */

#if defined (ACS_THREAD_HAS_DLL) && (ACS_THREAD_HAS_DLL == 1)
#  if defined (ACS_THREAD_BUILD_DLL)
#    define acsThread_EXPORT ACS_DLL_EXPORT
#    define ACS_THREAD_SINGLETON_DECLARATION(T) ACE_EXPORT_SINGLETON_DECLARATION (T)
#    define ACS_THREAD_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK) ACE_EXPORT_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#  else /* ACS_THREAD_BUILD_DLL */
#    define acsThread_EXPORT ACS_DLL_IMPORT
#    define ACS_THREAD_SINGLETON_DECLARATION(T) ACE_IMPORT_SINGLETON_DECLARATION (T)
#    define ACS_THREAD_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK) ACE_IMPORT_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#  endif /* ACS_THREAD_BUILD_DLL */
#else /* ACS_THREAD_HAS_DLL == 1 */
#  define acsThread_EXPORT
#  define ACS_THREAD_SINGLETON_DECLARATION(T)
#  define ACS_THREAD_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#endif /* ACS_THREAD_HAS_DLL == 1 */

#endif /* ACS_THREAD_EXPORT_H */

// End of auto generated file.

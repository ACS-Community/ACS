
// -*- C++ -*-
// generate_export_file.pl, v1.0 Matej Sekoranja (adpated from ACE)
// Definition for Win32 Export directives.
// This file is generated automatically by generateExportFile.pl
// ------------------------------
#ifndef MACICLIENT_EXPORT_H
#define MACICLIENT_EXPORT_H

#include <acsutil.h>

#if defined (ACS_HAS_STATIC_LIBS) && !defined (MACICLIENT_HAS_DLL)
#  define MACICLIENT_HAS_DLL 0
#endif /* ACS_HAS_STATIC_LIBS && MACICLIENT_HAS_DLL */

#if !defined (MACICLIENT_HAS_DLL)
#  define MACICLIENT_HAS_DLL 1
#endif /* ! MACICLIENT_HAS_DLL */

#if defined (MACICLIENT_HAS_DLL) && (MACICLIENT_HAS_DLL == 1)
#  if defined (MACICLIENT_BUILD_DLL)
#    define maciClient_EXPORT ACS_DLL_EXPORT
#    define MACICLIENT_SINGLETON_DECLARATION(T) ACE_EXPORT_SINGLETON_DECLARATION (T)
#    define MACICLIENT_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK) ACE_EXPORT_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#  else /* MACICLIENT_BUILD_DLL */
#    define maciClient_EXPORT ACS_DLL_IMPORT
#    define MACICLIENT_SINGLETON_DECLARATION(T) ACE_IMPORT_SINGLETON_DECLARATION (T)
#    define MACICLIENT_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK) ACE_IMPORT_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#  endif /* MACICLIENT_BUILD_DLL */
#else /* MACICLIENT_HAS_DLL == 1 */
#  define maciClient_EXPORT
#  define MACICLIENT_SINGLETON_DECLARATION(T)
#  define MACICLIENT_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#endif /* MACICLIENT_HAS_DLL == 1 */

#endif /* MACICLIENT_EXPORT_H */

// End of auto generated file.

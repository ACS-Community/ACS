
// -*- C++ -*-
// generate_export_file.pl, v1.0 Matej Sekoranja (adpated from ACE)
// Definition for Win32 Export directives.
// This file is generated automatically by generateExportFile.pl
// ------------------------------
#ifndef ACSQOS_EXPORT_H
#define ACSQOS_EXPORT_H

#include <acsutil.h>

#if defined (ACS_HAS_STATIC_LIBS) && !defined (ACSQOS_HAS_DLL)
#  define ACSQOS_HAS_DLL 0
#endif /* ACS_HAS_STATIC_LIBS && ACSQOS_HAS_DLL */

#if !defined (ACSQOS_HAS_DLL)
#  define ACSQOS_HAS_DLL 1
#endif /* ! ACSQOS_HAS_DLL */

#if defined (ACSQOS_HAS_DLL) && (ACSQOS_HAS_DLL == 1)
#  if defined (ACSQOS_BUILD_DLL)
#    define acsQoS_EXPORT ACS_DLL_EXPORT
#    define ACSQOS_SINGLETON_DECLARATION(T) ACE_EXPORT_SINGLETON_DECLARATION (T)
#    define ACSQOS_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK) ACE_EXPORT_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#  else /* ACSQOS_BUILD_DLL */
#    define acsQoS_EXPORT ACS_DLL_IMPORT
#    define ACSQOS_SINGLETON_DECLARATION(T) ACE_IMPORT_SINGLETON_DECLARATION (T)
#    define ACSQOS_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK) ACE_IMPORT_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#  endif /* ACSQOS_BUILD_DLL */
#else /* ACSQOS_HAS_DLL == 1 */
#  define acsQoS_EXPORT
#  define ACSQOS_SINGLETON_DECLARATION(T)
#  define ACSQOS_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#endif /* ACSQOS_HAS_DLL == 1 */

#endif /* ACSQOS_EXPORT_H */

// End of auto generated file.

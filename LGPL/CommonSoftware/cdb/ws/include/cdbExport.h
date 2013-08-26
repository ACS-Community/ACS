
// -*- C++ -*-
// generate_export_file.pl, v1.0 Matej Sekoranja (adpated from ACE)
// Definition for Win32 Export directives.
// This file is generated automatically by generateExportFile.pl
// ------------------------------
#ifndef CDB_EXPORT_H
#define CDB_EXPORT_H

#include "acsutil.h"

#if defined (ACS_HAS_STATIC_LIBS) && !defined (CDB_HAS_DLL)
#  define CDB_HAS_DLL 0
#endif /* ACS_HAS_STATIC_LIBS && CDB_HAS_DLL */

#if !defined (CDB_HAS_DLL)
#  define CDB_HAS_DLL 1
#endif /* ! CDB_HAS_DLL */

#if defined (CDB_HAS_DLL) && (CDB_HAS_DLL == 1)
#  if defined (CDB_BUILD_DLL)
#    define cdb_EXPORT ACS_DLL_EXPORT
#    define CDB_SINGLETON_DECLARATION(T) ACE_EXPORT_SINGLETON_DECLARATION (T)
#    define CDB_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK) ACE_EXPORT_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#  else /* CDB_BUILD_DLL */
#    define cdb_EXPORT ACS_DLL_IMPORT
#    define CDB_SINGLETON_DECLARATION(T) ACE_IMPORT_SINGLETON_DECLARATION (T)
#    define CDB_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK) ACE_IMPORT_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#  endif /* CDB_BUILD_DLL */
#else /* CDB_HAS_DLL == 1 */
#  define cdb_EXPORT
#  define CDB_SINGLETON_DECLARATION(T)
#  define CDB_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#endif /* CDB_HAS_DLL == 1 */

#endif /* CDB_EXPORT_H */

// End of auto generated file.

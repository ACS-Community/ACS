
// -*- C++ -*-
// $Id: archiveeventsExport.h,v 1.2 2012/01/21 22:48:11 tstaig Exp $
// Definition for Win32 Export directives.
// This file is generated automatically by generate_export_file.pl ARCHIVEEVENTS
// ------------------------------
#ifndef ARCHIVEEVENTS_EXPORT_H
#define ARCHIVEEVENTS_EXPORT_H

#include <acsutil.h>

#if defined (ACE_AS_STATIC_LIBS) && !defined (ARCHIVEEVENTS_HAS_DLL)
#  define ARCHIVEEVENTS_HAS_DLL 0
#endif /* ACE_AS_STATIC_LIBS && ARCHIVEEVENTS_HAS_DLL */

#if !defined (ARCHIVEEVENTS_HAS_DLL)
#  define ARCHIVEEVENTS_HAS_DLL 1
#endif /* ! ARCHIVEEVENTS_HAS_DLL */

#if defined (ARCHIVEEVENTS_HAS_DLL) && (ARCHIVEEVENTS_HAS_DLL == 1)
#  if defined (ARCHIVEEVENTS_BUILD_DLL)
#    define ARCHIVEEVENTS_Export ACE_Proper_Export_Flag
#    define ARCHIVEEVENTS_SINGLETON_DECLARATION(T) ACE_EXPORT_SINGLETON_DECLARATION (T)
#    define ARCHIVEEVENTS_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK) ACE_EXPORT_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#  else /* ARCHIVEEVENTS_BUILD_DLL */
#    define ARCHIVEEVENTS_Export ACE_Proper_Import_Flag
#    define ARCHIVEEVENTS_SINGLETON_DECLARATION(T) ACE_IMPORT_SINGLETON_DECLARATION (T)
#    define ARCHIVEEVENTS_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK) ACE_IMPORT_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#  endif /* ARCHIVEEVENTS_BUILD_DLL */
#else /* ARCHIVEEVENTS_HAS_DLL == 1 */
#  define ARCHIVEEVENTS_Export
#  define ARCHIVEEVENTS_SINGLETON_DECLARATION(T)
#  define ARCHIVEEVENTS_SINGLETON_DECLARE(SINGLETON_TYPE, CLASS, LOCK)
#endif /* ARCHIVEEVENTS_HAS_DLL == 1 */

// Set ARCHIVEEVENTS_NTRACE = 0 to turn on library specific tracing even if
// tracing is turned off for ACE.
#if !defined (ARCHIVEEVENTS_NTRACE)
#  if (ACE_NTRACE == 1)
#    define ARCHIVEEVENTS_NTRACE 1
#  else /* (ACE_NTRACE == 1) */
#    define ARCHIVEEVENTS_NTRACE 0
#  endif /* (ACE_NTRACE == 1) */
#endif /* !ARCHIVEEVENTS_NTRACE */

#if (ARCHIVEEVENTS_NTRACE == 1)
#  define ARCHIVEEVENTS_TRACE(X)
#else /* (ARCHIVEEVENTS_NTRACE == 1) */
#  if !defined (ACE_HAS_TRACE)
#    define ACE_HAS_TRACE
#  endif /* ACE_HAS_TRACE */
#  define ARCHIVEEVENTS_TRACE(X) ACE_TRACE_IMPL(X)
#  include "ace/Trace.h"
#endif /* (ARCHIVEEVENTS_NTRACE == 1) */

#endif /* ARCHIVEEVENTS_EXPORT_H */

// End of auto generated file.

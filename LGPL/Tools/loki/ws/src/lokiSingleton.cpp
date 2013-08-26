////////////////////////////////////////////////////////////////////////////////
// The Loki Library
// Copyright (c) 2001 by Andrei Alexandrescu
// This code accompanies the book:
// Alexandrescu, Andrei. "Modern C++ Design: Generic Programming and Design 
//     Patterns Applied". Copyright (c) 2001. Addison-Wesley.
// Permission to use, copy, modify, distribute and sell this software for any 
//     purpose is hereby granted without fee, provided that the above copyright 
//     notice appear in all copies and that both that copyright notice and this 
//     permission notice appear in supporting documentation.
// The author or Addison-Wesley Longman make no representations about the 
//     suitability of this software for any purpose. It is provided "as is" 
//     without express or implied warranty.
////////////////////////////////////////////////////////////////////////////////

// $Header: /diskb/tmp/stefano/project2/CVS/ACS/LGPL/Tools/loki/ws/src/lokiSingleton.cpp,v 1.3 2007/02/02 21:28:57 bjeram Exp $

#include <lokiSingleton.h>


#ifdef LOKI_ENABLE_NEW_SETLONGLIVITY_HELPER_DATA_IMPL
Loki::Private::TrackerArray* Loki::Private::pTrackerArray = 0;
#else
Loki::Private::TrackerArray Loki::Private::pTrackerArray = 0;
unsigned int Loki::Private::elements = 0;
#endif

////////////////////////////////////////////////////////////////////////////////
// function AtExitFn
// Ensures proper destruction of objects with longevity
////////////////////////////////////////////////////////////////////////////////

#ifdef LOKI_ENABLE_NEW_SETLONGLIVITY_HELPER_DATA_IMPL

void LOKI_C_CALLING_CONVENTION_QUALIFIER Loki::Private::AtExitFn()
{
    assert(pTrackerArray!=0 && !pTrackerArray->empty());
    
    // Pick the element at the top of the stack
    LifetimeTracker* pTop = pTrackerArray->back();
    
    // Remove that object off the stack _before_ deleting pTop
    pTrackerArray->pop_back();
    
    // Destroy the element
    //BJE: commented out because it causes trouble 
    //  possible cause ACE object manager
    //   delete pTop;
    
    // Destroy stack when it's empty _after_ deleting pTop
    if(pTrackerArray->empty())
    {
        delete pTrackerArray;
        pTrackerArray = 0;
    }
}

#else

void LOKI_C_CALLING_CONVENTION_QUALIFIER Loki::Private::AtExitFn()
{
    assert(elements > 0 && pTrackerArray != 0);
    // Pick the element at the top of the stack
    LifetimeTracker* pTop = pTrackerArray[elements - 1];
    // Remove that object off the stack
    // Don't check errors - realloc with less memory 
    //     can't fail
    pTrackerArray = static_cast<TrackerArray>(std::realloc(
        pTrackerArray, sizeof(*pTrackerArray) * --elements));
    // Destroy the element
    delete pTop;
}

#endif

////////////////////////////////////////////////////////////////////////////////
// Change log:
// June 20, 2001: ported by Nick Thurn to gcc 2.95.3. Kudos, Nick!!!
// January 10, 2002: Fixed bug in call to realloc - credit due to Nigel Gent and
//      Eike Petersen
// May 08, 2002: Refixed bug in call to realloc
////////////////////////////////////////////////////////////////////////////////

// $Log: lokiSingleton.cpp,v $
// Revision 1.3  2007/02/02 21:28:57  bjeram
// fixed (temporary) clash between ACE object manager and singelton longevity
//
// Revision 1.2  2007/02/01 17:29:01  sharring
//
// updating to newer version of loki library, with support for multi-threading enabled. manually renamed files to avoid name conflicts, by
// prepending "loki" to the names of header files. also manually edited lokiThreads.h to #define LOKI_OBJECT_LEVEL_THREADING; this could
// also be done with a compile FLAG, perhaps would be better.
//
// Revision 1.1.28.1  2007/02/01 07:36:57  sharring
//
// updating loki to newer version for testing in SFI in the hopes of fixing some
// multi-threading problems seen in acs logging code for which the stack trace
// indicates that loki smart pointers were involved.
//
// Revision 1.7  2006/01/16 20:59:53  rich_sposato
// Added cvs keywords.
//

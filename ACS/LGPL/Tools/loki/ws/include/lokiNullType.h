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

// Last update: November 22, 2001

#ifndef LOKI_NULLTYPE_INC_
#define LOKI_NULLTYPE_INC_

// $Header: /diskb/tmp/stefano/project2/CVS/ACS/LGPL/Tools/loki/ws/include/lokiNullType.h,v 1.2 2007/02/01 17:29:00 sharring Exp $

namespace Loki
{
////////////////////////////////////////////////////////////////////////////////
// class NullType
// Used as a placeholder for "no type here"
// Useful as an end marker in typelists 
////////////////////////////////////////////////////////////////////////////////

    class NullType {};
    
}   // namespace Loki

////////////////////////////////////////////////////////////////////////////////
// Change log:
// June 20, 2001: ported by Nick Thurn to gcc 2.95.3. Kudos, Nick!!!
// November 22, 2001: minor change to support porting to boost
////////////////////////////////////////////////////////////////////////////////

#endif // NULLTYPE_INC_

// $Log: lokiNullType.h,v $
// Revision 1.2  2007/02/01 17:29:00  sharring
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
// Revision 1.3  2006/01/16 19:05:09  rich_sposato
// Added cvs keywords.
//

////////////////////////////////////////////////////////////////////////////////
// The Loki Library
// Copyright (c) 2006 Richard Sposato
// Copyright (c) 2006 Peter Kümmel
// Permission to use, copy, modify, distribute and sell this software for any 
//     purpose is hereby granted without fee, provided that the above copyright 
//     notice appear in all copies and that both that copyright notice and this 
//     permission notice appear in supporting documentation.
// The authors make no representations about the 
//     suitability of this software for any purpose. It is provided "as is" 
//     without express or implied warranty.
////////////////////////////////////////////////////////////////////////////////

#ifndef LOKI_CONST_POLICY_INC_
#define LOKI_CONST_POLICY_INC_

// $Header: /diskb/tmp/stefano/project2/CVS/ACS/LGPL/Tools/loki/ws/include/lokiConstPolicy.h,v 1.2 2007/02/01 17:29:00 sharring Exp $


namespace Loki
{

////////////////////////////////////////////////////////////////////////////////
/// @note These policy classes are used in LockingPtr and SmartPtr to define
///  how const is propagated from the pointee.
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
///  \class DontPropagateConst
///
///  \ingroup ConstGroup
///  Don't propagate constness of pointed or referred object.
////////////////////////////////////////////////////////////////////////////////

    template< class T >
    struct DontPropagateConst
    {
        typedef T Type;
    };

////////////////////////////////////////////////////////////////////////////////
///  \class PropagateConst
///
///  \ingroup ConstGroup
///  Propagate constness of pointed or referred object.
////////////////////////////////////////////////////////////////////////////////

    template< class T >
    struct PropagateConst
    {
        typedef const T Type;
    };

// default will not break existing code
#ifndef LOKI_DEFAULT_CONSTNESS
#define LOKI_DEFAULT_CONSTNESS DontPropagateConst
#endif

} // end namespace Loki

#endif // end file guardian

// $Log: lokiConstPolicy.h,v $
// Revision 1.2  2007/02/01 17:29:00  sharring
// updating to newer version of loki library, with support for multi-threading enabled. manually renamed files to avoid name conflicts, by
// prepending "loki" to the names of header files. also manually edited lokiThreads.h to #define LOKI_OBJECT_LEVEL_THREADING; this could
// also be done with a compile FLAG, perhaps would be better.
//
// Revision 1.1.2.1  2007/02/01 07:36:57  sharring
//
// updating loki to newer version for testing in SFI in the hopes of fixing some
// multi-threading problems seen in acs logging code for which the stack trace
// indicates that loki smart pointers were involved.
//
// Revision 1.1  2006/02/19 22:04:28  rich_sposato
// Moved Const-policy structs from SmartPtr.h to ConstPolicy.h.
//

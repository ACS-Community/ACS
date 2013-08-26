////////////////////////////////////////////////////////////////////////////////
// The Loki Library
// Data Generator by Shannon Barber
// This code DOES NOT accompany the book:
// Alexandrescu, Andrei. "Modern C++ Design: Generic Programming and Design 
//     Patterns Applied". Copyright (c) 2001. Addison-Wesley.
//
// Code covered by the MIT License
// The author makes no representations about the suitability of this software
//  for any purpose. It is provided "as is" without express or implied warranty.
////////////////////////////////////////////////////////////////////////////////

// Last update: Oct 10, 2002

#ifndef LOKI_DATAGENERATORS_H
#define LOKI_DATAGENERATORS_H

// $Header: /diskb/tmp/stefano/project2/CVS/ACS/LGPL/Tools/loki/ws/include/lokiDataGenerators.h,v 1.2 2007/02/01 17:29:00 sharring Exp $

#include "lokiTypelist.h"

//Reference version

/************************************************************************************
// class template GenData
// Iteratates a Typelist, and invokes the functor GenFunc<T>
// for each type in the list, passing a functor along the way.
// The functor is designed to be an insertion iterator which GenFunc<T>
// can use to output information about the types in the list.
//

Example Use

template<typename T>
struct ExtractDataType
    {
    some_type operator()()
        {
        return create_value_from_type<T>;
        }
    };

Loki::IterateTypes<parameter_tl, ExtractDataType> gendata;
std::vector<some_type> stuff;
gendata(std::back_inserter(stuff));
*******************************************************************************/
namespace Loki
{
    namespace TL
        {
        template<typename T>
        struct nameof_type
            {
            const char* operator()()
                {
                return typeid(T).name();
                }
            };
        template<typename T>
        struct sizeof_type
            {
            size_t operator()()
                {
                return sizeof(T);
                }
            };
    template <class TList, template <class> class GenFunc>
    struct IterateTypes;
     
    template <class T1, class T2, template <class> class GenFunc>
    struct IterateTypes<Typelist<T1, T2>, GenFunc>
    {
    typedef IterateTypes<T1, GenFunc> head_t;
    head_t head;
    typedef IterateTypes<T2, GenFunc> tail_t;
    tail_t tail;
    template<class II>
    void operator()(II ii)
        {
        head.operator()(ii);
        tail.operator()(ii);
        }
    };
     
    template <class AtomicType, template <class> class GenFunc>
    struct IterateTypes
    {
    template<class II>
    void operator()(II ii)
        {
        GenFunc<AtomicType> genfunc;
        *ii = genfunc();
        ++ii; //Is this even needed?
        }
    };
    
    template <template <class> class GenFunc>
    struct IterateTypes<NullType, GenFunc>
    {
    template<class II>
    void operator()(II ii)
        {}
    };
    
    template<typename Types, template <class> class UnitFunc, typename II>
    void iterate_types(II ii)
        {
        Loki::TL::IterateTypes<Types, UnitFunc> it;
        it(ii);
        }
    }//ns TL
}//ns Loki

#endif //DATAGENERATORS_H
////////////////////////////////////////////////////////////////////////////////
// Change log:
// 9/20/02 Named changed from GenData to IterateTypes
// 10/8/02 insertion iterators are passed-by-value, not by-reference (oops)
////////////////////////////////////////////////////////////////////////////////

// $Log: lokiDataGenerators.h,v $
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
// Revision 1.4  2006/01/16 19:05:09  rich_sposato
// Added cvs keywords.
//


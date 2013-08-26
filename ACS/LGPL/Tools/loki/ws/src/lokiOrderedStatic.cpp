////////////////////////////////////////////////////////////////////////////////
// The Loki Library
// Copyright (c) 2005 Peter Kümmel
// Permission to use, copy, modify, distribute and sell this software for any 
//     purpose is hereby granted without fee, provided that the above copyright 
//     notice appear in all copies and that both that copyright notice and this 
//     permission notice appear in supporting documentation.
// The author makes no representations about the 
//     suitability of this software for any purpose. It is provided "as is" 
//     without express or implied warranty.
////////////////////////////////////////////////////////////////////////////////

// $Header: /diskb/tmp/stefano/project2/CVS/ACS/LGPL/Tools/loki/ws/src/lokiOrderedStatic.cpp,v 1.3 2007/02/22 09:00:32 bjeram Exp $

#include <lokiOrderedStatic.h>
#ifndef MAKE_VXWORKS
#include <limits>
#endif

#ifdef min 
#undef min 
#endif
 
#ifdef max 
#undef max 
#endif 

namespace Loki
{
    namespace Private
    {

        OrderedStaticCreatorFunc::OrderedStaticCreatorFunc()
        {
        }
        
        OrderedStaticCreatorFunc::~OrderedStaticCreatorFunc()
        {
        }
    

        OrderedStaticManagerClass::OrderedStaticManagerClass() :
                    staticObjects_(),
// there is no std::numeric_limits<> or VxWorks
#ifndef MAKE_VXWORKS
                    max_longevity_(std::numeric_limits<unsigned int>::min()),
                    min_longevity_(std::numeric_limits<unsigned int>::max())
#else
	            max_longevity_(0),
		    min_longevity_(__INT_MAX__* 2U + 1)
#endif
        {
        }

        OrderedStaticManagerClass::~OrderedStaticManagerClass()
        {
        }

        void OrderedStaticManagerClass::createObjects()
        {
            for(unsigned int longevity=max_longevity_; longevity>=min_longevity_; longevity--)
            {
                for(unsigned int i=0; i<staticObjects_.size(); i++)
                {
// there is no at method for Vector for VxWorks
#ifndef MAKE_VXWORKS
                    Data cur = staticObjects_.at(i);
#else
		   Data cur = staticObjects_[i];  
#endif
                    if(cur.longevity==longevity)
                        ( (*cur.object).*cur.creator )();
                }
            }
        }

        void OrderedStaticManagerClass::registerObject(unsigned int l, OrderedStaticCreatorFunc* o,Creator f)
        {
            staticObjects_.push_back(Data(l,o,f));

            if(l>max_longevity_) max_longevity_=l;
            if(l<min_longevity_) min_longevity_=l;
        }

        OrderedStaticManagerClass::Data::Data(unsigned int l, OrderedStaticCreatorFunc* o, Creator f)
            : longevity(l), object(o), creator(f)
        {
        }

    }//namespace Private

}//namespace Loki

// $Log: lokiOrderedStatic.cpp,v $
// Revision 1.3  2007/02/22 09:00:32  bjeram
// ported to VxWorks
//
// Revision 1.2  2007/02/01 17:29:01  sharring
//
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
// Revision 1.7  2006/03/27 16:09:57  syntheticpp
// undef all min/max macros, thx to Shen Lei
//
// Revision 1.6  2006/01/18 17:21:31  lfittl
// - Compile library with -Weffc++ and -pedantic (gcc)
// - Fix most issues raised by using -Weffc++ (initialization lists)
//
// Revision 1.5  2006/01/16 20:59:53  rich_sposato
// Added cvs keywords.
//

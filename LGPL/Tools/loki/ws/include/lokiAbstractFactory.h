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

// Last update: June 20, 2001

#ifndef LOKI_ABSTRACTFACTORY_INC_
#define LOKI_ABSTRACTFACTORY_INC_

// $Header: /diskb/tmp/stefano/project2/CVS/ACS/LGPL/Tools/loki/ws/include/lokiAbstractFactory.h,v 1.2 2007/02/01 17:29:00 sharring Exp $

#include "lokiTypelist.h"
#include "lokiSequence.h"
#include "lokiTypeManip.h"
#include "lokiHierarchyGenerators.h"

#include <cassert>

namespace Loki
{

////////////////////////////////////////////////////////////////////////////////
// class template AbstractFactoryUnit
// The building block of an Abstract Factory
////////////////////////////////////////////////////////////////////////////////

    template <class T>
    class AbstractFactoryUnit
    {
    public:
        virtual T* DoCreate(Type2Type<T>) = 0;
        virtual ~AbstractFactoryUnit() {}
    };

////////////////////////////////////////////////////////////////////////////////
// class template AbstractFactory
// Defines an Abstract Factory interface starting from a typelist
////////////////////////////////////////////////////////////////////////////////

    template
    <
        class TList,
        template <class> class Unit = AbstractFactoryUnit
    >
    class AbstractFactory : public GenScatterHierarchy<TList, Unit>
    {
    public:
        typedef TList ProductList;
        
        template <class T> T* Create()
        {
            Unit<T>& unit = *this;
            return unit.DoCreate(Type2Type<T>());
        }
    };
    
////////////////////////////////////////////////////////////////////////////////
// class template OpNewFactoryUnit
// Creates an object by invoking the new operator
////////////////////////////////////////////////////////////////////////////////

    template <class ConcreteProduct, class Base>
    class OpNewFactoryUnit : public Base
    {
        typedef typename Base::ProductList BaseProductList;
    
    protected:
        typedef typename BaseProductList::Tail ProductList;
    
    public:
        typedef typename BaseProductList::Head AbstractProduct;
        ConcreteProduct* DoCreate(Type2Type<AbstractProduct>)
        {
            return new ConcreteProduct;
        }
    };

////////////////////////////////////////////////////////////////////////////////
// class template PrototypeFactoryUnit
// Creates an object by cloning a prototype
// There is a difference between the implementation herein and the one described
//     in the book: GetPrototype and SetPrototype use the helper friend 
//     functions DoGetPrototype and DoSetPrototype. The friend functions avoid
//     name hiding issues. Plus, GetPrototype takes a reference to pointer
//     instead of returning the pointer by value.
////////////////////////////////////////////////////////////////////////////////

    template <class ConcreteProduct, class Base>
    class PrototypeFactoryUnit : public Base
    {
        typedef typename Base::ProductList BaseProductList;
    
    protected:
        typedef typename BaseProductList::Tail ProductList;

    public:
        typedef typename BaseProductList::Head AbstractProduct;

        PrototypeFactoryUnit(AbstractProduct* p = 0)
            : pPrototype_(p)
        {}
        
        friend void DoGetPrototype(const PrototypeFactoryUnit& me,
            AbstractProduct*& pPrototype)
        { pPrototype = me.pPrototype_; }
        
        friend void DoSetPrototype(PrototypeFactoryUnit& me, 
            AbstractProduct* pObj)
        { me.pPrototype_ = pObj; }
        
        template <class U>
        void GetPrototype(U*& p)
        { return DoGetPrototype(*this, p); }
        
        template <class U>
        void SetPrototype(U* pObj)
        { DoSetPrototype(*this, pObj); }
        
        AbstractProduct* DoCreate(Type2Type<AbstractProduct>)
        {
            assert(pPrototype_);
            return pPrototype_->Clone();
        }
        
    private:
        AbstractProduct* pPrototype_;
    };

////////////////////////////////////////////////////////////////////////////////
// class template ConcreteFactory
// Implements an AbstractFactory interface
////////////////////////////////////////////////////////////////////////////////

    template
    <
        class AbstractFact,
        template <class, class> class Creator = OpNewFactoryUnit,
        class TList = typename AbstractFact::ProductList
    >
    class ConcreteFactory
        : public GenLinearHierarchy<
            typename TL::Reverse<TList>::Result, Creator, AbstractFact>
    {
    public:
        typedef typename AbstractFact::ProductList ProductList;
        typedef TList ConcreteProductList;
    };

} // namespace Loki

////////////////////////////////////////////////////////////////////////////////
// Change log:
// June 20, 2001: ported by Nick Thurn to gcc 2.95.3. Kudos, Nick!!!
// September 25, 2004: Fixed bug in PrototypeFactoryUnit::GetPrototype, thanks
//   to a bug report submitted by funcall.
////////////////////////////////////////////////////////////////////////////////

#endif // ABSTRACTFACTORY_INC_

// $Log: lokiAbstractFactory.h,v $
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





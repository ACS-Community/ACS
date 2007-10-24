#ifndef ACSSMARTPOINTER_H
#define ACSSMARTPOINTER_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) National Research Council of Canada, 2007 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: acsComponentSmartPtr.h,v 1.1 2007/10/24 22:35:44 agrimstrup Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* arne  2007-10-09  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

/* The following piece of code alternates the linkage type to C for all
functions declared within the braces, which is necessary to use the
functions in C++-code.
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <logging.h>
#include <lokiThreads.h>
#include <lokiSmartPtr.h>

namespace maci {


/**
 *  Storage Policy class for Component Pointers.
 *  In addition to storing the pointer to the component being managed by
 *  the smart pointer, this class caches information needed when the 
 *  component is finally released.
 */
template <class T, class H>
class ComponentStorage
{
  public:
    typedef T* StoredType;    // the type of the pointee_ object
    typedef T* PointerType;   // type returned by operator->
    typedef T& ReferenceType; // type returned by operator*
    
    /**
     * Default Constructor
     */
    ComponentStorage() : handle(0), component_name(0), sticky(true), pointee_(Default())
	{}

    /**
     * Constructor that stores default management values with a live pointer.
     */
    ComponentStorage(const StoredType& p) : handle(0), component_name(0), sticky(true), pointee_(p)
        {}
    
    // The storage policy doesn't initialize the stored pointer 
    // which will be initialized by the OwnershipPolicy's Clone fn
    /**
     * Copy Constructor
     */
    ComponentStorage(const ComponentStorage& rhs) : handle(rhs.handle), component_name(rhs.component_name),
					      sticky(rhs.sticky), pointee_(0)
        {}
    
    /**
     * Copy Constructor for ClientStores of other types.
     * We don't allow copying of different types, so the attributes are set to default values.
     */
    template <typename U, typename V>
    ComponentStorage(const ComponentStorage<U,V>&) : handle(0), component_name(0), sticky(true), pointee_(0)
        {}
    
    /**
     * SetValues
     * Set the attribute values for the Component being managed.  This is a support
     * method for the ComponentSmartPtr constructor.
     * @param name is the name of the component that will be managed.
     * @param client is a pointer to the SimpleClient that provided the component.
     * @param s is flag that indicates if the component is sticky.
     * @param p is a pointer to the component that will be managed.
     */
    void setValues(const char *name, H *h, bool s, const StoredType& p)
	{
	    component_name = name;
            handle = h;
	    sticky = s;
	    pointee_ = p;
	};

    /**
     * Member Access Operator
     */
    PointerType operator->() const { return pointee_; }
    
    /**
     * Dereference Operator
     */
    ReferenceType operator*() const { return *pointee_; }
    
    /**
     * Swap
     * Exchange values with another instance of ComponentStorage.
     * @param rhs is the instance to exchange attributes with.
     */
    void Swap(ComponentStorage& rhs)
        {
	    std::swap(pointee_, rhs.pointee_);
	    std::swap(sticky, rhs.sticky);
	    std::swap(component_name, rhs.component_name);
	    std::swap(handle, rhs.handle);
	}
    
    // Accessors
    /**
     * GetImpl.
     * Retrieve the Component pointer from its storage object.
     */
    friend inline PointerType GetImpl(const ComponentStorage& sp)
        { return sp.pointee_; }
    
    /**
     * GetImplRef.
     * Retrieve the Component reference from its storage object.
     */
    friend inline const StoredType& GetImplRef(const ComponentStorage& sp)
        { return sp.pointee_; }
    
    /**
     * GetImplRef.
     * Retrieve the Component reference from its storage object.
     */
    friend inline StoredType& GetImplRef(ComponentStorage& sp)
        { return sp.pointee_; }
    
  protected:

    /**
     * Destroy.
     * Release the component reference managed by this object.
     */
    void Destroy()
        {
	    if (handle && sticky) {
// 		ACS_SHORT_LOG((LM_DEBUG, "Releasing component: '%s'.",  component_name));
		handle->releaseComponent( component_name);
	    }
	}    
    // Default value to initialize the pointer
    static StoredType Default()
        { return 0; }
    
  private:
    // Data
    H *handle;
    const char *component_name;
    bool sticky;
    StoredType pointee_;
};


class ContainerServices;


/******************************************************************************
 * The code below was taken from the lokiSmartPtr.h file, which is covered
 * by the license listed below.  It has been modified to allow two parameter
 * StoragePolicy classes to be used.
 ******************************************************************************/
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

template
<
    typename T,
    typename H = ContainerServices,
    template <class> class OwnershipPolicy = Loki::RefCountedMTAdj<Loki::ObjectLevelLockable>::RefCountedMT,
    class ConversionPolicy = Loki::DisallowConversion,
    template <class> class CheckingPolicy = Loki::NoCheck,
    template <class,class> class StoragePolicy = ComponentStorage,
    template<class> class ConstnessPolicy = Loki::LOKI_DEFAULT_CONSTNESS 
>
class SmartPtr
    : public StoragePolicy<T,H>
    , public OwnershipPolicy<typename StoragePolicy<T,H>::PointerType>
    , public CheckingPolicy<typename StoragePolicy<T,H>::StoredType>
    , public ConversionPolicy
{
    typedef StoragePolicy<T,H> SP;
    typedef OwnershipPolicy<typename StoragePolicy<T,H>::PointerType> OP;
    typedef CheckingPolicy<typename StoragePolicy<T,H>::StoredType> KP;
    typedef ConversionPolicy CP;
    
  public:
    typedef typename ConstnessPolicy<T>::Type* ConstPointerType;
    typedef typename ConstnessPolicy<T>::Type& ConstReferenceType;
    
    typedef typename SP::PointerType PointerType;
    typedef typename SP::StoredType StoredType;
    typedef typename SP::ReferenceType ReferenceType;
    
    typedef typename Loki::Select<OP::destructiveCopy,SmartPtr, const SmartPtr>::Result CopyArg;
    
    private:
        struct NeverMatched;
       
#ifdef LOKI_SMARTPTR_CONVERSION_CONSTRUCTOR_POLICY
        typedef typename Loki::Select< CP::allow, const StoredType&, NeverMatched>::Result ImplicitArg;
        typedef typename Loki::Select<!CP::allow, const StoredType&, NeverMatched>::Result ExplicitArg;
#else
        typedef const StoredType& ImplicitArg;
        typedef typename Loki::Select<false, const StoredType&, NeverMatched>::Result ExplicitArg;
#endif

    public:

        SmartPtr()
        { KP::OnDefault(GetImpl(*this)); }
        
        /**
	 * Constructor.
	 * Create a smart pointer for the component described.
	 * @param name is the name of the component.
	 * @param m is the reference of the manager used to manage the connection.
	 * @param h is the handle of the requestor of the component
	 * @param s is the flag indicating if the reference is sticky.
	 @ @param p is the pointer to the component.
	*/
        SmartPtr(const char *name, H* h, bool s, T* p)
	{
	    setValues(name, h, s, p);
	}

        explicit
        SmartPtr(ExplicitArg p) : SP(p)
        { KP::OnInit(GetImpl(*this)); }

        SmartPtr(ImplicitArg p) : SP(p)
        { KP::OnInit(GetImpl(*this)); }

        SmartPtr(CopyArg& rhs)
        : SP(rhs), OP(rhs), KP(rhs), CP(rhs)
        { GetImplRef(*this) = OP::Clone(GetImplRef(rhs)); }

        template
        <
            typename T1,
            typename H1,
            template <class> class OP1,
            class CP1,
            template <class> class KP1,
            template <class,class> class SP1,
            template <class> class CNP1
        >
        SmartPtr(const SmartPtr<T1, H1, OP1, CP1, KP1, SP1, CNP1 >& rhs)
        : SP(rhs), OP(rhs), KP(rhs), CP(rhs)
        { GetImplRef(*this) = OP::Clone(GetImplRef(rhs)); }

        template
        <
            typename T1,
            typename H1,
            template <class> class OP1,
            class CP1,
            template <class> class KP1,
            template <class,class> class SP1,
            template <class> class CNP1
        >
        SmartPtr(SmartPtr<T1, H1, OP1, CP1, KP1, SP1, CNP1 >& rhs)
        : SP(rhs), OP(rhs), KP(rhs), CP(rhs)
        { GetImplRef(*this) = OP::Clone(GetImplRef(rhs)); }

        SmartPtr(Loki::RefToValue<SmartPtr> rhs)
        : SP(rhs), OP(rhs), KP(rhs), CP(rhs)
        {}
        
        operator Loki::RefToValue<SmartPtr>()
        { return Loki::RefToValue<SmartPtr>(*this); }

        SmartPtr& operator=(CopyArg& rhs)
        {
            SmartPtr temp(rhs);
            temp.Swap(*this);
            return *this;
        }

        template
        <
            typename T1,
            typename H1,
            template <class> class OP1,
            class CP1,
            template <class> class KP1,
            template <class,class> class SP1,
            template <class> class CNP1
        >
        SmartPtr& operator=(const SmartPtr<T1, H1, OP1, CP1, KP1, SP1, CNP1 >& rhs)
        {
            SmartPtr temp(rhs);
            temp.Swap(*this);
            return *this;
        }
        
        template
        <
            typename T1,
            typename H1,
            template <class> class OP1,
            class CP1,
            template <class> class KP1,
            template <class,class> class SP1,
            template <class> class CNP1
        >
        SmartPtr& operator=(SmartPtr<T1, H1, OP1, CP1, KP1, SP1, CNP1 >& rhs)
        {
            SmartPtr temp(rhs);
            temp.Swap(*this);
            return *this;
        }
        
        void Swap(SmartPtr& rhs)
        {
            OP::Swap(rhs);
            CP::Swap(rhs);
            KP::Swap(rhs);
            SP::Swap(rhs);
        }
        
        ~SmartPtr()
        {
            if (OP::Release(GetImpl(*static_cast<SP*>(this))))
            {
                SP::Destroy();
            }
        }
        
        friend inline void Release(SmartPtr& sp, typename SP::StoredType& p)
        {
            p = GetImplRef(sp);
            GetImplRef(sp) = SP::Default();
        }
        
        friend inline void Reset(SmartPtr& sp, typename SP::StoredType p)
        { SmartPtr(p).Swap(sp); }

        template
        <
            typename T1,
            typename H1,
            template <class> class OP1,
            class CP1,
            template <class> class KP1,
            template <class,class> class SP1,
            template <class> class CNP1
        >
        bool Merge( SmartPtr< T1, H1, OP1, CP1, KP1, SP1, CNP1 > & rhs )
        {
            if ( GetImpl( *this ) != GetImpl( rhs ) )
            {
                return false;
            }
            return OP::Merge( rhs );
        }

        PointerType operator->()
        {
            KP::OnDereference(GetImplRef(*this));
            return SP::operator->();
        }

        ConstPointerType operator->() const
        {
            KP::OnDereference(GetImplRef(*this));
            return SP::operator->();
        }

        ReferenceType operator*()
        {
            KP::OnDereference(GetImplRef(*this));
            return SP::operator*();
        }
        
        ConstReferenceType operator*() const
        {
            KP::OnDereference(GetImplRef(*this));
            return SP::operator*();
        }
        
        bool operator!() const // Enables "if (!sp) ..."
        { return GetImpl(*this) == 0; }

        static inline T * GetPointer( const SmartPtr & sp )
        { return GetImpl( sp ); }

        // Ambiguity buster
        template
        <
            typename T1,
            typename H1,
            template <class> class OP1,
            class CP1,
            template <class> class KP1,
            template <class,class> class SP1,
            template <class> class CNP1
        >
        bool operator==(const SmartPtr<T1, H1, OP1, CP1, KP1, SP1, CNP1 >& rhs) const
        { return GetImpl(*this) == GetImpl(rhs); }

        // Ambiguity buster
        template
        <
            typename T1,
            typename H1,
            template <class> class OP1,
            class CP1,
            template <class> class KP1,
            template <class,class> class SP1,
            template <class> class CNP1
        >
        bool operator!=(const SmartPtr<T1, H1, OP1, CP1, KP1, SP1, CNP1 >& rhs) const
        { return !(*this == rhs); }

        // Ambiguity buster
        template
        <
            typename T1,
            typename H1,
            template <class> class OP1,
            class CP1,
            template <class> class KP1,
            template <class,class> class SP1,
            template <class> class CNP1
        >
        bool operator<(const SmartPtr<T1, H1, OP1, CP1, KP1, SP1, CNP1 >& rhs) const
        { return GetImpl(*this) < GetImpl(rhs); }

        // Ambiguity buster
        template
        <
            typename T1,
            typename H1,
            template <class> class OP1,
            class CP1,
            template <class> class KP1,
            template <class,class> class SP1,
            template <class> class CNP1
        >
        inline bool operator > ( const SmartPtr< T1, H1, OP1, CP1, KP1, SP1, CNP1 > & rhs )
        {
            return ( GetImpl( rhs ) < GetImpl( *this ) );
        }

        // Ambiguity buster
        template
        <
            typename T1,
            typename H1,
            template <class> class OP1,
            class CP1,
            template <class> class KP1,
            template <class,class> class SP1,
            template <class> class CNP1
        >
        inline bool operator <= ( const SmartPtr< T1, H1, OP1, CP1, KP1, SP1, CNP1 > & rhs )
        {
            return !( GetImpl( rhs ) < GetImpl( *this ) );
        }

        // Ambiguity buster
        template
        <
            typename T1,
            typename H1,
            template <class> class OP1,
            class CP1,
            template <class> class KP1,
            template <class,class> class SP1,
            template <class> class CNP1
        >
        inline bool operator >= ( const SmartPtr< T1, H1, OP1, CP1, KP1, SP1, CNP1 > & rhs )
        {
            return !( GetImpl( *this ) < GetImpl( rhs ) );
        }

    private:
        // Helper for enabling 'if (sp)'
        struct Tester
        {
            Tester(int) {}
            void dummy() {}
        };
        
        typedef void (Tester::*unspecified_boolean_type_)();

        typedef typename Loki::Select<CP::allow, Tester, unspecified_boolean_type_>::Result
            unspecified_boolean_type;

    public:
        // enable 'if (sp)'
        operator unspecified_boolean_type() const
        {
            return !*this ? 0 : &Tester::dummy;
        }

    private:
        // Helper for disallowing automatic conversion
        struct Insipid
        {
            Insipid(PointerType) {}
        };
        
        typedef typename Loki::Select<CP::allow, PointerType, Insipid>::Result
            AutomaticConversionResult;
    
    public:        
        operator AutomaticConversionResult() const
        { return GetImpl(*this); }
};

////////////////////////////////////////////////////////////////////////////////
// free comparison operators for class template SmartPtr
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
///  operator== for lhs = SmartPtr, rhs = raw pointer
///  \ingroup SmartPointerGroup
////////////////////////////////////////////////////////////////////////////////

    template
    <
        typename T,
        typename H,
        template <class> class OP,
        class CP,
        template <class> class KP,
        template <class,class> class SP,
        template <class> class CNP1,
        typename U
    >
    inline bool operator==(const SmartPtr<T, H, OP, CP, KP, SP, CNP1 >& lhs,
        U* rhs)
    { return GetImpl(lhs) == rhs; }

////////////////////////////////////////////////////////////////////////////////
///  operator== for lhs = raw pointer, rhs = SmartPtr
///  \ingroup SmartPointerGroup
////////////////////////////////////////////////////////////////////////////////

    template
    <
        typename T,
        typename H,
        template <class> class OP,
        class CP,
        template <class> class KP,
        template <class,class> class SP,
        template <class> class CNP1,
        typename U
    >
    inline bool operator==(U* lhs,
        const SmartPtr<T, H, OP, CP, KP, SP, CNP1 >& rhs)
    { return rhs == lhs; }

////////////////////////////////////////////////////////////////////////////////
///  operator!= for lhs = SmartPtr, rhs = raw pointer
///  \ingroup SmartPointerGroup
////////////////////////////////////////////////////////////////////////////////

    template
    <
        typename T,
        typename H,
        template <class> class OP,
        class CP,
        template <class> class KP,
        template <class,class> class SP,
        template <class> class CNP,
        typename U
    >
    inline bool operator!=(const SmartPtr<T, H, OP, CP, KP, SP, CNP >& lhs,
        U* rhs)
    { return !(lhs == rhs); }
    
////////////////////////////////////////////////////////////////////////////////
///  operator!= for lhs = raw pointer, rhs = SmartPtr
///  \ingroup SmartPointerGroup
////////////////////////////////////////////////////////////////////////////////

    template
    <
        typename T,
        typename H,
        template <class> class OP,
        class CP,
        template <class> class KP,
        template <class,class> class SP,
        template <class> class CNP,
        typename U
    >
    inline bool operator!=(U* lhs,
        const SmartPtr<T, H, OP, CP, KP, SP, CNP >& rhs)
    { return rhs != lhs; }

////////////////////////////////////////////////////////////////////////////////
///  operator< for lhs = SmartPtr, rhs = raw pointer
///  \ingroup SmartPointerGroup
////////////////////////////////////////////////////////////////////////////////

    template
    <
        typename T,
        typename H,
        template <class> class OP,
        class CP,
        template <class> class KP,
        template <class,class> class SP,
        template <class> class CNP,
        typename U
    >
    inline bool operator<(const SmartPtr<T, H, OP, CP, KP, SP, CNP >& lhs,
        U* rhs)
    {
        return ( GetImpl( lhs ) < rhs );
    }

////////////////////////////////////////////////////////////////////////////////
///  operator< for lhs = raw pointer, rhs = SmartPtr
///  \ingroup SmartPointerGroup
////////////////////////////////////////////////////////////////////////////////

    template
    <
        typename T,
        typename H,
        template <class> class OP,
        class CP,
        template <class> class KP,
        template <class,class> class SP,
        template <class> class CNP,
        typename U
    >
    inline bool operator<(U* lhs,
        const SmartPtr<T, H, OP, CP, KP, SP, CNP >& rhs)
    {
        return ( GetImpl( rhs ) < lhs );
    }

////////////////////////////////////////////////////////////////////////////////
//  operator> for lhs = SmartPtr, rhs = raw pointer
///  \ingroup SmartPointerGroup
////////////////////////////////////////////////////////////////////////////////

    template
    <
        typename T,
        typename H,
        template <class> class OP,
        class CP,
        template <class> class KP,
        template <class,class> class SP,
        template <class> class CNP,
        typename U
    >
    inline bool operator>(const SmartPtr<T, H, OP, CP, KP, SP, CNP >& lhs,
        U* rhs)
    { return rhs < lhs; }
        
////////////////////////////////////////////////////////////////////////////////
///  operator> for lhs = raw pointer, rhs = SmartPtr
///  \ingroup SmartPointerGroup
////////////////////////////////////////////////////////////////////////////////

    template
    <
        typename T,
        typename H,
        template <class> class OP,
        class CP,
        template <class> class KP,
        template <class,class> class SP,
        template <class> class CNP,
        typename U
    >
    inline bool operator>(U* lhs,
        const SmartPtr<T, H, OP, CP, KP, SP, CNP >& rhs)
    { return rhs < lhs; }
  
////////////////////////////////////////////////////////////////////////////////
///  operator<= for lhs = SmartPtr, rhs = raw pointer
///  \ingroup SmartPointerGroup
////////////////////////////////////////////////////////////////////////////////

    template
    <
        typename T,
        typename H,
        template <class> class OP,
        class CP,
        template <class> class KP,
        template <class,class> class SP,
        template <class> class CNP,
        typename U
    >
    inline bool operator<=(const SmartPtr<T, H, OP, CP, KP, SP, CNP >& lhs,
        U* rhs)
    { return !(rhs < lhs); }
        
////////////////////////////////////////////////////////////////////////////////
///  operator<= for lhs = raw pointer, rhs = SmartPtr
///  \ingroup SmartPointerGroup
////////////////////////////////////////////////////////////////////////////////

    template
    <
        typename T,
        typename H,
        template <class> class OP,
        class CP,
        template <class> class KP,
        template <class,class> class SP,
        template <class> class CNP,
        typename U
    >
    inline bool operator<=(U* lhs,
        const SmartPtr<T, H, OP, CP, KP, SP, CNP >& rhs)
    { return !(rhs < lhs); }

////////////////////////////////////////////////////////////////////////////////
///  operator>= for lhs = SmartPtr, rhs = raw pointer
///  \ingroup SmartPointerGroup
////////////////////////////////////////////////////////////////////////////////

    template
    <
        typename T,
        typename H,
        template <class> class OP,
        class CP,
        template <class> class KP,
        template <class,class> class SP,
        template <class> class CNP,
        typename U
    >
    inline bool operator>=(const SmartPtr<T, H, OP, CP, KP, SP, CNP >& lhs,
        U* rhs)
    { return !(lhs < rhs); }
        
////////////////////////////////////////////////////////////////////////////////
///  operator>= for lhs = raw pointer, rhs = SmartPtr
///  \ingroup SmartPointerGroup
////////////////////////////////////////////////////////////////////////////////

    template
    <
        typename T,
        typename H,
        template <class> class OP,
        class CP,
        template <class> class KP,
        template <class,class> class SP,
        template <class> class CNP,
        typename U
    >
    inline bool operator>=(U* lhs,
        const SmartPtr<T, H, OP, CP, KP, SP, CNP >& rhs)
    { return !(lhs < rhs); }

};

////////////////////////////////////////////////////////////////////////////////
///  specialization of std::less for SmartPtr
///  \ingroup SmartPointerGroup
////////////////////////////////////////////////////////////////////////////////

namespace std
{
    template
    <
        typename T,
        typename H,
        template <class> class OP,
        class CP,
        template <class> class KP,
        template <class,class> class SP,
        template <class> class CNP
    >
    struct less< maci::SmartPtr<T, H, OP, CP, KP, SP, CNP > >
        : public binary_function<maci::SmartPtr<T, H, OP, CP, KP, SP, CNP >,
            maci::SmartPtr<T, H, OP, CP, KP, SP, CNP >, bool>
    {
        bool operator()(const maci::SmartPtr<T, H, OP, CP, KP, SP, CNP >& lhs,
            const maci::SmartPtr<T, H, OP, CP, KP, SP, CNP >& rhs) const
        { return less<T*>()(GetImpl(lhs), GetImpl(rhs)); }
    };
};

#endif /* ACSSMARTPOINTER_H */

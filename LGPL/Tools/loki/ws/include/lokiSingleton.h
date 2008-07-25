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

#ifndef LOKI_SINGLETON_INC_
#define LOKI_SINGLETON_INC_

// $Header: /diskb/tmp/stefano/project2/CVS/ACS/LGPL/Tools/loki/ws/include/lokiSingleton.h,v 1.4 2008/07/25 07:50:57 cparedes Exp $

#include "lokiExport.h"
#include "lokiThreads.h"
#include <algorithm>
#include <stdexcept>
#include <cassert>
#include <cstdlib>
#include <new>
#include <vector>
#include <list>
#include <memory>

#ifdef _MSC_VER
#define LOKI_C_CALLING_CONVENTION_QUALIFIER __cdecl 
#else
#define LOKI_C_CALLING_CONVENTION_QUALIFIER 
#endif

///  \defgroup  SingletonGroup Singleton
///  \defgroup  CreationGroup Creation policies
///  \ingroup   SingletonGroup
///  \defgroup  LifetimeGroup Lifetime policies
///  \ingroup   SingletonGroup
///  The lifetimes of the singleton.
///  \par Special lifetime for SmallObjects
///  When the holded object is a Small(Value)Object or the holded object 
///  uses objects which are or inherit from Small(Value)Object
///  then you can't use the default lifetime: you must use the lifetime
///  \code Loki::LongevityLifetime::DieAsSmallObjectChild \endcode
///  Be aware of this when you use Loki::Factory, Loki::Functor, or Loki::Function.



namespace Loki
{
    typedef void (LOKI_C_CALLING_CONVENTION_QUALIFIER *atexit_pfn_t)();

    namespace Private
    {

#ifndef LOKI_MAKE_DLL
        void LOKI_C_CALLING_CONVENTION_QUALIFIER AtExitFn(); // declaration needed below   
#else
        void LOKI_EXPORT AtExitFn();
#endif

        class LifetimeTracker;

#define LOKI_ENABLE_NEW_SETLONGLIVITY_HELPER_DATA_IMPL        
#ifdef LOKI_ENABLE_NEW_SETLONGLIVITY_HELPER_DATA_IMPL

        // Helper data
        // std::list because of the inserts
        typedef std::list<LifetimeTracker*> TrackerArray;
        extern LOKI_EXPORT TrackerArray* pTrackerArray;
#else
        // Helper data
        typedef LifetimeTracker** TrackerArray;
        extern TrackerArray pTrackerArray;
        extern unsigned int elements;
#endif

        ////////////////////////////////////////////////////////////////////////////////
        // class LifetimeTracker
        // Helper class for SetLongevity
        ////////////////////////////////////////////////////////////////////////////////

        class LifetimeTracker
        {
        public:
            LifetimeTracker(unsigned int x) : longevity_(x) 
            {}
            
            virtual ~LifetimeTracker() = 0;
            
            static bool Compare(const LifetimeTracker* lhs,
                const LifetimeTracker* rhs)
            {
                return lhs->longevity_ > rhs->longevity_;
            }
            
        private:
            unsigned int longevity_;
        };
        
        // Definition required
        inline LifetimeTracker::~LifetimeTracker() {} 

        // Helper destroyer function
        template <typename T>
        struct Deleter
        {
            typedef void (*Type)(T*);
            static void Delete(T* pObj)
            { delete pObj; }
        };

        // Concrete lifetime tracker for objects of type T
        template <typename T, typename Destroyer>
        class ConcreteLifetimeTracker : public LifetimeTracker
        {
        public:
            ConcreteLifetimeTracker(T* p,unsigned int longevity, Destroyer d)
                : LifetimeTracker(longevity)
                , pTracked_(p)
                , destroyer_(d)
            {}
            
            ~ConcreteLifetimeTracker()
            { destroyer_(pTracked_); }
            
        private:
            T* pTracked_;
            Destroyer destroyer_;
        };

    } // namespace Private

    ////////////////////////////////////////////////////////////////////////////////
    ///  \ingroup LifetimeGroup
    ///  
    ///  Assigns an object a longevity; ensures ordered destructions of objects 
    ///  registered thusly during the exit sequence of the application
    ////////////////////////////////////////////////////////////////////////////////

#ifdef LOKI_ENABLE_NEW_SETLONGLIVITY_HELPER_DATA_IMPL

    template <typename T, typename Destroyer>
    void SetLongevity(T* pDynObject, unsigned int longevity,
        Destroyer d)
    {
        // manage lifetime of stack manually
        if(Private::pTrackerArray==0)
            Private::pTrackerArray = new Private::TrackerArray;

        // automatically delete the ConcreteLifetimeTracker object when a exception is thrown
        std::auto_ptr<Private::LifetimeTracker> 
            p( new Private::ConcreteLifetimeTracker<T, Destroyer>(pDynObject, longevity, d) );

        // Find correct position
        Private::TrackerArray::iterator pos = std::upper_bound(
            Private::pTrackerArray->begin(), 
            Private::pTrackerArray->end(), 
            p.get(), 
            Private::LifetimeTracker::Compare);
        
        // Insert the pointer to the ConcreteLifetimeTracker object into the queue
        Private::pTrackerArray->insert(pos, p.get());
        
        // nothing has thrown: don't delete the ConcreteLifetimeTracker object
        p.release();
        
        // Register a call to AtExitFn
        std::atexit(Private::AtExitFn);
    }

#else
    
    template <typename T, typename Destroyer>
    void SetLongevity(T* pDynObject, unsigned int longevity,
        Destroyer d)
    {
        TrackerArray pNewArray = static_cast<TrackerArray>(
                std::realloc(pTrackerArray, 
                    sizeof(*pTrackerArray) * (elements + 1)));
        if (!pNewArray) throw std::bad_alloc();
        
        // Delayed assignment for exception safety
        pTrackerArray = pNewArray;
        
        LifetimeTracker* p = new ConcreteLifetimeTracker<T, Destroyer>(
            pDynObject, longevity, d);
 
        // Insert a pointer to the object into the queue
        TrackerArray pos = std::upper_bound(
            pTrackerArray, 
            pTrackerArray + elements, 
            p, 
            LifetimeTracker::Compare);
        std::copy_backward(
            pos, 
            pTrackerArray + elements,
            pTrackerArray + elements + 1);
        *pos = p;
        ++elements;
        
        // Register a call to AtExitFn
        std::atexit(Private::AtExitFn);
    }

#endif

    template <typename T>
    void SetLongevity(T* pDynObject, unsigned int longevity,
        typename Private::Deleter<T>::Type d = Private::Deleter<T>::Delete)
    {
        SetLongevity<T, typename Private::Deleter<T>::Type>(pDynObject, longevity, d);
    }

    ////////////////////////////////////////////////////////////////////////////////
    ///  \struct CreateUsingNew 
    ///
    ///  \ingroup CreationGroup
    ///  Implementation of the CreationPolicy used by SingletonHolder
    ///  Creates objects using a straight call to the new operator 
    ////////////////////////////////////////////////////////////////////////////////
    template <class T> struct CreateUsingNew
    {
        static T* Create()
        { return new T; }
        
        static void Destroy(T* p)
        { delete p; }
    };
    
    ////////////////////////////////////////////////////////////////////////////////
    ///  \struct CreateUsing
    ///
    ///  \ingroup CreationGroup
    ///  Implementation of the CreationPolicy used by SingletonHolder
    ///  Creates objects using a custom allocater.
    ///  Usage: e.g. CreateUsing<std::allocator>::Allocator
    ////////////////////////////////////////////////////////////////////////////////
    template<template<class> class Alloc>
    struct CreateUsing
    {
        template <class T>
        struct Allocator
        {
            static Alloc<T> allocator;

            static T* Create()
            {
                return new (allocator.allocate(1)) T;
            }

            static void Destroy(T* p)
            {
                //allocator.destroy(p);
                p->~T();
                allocator.deallocate(p,1);
            }
        };
    };
    
    ////////////////////////////////////////////////////////////////////////////////
    ///  \struct CreateUsingMalloc
    ///
    ///  \ingroup CreationGroup
    ///  Implementation of the CreationPolicy used by SingletonHolder
    ///  Creates objects using a call to std::malloc, followed by a call to the 
    ///  placement new operator
    ////////////////////////////////////////////////////////////////////////////////
    template <class T> struct CreateUsingMalloc
    {
        static T* Create()
        {
            void* p = std::malloc(sizeof(T));
            if (!p) return 0;
            return new(p) T;
        }
        
        static void Destroy(T* p)
        {
            p->~T();
            std::free(p);
        }
    };
    

    ////////////////////////////////////////////////////////////////////////////////
    ///  \struct CreateStatic
    ///
    ///  \ingroup CreationGroup
    ///  Implementation of the CreationPolicy used by SingletonHolder
    ///  Creates an object in static memory
    ///  Implementation is slightly nonportable because it uses the MaxAlign trick 
    ///  (an union of all types to ensure proper memory alignment). This trick is 
    ///  nonportable in theory but highly portable in practice.
    ////////////////////////////////////////////////////////////////////////////////
    template <class T> struct CreateStatic
    {
        
#ifdef _MSC_VER
#pragma warning( push ) 
#pragma warning( disable : 4121 )
// alignment of a member was sensitive to packing
#endif // _MSC_VER

        union MaxAlign
        {
            char t_[sizeof(T)];
            short int shortInt_;
            int int_;
            long int longInt_;
            float float_;
            double double_;
            long double longDouble_;
            struct Test;
            int Test::* pMember_;
            int (Test::*pMemberFn_)(int);
        };
        
#ifdef _MSC_VER
#pragma warning( pop )
#endif // _MSC_VER
        
        static T* Create()
        {
            static MaxAlign staticMemory_;
            return new(&staticMemory_) T;
        }
        
        static void Destroy(T* p)
        {
            p->~T();
        }
    };

    ////////////////////////////////////////////////////////////////////////////////
    ///  \struct DefaultLifetime
    ///
    ///  \ingroup LifetimeGroup
    ///  Implementation of the LifetimePolicy used by SingletonHolder
    ///  Schedules an object's destruction as per C++ rules
    ///  Forwards to std::atexit
    ////////////////////////////////////////////////////////////////////////////////
    template <class T>
    struct DefaultLifetime
    {
        static void ScheduleDestruction(T*, atexit_pfn_t pFun)
        { std::atexit(pFun); }
        
        static void OnDeadReference()
        { throw std::logic_error("Dead Reference Detected"); }
    };

    ////////////////////////////////////////////////////////////////////////////////
    ///  \struct  PhoenixSingleton
    ///
    ///  \ingroup LifetimeGroup
    ///  Implementation of the LifetimePolicy used by SingletonHolder
    ///  Schedules an object's destruction as per C++ rules, and it allows object 
    ///  recreation by not throwing an exception from OnDeadReference
    ////////////////////////////////////////////////////////////////////////////////
    template <class T>
    class PhoenixSingleton
    {
    public:
        static void ScheduleDestruction(T*, atexit_pfn_t pFun)
        {
#ifndef ATEXIT_FIXED
            if (!destroyedOnce_)
#endif
                std::atexit(pFun);
        }
        
        static void OnDeadReference()
        {
#ifndef ATEXIT_FIXED
            destroyedOnce_ = true;
#endif
        }
        
    private:
#ifndef ATEXIT_FIXED
        static bool destroyedOnce_;
#endif
    };
    
#ifndef ATEXIT_FIXED
    template <class T> bool PhoenixSingleton<T>::destroyedOnce_ = false;
#endif

    ////////////////////////////////////////////////////////////////////////////////
    // Copyright (c) 2004 by Curtis Krauskopf - curtis@decompile.com
    ///
    ///  \struct  DeletableSingleton
    ///
    ///  \ingroup LifetimeGroup
    ///
    ///  A DeletableSingleton allows the instantiated singleton to be 
    ///  destroyed at any time. The singleton can be reinstantiated at 
    ///  any time, even during program termination.
    ///  If the singleton exists when the program terminates, it will 
    ///  be automatically deleted.
    ///
    ///  \par Usage:  
    ///  The singleton can be deleted manually:
    ///
    ///  DeletableSingleton<MyClass>::GracefulDelete();
    ////////////////////////////////////////////////////////////////////////////////
    template <class T>
    class DeletableSingleton
    {
    public:

        static void ScheduleDestruction(T*, atexit_pfn_t pFun)
        {
            static bool firstPass = true;
            isDead = false;
            deleter = pFun;
            if (firstPass || needCallback)
            {
                std::atexit(atexitCallback);
                firstPass = false;
                needCallback = false;
            }
        }
    
        static void OnDeadReference()
        { 
        }
        ///  delete singleton object manually
        static void GracefulDelete()
        {
            if (isDead)
                return;
            isDead = true;
            deleter();
        }
    
    protected:
        static atexit_pfn_t deleter;
        static bool isDead;
        static bool needCallback;
        
        static void atexitCallback()
        {
#ifdef ATEXIT_FIXED
            needCallback = true;
#else
            needCallback = false;
#endif
            GracefulDelete();
        }
    };
    
    template <class T>
    atexit_pfn_t DeletableSingleton<T>::deleter = 0;
    
    template <class T>
    bool DeletableSingleton<T>::isDead = true;
    
    template <class T>
    bool DeletableSingleton<T>::needCallback = true;

    ////////////////////////////////////////////////////////////////////////////////
    // class template Adapter
    // Helper for SingletonWithLongevity below
    ////////////////////////////////////////////////////////////////////////////////

    namespace Private
    {
        template <class T>
        struct Adapter
        {
            void operator()(T*) { return pFun_(); }
            atexit_pfn_t pFun_;
        };
    }

    ////////////////////////////////////////////////////////////////////////////////
    ///  \struct SingletonWithLongevity
    ///
    ///  \ingroup LifetimeGroup
    ///  Implementation of the LifetimePolicy used by SingletonHolder
    ///  Schedules an object's destruction in order of their longevities
    ///  Assumes a visible function GetLongevity(T*) that returns the longevity of the
    ///  object.
    ////////////////////////////////////////////////////////////////////////////////
    template <class T>
    class SingletonWithLongevity
    {
    public:
        static void ScheduleDestruction(T* pObj, atexit_pfn_t pFun)
        {
            Private::Adapter<T> adapter = { pFun };
            SetLongevity(pObj, GetLongevity(pObj), adapter);
        }
        
        static void OnDeadReference()
        { throw std::logic_error("Dead Reference Detected"); }
    };

    ////////////////////////////////////////////////////////////////////////////////
    ///  \struct NoDestroy
    ///
    ///  \ingroup LifetimeGroup
    ///  Implementation of the LifetimePolicy used by SingletonHolder
    ///  Never destroys the object
    ////////////////////////////////////////////////////////////////////////////////
    template <class T>
    struct NoDestroy
    {
        static void ScheduleDestruction(T*, atexit_pfn_t)
        {}
        
        static void OnDeadReference()
        {}
    };
    

    ////////////////////////////////////////////////////////////////////////////////
    ///  \defgroup LongevityLifetimeGroup LongevityLifetime
    ///  \ingroup LifetimeGroup
    ///
    ///  \namespace LongevityLifetime
    ///
    ///  \ingroup LongevityLifetimeGroup
    ///  \brief  In this namespace are special lifetime policies to manage lifetime
    ///  dependencies.
    ////////////////////////////////////////////////////////////////////////////////
    namespace LongevityLifetime
    {
        ////////////////////////////////////////////////////////////////////////////////
        ///  \struct  SingletonFixedLongevity 
        ///
        ///  \ingroup LongevityLifetimeGroup
        ///  Add your own lifetimes into the namespace 'LongevityLifetime'
        ///  with your prefered lifetime by adding a struct like this:
        ///
        ///  template<class T>
        ///  struct MyLifetime  : SingletonFixedLongevity< MyLifetimeNumber ,T> {}
        ////////////////////////////////////////////////////////////////////////////////
        template <unsigned int Longevity, class T>
        class SingletonFixedLongevity
        {
        public:
            virtual ~SingletonFixedLongevity() {}
            
            static void ScheduleDestruction(T* pObj, atexit_pfn_t pFun)
            {
                Private::Adapter<T> adapter = { pFun };
                SetLongevity(pObj, Longevity , adapter);
            }
            
            static void OnDeadReference()
            { throw std::logic_error("Dead Reference Detected"); }
        };

        ///  \struct DieLast
        ///  \ingroup LongevityLifetimeGroup
        ///  \brief  Longest possible SingletonWithLongevity lifetime: 0xFFFFFFFF
        template <class T>
        struct DieLast  : SingletonFixedLongevity<0xFFFFFFFF ,T>
        {};

        ///  \struct DieDirectlyBeforeLast
        ///  \ingroup LongevityLifetimeGroup
        ///  \brief  Lifetime is a one less than DieLast: 0xFFFFFFFF-1
        template <class T>
        struct DieDirectlyBeforeLast  : SingletonFixedLongevity<0xFFFFFFFF-1 ,T>
        {};

        ///  \struct DieFirst
        ///  \ingroup LongevityLifetimeGroup
        ///  \brief  Shortest possible SingletonWithLongevity lifetime: 0 
        template <class T>
        struct DieFirst : SingletonFixedLongevity<0,T>
        {};
    
    }//namespace LongevityLifetime

    ////////////////////////////////////////////////////////////////////////////////
    ///  \class FollowIntoDeath
    ///
    ///  \ingroup LifetimeGroup
    ///
    ///  Lifetime policyfor the SingletonHolder tempalte. 
    ///  Followers will die after the master dies Followers will not die, if
    ///    - master never dies (NoDestroy policy)
    ///    - master never created
    ///    - master dies not in the function registered with atexit
    ///    - master dies not by a call of a the atexit registerd function (DeletableSingleton::GracefulDelete)         
    ///
    ///  \par Usage:
    ///
    ///  Lifetimes of the master and the follower singletons,  e.g. with a M and a F class:
    ///   \code SingletonHolder< M , FollowIntoDeath::With<DefaultLifetime>::AsMasterLifetime > MasterSingleton; \endcode
    ///   \code SingletonHolder< F , CreateUsingNew, FollowIntoDeath::AfterMaster< MasterSingleton >::IsDestroyed > FollowerSingleton \endcode
    ////////////////////////////////////////////////////////////////////////////////
    class FollowIntoDeath
    {
        template<class T>
        class Followers
        {
            typedef std::vector<atexit_pfn_t> Container;
            typedef typename Container::iterator iterator;
            static Container* followers_;
        
        public:
            static void Init()
            {
                static bool done = false;
                if(!done)
                {
                    followers_ = new Container;
                    done = true;
                }
            }

            static void AddFollower(atexit_pfn_t ae)
            {
                Init();
                followers_->push_back(ae);
            }

            static void DestroyFollowers()
            {
                Init();
                for(iterator it = followers_->begin();it != followers_->end();++it)
                    (*it)();    
                delete followers_;
            }
        };

    public:

        ///  \struct With
        ///  Template for the master 
        ///  \param Lifetime Lifetime policy for the master
        template<template <class> class Lifetime>
        struct With
        {
            ///  \struct AsMasterLifetime
            ///  Policy for master
            template<class Master>
            struct AsMasterLifetime
            {
                static void ScheduleDestruction(Master* pObj, atexit_pfn_t pFun)
                {
                    Followers<Master>::Init();
                    Lifetime<Master>::ScheduleDestruction(pObj, pFun);

                    // use same policy for the followers and force a new 
                    // template instantiation,  this adds a additional atexit entry
                    // does not work with SetLonlevity, but there you can control
                    // the lifetime with the GetLongevity function.
                    Lifetime<Followers<Master> >::ScheduleDestruction(0,Followers<Master>::DestroyFollowers);
                }

                static void OnDeadReference()
                { 
                    throw std::logic_error("Dead Reference Detected"); 
                }
            };
        };

        ///  \struct AfterMaster
        ///  Template for the follower
        ///  \param Master Master to follow into death
        template<class Master>
        struct AfterMaster
        {
            ///  \struct IsDestroyed
            ///  Policy for followers 
            template<class F>
            struct IsDestroyed
            {
                static void ScheduleDestruction(F*, atexit_pfn_t pFun)
                {
                    Followers<Master>::AddFollower(pFun);
                }
      
                static void OnDeadReference()
                { 
                    throw std::logic_error("Dead Reference Detected"); 
                }
            };
        };
    };

    template<class T>
    typename FollowIntoDeath::Followers<T>::Container* 
    FollowIntoDeath::Followers<T>::followers_ = 0;
    
    
    
    ////////////////////////////////////////////////////////////////////////////////
    ///  \class  SingletonHolder
    ///
    ///  \ingroup SingletonGroup
    ///
    ///  Provides Singleton amenities for a type T
    ///  To protect that type from spurious instantiations, 
    ///  you have to protect it yourself.
    ///  
    ///  \param CreationPolicy Creation policy, default: CreateUsingNew
    ///  \param LifetimePolicy Lifetime policy, default: DefaultLifetime,
    ///  \param ThreadingModel Threading policy, 
    ///                         default: LOKI_DEFAULT_THREADING_NO_OBJ_LEVEL
    ////////////////////////////////////////////////////////////////////////////////
    template
    <
        typename T,
        template <class> class CreationPolicy = CreateUsingNew,
        template <class> class LifetimePolicy = DefaultLifetime,
        template <class, class> class ThreadingModel = LOKI_DEFAULT_THREADING_NO_OBJ_LEVEL,
        class MutexPolicy = LOKI_DEFAULT_MUTEX
    >
    class SingletonHolder
    {
    public:

        ///  Type of the singleton object
        typedef T ObjectType;

        ///  Returns a reference to singleton object
        static T& Instance();
        
    private:
        // Helpers
        static void MakeInstance();
        static void LOKI_C_CALLING_CONVENTION_QUALIFIER DestroySingleton();
        
        // Protection
        SingletonHolder();
        
        // Data
        typedef typename ThreadingModel<T*,MutexPolicy>::VolatileType PtrInstanceType;
        static PtrInstanceType pInstance_;
        static bool destroyed_;
    };
    
    ////////////////////////////////////////////////////////////////////////////////
    // SingletonHolder's data
    ////////////////////////////////////////////////////////////////////////////////

    template
    <
        class T,
        template <class> class C,
        template <class> class L,
        template <class, class> class M,
        class X
    >
    typename SingletonHolder<T, C, L, M, X>::PtrInstanceType
        SingletonHolder<T, C, L, M, X>::pInstance_;

    template
    <
        class T,
        template <class> class C,
        template <class> class L,
        template <class, class> class M,
        class X
    >
    bool SingletonHolder<T, C, L, M, X>::destroyed_;

    ////////////////////////////////////////////////////////////////////////////////
    // SingletonHolder::Instance
    ////////////////////////////////////////////////////////////////////////////////

    template
    <
        class T,
        template <class> class CreationPolicy,
        template <class> class LifetimePolicy,
        template <class, class> class ThreadingModel,
        class MutexPolicy
    >
    inline T& SingletonHolder<T, CreationPolicy, 
        LifetimePolicy, ThreadingModel, MutexPolicy>::Instance()
    {
        if (!pInstance_)
        {
            MakeInstance();
        }
        return *pInstance_;
    }

    ////////////////////////////////////////////////////////////////////////////////
    // SingletonHolder::MakeInstance (helper for Instance)
    ////////////////////////////////////////////////////////////////////////////////

    template
    <
        class T,
        template <class> class CreationPolicy,
        template <class> class LifetimePolicy,
        template <class, class> class ThreadingModel,
        class MutexPolicy
    >
    void SingletonHolder<T, CreationPolicy, 
        LifetimePolicy, ThreadingModel, MutexPolicy>::MakeInstance()
    {
        typename ThreadingModel<SingletonHolder,MutexPolicy>::Lock guard;
        (void)guard;
        
        if (!pInstance_)
        {
            if (destroyed_)
            {
                destroyed_ = false;
                LifetimePolicy<T>::OnDeadReference();
            }
            pInstance_ = CreationPolicy<T>::Create();
            LifetimePolicy<T>::ScheduleDestruction(pInstance_, 
                &DestroySingleton);
        }
    }

    template
    <
        class T,
        template <class> class CreationPolicy,
        template <class> class L,
        template <class, class> class M,
        class X
    >
    void LOKI_C_CALLING_CONVENTION_QUALIFIER 
    SingletonHolder<T, CreationPolicy, L, M, X>::DestroySingleton()
    {
        assert(!destroyed_);
        CreationPolicy<T>::Destroy(pInstance_);
        pInstance_ = 0;
        destroyed_ = true;
    }


    ////////////////////////////////////////////////////////////////////////////////
    ///  \class  Singleton
    ///
    ///  \ingroup SingletonGroup
    ///
    ///  Convenience template to implement a getter function for a singleton object.
    ///  Often needed in a shared library which hosts singletons.
    ///  
    ///  \par Usage
    ///
    ///  see test/SingletonDll
    ///
    ////////////////////////////////////////////////////////////////////////////////

    template<class T>
    class Singleton
    {
    public:
        static T& Instance();
    };

} // namespace Loki


/// \def LOKI_SINGLETON_INSTANCE_DEFINITION(SHOLDER)
/// Convenience macro for the definition of the static Instance member function
/// Put this macro called with a SingletonHolder typedef into your cpp file.

#define LOKI_SINGLETON_INSTANCE_DEFINITION(SHOLDER)                     \
namespace Loki                                                          \
{                                                                        \
    template<>                                                          \
    SHOLDER::ObjectType&  Singleton<SHOLDER::ObjectType>::Instance()    \
    {                                                                   \
        return SHOLDER::Instance();                                     \
    }                                                                    \
}



////////////////////////////////////////////////////////////////////////////////
// Change log:
// May 21, 2001: Correct the volatile qualifier - credit due to Darin Adler
// June 20, 2001: ported by Nick Thurn to gcc 2.95.3. Kudos, Nick!!!
// January 08, 2002: Fixed bug in call to realloc - credit due to Nigel Gent and
//      Eike Petersen
// March 08, 2002: moved the assignment to pTrackerArray in SetLongevity to fix
//      exception safety issue. Credit due to Kari Hoijarvi
// May 09, 2002: Fixed bug in Compare that caused longevities to act backwards.
//      Credit due to Scott McDonald.
////////////////////////////////////////////////////////////////////////////////

#endif // SINGLETON_INC_

// $Log: lokiSingleton.h,v $
// Revision 1.4  2008/07/25 07:50:57  cparedes
// Removing use namespace from included files and updating
// the files where the use namespace was assumed
//
// Revision 1.3  2007/02/01 17:29:00  sharring
//
// updating to newer version of loki library, with support for multi-threading enabled. manually renamed files to avoid name conflicts, by
// prepending "loki" to the names of header files. also manually edited lokiThreads.h to #define LOKI_OBJECT_LEVEL_THREADING; this could
// also be done with a compile FLAG, perhaps would be better.
//
// Revision 1.2.28.1  2007/02/01 07:36:57  sharring
//
// updating loki to newer version for testing in SFI in the hopes of fixing some
// multi-threading problems seen in acs logging code for which the stack trace
// indicates that loki smart pointers were involved.
//
// Revision 1.31  2006/06/19 12:39:08  syntheticpp
// replace tabs with 4 spaces
//
// Revision 1.30  2006/05/20 10:23:07  syntheticpp
// add warnings in the documentation about the special lifetime when using SmallObjects
//
// Revision 1.29  2006/03/15 08:47:18  syntheticpp
// gcc: specialization only in the correct namespace, thx to Sam Miller
//
// Revision 1.28  2006/03/03 11:58:24  syntheticpp
// also compile with gcc
//
// Revision 1.27  2006/02/28 10:31:01  syntheticpp
// replace tabs
//
// Revision 1.26  2006/02/28 10:30:17  syntheticpp
// add singleton object getter function template
//
// Revision 1.25  2006/02/27 19:59:20  syntheticpp
// add support of loki.dll
//
// Revision 1.24  2006/01/22 13:37:33  syntheticpp
// use macro LOKI_DEFAULT_MUTEX for Mutex default value, defined in Threads.h
//
// Revision 1.23  2006/01/22 00:44:17  syntheticpp
// add additional template parameter for the changed threading classes
//
// Revision 1.22  2006/01/19 23:11:55  lfittl
// - Disabled -Weffc++ flag, fixing these warnings produces too much useless code
// - Enabled -pedantic, -Wold-style-cast and -Wundef for src/ and test/
//
// Revision 1.21  2006/01/16 20:10:51  syntheticpp
// another fight against tabs
//
// Revision 1.20  2006/01/16 19:56:30  syntheticpp
// add support of allocators with a standard interface, thanks to Miguel A. Figueroa-Villanueva
//
// Revision 1.19  2006/01/16 19:05:09  rich_sposato
// Added cvs keywords.
//




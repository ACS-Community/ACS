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

#ifndef SINGLETON_INC_
#define SINGLETON_INC_

#include "lokiThreads.h"
#include <algorithm>
#include <stdexcept>
#include <cassert>
#include <cstdlib>
#include <new>

#ifdef _MSC_VER
#define C_CALLING_CONVENTION_QUALIFIER __cdecl 
#else
#define C_CALLING_CONVENTION_QUALIFIER 
#endif

namespace Loki
{
    typedef void (C_CALLING_CONVENTION_QUALIFIER *atexit_pfn_t)();

    namespace Private
    {
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
        
        // Helper data
        typedef LifetimeTracker** TrackerArray;
        extern TrackerArray pTrackerArray;
        extern unsigned int elements;

        // Helper destroyer function
        template <typename T>
        struct Deleter
        {
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

        void C_CALLING_CONVENTION_QUALIFIER AtExitFn(); // declaration needed below    
    } // namespace Private

////////////////////////////////////////////////////////////////////////////////
// function template SetLongevity
// Assigns an object a longevity; ensures ordered destructions of objects 
//     registered thusly during the exit sequence of the application
////////////////////////////////////////////////////////////////////////////////

    template <typename T, typename Destroyer>
    void SetLongevity(T* pDynObject, unsigned int longevity,
        Destroyer d = Private::Deleter<T>::Delete)
    {
        using namespace Private;
        
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

////////////////////////////////////////////////////////////////////////////////
// class template CreateUsingNew
// Implementation of the CreationPolicy used by SingletonHolder
// Creates objects using a straight call to the new operator 
////////////////////////////////////////////////////////////////////////////////

    template <class T> struct CreateUsingNew
    {
        static T* Create()
        { return new T; }
        
        static void Destroy(T* p)
        { delete p; }
    };
    
////////////////////////////////////////////////////////////////////////////////
// class template CreateUsingNew
// Implementation of the CreationPolicy used by SingletonHolder
// Creates objects using a call to std::malloc, followed by a call to the 
//     placement new operator
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
// class template CreateStatic
// Implementation of the CreationPolicy used by SingletonHolder
// Creates an object in static memory
// Implementation is slightly nonportable because it uses the MaxAlign trick 
//     (an union of all types to ensure proper memory alignment). This trick is 
//     nonportable in theory but highly portable in practice.
////////////////////////////////////////////////////////////////////////////////

    template <class T> struct CreateStatic
    {
#if defined(_MSC_VER) && _MSC_VER >= 1300
#pragma warning( push ) 
 // alignment of a member was sensitive to packing
#pragma warning( disable : 4121 )
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
#if defined(_MSC_VER) && _MSC_VER >= 1300
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
// class template DefaultLifetime
// Implementation of the LifetimePolicy used by SingletonHolder
// Schedules an object's destruction as per C++ rules
// Forwards to std::atexit
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
// class template PhoenixSingleton
// Implementation of the LifetimePolicy used by SingletonHolder
// Schedules an object's destruction as per C++ rules, and it allows object 
//    recreation by not throwing an exception from OnDeadReference
////////////////////////////////////////////////////////////////////////////////

    template <class T>
    class PhoenixSingleton
    {
    public:
        static void ScheduleDestruction(T*, atexit_pfn_t pFun)
        {
#ifndef ATEXIT_FIXED
#ifndef MAKE_VXWORKS
            if (!destroyedOnce_)
#endif
#endif
                std::atexit(pFun);
        }
        
        static void OnDeadReference()
        {
#ifndef ATEXIT_FIXED
#ifndef MAKE_VXWORKS
            destroyedOnce_ = true;
#endif
#endif
        }
        
    private:
#ifndef ATEXIT_FIXED
#ifndef MAKE_VXWORKS
        static bool destroyedOnce_;
#endif
#endif
    };
    
#ifndef ATEXIT_FIXED 
#ifndef MAKE_VXWORKS
    template <class T> bool PhoenixSingleton<T>::destroyedOnce_ = false;
#endif
#endif
        
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
// class template SingletonWithLongevity
// Implementation of the LifetimePolicy used by SingletonHolder
// Schedules an object's destruction in order of their longevities
// Assumes a visible function GetLongevity(T*) that returns the longevity of the
//     object
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
// class template NoDestroy
// Implementation of the LifetimePolicy used by SingletonHolder
// Never destroys the object
////////////////////////////////////////////////////////////////////////////////

    template <class T>
    struct NoDestroy
    {
        static void ScheduleDestruction(T*, atexit_pfn_t pFun)
        {}
        
        static void OnDeadReference()
        {}
    };

////////////////////////////////////////////////////////////////////////////////
// class template SingletonHolder
// Provides Singleton amenities for a type T
// To protect that type from spurious instantiations, you have to protect it
//     yourself.
////////////////////////////////////////////////////////////////////////////////

    template
    <
        typename T,
        template <class> class CreationPolicy = CreateUsingNew,
        template <class> class LifetimePolicy = DefaultLifetime,
        template <class> class ThreadingModel = SingleThreaded
    >
    class SingletonHolder
    {
    public:
        static T& Instance();
        
    private:
        // Helpers
        static void MakeInstance();
        static void C_CALLING_CONVENTION_QUALIFIER DestroySingleton();
        
        // Protection
        SingletonHolder();
        
        // Data
        typedef typename ThreadingModel<T*>::VolatileType PtrInstanceType;
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
        template <class> class M
    >
    typename SingletonHolder<T, C, L, M>::PtrInstanceType
        SingletonHolder<T, C, L, M>::pInstance_;

    template
    <
        class T,
        template <class> class C,
        template <class> class L,
        template <class> class M
    >
    bool SingletonHolder<T, C, L, M>::destroyed_;

////////////////////////////////////////////////////////////////////////////////
// SingletonHolder::Instance
////////////////////////////////////////////////////////////////////////////////

    template
    <
        class T,
        template <class> class CreationPolicy,
        template <class> class LifetimePolicy,
        template <class> class ThreadingModel
    >
    inline T& SingletonHolder<T, CreationPolicy, 
        LifetimePolicy, ThreadingModel>::Instance()
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
        template <class> class ThreadingModel
    >
    void SingletonHolder<T, CreationPolicy, 
        LifetimePolicy, ThreadingModel>::MakeInstance()
    {
        typename ThreadingModel<T>::Lock guard;
        (void)guard;
        
        if (!pInstance_)
        {
            if (destroyed_)
            {
                LifetimePolicy<T>::OnDeadReference();
                destroyed_ = false;
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
        template <class> class M
    >
    void C_CALLING_CONVENTION_QUALIFIER 
    SingletonHolder<T, CreationPolicy, L, M>::DestroySingleton()
    {
        assert(!destroyed_);
        CreationPolicy<T>::Destroy(pInstance_);
        pInstance_ = 0;
        destroyed_ = true;
    }
} // namespace Loki

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

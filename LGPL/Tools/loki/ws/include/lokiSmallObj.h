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

// $Header: /diskb/tmp/stefano/project2/CVS/ACS/LGPL/Tools/loki/ws/include/lokiSmallObj.h,v 1.3 2007/02/07 12:25:11 bjeram Exp $


#ifndef LOKI_SMALLOBJ_INC_
#define LOKI_SMALLOBJ_INC_

#include "lokiExport.h"
#include "lokiThreads.h"
#include "lokiSingleton.h"
#include <cstddef>
#include <new> // needed for std::nothrow_t parameter.


#ifndef LOKI_DEFAULT_CHUNK_SIZE
#define LOKI_DEFAULT_CHUNK_SIZE 4096
#endif

#ifndef LOKI_MAX_SMALL_OBJECT_SIZE
#define LOKI_MAX_SMALL_OBJECT_SIZE 256
#endif

#ifndef LOKI_DEFAULT_OBJECT_ALIGNMENT
#define LOKI_DEFAULT_OBJECT_ALIGNMENT 4
#endif

#ifndef LOKI_DEFAULT_SMALLOBJ_LIFETIME
#define LOKI_DEFAULT_SMALLOBJ_LIFETIME LongevityLifetime::DieAsSmallObjectParent
#endif

#if defined(LOKI_SMALL_OBJECT_USE_NEW_ARRAY) && defined(_MSC_VER)
#pragma message("Don't define LOKI_SMALL_OBJECT_USE_NEW_ARRAY when using a Microsoft compiler to prevent memory leaks.")
#pragma message("now calling '#undef LOKI_SMALL_OBJECT_USE_NEW_ARRAY'")
#undef LOKI_SMALL_OBJECT_USE_NEW_ARRAY
#endif

///  \defgroup  SmallObjectGroup Small objects
///
///  \defgroup  SmallObjectGroupInternal Internals
///  \ingroup   SmallObjectGroup



namespace Loki
{
    namespace LongevityLifetime
    {
        /** @struct DieAsSmallObjectParent
            @ingroup SmallObjectGroup
            Lifetime policy to manage lifetime dependencies of 
            SmallObject base and child classes.
            The Base class should have this lifetime
        */
        template <class T>
        struct DieAsSmallObjectParent  : DieLast<T> {};

        /** @struct DieAsSmallObjectChild
            @ingroup SmallObjectGroup
            Lifetime policy to manage lifetime dependencies of 
            SmallObject base and child classes.
            The Child class should have this lifetime
        */
        template <class T>
        struct DieAsSmallObjectChild  : DieDirectlyBeforeLast<T> {};

    } 

    class FixedAllocator;

    /** @class SmallObjAllocator
        @ingroup SmallObjectGroupInternal
     Manages pool of fixed-size allocators.
     Designed to be a non-templated base class of AllocatorSingleton so that
     implementation details can be safely hidden in the source code file.
     */
    class LOKI_EXPORT SmallObjAllocator
    {
    protected:
        /** The only available constructor needs certain parameters in order to
         initialize all the FixedAllocator's.  This throws only if
         @param pageSize # of bytes in a page of memory.
         @param maxObjectSize Max # of bytes which this may allocate.
         @param objectAlignSize # of bytes between alignment boundaries.
         */
        SmallObjAllocator( std::size_t pageSize, std::size_t maxObjectSize,
            std::size_t objectAlignSize );

        /** Destructor releases all blocks, all Chunks, and FixedAllocator's.
         Any outstanding blocks are unavailable, and should not be used after
         this destructor is called.  The destructor is deliberately non-virtual
         because it is protected, not public.
         */
        ~SmallObjAllocator( void );

    public:
        /** Allocates a block of memory of requested size.  Complexity is often
         constant-time, but might be O(C) where C is the number of Chunks in a
         FixedAllocator. 

         @par Exception Safety Level
         Provides either strong-exception safety, or no-throw exception-safety
         level depending upon doThrow parameter.  The reason it provides two
         levels of exception safety is because it is used by both the nothrow
         and throwing new operators.  The underlying implementation will never
         throw of its own accord, but this can decide to throw if it does not
         allocate.  The only exception it should emit is std::bad_alloc.

         @par Allocation Failure
         If it does not allocate, it will call TrimExcessMemory and attempt to
         allocate again, before it decides to throw or return NULL.  Many
         allocators loop through several new_handler functions, and terminate
         if they can not allocate, but not this one.  It only makes one attempt
         using its own implementation of the new_handler, and then returns NULL
         or throws so that the program can decide what to do at a higher level.
         (Side note: Even though the C++ Standard allows allocators and
         new_handlers to terminate if they fail, the Loki allocator does not do
         that since that policy is not polite to a host program.)

         @param size # of bytes needed for allocation.
         @param doThrow True if this should throw if unable to allocate, false
          if it should provide no-throw exception safety level.
         @return NULL if nothing allocated and doThrow is false.  Else the
          pointer to an available block of memory.
         */
        void * Allocate( std::size_t size, bool doThrow );

        /** Deallocates a block of memory at a given place and of a specific
        size.  Complexity is almost always constant-time, and is O(C) only if
        it has to search for which Chunk deallocates.  This never throws.
         */
        void Deallocate( void * p, std::size_t size );

        /** Deallocates a block of memory at a given place but of unknown size
        size.  Complexity is O(F + C) where F is the count of FixedAllocator's
        in the pool, and C is the number of Chunks in all FixedAllocator's.  This
        does not throw exceptions.  This overloaded version of Deallocate is
        called by the nothow delete operator - which is called when the nothrow
        new operator is used, but a constructor throws an exception.
         */
        void Deallocate( void * p );

        /// Returns max # of bytes which this can allocate.
        inline std::size_t GetMaxObjectSize() const
        { return maxSmallObjectSize_; }

        /// Returns # of bytes between allocation boundaries.
        inline std::size_t GetAlignment() const { return objectAlignSize_; }

        /** Releases empty Chunks from memory.  Complexity is O(F + C) where F
        is the count of FixedAllocator's in the pool, and C is the number of
        Chunks in all FixedAllocator's.  This will never throw.  This is called
        by AllocatorSingleto::ClearExtraMemory, the new_handler function for
        Loki's allocator, and is called internally when an allocation fails.
        @return True if any memory released, or false if none released.
         */
        bool TrimExcessMemory( void );

        /** Returns true if anything in implementation is corrupt.  Complexity
         is O(F + C + B) where F is the count of FixedAllocator's in the pool,
         C is the number of Chunks in all FixedAllocator's, and B is the number
         of blocks in all Chunks.  If it determines any data is corrupted, this
         will return true in release version, but assert in debug version at
         the line where it detects the corrupted data.  If it does not detect
         any corrupted data, it returns false.
         */
        bool IsCorrupt( void ) const;

    private:
        /// Default-constructor is not implemented.
        SmallObjAllocator( void );
        /// Copy-constructor is not implemented.
        SmallObjAllocator( const SmallObjAllocator & );
        /// Copy-assignment operator is not implemented.
        SmallObjAllocator & operator = ( const SmallObjAllocator & );

        /// Pointer to array of fixed-size allocators.
        Loki::FixedAllocator * pool_;

        /// Largest object size supported by allocators.
        const std::size_t maxSmallObjectSize_;

        /// Size of alignment boundaries.
        const std::size_t objectAlignSize_;
    };


    /** @class AllocatorSingleton
        @ingroup SmallObjectGroupInternal
     This template class is derived from
     SmallObjAllocator in order to pass template arguments into it, and still
     have a default constructor for the singleton.  Each instance is a unique
     combination of all the template parameters, and hence is singleton only 
     with respect to those parameters.  The template parameters have default
     values and the class has typedefs identical to both SmallObject and
     SmallValueObject so that this class can be used directly instead of going
     through SmallObject or SmallValueObject.  That design feature allows
     clients to use the new_handler without having the name of the new_handler
     function show up in classes derived from SmallObject or SmallValueObject.
     Thus, the only functions in the allocator which show up in SmallObject or
     SmallValueObject inheritance hierarchies are the new and delete
     operators.
    */
    template
    <
        template <class, class> class ThreadingModel = LOKI_DEFAULT_THREADING_NO_OBJ_LEVEL,
        std::size_t chunkSize = LOKI_DEFAULT_CHUNK_SIZE,
        std::size_t maxSmallObjectSize = LOKI_MAX_SMALL_OBJECT_SIZE,
        std::size_t objectAlignSize = LOKI_DEFAULT_OBJECT_ALIGNMENT,
        template <class> class LifetimePolicy = LOKI_DEFAULT_SMALLOBJ_LIFETIME,
        class MutexPolicy = LOKI_DEFAULT_MUTEX
    >
    class AllocatorSingleton : public SmallObjAllocator
    {
    public:

        /// Defines type of allocator.
        typedef AllocatorSingleton< ThreadingModel, chunkSize,
            maxSmallObjectSize, objectAlignSize, LifetimePolicy > MyAllocator;

        /// Defines type for thread-safety locking mechanism.
        typedef ThreadingModel< MyAllocator, MutexPolicy > MyThreadingModel;

        /// Defines singleton made from allocator.
        typedef Loki::SingletonHolder< MyAllocator, Loki::CreateStatic,
            LifetimePolicy, ThreadingModel > MyAllocatorSingleton;

        /// Returns reference to the singleton.
        inline static AllocatorSingleton & Instance( void )
        {
            return MyAllocatorSingleton::Instance();
        }

        /// The default constructor is not meant to be called directly.
        inline AllocatorSingleton() :
            SmallObjAllocator( chunkSize, maxSmallObjectSize, objectAlignSize )
            {}

        /// The destructor is not meant to be called directly.
        inline ~AllocatorSingleton( void ) {}

        /** Clears any excess memory used by the allocator.  Complexity is
         O(F + C) where F is the count of FixedAllocator's in the pool, and C
         is the number of Chunks in all FixedAllocator's.  This never throws.
         @note This function can be used as a new_handler when Loki and other
         memory allocators can no longer allocate.  Although the C++ Standard
         allows new_handler functions to terminate the program when they can
         not release any memory, this will not do so.
         */
        static void ClearExtraMemory( void );

        /** Returns true if anything in implementation is corrupt.  Complexity
         is O(F + C + B) where F is the count of FixedAllocator's in the pool,
         C is the number of Chunks in all FixedAllocator's, and B is the number
         of blocks in all Chunks.  If it determines any data is corrupted, this
         will return true in release version, but assert in debug version at
         the line where it detects the corrupted data.  If it does not detect
         any corrupted data, it returns false.
         */
        static bool IsCorrupted( void );

    private:
        /// Copy-constructor is not implemented.
        AllocatorSingleton( const AllocatorSingleton & );
        /// Copy-assignment operator is not implemented.
        AllocatorSingleton & operator = ( const AllocatorSingleton & );
    };



    template
    <
        template <class, class> class TM,
	std::size_t chunkSize,
	std::size_t maxSmallObjectSize,
	std::size_t objectAlignSize,
        template <class> class LP,
        class MX
    >
    void AllocatorSingleton< TM, chunkSize, maxSmallObjectSize, objectAlignSize, LP, MX >::ClearExtraMemory( void )
    {
        typename MyThreadingModel::Lock lock;
        (void)lock; // get rid of warning
        Instance().TrimExcessMemory();
    }

    template
    <
        template <class, class> class TM,
	std::size_t chunkSize,
	std::size_t maxSmallObjectSize,
	std::size_t objectAlignSize,
        template <class> class LP,
        class MX
    >
    bool AllocatorSingleton< TM, chunkSize, maxSmallObjectSize, objectAlignSize, LP, MX >::IsCorrupted( void )
    {
        typename MyThreadingModel::Lock lock;
        (void)lock; // get rid of warning
        return Instance().IsCorrupt();
    }

    /** This standalone function provides the longevity level for Small-Object
     Allocators which use the Loki::SingletonWithLongevity policy.  The
     SingletonWithLongevity class can find this function through argument-
     dependent lookup.

     @par Longevity Levels
     No Small-Object Allocator depends on any other Small-Object allocator, so
     this does not need to calculate dependency levels among allocators, and
     it returns just a constant.  All allocators must live longer than the
     objects which use the allocators, it must return a longevity level higher
     than any such object.
     */
    template
    <
        template <class, class> class TM,
	std::size_t chunkSize,
	std::size_t maxSmallObjectSize,
	std::size_t objectAlignSize,
        template <class> class LP,
        class MX
    >
    inline unsigned int GetLongevity(
        AllocatorSingleton< TM, chunkSize, maxSmallObjectSize, objectAlignSize, LP, MX > * )
    {
        // Returns highest possible value.
        return 0xFFFFFFFF;
    }


    /** @class SmallObjectBase
        @ingroup SmallObjectGroup
     Base class for small object allocation classes.
     The shared implementation of the new and delete operators are here instead
     of being duplicated in both SmallObject or SmallValueObject, later just 
     called Small-Objects.  This class is not meant to be used directly by clients, 
     or derived from by clients. Class has no data members so compilers can 
     use Empty-Base-Optimization.

     @par Lifetime Policy
     
     The SmallObjectBase template needs a lifetime policy because it owns
     a singleton of SmallObjAllocator which does all the low level functions. 
     When using a Small-Object in combination with the SingletonHolder template
     you have to choose two lifetimes, that of the Small-Object and that of
     the singleton. The rule is: The Small-Object lifetime must be greater than
     the lifetime of the singleton hosting the Small-Object. Violating this rule
     results in a crash on exit, because the hosting singleton tries to delete
     the Small-Object which is then already destroyed. 
     
     The lifetime policies recommended for use with Small-Objects hosted 
     by a SingletonHolder template are 
         - LongevityLifetime::DieAsSmallObjectParent / LongevityLifetime::DieAsSmallObjectChild
         - SingletonWithLongevity
         - FollowIntoDeath (not supported by MSVC 7.1)
         - NoDestroy
     
     The default lifetime of Small-Objects is 
     LongevityLifetime::DieAsSmallObjectParent to
     insure that memory is not released before a object with the lifetime
     LongevityLifetime::DieAsSmallObjectChild using that
     memory is destroyed. The LongevityLifetime::DieAsSmallObjectParent
     lifetime has the highest possible value of a SetLongevity lifetime, so
     you can use it in combination with your own lifetime not having also
     the highest possible value.
     
     The DefaultLifetime and PhoenixSingleton policies are *not* recommended 
     since they can cause the allocator to be destroyed and release memory 
     for singletons hosting a object which inherit from either SmallObject
     or SmallValueObject.  
     
     @par Lifetime usage
    
        - LongevityLifetime: The Small-Object has 
          LongevityLifetime::DieAsSmallObjectParent policy and the Singleton
          hosting the Small-Object has LongevityLifetime::DieAsSmallObjectChild. 
          The child lifetime has a hard coded SetLongevity lifetime which is 
          shorter than the lifetime of the parent, thus the child dies 
          before the parent.
         
        - Both Small-Object and Singleton use SingletonWithLongevity policy.
          The longevity level for the singleton must be lower than that for the
          Small-Object. This is why the AllocatorSingleton's GetLongevity function 
          returns the highest value.
         
        - FollowIntoDeath lifetime: The Small-Object has 
          FollowIntoDeath::With<LIFETIME>::AsMasterLiftime
          policy and the Singleton has 
          FollowIntoDeath::AfterMaster<MASTERSINGLETON>::IsDestroyed policy,
          where you could choose the LIFETIME. 
        
        - Both Small-Object and Singleton use NoDestroy policy. 
          Since neither is ever destroyed, the destruction order does not matter.
          Note: yow will get memory leaks!
         
        - The Small-Object has NoDestroy policy but the Singleton has
          SingletonWithLongevity policy. Note: yow will get memory leaks!
         
     
     You should *not* use NoDestroy for the singleton, and then use
     SingletonWithLongevity for the Small-Object. 
     
     @par Examples:
     
     - test/SmallObj/SmallSingleton.cpp
     - test/Singleton/Dependencies.cpp
     */
    template
    <
        template <class, class> class ThreadingModel,
        std::size_t chunkSize,
        std::size_t maxSmallObjectSize,
        std::size_t objectAlignSize,
        template <class> class LifetimePolicy,
        class MutexPolicy
    >
    class SmallObjectBase
    {

#if (LOKI_MAX_SMALL_OBJECT_SIZE != 0) && (LOKI_DEFAULT_CHUNK_SIZE != 0) && (LOKI_DEFAULT_OBJECT_ALIGNMENT != 0)

    public:        
        /// Defines type of allocator singleton, must be public 
        /// to handle singleton lifetime dependencies.
        typedef AllocatorSingleton< ThreadingModel, chunkSize,
            maxSmallObjectSize, objectAlignSize, LifetimePolicy > ObjAllocatorSingleton;
    
    private:

        /// Defines type for thread-safety locking mechanism.
        typedef ThreadingModel< ObjAllocatorSingleton, MutexPolicy > MyThreadingModel;

        /// Use singleton defined in AllocatorSingleton.
        typedef typename ObjAllocatorSingleton::MyAllocatorSingleton MyAllocatorSingleton;
        
    public:

        /// Throwing single-object new throws bad_alloc when allocation fails.
#ifdef _MSC_VER
        /// @note MSVC complains about non-empty exception specification lists.
        static void * operator new ( std::size_t size )
#else
        static void * operator new ( std::size_t size ) throw ( std::bad_alloc )
#endif
        {
            typename MyThreadingModel::Lock lock;
            (void)lock; // get rid of warning
            return MyAllocatorSingleton::Instance().Allocate( size, true );
        }

        /// Non-throwing single-object new returns NULL if allocation fails.
        static void * operator new ( std::size_t size, const std::nothrow_t & ) throw ()
        {
            typename MyThreadingModel::Lock lock;
            (void)lock; // get rid of warning
            return MyAllocatorSingleton::Instance().Allocate( size, false );
        }

        /// Placement single-object new merely calls global placement new.
        inline static void * operator new ( std::size_t size, void * place )
        {
            return ::operator new( size, place );
        }

        /// Single-object delete.
        static void operator delete ( void * p, std::size_t size ) throw ()
        {
            typename MyThreadingModel::Lock lock;
            (void)lock; // get rid of warning
            MyAllocatorSingleton::Instance().Deallocate( p, size );
        }

        /** Non-throwing single-object delete is only called when nothrow
         new operator is used, and the constructor throws an exception.
         */
        static void operator delete ( void * p, const std::nothrow_t & ) throw()
        {
            typename MyThreadingModel::Lock lock;
            (void)lock; // get rid of warning
            MyAllocatorSingleton::Instance().Deallocate( p );
        }

        /// Placement single-object delete merely calls global placement delete.
        inline static void operator delete ( void * p, void * place )
        {
            ::operator delete ( p, place );
        }

#ifdef LOKI_SMALL_OBJECT_USE_NEW_ARRAY

        /// Throwing array-object new throws bad_alloc when allocation fails.
#ifdef _MSC_VER
        /// @note MSVC complains about non-empty exception specification lists.
        static void * operator new [] ( std::size_t size )
#else
        static void * operator new [] ( std::size_t size )
            throw ( std::bad_alloc )
#endif
        {
            typename MyThreadingModel::Lock lock;
            (void)lock; // get rid of warning
            return MyAllocatorSingleton::Instance().Allocate( size, true );
        }

        /// Non-throwing array-object new returns NULL if allocation fails.
        static void * operator new [] ( std::size_t size,
            const std::nothrow_t & ) throw ()
        {
            typename MyThreadingModel::Lock lock;
            (void)lock; // get rid of warning
            return MyAllocatorSingleton::Instance().Allocate( size, false );
        }

        /// Placement array-object new merely calls global placement new.
        inline static void * operator new [] ( std::size_t size, void * place )
        {
            return ::operator new( size, place );
        }

        /// Array-object delete.
        static void operator delete [] ( void * p, std::size_t size ) throw ()
        {
            typename MyThreadingModel::Lock lock;
            (void)lock; // get rid of warning
            MyAllocatorSingleton::Instance().Deallocate( p, size );
        }

        /** Non-throwing array-object delete is only called when nothrow
         new operator is used, and the constructor throws an exception.
         */
        static void operator delete [] ( void * p,
            const std::nothrow_t & ) throw()
        {
            typename MyThreadingModel::Lock lock;
            (void)lock; // get rid of warning
            MyAllocatorSingleton::Instance().Deallocate( p );
        }

        /// Placement array-object delete merely calls global placement delete.
        inline static void operator delete [] ( void * p, void * place )
        {
            ::operator delete ( p, place );
        }
#endif  // #if use new array functions.

#endif  // #if default template parameters are not zero

    protected:
        inline SmallObjectBase( void ) {}
        inline SmallObjectBase( const SmallObjectBase & ) {}
        inline SmallObjectBase & operator = ( const SmallObjectBase & ) {}
        inline ~SmallObjectBase() {}
    }; // end class SmallObjectBase


    /** @class SmallObject
        @ingroup SmallObjectGroup
     SmallObject Base class for polymorphic small objects, offers fast
     allocations & deallocations.  Destructor is virtual and public.  Default
     constructor is trivial.   Copy-constructor and copy-assignment operator are
     not implemented since polymorphic classes almost always disable those
     operations.  Class has no data members so compilers can use
     Empty-Base-Optimization.
     */
    template
    <
        template <class, class> class ThreadingModel = LOKI_DEFAULT_THREADING_NO_OBJ_LEVEL,
        std::size_t chunkSize = LOKI_DEFAULT_CHUNK_SIZE,
        std::size_t maxSmallObjectSize = LOKI_MAX_SMALL_OBJECT_SIZE,
        std::size_t objectAlignSize = LOKI_DEFAULT_OBJECT_ALIGNMENT,
        template <class> class LifetimePolicy = LOKI_DEFAULT_SMALLOBJ_LIFETIME,
        class MutexPolicy = LOKI_DEFAULT_MUTEX
    >
    class SmallObject : public SmallObjectBase< ThreadingModel, chunkSize,
            maxSmallObjectSize, objectAlignSize, LifetimePolicy, MutexPolicy >
    {

    public:
        virtual ~SmallObject() {}
    protected:
        inline SmallObject( void ) {}

    private:
        /// Copy-constructor is not implemented.
        SmallObject( const SmallObject & );
        /// Copy-assignment operator is not implemented.
        SmallObject & operator = ( const SmallObject & );
    }; // end class SmallObject


    /** @class SmallValueObject
        @ingroup SmallObjectGroup
     SmallValueObject Base class for small objects with value-type
     semantics - offers fast allocations & deallocations.  Destructor is
     non-virtual, inline, and protected to prevent unintentional destruction
     through base class.  Default constructor is trivial.   Copy-constructor
     and copy-assignment operator are trivial since value-types almost always
     need those operations.  Class has no data members so compilers can use
     Empty-Base-Optimization.
     */
    template
    <
        template <class, class> class ThreadingModel = LOKI_DEFAULT_THREADING_NO_OBJ_LEVEL,
        std::size_t chunkSize = LOKI_DEFAULT_CHUNK_SIZE,
        std::size_t maxSmallObjectSize = LOKI_MAX_SMALL_OBJECT_SIZE,
        std::size_t objectAlignSize = LOKI_DEFAULT_OBJECT_ALIGNMENT,
        template <class> class LifetimePolicy = LOKI_DEFAULT_SMALLOBJ_LIFETIME,
        class MutexPolicy = LOKI_DEFAULT_MUTEX
    >
    class SmallValueObject : public SmallObjectBase< ThreadingModel, chunkSize,
            maxSmallObjectSize, objectAlignSize, LifetimePolicy, MutexPolicy >
    {
    protected:
        inline SmallValueObject( void ) {}
        inline SmallValueObject( const SmallValueObject & ) {}
        inline SmallValueObject & operator = ( const SmallValueObject & ) {}
        inline ~SmallValueObject() {}
    }; // end class SmallValueObject

} // namespace Loki

////////////////////////////////////////////////////////////////////////////////
// Change log:
// June 20, 2001: ported by Nick Thurn to gcc 2.95.3. Kudos, Nick!!!
// Nov. 26, 2004: re-implemented by Rich Sposato.
//
// $Log: lokiSmallObj.h,v $
// Revision 1.3  2007/02/07 12:25:11  bjeram
// reanmed dangurous template prameter list CS ,...  with chunkSize, ... there was problem due to defining CS somwhere in RTAI code
//
// Revision 1.2  2007/02/01 17:29:00  sharring
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
// Revision 1.30  2006/06/19 12:39:08  syntheticpp
// replace tabs with 4 spaces
//
// Revision 1.29  2006/03/08 17:07:11  syntheticpp
// replace tabs with 4 spaces in all files
//
// Revision 1.28  2006/02/27 19:59:20  syntheticpp
// add support of loki.dll
//
// Revision 1.27  2006/02/20 21:56:06  rich_sposato
// Fixed typo.
//
// Revision 1.26  2006/01/22 13:37:33  syntheticpp
// use macro LOKI_DEFAULT_MUTEX for Mutex default value, defined in Threads.h
//
// Revision 1.25  2006/01/22 13:31:45  syntheticpp
// add additional template parameter for the changed threading classes
//
// Revision 1.24  2005/12/08 22:09:08  rich_sposato
// Added functions to check for memory corruption.  Also made some minor
// coding changes.
//
// Revision 1.23  2005/11/13 16:51:22  syntheticpp
// update documentation due to the new lifetime policies
//
// Revision 1.22  2005/11/07 12:06:43  syntheticpp
// change lifetime policy DieOrder to a msvc7.1 compilable version. Make this the default lifetime for SmallObject
//
// Revision 1.21  2005/11/05 17:43:55  syntheticpp
// disable FollowIntoDeath/DieOrder lifetime policies when using the msvc 7.1 compiler, bug article: 839821 'Microsoft has confirmed that this is a problem..'
//
// Revision 1.20  2005/11/02 20:01:10  syntheticpp
// more doxygen documentation, modules added
//
// Revision 1.19  2005/11/01 11:11:52  syntheticpp
// add lifetime policies to manage singleton lifetime dependencies: FollowIntoDeath and DieOrder. Change SmallObject.h to avoid memory leaks by default
//
// Revision 1.18  2005/10/30 14:03:23  syntheticpp
// replace tabs space
//
// Revision 1.17  2005/10/29 08:10:13  syntheticpp
// #undef LOKI_SMALL_OBJECT_USE_NEW_ARRAY when using a Microsoft compiler
//
// Revision 1.16  2005/10/26 00:50:44  rich_sposato
// Minor changes to documentation comments.
//
// Revision 1.15  2005/10/15 19:41:23  syntheticpp
// fix bug 1327060. Add missing template parameter to make different static variables possible
//
// Revision 1.14  2005/10/13 22:43:03  rich_sposato
// Added documentation comments about lifetime policies.
//
// Revision 1.13  2005/10/07 01:22:09  rich_sposato
// Added GetLongevity function so allocator can work with a certain lifetime
// policy class used with Loki::SingletonHolder.
//
// Revision 1.12  2005/10/06 00:19:56  rich_sposato
// Added clarifying comment about destructor.
//
// Revision 1.11  2005/09/27 00:41:13  rich_sposato
// Added array forms of new and delete.
//
// Revision 1.10  2005/09/26 21:38:54  rich_sposato
// Changed include path to be direct instead of relying upon project settings.
//
// Revision 1.9  2005/09/26 07:33:04  syntheticpp
// move macros into LOKI_ namespace
//
// Revision 1.8  2005/09/09 00:24:59  rich_sposato
// Added functions to trim extra memory within allocator.  Made a new_handler
// function for allocator.  Added deallocator function for nothrow delete
// operator to insure nothing is leaked when constructor throws.
//
// Revision 1.7  2005/09/01 22:01:33  rich_sposato
// Added #ifdef to deal with MSVC warning about exception specification lists.
//
// Revision 1.6  2005/08/27 13:22:56  syntheticpp
// samll fix
//
// Revision 1.5  2005/08/25 15:49:51  syntheticpp
// small corrections
//
// Revision 1.4  2005/08/25 15:23:14  syntheticpp
// small corrections
//
// Revision 1.3  2005/07/31 14:00:48  syntheticpp
// make object level threading possible
//
// Revision 1.2  2005/07/31 13:51:31  syntheticpp
// replace old implementation with the ingeious from Rich Sposato
//
// Revision 1.2  2005/07/22 00:22:38  rich_sposato
// Added SmallValueObject, SmallObjectBase, and AllocatorSingleton classes.
//
////////////////////////////////////////////////////////////////////////////////

#endif // SMALLOBJ_INC_






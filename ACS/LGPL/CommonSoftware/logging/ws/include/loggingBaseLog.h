#ifndef logging_base_log_H
#define logging_base_log_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) Associated Universities Inc., 2005 
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
* "@(#) $Id: loggingBaseLog.h,v 1.16 2012/02/29 12:50:09 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-03-09  created
*/

/** @file loggingBaseLog.h
 *  Header file for base class used by ALMA logging.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <string>
#include <lokiSmartPtr.h>
#include "loggingBaseExport.h"

namespace Logging 
{
    //------------------------------------------------------------------------------
    /**
     * Pure abstract logging interface. Includes the bare
     * minimum logging concepts used by ALMA.
     */
    class loggingBase_EXPORT BaseLog
    {
      public:

	virtual ~BaseLog(){}

	/**
	 * This enumeration is intended to be the various logging levels for
	 * ALMA software starting with the lowest priority up to the highest. It is
	 * based on ACE logging levels.
	 */
	enum Priority
	{
	    LM_SHUTDOWN = 01,
	    /// Messages indicating function-calling sequence
	    LM_TRACE = 02,

        LM_DELOUSE = 03,
	    
	    /// Messages that contain information normally of use only when
	    /// debugging a program
	    LM_DEBUG = 04,
	    
	    /// Informational messages 
	    LM_INFO = 010,
	    
	    /// Conditions that are not error conditions, but that may require
	    /// special handling
	    LM_NOTICE = 020,
	    
	    /// Warning messages
	    LM_WARNING = 040,
	    
	    /// Error messages
	    LM_ERROR = 0200,
	    
	    /// Critical conditions, such as hard device errors
	    LM_CRITICAL = 0400,
	    
	    /// A condition that should be corrected immediately, such as a
	    /// corrupted system database
	    LM_ALERT = 01000,
	    
	    /// A panic condition.  This is normally broadcast to all users
	    LM_EMERGENCY = 02000
	};


	/**
	 * Defines the information contained within a log.
	 * @param priority Priority of the log
	 * @param message Log message.
	 * @param file Name of the file from which the log came from.
	 * @param line Line number from where the log was published.
	 * @param method Name of the method from where the log was published.
	 * @param timeStamp Time that the log was created. 
	 * @TODO: timeStamp should be of type ACS::Time but we supposedly
	 *        cannot introduce CORBA into this class because it's 
	 *        being used by OFFLINE tasks. For now, we just use unsigned
	 *        long long which is compatible with ACS::Time.
	 */
	struct LogRecord {
	    Priority priority;
	    std::string message;
	    std::string file;
	    unsigned long line;
	    std::string method;
	    unsigned long long timeStamp;
	};
	
	//----------------------------------------------------
	/**
	 * This method just delegates to another signature of log 
	 * adding the timestamp in the process.
	 * @param priority Priority of the log
	 * @param message Log message.
	 * @param file Name of the file from which the log came from.
	 * @param line Line number from where the log was published.
	 * @param method Name of the method from where the log was published.
	 * @return void
	 */
	virtual void
	log(Priority priority,
	    const std::string &message,
	    const std::string &file,
	    unsigned long line,
	    const std::string &method);
	    
	/**
	 * It it completely up to the subclass developer to decide exactly
	 * what this method does (e.g., writes the log to file, sends it 
	 * over the network, etc.)
	 * @param lr log record
	 * @return void
	 */
	virtual void
	log(const LogRecord &lr) = 0;

	//----------------------------------------------------
	/**
	 * Retrieves the name of this instance. Exactly what that name
	 * is depends on the subclass.
	 * @return The name of this instance
	 */
	virtual std::string
	getName() const = 0;

	/**
	 * This constant member is the value of a string param of log that cannot
	 * be determined for some reason or another.
	 */
	static const std::string FIELD_UNAVAILABLE;

	/**
	 * This constant member is the value of the global logger's name.
	 */
	static const std::string GLOBAL_LOGGER_NAME;

	/**
	 * This constant member is the value of the anonymous logger's name.
	 */
	static const std::string ANONYMOUS_LOGGER_NAME;

	/**
	 * This constant member is the name of loggers being used from a static
	 * context.
	 */
	static const std::string STATIC_LOGGER_NAME;
    };
    //------------------------------------------------------------------------------
    /**
     * Because of a bug within the Loki smart pointer library which semi-randomly causes
     * assert failures when exiting executables (i.e., at container shutdown), it it necessary to 
     * provide our own implementation of RefCounted which does not use the the Loki SmallObj
     * class. RefCounted is the implementation of a classic external reference counting implementation
     * of the OwnerShipPolicy used by loki::SmartPtr. It's based on the class of the same name defined
     * in $ACSROOT/include/lokiSmartPtr.h
     */

    template <class P>
    class RefCounted
    {
      public:
        RefCounted() 
	    {
		pCount_ = new unsigned int();
		assert(pCount_);
		*pCount_ = 1;
	    }
        
        RefCounted(const RefCounted& rhs) 
	    : pCount_(rhs.pCount_)
	    {}
        
        // MWCW lacks template friends, hence the following kludge
        template <typename P1>
        RefCounted(const RefCounted<P1>& rhs) 
	    : pCount_(reinterpret_cast<const RefCounted&>(rhs).pCount_)
	    {}
        
        P Clone(const P& val)
	    {
		++*pCount_;
		return val;
	    }
        
        bool Release(const P&)
	    {
		if (!--*pCount_)
		    {
		    delete pCount_;
		    return true;
		    }
		return false;
	    }
        
        void Swap(RefCounted& rhs)
	    { std::swap(pCount_, rhs.pCount_); }
	
        enum { destructiveCopy = false };
	
      private:
        // Data
        unsigned int* pCount_;
    };  

#ifdef MAKE_VXWORKS
// we have to define (copy from loki) our own RefCountedMT for VxWorks since the one for loki is too compley for VxWorks compiler
	template <class P>
        class RefCountedMT : public Loki::ObjectLevelLockable<RefCountedMT<P>, LOKI_DEFAULT_MUTEX >
        {
            typedef Loki::ObjectLevelLockable<RefCountedMT<P>, LOKI_DEFAULT_MUTEX > base_type;
            typedef typename base_type::IntType       CountType;
            typedef volatile CountType               *CountPtrType;

        public:
            RefCountedMT()
            {
                pCount_ = reinterpret_cast<CountPtrType>(
                    Loki::SmallObject<LOKI_DEFAULT_THREADING_NO_OBJ_LEVEL>::operator new(
                        sizeof(*pCount_)));
                assert(pCount_);
                // *pCount_ = 1;
                Loki::ObjectLevelLockable<RefCountedMT, LOKI_DEFAULT_MUTEX>::AtomicAssign(*pCount_, 1);
            }

            RefCountedMT(const RefCountedMT& rhs) : 
		Loki::ObjectLevelLockable<RefCountedMT<P>, LOKI_DEFAULT_MUTEX>( rhs ),
		pCount_(rhs.pCount_)
            {}

            //MWCW lacks template friends, hence the following kludge
            template <typename P1>
            RefCountedMT(const RefCountedMT<P1>& rhs) 
            : pCount_(reinterpret_cast<const RefCountedMT<P>&>(rhs).pCount_)
            {}

            P Clone(const P& val)
            {
		Loki::ObjectLevelLockable<RefCountedMT, LOKI_DEFAULT_MUTEX>::AtomicIncrement(*pCount_);
                return val;
            }

            bool Release(const P&)
            {
                if (! Loki::ObjectLevelLockable<RefCountedMT, LOKI_DEFAULT_MUTEX>::AtomicDecrement(*pCount_))
                {
                    Loki::SmallObject<LOKI_DEFAULT_THREADING_NO_OBJ_LEVEL>::operator delete(
                        const_cast<CountType *>(pCount_), 
                        sizeof(*pCount_));
                    return true;
                }
                return false;
            }

            void Swap(RefCountedMT& rhs)
            { std::swap(pCount_, rhs.pCount_); }

            enum { destructiveCopy = false };

        private:
            // Data
            CountPtrType pCount_;
        };
#endif //MAKE_VXWORKS
};

#endif /*!_H*/

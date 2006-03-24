#ifndef _ACS_THREAD_MANAGER_H
#define _ACS_THREAD_MANAGER_H
/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: acsThreadManager.h,v 1.15 2006/03/24 12:12:49 vwang Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram  yyyy-mm-dd  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "acsThread.h"

namespace ACS
{
 
    /**
     * @class ThreadManager
     * This clas is an extension of ACS::ThreadManagerBase which is used with ACS::Thread
     */
    class ThreadManager : public ACS::ThreadManagerBase
    {
	friend class Thread;

      public:

	/**
	 * Thread Manager Constructor
	 */
	ThreadManager() : ACS::ThreadManagerBase() { ACS_CHECK_LOGGER; logger_m = ::getLogger(); }

	/**
	 * Thread Manager Constructor, that takes logger as parameter.
	 * This allows to provide an external, pre-configured, logger
	 * in the ThreadManager.
	 */
	ThreadManager(Logging::Logger::LoggerSmartPtr logger) : 
	    ACS::ThreadManagerBase(), 
	    logger_m(logger) {}

	/**
	 * create methods which create a user defined  thread object 
	 * of type T (= derived from ACS::Thread)
	 * without a parameter
	 * and adds it to the list of Threads.
	 */
	template <class T>
	T* create(const ACE_CString name);

       /* remove suspended parameter, same as create(name)
	template <class T>
	T* create(const ACE_CString name,
		  const bool suspended);
        */

	template <class T>
	T* create(const ACE_CString name,
//		  const bool suspended,
		  const TimeInterval responseTime,
		  const TimeInterval sleepTime);

	template <class T>
	T* create(const ACE_CString name,
//		  const bool suspended,
		  const TimeInterval responseTime,
		  const TimeInterval sleepTime,
		  bool del);

	template <class T>
	T* create(const ACE_CString name,
//		  const bool suspended,
		  const TimeInterval responseTime,
		  const TimeInterval sleepTime,
		  bool del,
		  const long thrFlags);


	/**
	 * create methods which create a user defined 
	 * thread object of type T (= derived from ACS::Thread)
	 * with a parameter of type P
	 * and add it to the list of Threads.
	 */
	template <class T, class P>
	T* create(const ACE_CString name, P&);

       /* to remove parameter suspended, same as create( name, P& )
	template <class T, class P>
	T* create(const ACE_CString name, P&,
		  const bool suspended);
        */

	template <class T, class P>
	T* create(const ACE_CString name, P&,
//		  const bool suspended,
		  const TimeInterval responseTime,
		  const TimeInterval sleepTime);
	
	template <class T, class P>
	T* create(const ACE_CString name, P&,
//		  const bool suspended,
		  const TimeInterval responseTime,
		  const TimeInterval sleepTime,
		  bool del);

	template <class T, class P>
	T* create(const ACE_CString name, P&,
//		  const bool suspended,
		  const TimeInterval responseTime,
		  const TimeInterval sleepTime,
		  bool del,
		  const long thrFlags);

	/**
	 * destoy method destroys a thread object
	 * @param the pointer to the thread object
	 */
	void destroy(ACS::Thread* thr)
	    {
		if (thr!=NULL)
		    {
		    removeFromMap (thr->getName());
		    delete thr;
		    thr = 0;
		    }//if
	    }//destroy

	Logging::Logger::LoggerSmartPtr getLogger() 
	    { 
		if (logger_m!=0)
		    {
		    return logger_m; 
		    }
		else
		    {
		    ACS_CHECK_LOGGER;
		    return ::getLogger();
		    }
	    }

      private:
	
	/**
	 * ALMA C++ coding standards state assignment operators should be disabled.
	 */
	void operator=(const ThreadManager&);

	/**
	 * ALMA C++ coding standards state copy constructors should be disabled.
	 */
	ThreadManager(const ThreadManager&);

	Logging::Logger::LoggerSmartPtr logger_m;

        /**
	 * @class ThreadManagerTSS
	 * This is an internal class of ThreadManager 
	 * which is used for sending the thread manager 
	 * pointer to a thread object. 
	 * It is the Thread Safe Storage for the thread managers.
	 */	
	class ThreadManagerTSS
	{
	  public:
	    ThreadManagerTSS() : m_tm(0){}

	    /**
	     * method to set thread manager for certain thread
	     */
	    void setThreadManager(ThreadManager* _tm){ m_tm = _tm; }
	    
            /**
	     * return thread manager used in current thread (TSS!)
             * @param _reset should be thread manager pointer set to 0 after
             */
	    ThreadManager* getThreadManager(bool _reset)
		{ 
		    ThreadManager* tmp_tm = m_tm;
		    if (_reset) 
			m_tm = 0;
		    return tmp_tm; 
		}
	    
            /**
	     * resets thread manager pointer to 0
	     */
	    void resetThreadManager() {m_tm = 0;}

	  protected:
	    ThreadManager* m_tm;	
	};

	/**
	 * Thread Manager TSS used for sending thread manager pointer to thread objects
	 */
	static ACE_TSS<ThreadManagerTSS> threadManagerTSS;

    };//class ThreadManager

/* implementation for template functions*/
#include "acsThreadManager.i"

};//namespace ACS

#endif 

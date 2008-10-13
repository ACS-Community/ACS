#ifndef _ACS_THREAD_MANAGER_I
#define _ACS_THREAD_MANAGER_I

template<class T>
T* ThreadManager::create(const ACE_CString name)
{
//ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
    if (getThreadByName(name)!=NULL)
	{
	acsthreadErrType::ThreadAlreadyExistExImpl ex(__FILE__, __LINE__, "ACS::ThreadManager<>::create");
	ex.setThreadName(name);
	throw ex;
	}

    try
	{
	threadManagerTSS->setThreadManager(this);
	T* thread_p = new T(name);
	/*
	 * Here I call resume with complete ACS::Thread::resume
	 * scope to avoid ambiguities when the thread class
	 * inherits also from other classes with a resume method.
	 * This happens in CONTROL/Device/MountController,
	 * whose Threads are also ACE_Tasks
	 */
//	thread_p->ACS::Thread::resume(); // the thread can be started !!

	return thread_p;
	}
    catch(ACSErr::ACSbaseExImpl& ex)
	{
	threadManagerTSS->resetThreadManager();
	throw acsthreadErrType::CanNotCreateThreadExImpl(ex, __FILE__, __LINE__, "ACS::ThreadManager<>::create");
	}
    catch (...)
	{
	threadManagerTSS->resetThreadManager();
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
							"ACS::ThreadManager<>::create");
	throw acsthreadErrType::CanNotCreateThreadExImpl(uex, __FILE__, __LINE__,
							 "ACS::ThreadManager<>::create");
	}//try-catch
}//create


template<class T>
T* ThreadManager::create(const ACE_CString name,
			 const TimeInterval responseTime,
			 const TimeInterval sleepTime)
{
//ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
    if (getThreadByName(name)!=NULL)
	{
	acsthreadErrType::ThreadAlreadyExistExImpl ex(__FILE__, __LINE__,
						      "ACS::ThreadManager<>::create");
	ex.setThreadName(name);
	throw ex;
	}
    try
	{
	threadManagerTSS->setThreadManager(this);
	T* thread_p = new T(name, responseTime, sleepTime);
//	if(!suspended) thread_p->ACS::Thread::resume(); // the thread can be started !!

	return thread_p;
	}
    catch(ACSErr::ACSbaseExImpl& ex)
	{
	threadManagerTSS->resetThreadManager();
	throw acsthreadErrType::CanNotCreateThreadExImpl(ex, __FILE__, __LINE__,
							 "ACS::ThreadManager<>::create");
	}
    catch (...)
	{
	threadManagerTSS->resetThreadManager();
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
							"ACS::ThreadManager<>::create");
	throw acsthreadErrType::CanNotCreateThreadExImpl(uex, __FILE__, __LINE__,
							 "ACS::ThreadManager<>::create");
	}//try-catch
}//create

template<class T>
T* ThreadManager::create(const ACE_CString name,
			 const TimeInterval responseTime,
			 const TimeInterval sleepTime,
			 bool del)
{
//ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
    if (getThreadByName(name)!=NULL)
	{
	acsthreadErrType::ThreadAlreadyExistExImpl ex(__FILE__, __LINE__,
						      "ACS::ThreadManager<>::create");
	ex.setThreadName(name);
	throw ex;
	}
    try
	{
	threadManagerTSS->setThreadManager(this);
	T* thread_p = new T(name, responseTime, sleepTime, del);
//	if(!suspended) thread_p->ACS::Thread::resume(); // the thread can be started !!

	return thread_p;
	}
    catch(ACSErr::ACSbaseExImpl& ex)
	{
	threadManagerTSS->resetThreadManager();
	throw acsthreadErrType::CanNotCreateThreadExImpl(ex, __FILE__, __LINE__,
							 "ACS::ThreadManager<>::create");
	}
    catch (...)
	{
	threadManagerTSS->resetThreadManager();
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
							"ACS::ThreadManager<>::create");
	throw acsthreadErrType::CanNotCreateThreadExImpl(uex, __FILE__, __LINE__,
							 "ACS::ThreadManager<>::create");
	}//try-catch
}//create

template<class T>
T* ThreadManager::create(const ACE_CString name,
			 const TimeInterval responseTime,
			 const TimeInterval sleepTime,
			 bool del,
             const long thrFlags)
{
//ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
    if (getThreadByName(name)!=NULL)
	{
	acsthreadErrType::ThreadAlreadyExistExImpl ex(__FILE__, __LINE__,
						      "ACS::ThreadManager<>::create");
	ex.setThreadName(name);
	throw ex;
	}
    try
	{
	threadManagerTSS->setThreadManager(this);
	T* thread_p = new T(name, responseTime, sleepTime, del, thrFlags);
//	if(!suspended) thread_p->ACS::Thread::resume(); // the thread can be started !!

	return thread_p;
	}
    catch(ACSErr::ACSbaseExImpl& ex)
	{
	threadManagerTSS->resetThreadManager();
	throw acsthreadErrType::CanNotCreateThreadExImpl(ex, __FILE__, __LINE__,
							 "ACS::ThreadManager<>::create");
	}
    catch (...)
	{
	threadManagerTSS->resetThreadManager();
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
							"ACS::ThreadManager<>::create");
	throw acsthreadErrType::CanNotCreateThreadExImpl(uex, __FILE__, __LINE__,
							 "ACS::ThreadManager<>::create");
	}//try-catch
}//create

template<class T>
T* ThreadManager::create(const ACE_CString name,
			 const TimeInterval responseTime,
			 const TimeInterval sleepTime,
			 bool del,
             const long thrFlags,
             const size_t stackSize)
{
//ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
    if (getThreadByName(name)!=NULL)
	{
	acsthreadErrType::ThreadAlreadyExistExImpl ex(__FILE__, __LINE__,
						      "ACS::ThreadManager<>::create");
	ex.setThreadName(name);
	throw ex;
	}
    try
	{
	threadManagerTSS->setThreadManager(this);
	T* thread_p = new T(name, responseTime, sleepTime, del, thrFlags, stackSize);
//	if(!suspended) thread_p->ACS::Thread::resume(); // the thread can be started !!

	return thread_p;
	}
    catch(ACSErr::ACSbaseExImpl& ex)
	{
	threadManagerTSS->resetThreadManager();
	throw acsthreadErrType::CanNotCreateThreadExImpl(ex, __FILE__, __LINE__,
							 "ACS::ThreadManager<>::create");
	}
    catch (...)
	{
	threadManagerTSS->resetThreadManager();
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
							"ACS::ThreadManager<>::create");
	throw acsthreadErrType::CanNotCreateThreadExImpl(uex, __FILE__, __LINE__,
							 "ACS::ThreadManager<>::create");
	}//try-catch
}//create

/***************************************
 * implementations with parameter      *
 ***************************************/

template<class T, class P>
T* ThreadManager::create(const ACE_CString name, P& param)
{
//ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
    if (getThreadByName(name)!=NULL)
	{
	acsthreadErrType::ThreadAlreadyExistExImpl ex(__FILE__, __LINE__,
						      "ACS::ThreadManager<>::create");
	ex.setThreadName(name);
	throw ex;
    	}

    try
	{
	threadManagerTSS->setThreadManager(this);
	T* thread_p = new T(name,param);
//	thread_p->ACS::Thread::resume(); // the thread can be started !!

	return thread_p;
	}
    catch(ACSErr::ACSbaseExImpl& ex)
	{
	threadManagerTSS->resetThreadManager();
	throw acsthreadErrType::CanNotCreateThreadExImpl(ex, __FILE__, __LINE__,
							 "ACS::ThreadManager<>::create");
	}
    catch (...)
	{
	threadManagerTSS->resetThreadManager();
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
							"ACS::ThreadManager<>::create");
	throw acsthreadErrType::CanNotCreateThreadExImpl(uex, __FILE__, __LINE__,
							 "ACS::ThreadManager<>::create");
	}//try-catch
}//create

template<class T, class P>
T* ThreadManager::create(const ACE_CString name, P& param,
			 const TimeInterval responseTime,
			 const TimeInterval sleepTime)
{
//ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
    if (getThreadByName(name)!=NULL)
	{
	acsthreadErrType::ThreadAlreadyExistExImpl ex(__FILE__, __LINE__,
						      "ACS::ThreadManager<>::create");
	ex.setThreadName(name);
	throw ex;
	}

    try
	{
	threadManagerTSS->setThreadManager(this);
	T* thread_p = new T(name, param, responseTime, sleepTime);
//	if(!suspended) thread_p->ACS::Thread::resume(); // the thread can be started !!

	return thread_p;
	}
    catch(ACSErr::ACSbaseExImpl& ex)
	{
	threadManagerTSS->resetThreadManager();
	throw acsthreadErrType::CanNotCreateThreadExImpl(ex, __FILE__, __LINE__,
							 "ACS::ThreadManager<>::create");
	}
    catch (...)
	{
	threadManagerTSS->resetThreadManager();
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
							"ACS::ThreadManager<>::create");
	throw acsthreadErrType::CanNotCreateThreadExImpl(uex, __FILE__, __LINE__,
							 "ACS::ThreadManager<>::create");
	}//try-catch
}//create

template<class T, class P>
T* ThreadManager::create(const ACE_CString name, P& param,
			 const TimeInterval responseTime,
			 const TimeInterval sleepTime,
			 bool del)
{
//ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
    if (getThreadByName(name)!=NULL)
	{
	acsthreadErrType::ThreadAlreadyExistExImpl ex(__FILE__, __LINE__,
						      "ACS::ThreadManager<>::create");
	ex.setThreadName(name);
	throw ex;
	}

    try
	{
	threadManagerTSS->setThreadManager(this);
	T* thread_p = new T(name, param, responseTime, sleepTime, del);
//	if(!suspended) thread_p->ACS::Thread::resume(); // the thread can be started !!

	return thread_p;
	}
    catch(ACSErr::ACSbaseExImpl& ex)
	{
	threadManagerTSS->resetThreadManager();
	throw acsthreadErrType::CanNotCreateThreadExImpl(ex, __FILE__, __LINE__,
							 "ACS::ThreadManager<>::create");
	}
    catch (...)
	{
	threadManagerTSS->resetThreadManager();
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
							"ACS::ThreadManager<>::create");
	throw acsthreadErrType::CanNotCreateThreadExImpl(uex, __FILE__, __LINE__,
							 "ACS::ThreadManager<>::create");
	}//try-catch
}//create

template<class T, class P>
T* ThreadManager::create(const ACE_CString name, P& param,
			 const TimeInterval responseTime,
			 const TimeInterval sleepTime,
			 bool del,
			 const long thrFlags)
{
//ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
    if (getThreadByName(name)!=NULL)
	{
	acsthreadErrType::ThreadAlreadyExistExImpl ex(__FILE__, __LINE__,
						      "ACS::ThreadManager<>::create");
	ex.setThreadName(name);
	throw ex;
	}

    try
	{
	threadManagerTSS->setThreadManager(this);
	T* thread_p = new T(name, param, responseTime, sleepTime, del, thrFlags);
//	if(!suspended) thread_p->ACS::Thread::resume(); // the thread can be started !!

	return thread_p;
	}
    catch(ACSErr::ACSbaseExImpl& ex)
	{
	threadManagerTSS->resetThreadManager();
	throw acsthreadErrType::CanNotCreateThreadExImpl(ex, __FILE__, __LINE__,
							 "ACS::ThreadManager<>::create");
	}
       catch (...)
	{
	threadManagerTSS->resetThreadManager();
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
							"ACS::ThreadManager<>::create");
	throw acsthreadErrType::CanNotCreateThreadExImpl(uex, __FILE__, __LINE__,
							 "ACS::ThreadManager<>::create");
	}//try-catch
}//create

template<class T, class P>
T* ThreadManager::create(const ACE_CString name, P& param,
			 const TimeInterval responseTime,
			 const TimeInterval sleepTime,
			 bool del,
			 const long thrFlags,
			 const size_t stackSize)
{
//ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);
    if (getThreadByName(name)!=NULL)
	{
	acsthreadErrType::ThreadAlreadyExistExImpl ex(__FILE__, __LINE__,
						      "ACS::ThreadManager<>::create");
	ex.setThreadName(name);
	throw ex;
	}

    try
	{
	threadManagerTSS->setThreadManager(this);
	T* thread_p = new T(name, param, responseTime, sleepTime, del, thrFlags, stackSize);

	return thread_p;
	}
    catch(ACSErr::ACSbaseExImpl& ex)
	{
	threadManagerTSS->resetThreadManager();
	throw acsthreadErrType::CanNotCreateThreadExImpl(ex, __FILE__, __LINE__,
							 "ACS::ThreadManager<>::create");
	}
       catch (...)
	{
	threadManagerTSS->resetThreadManager();
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
							"ACS::ThreadManager<>::create");
	throw acsthreadErrType::CanNotCreateThreadExImpl(uex, __FILE__, __LINE__,
							 "ACS::ThreadManager<>::create");
	}//try-catch
}//create

#endif

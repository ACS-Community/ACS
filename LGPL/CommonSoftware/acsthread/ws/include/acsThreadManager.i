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
	T* thread_p = new T(name, true);
	/*
	 * Here I call resume with complete ACS::Thread::resume
	 * scope to avoid ambiguities when the thread class
	 * inherits also from other classes with a resume method.
	 * This happens in CONTROL/Device/MountController,
	 * whose Threads are also ACE_Tasks
	 */ 
	thread_p->ACS::Thread::resume(); // the thread can be started !!

	return thread_p;
	} 
    catch(ACSErr::ACSbaseExImpl& ex)
	{
	threadManagerTSS->resetThreadManager();
	throw acsthreadErrType::CanNotCreateThreadExImpl(ex, __FILE__, __LINE__, "ACS::ThreadManager<>::create");
	}
}//create

template<class T>
T* ThreadManager::create(const ACE_CString name,
			 const bool suspended)
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
	T* thread_p = new T(name, true);
	if(!suspended) thread_p->ACS::Thread::resume(); // the thread can be started !!

	return thread_p;
	} 
    catch(ACSErr::ACSbaseExImpl& ex)
	{
	threadManagerTSS->resetThreadManager();
	throw acsthreadErrType::CanNotCreateThreadExImpl(ex, __FILE__, __LINE__, 
							 "ACS::ThreadManager<>::create");
	}
}//create

template<class T>
T* ThreadManager::create(const ACE_CString name,
			 const bool suspended,
			 const TimeInterval& responseTime,
			 const TimeInterval& sleepTime)
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
	T* thread_p = new T(name, true, responseTime, sleepTime);
	if(!suspended) thread_p->ACS::Thread::resume(); // the thread can be started !!

	return thread_p;
	} 
    catch(ACSErr::ACSbaseExImpl& ex)
	{
	threadManagerTSS->resetThreadManager();
	throw acsthreadErrType::CanNotCreateThreadExImpl(ex, __FILE__, __LINE__, 
							 "ACS::ThreadManager<>::create");
	}
}//create


template<class T>
T* ThreadManager::create(const ACE_CString name,
			 const bool suspended,
			 const TimeInterval& responseTime,
			 const TimeInterval& sleepTime,
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
	T* thread_p = new T(name, true, responseTime, sleepTime, del);
	if(!suspended) thread_p->ACS::Thread::resume(); // the thread can be started !!

	return thread_p;
	} 
    catch(ACSErr::ACSbaseExImpl& ex)
	{
	threadManagerTSS->resetThreadManager();
	throw acsthreadErrType::CanNotCreateThreadExImpl(ex, __FILE__, __LINE__, 
							 "ACS::ThreadManager<>::create");
	}
}//create

template<class T>
T* ThreadManager::create(const ACE_CString name,
			 const bool suspended,
			 const TimeInterval& responseTime,
			 const TimeInterval& sleepTime,
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
	T* thread_p = new T(name, true, responseTime, sleepTime, del, thrFlags);
	if(!suspended) thread_p->ACS::Thread::resume(); // the thread can be started !!

	return thread_p;
	} 
    catch(ACSErr::ACSbaseExImpl& ex)
	{
	threadManagerTSS->resetThreadManager();
	throw acsthreadErrType::CanNotCreateThreadExImpl(ex, __FILE__, __LINE__, 
							 "ACS::ThreadManager<>::create");
	}
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
	T* thread_p = new T(name,param,true);
	thread_p->ACS::Thread::resume(); // the thread can be started !!

	return thread_p;
	} 
    catch(ACSErr::ACSbaseExImpl& ex)
	{
	threadManagerTSS->resetThreadManager();
	throw acsthreadErrType::CanNotCreateThreadExImpl(ex, __FILE__, __LINE__, 
							 "ACS::ThreadManager<>::create");
	}
}//create

template<class T, class P>
T* ThreadManager::create(const ACE_CString name, P& param,
			 const bool suspended)
{
//    ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_addRemoveMutex);	
    if (getThreadByName(name)!=NULL) 
	{
	acsthreadErrType::ThreadAlreadyExistExImpl ex(__FILE__, __LINE__, 
						      "ACS::ThreadManager<>::create");
	}

    try
	{
	threadManagerTSS->setThreadManager(this);
	T* thread_p = new T(name, param, true);
	if(!suspended) thread_p->ACS::Thread::resume(); // the thread can be started !!

	return thread_p;
	} 
    catch(ACSErr::ACSbaseExImpl& ex)
	{
	threadManagerTSS->resetThreadManager();
	throw acsthreadErrType::CanNotCreateThreadExImpl(ex, __FILE__, __LINE__, 
							 "ACS::ThreadManager<>::create");
	}
}//create

template<class T, class P>
T* ThreadManager::create(const ACE_CString name, P& param,
			 const bool suspended,
			 const TimeInterval& responseTime,
			 const TimeInterval& sleepTime)
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
	T* thread_p = new T(name, param, true, responseTime, sleepTime);
	if(!suspended) thread_p->ACS::Thread::resume(); // the thread can be started !!

	return thread_p;
	} 
    catch(ACSErr::ACSbaseExImpl& ex)
	{
	threadManagerTSS->resetThreadManager();
	throw acsthreadErrType::CanNotCreateThreadExImpl(ex, __FILE__, __LINE__, 
							 "ACS::ThreadManager<>::create");
	}
}//create

template<class T, class P>
T* ThreadManager::create(const ACE_CString name, P& param,
			 const bool suspended,
			 const TimeInterval& responseTime,
			 const TimeInterval& sleepTime,
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
	T* thread_p = new T(name, param, true, responseTime, sleepTime, del);
	if(!suspended) thread_p->ACS::Thread::resume(); // the thread can be started !!

	return thread_p;
	} 
    catch(ACSErr::ACSbaseExImpl& ex)
	{
	threadManagerTSS->resetThreadManager();
	throw acsthreadErrType::CanNotCreateThreadExImpl(ex, __FILE__, __LINE__, 
							 "ACS::ThreadManager<>::create");
	}
}//create

template<class T, class P>
T* ThreadManager::create(const ACE_CString name, P& param,
			 const bool suspended,
			 const TimeInterval& responseTime,
			 const TimeInterval& sleepTime,
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
	T* thread_p = new T(name, param, true, responseTime, sleepTime, del, thrFlags);
	if(!suspended) thread_p->ACS::Thread::resume(); // the thread can be started !!

	return thread_p;
	} 
    catch(ACSErr::ACSbaseExImpl& ex)
	{
	threadManagerTSS->resetThreadManager();
	throw acsthreadErrType::CanNotCreateThreadExImpl(ex, __FILE__, __LINE__, 
							 "ACS::ThreadManager<>::create");
	}
}//create

#endif 

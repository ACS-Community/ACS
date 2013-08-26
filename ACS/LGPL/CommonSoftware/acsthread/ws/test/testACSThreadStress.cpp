/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2005 
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
* "@(#) $Id: testACSThreadStress.cpp,v 1.3 2006/03/24 12:42:31 vwang Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2005-02-15  created
*/

#define NO_SHORT_LOG

#include "acsThreadManager.h"
#include "acsThread.h"
#include <logging.h>

static char *rcsId="@(#) $Id: testACSThreadStress.cpp,v 1.3 2006/03/24 12:42:31 vwang Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

/*****************************
 * Test class declaration
 *****************************/

class threadManagerTestImpl
{
  public:
    /**
     * Constructor
     * @param containerServices ContainerServices which are needed for various component
     * related methods.
     * @param name component name
     */
    threadManagerTestImpl(ACS::ThreadManager &tmgr): tmgr_m(tmgr)
        {
	};

    /**
     * Destructor
     */
    virtual ~threadManagerTestImpl() {};

    virtual void start();

    void increaseFinishCounter()
	{
	    mutex.acquire();
	    ++finish_counter;
	    mutex.release();
	};

  private:
    ACE_Thread_Mutex mutex;
    bool component_running;
    unsigned long thread_counter;
    unsigned long finish_counter;
    unsigned long failure_counter; 

    ACS::ThreadManager &tmgr_m;
};

/*********************************
 * testThread class declaration
 *********************************/

#include <cmath>

class testThread: public ACS::Thread
{
  public:
    typedef struct
    {
	threadManagerTestImpl* tm;
    } structForThread;

    testThread(const ACE_CString &name,
	       const structForThread* data,
	       const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime,
	       const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime,
	       const bool del=true);
    virtual ~testThread();

    virtual void run();
    virtual void onStop();

  private:
    threadManagerTestImpl* tm;

}; /* end testThread class definition */

/*****************************
 * Test class method bodies
 *****************************/

void threadManagerTestImpl::start()
{
    finish_counter  = 0UL;
    failure_counter = 0UL;
    thread_counter  = 0UL;

    const bool print(false);

// GCH: const unsigned long max(50000UL);
// GCH: const unsigned long print_every(1000UL);
    const unsigned long max(1000UL);
    const unsigned long print_every(100UL);


    ACS_SHORT_LOG((LM_INFO, "testThreadManagerImpl::start: Starting %ld threads, printing every %ld", max, print_every));

    testThread::structForThread* foo(new testThread::structForThread);
    foo->tm = this;

    do
	{
	  std::ostringstream c;
	  c << "Thread" << thread_counter++  << std::ends;

	  try
	      {
	      tmgr_m.create<testThread, testThread::structForThread*>(c.str().c_str(), foo);

	      if((print == true) || (thread_counter % print_every == 0))
		  {
		    std::cout << "Created thread " << c.str() << std::endl;
		  }
	      // Thread creation is successful (no exception), so leave while loop.
	      continue;
	      }
	  catch(acsthreadErrType::ThreadAlreadyExistExImpl)
	      {
	      failure_counter++;
#ifdef NO_SHORT_LOG
	ACE_CString thrName = (const char*)(c.str().c_str());
	std::cout << "#testThreadManagerImpl::start: A thread with the name \"" << c.str() << "\" already exists. Trying next one. " << thrName.c_str() << std::endl;
	
	ACS::ThreadBase *tb = tmgr_m.getThreadByName(thrName.c_str());
	ACE_OS::printf("after\n");
	if (tb==NULL)
	    ACE_OS::printf("Thread is null\n");
	else
	    ACE_OS::printf("Is thread %s alive %d\n", tb->isAlive());

#else
	      ACS_SHORT_LOG((LM_ERROR, "testThreadManagerImpl::start: A thread with the name \"%s\" already exists. Trying next one.", c.str().c_str()));
	     
#endif
	      }
	  catch(acsthreadErrType::CanNotCreateThreadExImpl)
	      {
	      failure_counter++;
	      thread_counter--;
#ifdef NO_SHORT_LOG
		std::cout << "testThreadManagerImpl::start: Cannot create a new thread! RETRY!(" << c.str() << ")" << std::endl;
#else
	      ACS_SHORT_LOG((LM_ERROR, "testThreadManagerImpl::start: Cannot create a new thread! RETRY (%s)", c.str().c_str()));
#endif
	      sleep(1);
	      }
	}
    while(thread_counter < max);

    delete foo;

  std::cout << "testThreadManagerImpl::start: Counter of failed threads = " << failure_counter << std::endl;
  std::cout << "Threads in thread manager " << tmgr_m.getThreadCount() << std::endl;

#ifdef NO_SHORT_LOG
  std::cout << "testThreadManagerImpl::start: Counter of finished threads = " << finish_counter << ". Sleeping 10s." << std::endl;
#else
    ACS_SHORT_LOG((LM_INFO,"testThreadManagerImpl::start: Counter of finished threads = %d. Sleeping 10s.", finish_counter));
#endif

  ACE_OS::sleep(10);

#ifdef NO_SHORT_LOG
  std::cout << "testThreadManagerImpl::start: Counter of finished threads = " << finish_counter << ". Sleeping 20s." << std::endl;
#else
    ACS_SHORT_LOG((LM_INFO,"testThreadManagerImpl::start: Counter of finished threads = %d. Sleeping 10s.", finish_counter));
#endif

  ACE_OS::sleep(20);

#ifdef NO_SHORT_LOG
  std::cout << "testThreadManagerImpl::start: Counter of finished threads = " << finish_counter << "." << std::endl;
  std::cout << "2 Threads in thread manager " << tmgr_m.getThreadCount() << std::endl;
#else
    ACS_SHORT_LOG((LM_INFO,"testThreadManagerImpl::start: Counter of finished threads = %d.", finish_counter));
#endif


#ifdef NO_SHORT_LOG
  std::cout << "testThreadManagerImpl::start: Ready." << std::endl;
#else
    ACS_SHORT_LOG((LM_INFO,"testThreadManagerImpl::start: Ready."));
#endif

}

/*****************************
 * Thread class method bodies
 *****************************/

testThread::testThread(const ACE_CString &name,
		       const structForThread* data,
		       const ACS::TimeInterval &responseTime,
		       const ACS::TimeInterval &sleepTime,
		       const bool del):
    ACS::Thread(name, responseTime, sleepTime, del),
    tm(data->tm)
{
}

testThread::~testThread()
{
#ifdef NO_SHORT_LOG
//	std::cout << "testThread::~testThread: " << getName().c_str() << " destroyed." << std::endl;
#else
    // GCH: ACS_SHORT_LOG((LM_INFO, "testThread::~testThread: %s destroyed.", my_name.c_str()));
#endif
}

void testThread::run()
{

    // std::cout << "testThread::run: (" << getName().c_str() << ") Running... " << std::endl;
ACS_DEBUG_PARAM("testThread::run-begin", "Thread name: %s", getName().c_str());
//    double j(0);
    for(int i(0); i < 30000; ++i)
	{
//	j += std::pow(j, i);
//	j /= std::sin(j / 2.0 * M_PI);
	}
   ACS_DEBUG_PARAM("testThread::run-end", "Thread name: %s", getName().c_str()); 
    tm->increaseFinishCounter();

//    this->exit();
}

void testThread::onStop()
{
}


/*****************************
 *  MAIN function
 *****************************/

int main(int argc, char *argv[])
{
    LoggingProxy logger_m(0, 3, 31);
    LoggingProxy::init(&logger_m);

    ACS::ThreadManager tm;
 

    threadManagerTestImpl tmt(tm);

    tmt.start();
    sleep(5);
    tmt.start();
    sleep(5);
    tmt.start();
    sleep(5);
    tmt.start();

    sleep(10);
    
    LoggingProxy::done();
    sleep(10);
    
    return 0;

}










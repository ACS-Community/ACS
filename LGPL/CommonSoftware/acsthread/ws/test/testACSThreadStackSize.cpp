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
* "@(#) $Id: testACSThreadStackSize.cpp,v 1.2 2008/10/15 00:28:13 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2005-02-15  created
*/

// Uncomment this if you are using the VLT environment
// #include "vltPort.h"


#include "acsThreadManager.h"

#define MAX_THREAD 2000


static char *rcsId="@(#) $Id: testACSThreadStackSize.cpp,v 1.2 2008/10/15 00:28:13 bjeram Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);


class TestACSThread :public ACS::Thread
{
  public:
    TestACSThread(const ACE_CString& name,
			       const ACS::TimeInterval& responseTime,
			       const ACS::TimeInterval& sleepTime,
			       bool del,
			       const long thrFlags,
			       const size_t stackSize) :
	ACS::Thread(name, responseTime, sleepTime, del, thrFlags, stackSize)
	{
	    ACS_TRACE("TestACSThread::TestACSThread");
	}

    ~TestACSThread()
	{
	    ACS_TRACE("TestACSThread::~TestACSThread");

	    terminate();
	}

    virtual void runLoop()
	{
	    sleep();
	}
  protected:
    ACE_CString msg;
};

///////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////

class TestACSThreadWithParameter :public ACS::Thread
{
  public:
    TestACSThreadWithParameter(const ACE_CString& name,
			       char  *parmMsg,
			       const ACS::TimeInterval& responseTime,
			       const ACS::TimeInterval& sleepTime,
			       bool del,
			       const long thrFlags,
			       const size_t stackSize) :
	ACS::Thread(name, responseTime, sleepTime, del, thrFlags, stackSize)
	{
	    ACS_TRACE("TestACSThreadWithParameter::TestACSThreadWithParameter");
	    msg = parmMsg;
	    ACS_LOG(LM_SOURCE_INFO, "TestACSThreadWithParameter::TestACSThreadWithParameter",
		    (LM_INFO, "Thread parameter: %s", parmMsg));
	}

    ~TestACSThreadWithParameter()
	{
	    ACS_TRACE("TestACSThreadWithParameter::~TestACSThreadWithParameter");

	    terminate();
	}

    virtual void runLoop()
	{
    	sleep();
    		}
  protected:
    ACE_CString msg;
};

int main(int argc, char *argv[])
{

	int i, n;
	int STACKSIZE;
	ACS::Thread *threads[MAX_THREAD];
	ACS::ThreadManager tm;

	if (argc < 2)
	{
		printf ("Usage: %s threads# [stacksize in kB]\n", argv[0]);
		return 1;
	}


	n = atoi(argv[1]);

	if ((n < 1) || (n > MAX_THREAD))
	{
		printf ("The # of thread should between 1 and %d.\n",MAX_THREAD);
		return 2;
	}//if

	LoggingProxy logger_m(0, 0, 31);
	LoggingProxy::init(&logger_m);

	STACKSIZE = atoi(argv[2]) * 1024;

	ACS_LOG(LM_SOURCE_INFO,"main",
			(LM_INFO, "=============== 1 - Trying to create %d threads with a parameter and stack size, n"));
	char *msg="Thread msg";
	try
	{
		for(i=0; i<n; i++)
		{
			std::ostringstream c;
			c << "Thread#" << i  << std::ends;
			threads[i]=
				tm.create<TestACSThreadWithParameter, char*>(c.str().c_str(),
						msg,
						ACS::ThreadBase::defaultResponseTime,
						ACS::ThreadBase::defaultSleepTime,
						false,
						THR_NEW_LWP | THR_DETACHED,
						STACKSIZE);
				threads[i]->resume();
				ACS_LOG(LM_SOURCE_INFO,"main",
						(LM_INFO, "Thread with a parameter #%d created", i));
		}//for
	}
	catch(ACSErr::ACSbaseExImpl &ex)
	{
		ex.log();
	}
	ACS_LOG(LM_SOURCE_INFO,"main",
			(LM_INFO, "Created %d threads with a parameter with stack size %d kB", i, atoi(argv[2])));

	ACS_LOG(LM_SOURCE_INFO,"main",
			(LM_INFO, "Going to destroy threads"));
	for(--i;i>=0;i--)
	{
		tm.destroy(threads[i]);
	//	ACS_LOG(LM_SOURCE_INFO,"main", (LM_INFO, "Thread #%d deleted", i));
	}//for

	sleep(1);

	ACS_LOG(LM_SOURCE_INFO,"main",
			(LM_INFO, "=============== 2 - Trying to create %d threads with stack size", n));

	try
		{
			for(i=0; i<n; i++)
			{
				std::ostringstream c;
				c << "Thread#" << i  << std::ends;
				threads[i]=
					tm.create<TestACSThread>(c.str().c_str(),
							ACS::ThreadBase::defaultResponseTime,
							ACS::ThreadBase::defaultSleepTime,
							false,
							THR_NEW_LWP | THR_DETACHED,
							STACKSIZE);
					threads[i]->resume();
					ACS_LOG(LM_SOURCE_INFO,"main",
							(LM_INFO, "Thread #%d created", i));
			}//for
		}
		catch(ACSErr::ACSbaseExImpl &ex)
		{
			ex.log();
		}
		ACS_LOG(LM_SOURCE_INFO,"main",
				(LM_INFO, "Created %d threads with stack size %d kB", i, atoi(argv[2])));

		ACS_LOG(LM_SOURCE_INFO,"main",
				(LM_INFO, "Going to destroy threads"));
		for(--i;i>=0;i--)
		{
			tm.destroy(threads[i]);
		//	ACS_LOG(LM_SOURCE_INFO,"main", (LM_INFO, "Thread #%d deleted", i));
		}//for


	/**
	 * Wait for everything to cleanup and go home
	 */
	sleep(5);
	ACS_LOG(LM_SOURCE_INFO,"main",
			(LM_INFO, "=============== The end"));

	return 0;
}









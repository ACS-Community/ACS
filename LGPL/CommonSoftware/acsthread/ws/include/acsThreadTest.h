#ifndef _ACS_THREAD_TEST_H
#define _ACS_THREAD_TEST_H
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
* "@(#) $Id: acsThreadTest.h,v 1.4 2006/03/24 12:12:49 vwang Exp $"
*
* Classes used in ACS Thread modular tests
*
*/

#include "acsThread.h"


class TestACSThread :public ACS::Thread
{
  public:
    TestACSThread(const ACE_CString& name, 
		  const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime, 
		  const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime,
		  bool del=false
	) :
	ACS::Thread(name, responseTime, sleepTime, del)
	{
	    ACS_TRACE("TestACSThread::TestACSThread");
	    loopCounter_m = 0;
	}

    TestACSThread(const ACE_CString& name,
		  const ACS::TimeInterval& responseTime, 
		  const ACS::TimeInterval& sleepTime,
		  bool del,
		  const long _thrFlags
	) :
	ACS::Thread(name, responseTime, sleepTime, del, _thrFlags)
	{
	    ACS_TRACE("TestACSThread::TestACSThread");
	    loopCounter_m = 0;
	}

    /**
     * Destructor.
     *
     * Notice that here we have to call terminate() to protect the thread from 
     * concurrency problems.
     * The runLoop() method uses data members of the thread (loopCounter).
     * When the thread object get deleted, we have to be sure
     * the thread is stopped before we delete the member variables, and 
     * therefore we have to call terminate() here.
     * 
     * The fact that terminate is called in the destructor
     * of ACS::ThreadBase does not help, because it can happen
     * the runLoop() method is called between this destructor and the
     * ones of the parent classes.
     * 
     * This would cause access to already unallocated memory.
     *
     */
    ~TestACSThread() 
	{ 
	    ACS_TRACE("TestACSThread::~TestACSThread"); 

	    terminate(); 
	}

    /**
     * This is the method executed in the thread loop.
     * It executes 10 times, then it exits.
     */
    virtual void runLoop()
	{
	    if (loopCounter_m==10)
	    {
		exit();
	    }
	    
	    ACS_LOG(LM_SOURCE_INFO,
		    "TestACSThread::runLoop", 
		    (LM_INFO, "%s: runLoop (%d)", getName().c_str(), loopCounter_m));
	    ++ loopCounter_m;
	}

  protected:
    int loopCounter_m;

};

#endif /* end _ACS_THREAD_TEST_H */

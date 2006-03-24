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
* "@(#) $Id: testACSThreadRestart.cpp,v 1.2 2006/03/24 12:42:31 vwang Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* vwwang   2006-02-20 created
*/

#include "acsThread.h"
#include "acsThreadManager.h"

class Test :public ACS::Thread
{
  public:
    /* constructor,
     * if you want to restart a thread, 
     * the parameter del must be false (not auto-delete)
     */
    Test(const ACE_CString& name,
	 const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime, 
	 const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime )
        :  ACS::Thread(name, responseTime, sleepTime, false) 
    {
       ACS_TRACE("Test::Test");
    }

    ~Test() 
    { 
       ACS_TRACE("Test::~Test"); 

       terminate(); 
    }

    /**
     * This is the method executed in the thread loop. 
     * will run for more then 10 secs per loop
     * I use 10 loop here to show it is still runing in log
     */
    virtual void runLoop()
    {
       for( int loopCounter = 0 ; loopCounter<10; loopCounter++ ) {
         ACS_LOG(LM_SOURCE_INFO,
                 "Test::runLoop", 
                 (LM_INFO, "%s: runLoop (%d)", getName().c_str(), loopCounter));
         ACE_OS::sleep(1);
       }
    }


};

int main(int argc, char *argv[])
{
    LoggingProxy logger_m(0, 0, 31);
    LoggingProxy::init(&logger_m);

    ACS::ThreadManager tm;

    /*
     * Creates an thread and try to restart it before one loop finish
     * the runLoop will run for 10 sec. but the responseTime is 0.5
     * in restart, both stop() and cancel() will fail
     */
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Creating thread"));
    Test *a = 
	tm.create<Test>("TestA", 
			 5 * 1000000 /* 0.5s */, 5 * 1000000 /* 0.5s */ );
    a->resume();

    ACE_OS::sleep(1);
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "try to restart thread"));
    
    bool done = a->restart();

    if( !done ) {
      ACS_LOG(LM_SOURCE_INFO,"main", (LM_INFO, "restart fail, as we expected"));
      ACE_OS::sleep(15);
    }

    
    ACS_LOG(LM_SOURCE_INFO,"main", (LM_INFO, "Now try to delete TestA thread"));
    delete a;

    /*
     * Creates an thread and try to restart it 
     * the runLoop will run for 10 sec. the responseTime should be more than 10 sec
     * in restart, stop() will be successful
     */
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Creating thread"));
     
    a =	tm.create<Test>("TestB", 
			 11 * 10 * 1000000 /* 11s */, 5 * 100000 /* 0.05s */ );

    a->resume();
    ACE_OS::sleep(1);
    done = a->restart();

    if( !done ) {
      ACS_LOG(LM_SOURCE_INFO,"main", (LM_INFO, "restart fail, something wrong !"));
    }

    sleep(2);
    ACS_LOG(LM_SOURCE_INFO,"main", (LM_INFO, "Now try to delete TestB thread"));
    delete a;
    
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Done"));
    return 0;

}









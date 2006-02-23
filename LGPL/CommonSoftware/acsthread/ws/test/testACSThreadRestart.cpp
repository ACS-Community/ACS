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
* "@(#) $Id: testACSThreadRestart.cpp,v 1.1 2006/02/23 13:26:48 vwang Exp $"
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
    /* constructor, if you want to restart a thread, the parameter del must be false (not auto-delete) */
    Test(const ACE_CString& name, bool suspended=false,
	 const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime, 
	 const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime )
        :  ACS::Thread(name, suspended, responseTime, sleepTime, false) 
    {
       ACS_TRACE("Test::Test");
    }

    ~Test() 
    { 
       ACS_TRACE("Test::~Test"); 

       terminate(); 
    }

    /**
     * This is the method executed in the thread loop.  will run for more then 10 secs per loop
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
     */
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Creating thread"));
    Test *a = 
	tm.create<Test>("TestA", 
			 false, 
			 5 * 1000000 /* 0.5s */, 5 * 1000000 /* 0.5s */ );

    ACE_OS::sleep(1);

    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "try to restart thread"));
    
    bool done;

    while(true) {
      done = a->restart();

      if( done ) {
        ACS_LOG(LM_SOURCE_INFO,"main", (LM_INFO, "restart success"));
        break;
      }
      else {
        ACS_LOG(LM_SOURCE_INFO,"main", (LM_INFO, "restart fail, will retry"));
        ACE_OS::sleep(15);
      }
    } /* end of while */

    ACE_OS::sleep(1);
    
    while(true) {
      done = a->stop();

      if( done ) {
        ACS_LOG(LM_SOURCE_INFO,"main", (LM_INFO, "stop success"));
        break;
      }
      else {
        ACS_LOG(LM_SOURCE_INFO,"main", (LM_INFO, "stop fail, will retry"));
        ACE_OS::sleep(10);
      }
    } /* end of while */
    
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Done"));
    return 0;

}









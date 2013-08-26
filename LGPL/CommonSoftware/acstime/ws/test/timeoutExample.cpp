// @(#) $Id: timeoutExample.cpp,v 1.8 2008/10/01 03:11:48 cparedes Exp $
//
// Copyright (C) 2001
// Associated Universities, Inc. Washington DC, USA.
//
// Produced for the ALMA project
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//
// Correspondence concerning ALMA Software should be addressed as follows:
//   Internet email: alma-sw-admin@nrao.edu

//
// DESCRIPTION
// An ACS client to test the Clock interface timeout methods.
//

#include <iostream>

#include <maciSimpleClient.h>
#include <baci.h>
#include "acstime.h"
#include "acstimeS.h"

using namespace std;
 using namespace maci;
 using namespace ACS;

//------------------------------------------------------------------------------
class TimeoutHandlerImpl : public virtual POA_acstime::TimeoutHandler
{
  public:
    void handleTimeout(const acstime::Epoch& e)
	{ 
	    cout << "The current time is: " << e.value << endl; 
	}
};
//------------------------------------------------------------------------------
int main(int argc,char* argv[]) 
{
    SimpleClient client;
    if(client.init(argc,argv) == 0)
	{
	return -1;
	}
    else
	{
	client.login();
	}
    
    // get reference to Clock device
    acstime::Clock_var clock = client.get_object<acstime::Clock>("CLOCK1", 0, true);
    ACS::ROuLongLong_var now = clock->now();
    
    // get reference to Timer device
    acstime::Timer_var timer = client.get_object<acstime::Timer>("TIMER1",0,true);
    
    // first timeout occurs now plus 3 seconds
    acstime::Epoch start;
    ACSErr::Completion_var completion;
    start.value = now->get_sync(completion.out()) + 3000000ULL;
    
    // timeout only occurs once
    acstime::Duration period;
    period.value = 0LL;
    
    
    // create timeout handler and schedule its timeout
    TimeoutHandlerImpl* myHandler = new TimeoutHandlerImpl();
    
    int id1 = timer->schedule(myHandler->_this(),start,period);
    
    ACS_SHORT_LOG((LM_DEBUG,"Client going to sleep ..."));
    ACE_Time_Value sleep(10);
    client.run(sleep);
    
    // cleanup
    timer->cancel(id1);  // cancel "one shot" timeout, un-necessary
    
    
    client.manager()->release_component(client.handle(), "CLOCK1");
    client.manager()->release_component(client.handle(), "TIMER1");
    client.logout();
    
    return 0;
}








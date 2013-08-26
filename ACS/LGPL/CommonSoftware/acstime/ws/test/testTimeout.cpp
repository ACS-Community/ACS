// @(#) $Id: testTimeout.cpp,v 1.10 2008/10/01 03:11:48 cparedes Exp $
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

using namespace std;
 using namespace maci;
 using namespace ACS;

//------------------------------------------------------------------------------

class TimeoutHandlerImpl : public virtual POA_acstime::TimeoutHandler
{
  public:
    // this method is called when timeout occurs
    void handleTimeout(const acstime::Epoch& e
		       )
	{ 
	    cout << "I'm in time handler " << m_id << endl; 
	}

    void setId(long id)
	{ 
	    m_id = id; 
	}

  private:
    long m_id;
};

//------------------------------------------------------------------------------

int main(int argc,char* argv[]) 
{
    
    try
	{
	// create instance of SimpleClient and init() it.
	SimpleClient ci;
	if(ci.init(argc,argv) == 0)
	    {
	    ACE_DEBUG((LM_DEBUG,"Cannot init client"));
	    return -1;
	    }

	// ???
	ci.login();

	// get reference to Clock device
	CORBA::Object_var obj = ci.get_object("CLOCK1",0,true);
	
	if(CORBA::is_nil(obj.in())) 
	    {
	    ACS_SHORT_LOG((LM_DEBUG,"Cannot get Object"));
	    return -1;
	    }
	
	// narrow object to obtain Clock reference
	acstime::Clock_var c_var = acstime::Clock::_narrow(obj.in());
	
        if (CORBA::is_nil(c_var.in())) 
            {
            cerr << "Nil Clock reference" << endl;
            return -1;
            }

	// get reference to Timer device
	CORBA::Object_var obj2 = ci.get_object("TIMER1",0,true);
	
	if(CORBA::is_nil(obj2.in())) 
	    {
	    ACS_SHORT_LOG((LM_DEBUG,"Cannot get Timer Object"));
	    return -1;
	    }
	
	// narrow object to obtain Timer reference
	acstime::Timer_var t_var = acstime::Timer::_narrow(obj2.in());
	
        if (CORBA::is_nil(t_var.in())) 
            {
            cerr << "Nil Timer reference" << endl;
            return -1;
            }
	
	ACSErr::Completion_var completion;
	ACS::RWlong_var m_array2TAI = c_var->array2TAI();
	ACS::RWlong_var m_TAI2UTC = c_var->TAI2UTC();
	ACS::ROuLongLong_var m_now = c_var->now();

	// create Epoch object
	EpochHelper *e_var = new EpochHelper();
	// create Duration object
	DurationHelper *d_var = new DurationHelper();

        // first timeout occurs now plus 3 seconds
        acstime::Epoch start;
	start.value = m_now->get_sync(completion.out());
	
	e_var->value(start);
	
	d_var->second(3);
	
	e_var->add(d_var->value());
	
        start = e_var->value();
	

        // after first, timeouts occur time after this duration
	d_var->second(1);
	
	acstime::Duration period;
	period = d_var->value();
	

        // create "continuous" timeout handler and schedule its timeout
        TimeoutHandlerImpl* mh1 = new TimeoutHandlerImpl();
        acstime::TimeoutHandler_ptr th_ptr1 = mh1->_this(); 
        
        int id1 = t_var->schedule(th_ptr1,start,period);
	
        cout << "id1=" << id1 << endl;
        mh1->setId(id1);

        // create "one shot" timeout handler and schedule its timeout
        TimeoutHandlerImpl* mh2 = new TimeoutHandlerImpl();
        acstime::TimeoutHandler_ptr th_ptr2 = mh2->_this(); 
        
	d_var->reset();
	
	d_var->second(4);
	
	d_var->microSecond(500000);
	
	e_var->add(d_var->value());
	
        start = e_var->value();
	
	d_var->reset();
	
	period = d_var->value();
	
        int id2 = t_var->schedule(th_ptr2,start,period);
	
        cout << "id2=" << id2 << endl;
        mh2->setId(id2);

        // create "one shot" timeout handler and schedule its timeout
        // but immediately cancel it
        TimeoutHandlerImpl* mh3 = new TimeoutHandlerImpl();
        acstime::TimeoutHandler_ptr th_ptr3 = mh3->_this(); 
        
	d_var->reset();
	
	d_var->second(5);
	
	e_var->add(d_var->value());
	
        start = e_var->value();
	
        int id3 = t_var->schedule(th_ptr3,start,period);
	
        cout << "id3=" << id3 << endl;
        mh3->setId(id3);
        t_var->cancel(id3);  // cancel timeout

	ACE_DEBUG((LM_DEBUG,"Client going to sleep ..."));
        ACE_Time_Value sleep(10);
        ci.run(sleep);

        // cleanup
        t_var->cancel(id1);  // cancel "continuous" timeout
        t_var->cancel(id2);  // cancel "one shot" timeout, un-necessary

	// ???
	ci.logout();
	}
    catch( CORBA::Exception &ex )
	{
          cout << "bad" << endl;
	ACE_PRINT_EXCEPTION(ex,"Error!");
	return -1;
	}
    
    return 0;
}









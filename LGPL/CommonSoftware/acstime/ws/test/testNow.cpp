// $Id: testNow.cpp,v 1.11 2006/09/01 02:20:54 cparedes Exp $
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
// An ACS client to test the Clock::now() method.
// It's modeled after "timebug.py" from B. Glendenning
// The program's output should has the current time updated on each second
// Sample output ---
//     2001-08-03T09:13:59
//     2001-08-03T09:14:00
//     2001-08-03T09:14:01
//     2001-08-03T09:14:02
//     2001-08-03T09:14:03
//     2001-08-03T09:14:04

#include <iostream>

#include <maciSimpleClient.h>
#include <baci.h>
#include "acstime.h"

 using namespace maci;
 using namespace ACS;

//------------------------------------------------------------------------------

int main(int argc,char *argv[])
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
	    ACS_SHORT_LOG((LM_DEBUG,"Cannot get CLOCK Object"));
	    return -1;
	    }
	
	// narrow object to obtain Clock reference
	acstime::Clock_var c_var = acstime::Clock::_narrow(obj.in());
	
	if (CORBA::is_nil(c_var.in())) 
	    {
	    std::cerr << "Nil Clock reference" << std::endl;
	    return -1;
	    }
	
	// create Epoch object with value Jan 1, 1970 00:00:00
	EpochHelper *jan11970Epoch = new EpochHelper();
	jan11970Epoch->year(1970);
	jan11970Epoch->dayOfYear(1);

	// create Epoch object to hold now() value
	ACSErr::Completion_var completion;
	ACS::RWlong_var m_array2TAI = c_var->array2TAI();
	ACS::RWlong_var m_TAI2UTC = c_var->TAI2UTC();
	ACS::ROuLongLong_var m_now = c_var->now();
	EpochHelper *tmpEpoch = new EpochHelper();

	while (1)	// Forever
	    {
	    // set tmpEpoch to this instant
	    acstime::Epoch eout;
	    eout.value = m_now->get_sync(completion.out());
	    
	    tmpEpoch->value(eout);
	    

	    

	    // set interval to number of seconds from Jan11970 to now

	    DurationHelper *duration = new DurationHelper(tmpEpoch->difference(jan11970Epoch->value()));
	    

	    long day = duration->day();
	    
	    

	    long hour = duration->hour();
	    
	    long minute = duration->minute();
	    
	    long second = duration->second();
	    
	    long microSecond = duration->microSecond();
	    
	    double interval = day * 24.0 * 3600.0
	                    + hour * 3600.0
	                    + minute * 60.0
                            + second * 1.0
                            + microSecond * 1.0E-6;

	    // add interval to jan1 1970 to get back to "now"
	    duration->reset();
	    duration->normalize(true);
	    duration->second((long)interval);
	    
	    duration->microSecond(0);
	    

	    // set tmpEpoch to Jan 1 1970
	    eout = jan11970Epoch->value();
	    
	    tmpEpoch->value(eout);
	    

	    // add duration in
	    tmpEpoch->add(duration->value());
	    

	    // output tmpEpoch value
	    std::string pStr = tmpEpoch->toString(acstime::TSArray,
					    "",
					    m_array2TAI->get_sync(completion.out()), 
					    m_TAI2UTC->get_sync(completion.out()));
	    
	    std::cout << pStr.c_str() << std::endl;

	    // clean up & wait for next time around loop
	    delete duration;
	    
	    sleep(1);
	    }

	// ???
	ci.logout();
	}
    catch( CORBA::Exception &ex )
	{
	ACE_PRINT_EXCEPTION(ex,"Error!");
	return -1;
	} 

    return 0;
}







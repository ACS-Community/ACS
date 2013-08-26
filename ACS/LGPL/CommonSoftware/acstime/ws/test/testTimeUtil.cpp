// $Id: testTimeUtil.cpp,v 1.10 2006/09/01 02:20:54 cparedes Exp $
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
// An client to test the C++ TimeUtil class.
//

#include <iostream>

#include <maciSimpleClient.h>
#include <baci.h>
#include "acstime.h"

using namespace std;
 using namespace maci;
 using namespace ACS;

//------------------------------------------------------------------------------

int main(int argc, char *argv[])
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
	    cerr << "Nil Clock reference" << endl;
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
       
	// fill Epoch
	e_var->fromString(acstime::TSArray,
                          "1995-4-28T17:23:15.654321");
	
	std::string pStr = e_var->toString(acstime::TSArray,"", m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;
	acstime::Epoch eout;
	eout = e_var->value();
	

	// convert EpochS to ACE_Time_Value
	ACE_Time_Value ace = TimeUtil::epoch2ace(eout);
	cout << ace.sec() << endl;
	cout << ace.usec() << endl;

	// convert ACE_Time_Value to timeval
	struct timeval tv = ace;
	cout << tv.tv_sec << endl;
	cout << tv.tv_usec << endl;

	// convert timeval to ACE_Time_Value
	ace.set(tv);
	cout << ace.sec() << endl;
	cout << ace.usec() << endl;

	// convert ACE_Time_Value to timespec
	struct timespec ts = ace;
	cout << ts.tv_sec << endl;
	cout << ts.tv_nsec << endl;

	// convert timespec to ACE_Time_Value
	ace.set(ts);
	cout << ace.sec() << endl;
	cout << ace.usec() << endl;

	// convert ACE_Time_Value to Epoch
	eout = TimeUtil::ace2epoch(ace);
	e_var->value(eout);
	
	pStr = e_var->toString(acstime::TSArray,"", m_array2TAI->get_sync(completion.out()), m_TAI2UTC->get_sync(completion.out()));
	
        cout << pStr.c_str() << endl;

	// fill Duration
	d_var->fromString("1069 02:48:05.477581");
	
	pStr = d_var->toString("");
	
        cout << pStr.c_str() << endl;
	acstime::Duration dout;
	dout = d_var->value();
	

	// convert EpochS to ACE_Time_Value
	ace = TimeUtil::duration2ace(dout);
	cout << ace.sec() << endl;
	cout << ace.usec() << endl;

	// convert ACE_Time_Value to timeval
	tv = ace;
	cout << tv.tv_sec << endl;
	cout << tv.tv_usec << endl;

	// convert timeval to ACE_Time_Value
	ace.set(tv);
	cout << ace.sec() << endl;
	cout << ace.usec() << endl;

	// convert ACE_Time_Value to timespec
	ts = ace;
	cout << ts.tv_sec << endl;
	cout << ts.tv_nsec << endl;

	// convert timespec to ACE_Time_Value
	ace.set(ts);
	cout << ace.sec() << endl;
	cout << ace.usec() << endl;

	// convert ACE_Time_Value to Epoch
	dout = TimeUtil::ace2duration(ace);
	d_var->value(dout);
	
	pStr = d_var->toString("");
	
        cout << pStr.c_str() << endl;

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







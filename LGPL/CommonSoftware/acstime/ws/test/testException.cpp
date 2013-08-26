// @(#) $Id: testException.cpp,v 1.7 2006/09/01 02:20:54 cparedes Exp $
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
// An ACS client to test exception handling by the TimeService interfaces.
//

#include <maciSimpleClient.h>
#include <baciS.h>
#include <baciCORBA.h>
#include "ClockC.h"

 using namespace maci;
 using namespace ACS;

//------------------------------------------------------------------------------

int main(int argc, char *argv[])
{
    // container name is given as argument
    if (argc < 2)
        {
	cout << "Usage: " << argv[0] << " <Container name> <options>" << endl;
	cout << "For example: " << argv[0] << " ARTM" << endl;
	return -1;
	}
    ACE_CString containerName = argv[1];

    
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
	ACE_CString objName = containerName + ":CLOCK";
	CORBA::Object_var obj = ci.get_object(objName.c_str(),0,true);
	if(CORBA::is_nil(obj.in())) 
	    {
	    ACS_SHORT_LOG((LM_DEBUG,"Cannot get Object"));
	    return -1;
	    }

	// narrow object to obtain Clock reference
	acstime::Clock_var c_var 
	    = acstime::Clock::_narrow(obj.in());
	if (CORBA::is_nil(c_var.in())) 
	    {
	    cerr << "Nil Clock reference" << endl;
	    return -1;
	    }

	// create two Epoch objects
	acstime::Epoch_var e1_var = c_var->createEpoch();
	acstime::Epoch_var e2_var = c_var->createEpoch();
	if (CORBA::is_nil(e1_var.in()) || CORBA::is_nil(e2_var.in()))
	    {
	    cerr << "Nil Epoch reference" << endl;
	    return -1;
	    }

	// create two Duration objects
	acstime::Duration_var d1_var = c_var->createDuration();
	acstime::Duration_var d2_var = c_var->createDuration();
	if (CORBA::is_nil(d1_var.in()) || CORBA::is_nil(d2_var.in()))
	    {
	    cerr << "Nil Duration reference" << endl;
	    return -1;
	    }

        // test Duration attribute out-of-range
        try
            {
            d1_var->minute(400);
            cout << "Exception did NOT occur" << endl;
            }
        catch(CORBA::BAD_PARAM &ex)
            {
              cout << "Duration out-of-range minute" << endl;
            }

        // test Epoch attribute out-of-range
        try
            {
            e1_var->year(2001);
            e1_var->dayOfYear(400);
            cout << "Exception did NOT occur" << endl;
            }
        catch (CORBA::BAD_PARAM &ex)
            {
            cout << "Epoch out-of-range day-of-year" << endl;
            }

        // test duration.add() overflow
        try
            {
            d1_var->fromString("6000000 00:00:00");
            d2_var->fromString("6000000 00:00:00");
            d1_var->add(d2_var.in());
            cout << "Exception did NOT occur" << endl;
            }
        catch(acstime::overflowOrUnderflow &ex);
            {
            cout << "Duration add overflow" << endl;
            }

        // other needed exception tests 
        // duration.subtract()
        // duration.multiply()
        // duration.fromString() 
        // epoch.add()
        // epoch.subtract()
        // epoch.fromString() 

	// delete Epoch and Duration objects
	c_var->destroyEpoch(e1_var.in());
	c_var->destroyEpoch(e2_var.in());
	c_var->destroyDuration(d1_var.in());
	c_var->destroyDuration(d2_var.in());

	// ???
	ci.logout();
	}
    catch( CORBA::Exception &ex )
        {
	ACE_PRINT_EXCEPTION(ex,"catch any");
	}

    return 0;
}





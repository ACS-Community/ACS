// @(#) $Id: testProp.cpp,v 1.7 2006/09/01 02:20:54 cparedes Exp $
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
// An ACS client to test the Properties of the Clock interface.
//

#include <iostream>

#include <maciSimpleClient.h>
#include <baci.h>
#include "acstime.h"

using namespace std;
 using namespace maci;
 using namespace ACS;

//------------------------------------------------------------------------------

int main(int argc,char* argv[])
{
    // create instance of SimpleClient and init() it.
    SimpleClient ci;
    if(ci.init(argc,argv) == 0)
	{
	ACE_DEBUG((LM_DEBUG,"Cannot init client"));
	return -1;
	}
    ci.login();

    
    try
	{
	// get reference to Clock device
	CORBA::Object_var obj = ci.get_object("CLOCK1",0,true);
	if(CORBA::is_nil(obj.in())) 
	    {
	    return -1;
	    }

	// narrow object to obtain Clock reference
	acstime::Clock_var dev = acstime::Clock::_narrow(obj.in());
	
	if (CORBA::is_nil(dev.in())) 
	    {
	    std::cerr << "Nil Clock reference" << std::endl;
	    return -1;
	    }
	
	//---------------------------------------------------------------
	ACSErr::Completion_var completion;
	ACSErr::Completion* c;
	int rtnVal;
	
	// get pointer to array2TAI Property
	ACS::RWlong_ptr p_a2t = dev->array2TAI();
	

	for (int i = 0; i < 200; i = (i + 1) * 2)
	    {
	    // set array2TAI value
	    c = p_a2t->set_sync(i);
	    
	    if (c->code == 0)
		cout << "Set array2TAI=" << i << endl;
	    else
		cout << "ERROR array2TAI=" << c->code << endl;
	    
	    // get array2TAI value
	    rtnVal = p_a2t->get_sync(completion.out());
	    
	    if (completion->code != 0)
		cout << "ERROR array2TAI get=" << completion->code << endl;
	    else
		{
		cout << "Got array2TAI=" << rtnVal << endl;
		if (i != rtnVal)
		    cout << "ERROR array2TAI get != set" << endl;
		}
	    }
	
	// get pointer to TAI2UTC Property
	ACS::RWlong_ptr p_t2u = dev->TAI2UTC();
	
	
	for (int i = 0; i < 200; i = (i + 1) * 2)
	    {
	    // set TAI2UTC value
	    c = p_t2u->set_sync(i);
	    
	    if (c->code == 0)
		cout << "Set TAI2UTC=" << i << endl;
	    else
		cout << "ERROR TAI2UTC=" << c->code << endl;

	    // get TAI2UTC value
	    rtnVal = p_t2u->get_sync(completion.out());
	    
	    if (completion->code != 0)
		cout << "ERROR TAI2UTC get=" << completion->code << endl;
	    else
		{
		cout << "Got TAI2UTC=" << rtnVal << endl;
		if (i != rtnVal)
		    cout << "ERROR TAI2UTC get != set" << endl;
		}
	    }
	}
    catch( CORBA::Exception &ex )
	{
	ACE_PRINT_EXCEPTION(ex,"Error!");
        ci.logout();
	return -1;
	}

    ci.logout();
    return 0;
}







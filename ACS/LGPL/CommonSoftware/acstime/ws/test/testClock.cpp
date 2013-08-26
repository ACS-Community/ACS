// @(#) $Id: testClock.cpp,v 1.8 2006/10/19 07:52:23 bjeram Exp $
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
	acstime::Clock_var dev = ci.getComponent<acstime::Clock>("CLOCK1", 0, true);
	
	//---------------------------------------------------------------
	const char *joe = "2003-3-11T5:36:10.955161";

	std::cout << "This is a simple test which just makes sure an ISO-8601" << std::endl;
	cout << "can be converted to an ACS Epoch and be changed back into" << endl;
	cout << "a string." << endl << endl;

	cout << "The value we will be working with is: " << joe << "!" << endl; 
	
	acstime::Epoch tEpoch = dev->fromISO8601(acstime::TSArray, joe); 
	
	cout << "The ACS epoch value is then: " << tEpoch.value << "!" << endl;
	
	cout << "Finally this is converted back to a string: " 
	     << dev->toISO8601(acstime::TSArray, tEpoch) << "!" << endl;
	
	}
    catch( maciErrType::CannotGetComponentExImpl &_ex )
	{
	_ex.log();
        ci.logout();
	return -1;
	}

    ci.logout();
    return 0;
}





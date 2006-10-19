/*******************************************************************************
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *
 *
 *
 * "@(#) $Id: acssampPerfServer.cpp,v 1.6 2006/10/19 15:20:40 rcirami Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * oat       2003-07-18 created
 */


/**
 * This test program is used to demonstrate/test the ACS 
 * sampling system. In particular it samples
 * LAMP1:brightness property with a frequency of 0.1 Hz and
 * report rate 1 sec.
 */


#include <vltPort.h>
#include <acsutil.h>

#include <signal.h>

#include <maciSimpleClient.h>
#include <acssampC.h>
#include <acssampS.h>
#include <baciS.h>
#include <acserr.h>
#include <ACSErrTypeCommon.h>
#include <iostream>


ACE_RCSID(acssampPerfServer, perfServer, "$Id: acssampPerfServer.cpp,v 1.6 2006/10/19 15:20:40 rcirami Exp $")

using namespace std;
 using namespace maci; 
 using namespace ACSErrTypeCommon; 


static int endme = 1;
static void stopLoop (int dummy) { endme=0; }

    
int main(int argc, char *argv[])
{

    ACS::TimeInterval samplingFrequency;
    ACS::TimeInterval reportRate;

    if (argc == 3)
	{
	samplingFrequency=atoll(argv[1]);
	reportRate=atoll(argv[2]);
	}
    else if (argc == 1)
	{
	samplingFrequency=1000000;
	reportRate=10000000;
	}
    else
	{
	ACS_SHORT_LOG((LM_INFO, "usage: acssampOnlyNCServer <sampFrequency> <reportRate>"));
	cout << endl;
	return -1;
	}

    cout << "used value >> samplingFrequency: " << samplingFrequency 
	 << "; reportRate: " << reportRate << endl;

    signal(SIGINT,stopLoop);

    /// Creates and initializes the SimpleClient object
    SimpleClient client;
    if (!client.init(argc,argv))
	{
	return -1;
	}
    else
	{
	client.login();
	}
    
    
    ACS_SHORT_LOG((LM_INFO, "Getting Component"));

    try
	{
	
        // obtain the reference to the SAMP (factory) object
        acssamp::Samp_var foo = client.get_object<acssamp::Samp>("SAMP1", 0, true);

	if (!CORBA::is_nil(foo.in()))
	    {

	    ACS_SHORT_LOG((LM_DEBUG, "Got samp descriptor()."));

	    // calls the initSampObj to create dynamically a new sampling object
	    acssamp::SampObj_ptr fooNew = 
		foo->initSampObj("LAMP1","brightness",samplingFrequency,reportRate);

	    ACS_SHORT_LOG((LM_INFO,"*** Start to sample ***"));

	    ACS_SHORT_LOG((LM_INFO,"Not Channel: %s",fooNew->getChannelName()));
	  
	  
	    ACS_SHORT_LOG((LM_INFO,"Sleeping 15 seconds to allow NC Client connection ..."));
	    ACE_OS::sleep(15);
	    ACS_SHORT_LOG((LM_INFO," ... done"));


	    // starts the sampling
	    fooNew->start();

	    cout << "Infinite loop started; press Ctrl-C to stop it ..." << endl;
	    while(endme)
		ACE_OS::sleep(1);
	    cout << "... out of the loop!" << endl;

	    // stop and clen-up everything
	    fooNew->stop();
	    ACE_OS::sleep(2);
	    fooNew->destroy();

	    CORBA::release(fooNew);

	    }	
	} /* end main try */
    catch (OutOfBoundsEx & ex)
	{   
	ACS_SHORT_LOG((LM_INFO, "OutOfBoundsEx exception catched !"));
	OutOfBoundsExImpl err(ex);
	err.log();
	}
    catch (CouldntAccessComponentEx & ex)
	{   
	ACS_SHORT_LOG((LM_INFO, "CouldntAccessComponentEx exception catched !"));
	CouldntAccessComponentExImpl err(ex);
	err.log();
	}
    catch (CouldntAccessPropertyEx & ex)
	{   
	ACS_SHORT_LOG((LM_INFO, "CouldntAccessPropertyEx exception catched !"));
	CouldntAccessPropertyExImpl err(ex);
	err.log();
	}
   catch (CouldntCreateObjectEx & ex)
	{   
	ACS_SHORT_LOG((LM_INFO, "CouldntCreateObjectEx exception catched !"));
	CouldntCreateObjectExImpl err(ex);
	err.log();
	}
   catch (TypeNotSupportedEx & ex)
	{   
	ACS_SHORT_LOG((LM_INFO, "TypeNotSupportedEx exception catched !"));
	TypeNotSupportedExImpl err(ex);
	err.log();
	}
    catch (...)
	{
	ACS_SHORT_LOG((LM_INFO, "XXXXXXXX Exception ... catched !"));
//	ACE_PRINT_EXCEPTION (ACE_ANY_EXCEPTION, "main");
	}


    /// We release our Ccomponent and logout from the Manager
    client.manager()->release_component(client.handle(), "SAMP1");
    client.logout();
    
    
    /// sleep for 3 sec to allow everytihng to cleanup and stableize
    ACE_OS::sleep(3);
    
    return 0;
}

/*___oOo___*/



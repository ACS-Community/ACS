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
* "@(#) $Id: acsexmplTestMemory.cpp,v 1.88 2005/09/21 13:15:22 vwang Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-06-17 fixed client.init(argc,argv)
* gchiozzi 2002-02-13 created
*/

/**
 * This test program is used to investigate memory leaks and
 * maciManager and maciContainer stability.
 * It gets on the command line the number of time some basic calls
 * used to retrieve components -> property -> property value
 * are executed in a loop:
 *	# get_object
 *      # mount->actAz
 *      # get_sync
 */

#include <acsutil.h>

#include <maciSimpleClient.h>
#include <acsexmplMountC.h>
#include <acsutil.h>

using namespace maci;

/*
 *  The test main creates two loops that run in parallel threads
 */
    int main(int argc, char **argv)
{
    int repeat[3] = { 1, 1, 1 };

    /*
     * Checks command line arguments.
     * We need the name of the component to get in touch with.
     */
    if (argc < 5)
	{
	ACS_SHORT_LOG((LM_INFO, "Usage: %s <Mount name> <# get_object> "
		       "<# mount->actAz> <# get_sync> <options>", argv[0]));
	return -1;
	}

    /*
     * Creates and initialyses the SimpleClient object
     */
    SimpleClient client;

    if (client.init(argc,argv) == 0)
	{
	ACE_DEBUG((LM_DEBUG,"Cannot init client"));
	return -1;
	}
    ACS_SHORT_LOG((LM_INFO, "Wellcome to %s!", argv[0]));
    ACS_SHORT_LOG((LM_INFO, "Login into maciManager!"));
    client.login();

    
    
    try
	{
	/**
	 * Parsing command line options
	 */
	repeat[0]  = atoi(argv[2]);
	repeat[1]  = atoi(argv[3]);
	repeat[2]  = atoi(argv[4]);

	ACS_SHORT_LOG ((LM_INFO, "Command line options:"));
	ACS_SHORT_LOG ((LM_INFO, "\t# get_object  : %d",repeat[0]));
	ACS_SHORT_LOG ((LM_INFO, "\t# mount->actAz: %d",repeat[1]));
	ACS_SHORT_LOG ((LM_INFO, "\t# get_sync    : %d",repeat[2]));

	    
	/**
	 * Get reference to Mount device
	 */
        CORBA::Object_var object;
	MOUNT_ACS::Mount_var mDev;
	for(int i=0; i<repeat[0]; i++)
	    {
	    object = client.get_object(argv[1],"",true);

	    ACS_SHORT_LOG((LM_INFO,"Got object"));

	    if (CORBA::is_nil(object.in()) )
		{
		ACS_SHORT_LOG ((LM_INFO, "Cannot get Object"));
		return -1;
		}

	    mDev = MOUNT_ACS::Mount::_narrow(object.in ());
	    
	    ACS_SHORT_LOG((LM_INFO, "MOUNT device narrowed"));
	    if( CORBA::is_nil(mDev.in()) )
		{
		ACS_SHORT_LOG((LM_INFO, "Failed to narrow Device :-("));
		return -2;
		}
	    ACS_SHORT_LOG((LM_INFO, "Device narrowed."));
	    }


	/*
	 * Get a reference to the property
	 */

        ACS::ROdouble_var ROactAz;
	for(int i=0; i<repeat[1]; i++)
	    {
	    ROactAz = mDev->actAz();
	    
	    ACS_SHORT_LOG((LM_INFO," Got property.."));
	    }
	
        /*
	 * Now get its value
	 */
	ACSErr::Completion_var c;
	double actAz;
	for(int i=0; i<repeat[2]; i++)
	    {
	    actAz = ROactAz->get_sync(c);
	    ACS_SHORT_LOG((LM_INFO," Property value = %6.3f",actAz));
	    }

	/*
	 * Now release the component
	 */
	ACS_SHORT_LOG((LM_INFO,"Releasing..."));
	client.manager()->release_component(client.handle(), argv[1]);
	

	}
    catch( CORBA::Exception &ex )
	{
	ACE_PRINT_EXCEPTION (ex,"Error!");
	return -1;
	}
    
    ACS_SHORT_LOG((LM_INFO,"The end"));
	
    return 0;
}







/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) Associated Universities Inc., 2002
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
* "@(#) $Id: acsutilORBHelper.cpp,v 1.2 2006/01/09 18:52:17 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david  20/09/02  created 
*/
//-----------------------------------------------------------------------------
#include "acsutilORBHelper.h"
#include <acsutilPorts.h>
#include <acscommonC.h>
#include <tao/PortableServer/PortableServer.h>
//-----------------------------------------------------------------------------
CORBA::ORB_ptr ORBHelper::orb_mp = 0;
//-----------------------------------------------------------------------------
ORBHelper::ORBHelper() :
    threadManager_mp(0),
    orbRunYet_m(false)
{
    //create a new ACE thread manager
    threadManager_mp = ACE_Thread_Manager::instance();
    
    //CORBA naming service corbaloc
    ACE_CString nameService;
    
    //get NameService Reference
    nameService += acscommon::NAMING_SERVICE_NAME;
    nameService +="=corbaloc::";
    nameService += ACSPorts::getIP();
    nameService += ":"; 
    nameService += ACSPorts::getNamingServicePort().c_str();
    nameService += "/"; 
    nameService += acscommon::NAMING_SERVICE_NAME;
    
    // initialize ORB
    int argc = 5;
    char* argv[] = { "", 
		     "-ORBInitRef",
		     (char *)nameService.c_str(),
		     "-ORBDottedDecimalAddresses",
		     "1"};
    
    //initialize the ORB
    orb_mp = CORBA::ORB_init(argc, argv, "");
    //ensure it's a real reference
    ACE_ASSERT(!CORBA::is_nil(orb_mp));
	    
    //get the Root POA
    CORBA::Object_var poaObject = orb_mp->resolve_initial_references("RootPOA");
    //ensure it's a real reference
    ACE_ASSERT(!CORBA::is_nil(poaObject.in()));
    //narrow the Root POA
    PortableServer::POA_var rootPOA = PortableServer::POA::_narrow(poaObject.in());
    //ensure it's a real reference
    ACE_ASSERT(!CORBA::is_nil(rootPOA.in()));
    
    //get the POA manager
    PortableServer::POAManager_var poaManager = rootPOA->the_POAManager();
    //ensure it's a real reference
    ACE_ASSERT(!CORBA::is_nil(poaManager.in()));
    
    //finally activate the POA manager to start processing incoming and
    //outgoing calls
    poaManager->activate();

    //spawn a separate thread to start the ORB in
    ACE_ASSERT(threadManager_mp!=0);
    threadManager_mp->spawn(ACE_THR_FUNC(ORBHelper::runOrbThread), static_cast<void *>(this));

    //block until the ORB thread has really started!
    while(orbRunYet_m==false)
	{
	ACE_OS::sleep(1);
	}
    ACE_OS::sleep(1);
}
//-----------------------------------------------------------------------------
CORBA::ORB_ptr 
ORBHelper::getORB() 
{
    if(orb_mp == 0)
	{
	//container/client has not set the static member using setORB.
	//OK, we'll create our own ORB then implicitly.
	ORBHelperSingleton::Instance();
	}

    return orb_mp; 
}
//-----------------------------------------------------------------------------
void
ORBHelper::setORB(CORBA::ORB_ptr orb_p)
{
    //if it's been set before we just ignore the call
    if(orb_mp != 0)
	{
	return;
	}
    orb_mp = orb_p;

}
//-----------------------------------------------------------------------------
ORBHelper::~ORBHelper()
{
    //seeing as both the destructor and constructor are protected, this
    //check shouldn't be necessary but just to be on the safe side...
    if ((threadManager_mp!=0) && (orb_mp != 0))
	{
	//TODO - the following two lines of code should work but
	//do not. Probably TAO's ORB is a singleton itself and is 
	//destroyed long before this destructor is called. valgrind
	//does not show any memory leaks so it it probably safe
	//as-is.
	//shutdown the ORB and block until this is really done.
	//orb_mp->shutdown(true);
	//destroy it
	//orb_mp->destroy();
	//make it possible to resurrect everything (because we use Loki 
	//PhonenixSingletons.
	orb_mp=0;
	threadManager_mp=0;
	}
}
//-----------------------------------------------------------------------------
void
ORBHelper::runOrbThread(void *this_p)
{
    ACE_ASSERT(this_p!=0); 
    
    // Variables have to be passed explicitly
    ORBHelper *helper_p = static_cast<ORBHelper *>(this_p);
        
    //Run the orb. This is a blocking call.
    helper_p->orbRunYet_m = true;
    helper_p->orb_mp->run();
}
//-----------------------------------------------------------------------------
/*___oOo___*/

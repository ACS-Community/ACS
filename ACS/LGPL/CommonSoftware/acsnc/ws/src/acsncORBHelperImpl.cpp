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
* "@(#) $Id: acsncORBHelperImpl.cpp,v 1.61 2008/10/09 07:57:41 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david  20/09/02  created 
*/
//-----------------------------------------------------------------------------
#include "acsncORBHelper.h"
#include <acsutilPorts.h>
#include "acsncC.h"
#include <acscommonC.h>
//-----------------------------------------------------------------------------
 using namespace baci;
 using namespace ACSErrTypeCommon;
//-----------------------------------------------------------------------------
namespace nc {
//-----------------------------------------------------------------------------
ORBHelper::ORBHelper() :
    orb_mp(0),
    threadManager_mp(0)
{
    ACS_TRACE("ORBHelper::ORBHelper");
    threadManager_mp = new BACIThreadManager();
    init_ORB();
}
//-----------------------------------------------------------------------------
ORBHelper::ORBHelper(int argc, char *argv[]) :
    orb_mp(0),
    threadManager_mp(0)
{
    ACS_TRACE("ORBHelper::ORBHelper");
    threadManager_mp = new BACIThreadManager();
    init_ORB(argc, argv);
}
//-----------------------------------------------------------------------------
ORBHelper::~ORBHelper()
{
    ACS_TRACE("ORBHelper::~ORBHelper");
    
    try
	{
	if (orb_mp != 0)
	    {
	    orb_mp->shutdown(true);
	    orb_mp->destroy();
	    orb_mp=0;
	    }
	
	if (threadManager_mp != 0)
	    {
	    threadManager_mp->terminateAll();
	    delete threadManager_mp;
	    threadManager_mp=0; 
	    }
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR, "ORBHelper::~ORBHelper destructor failed!"));
	}
}
//-----------------------------------------------------------------------------
void
ORBHelper::init_ORB()
{
    ACS_TRACE("ORBHelper::init_ORB");

    //get IP
    ACE_CString nameService;
    
    //get NameService Reference
    nameService += acscommon::NAMING_SERVICE_NAME;
    nameService +="=corbaloc::";
    nameService += maci::MACIHelper::getManagerHostname(1, NULL);
    nameService += ":"; 
    nameService += ACSPorts::getNamingServicePort().c_str();
    nameService += "/"; 
    nameService += acscommon::NAMING_SERVICE_NAME;
    
    // initialize ORB
    int argc = 5;
    const char* orbArgs[] = { "", 
			      "-ORBInitRef",
			      nameService.c_str(),
			      "-ORBDottedDecimalAddresses",
                              "1"};
    
    init_ORB(argc,const_cast<char**>(orbArgs));
}
//-----------------------------------------------------------------------------
void 
ORBHelper::init_ORB(int argc, char *argv[])
{
    ACS_TRACE("ORBHelper::init_ORB");
    
    try
	{
	//initialize the ORB
	orb_mp = CORBA::ORB_init(argc, argv, "");
	//ensure it's a real reference
	if((orb_mp == 0)||(CORBA::is_nil(orb_mp) == true))
	    {
	    ACS_SHORT_LOG((LM_ERROR,"ORBHelper::init_ORB unable to create ORB!"));
	    CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::ORBHelper::init_ORB");
	    throw err.getCORBAProblemEx();
	    }
	
	//get the Root POA
	CORBA::Object_var poaObject = orb_mp->resolve_initial_references("RootPOA");
	//ensure it's a real reference
	if (CORBA::is_nil(poaObject.in()) == true)
	    {
	    ACS_SHORT_LOG((LM_ERROR,"ORBHelper::init_ORB unable to initialize RootPOA object!"));
	    CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::ORBHelper::init_ORB");
	    throw err.getCORBAProblemEx();
	    }
	//narrow the Root POA
	PortableServer::POA_var rootPOA = PortableServer::POA::_narrow(poaObject.in());
	if (rootPOA.in() == PortableServer::POA::_nil())
	    {
	    ACS_SHORT_LOG((LM_ERROR,"ORBHelper::init_ORB unable to narrow RootPOA object!"));
	    CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::ORBHelper::init_ORB");
	    throw err.getCORBAProblemEx();
	    }
	
	//get the POA manager
	PortableServer::POAManager_var poaManager = rootPOA->the_POAManager();
	//ensure it's a real reference
	if(poaManager.in()==PortableServer::POAManager::_nil())
	    {
	    ACS_SHORT_LOG((LM_ERROR,"ORBHelper::init_ORB unable to reference POAManager object!"));
	    CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::ORBHelper::init_ORB");
	    throw err.getCORBAProblemEx();
	    }
	
	//finally activate the POA manager to start processing incoming and
	//outgoing calls
	poaManager->activate();
	}
    catch(CORBAProblemExImpl)
	{
	ACS_SHORT_LOG((LM_TRACE,"ORBHelper::init_ORB encountered a nil pointer!"));
	//was thrown by ORBHelper code...OK to rethrow
	throw;
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR, "ORBHelper::init_ORB(...) unable to initialize the ORB!"));
	CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::ORBHelper::init_ORB");
	throw err.getCORBAProblemEx();
	}
}
//-----------------------------------------------------------------------------
void 
ORBHelper::runOrb()
{
    ACS_TRACE("ORBHelper::runOrb");

    if(threadManager_mp == 0)
	{
	ACS_SHORT_LOG((LM_ERROR,"ORBHelper::runORB bad thread manager!"));
	CouldntCreateThreadExImpl err = CouldntCreateThreadExImpl(__FILE__,__LINE__,"nc::ORBHelper::runORB");
	throw err.getCouldntCreateThreadEx();
	}
    threadManager_mp->create("runOrbThread", (void *)ORBHelper::runOrbThread, static_cast<void *>(this));
    threadManager_mp->resume("runOrbThread");
    
    ACS_SHORT_LOG((LM_INFO,"ORBHelper::runORB the ORB is now running!"));
    return;   
}
//-----------------------------------------------------------------------------
void* 
ORBHelper::runOrbThread(void *this_p)
{
    ACS_TRACE("ORBHelper::runOrbThread");

    try
	{
	if (this_p == 0)
	    {
	    ACS_SHORT_LOG((LM_ERROR,"ORBHelper::runORBThread bad parameter passed!"));
	    return static_cast<void *>(0);
	    }
	
	BACIThreadParameter *baciParameter_p = static_cast<BACIThreadParameter *>(this_p);
	BACIThread *myself_p = baciParameter_p->getBACIThread();
	
	
	// Variables have to be passed explicitly
	ORBHelper *helper_p = static_cast<ORBHelper *>(const_cast<void *>(baciParameter_p->getParameter()));
	
	
	// Actually start the thread here
	if (BACIThread::InitThread != 0) 
	    {
	    BACIThread::InitThread("runOrbThread");
	    }
	
	
	//this should cause only one execution of helper_p->orb_mp->run()
	bool execute=true;
	while((myself_p->check() == true) && (execute == true) )
	    {
	    if(myself_p->isSuspended() == false)
		{
		helper_p->orb_mp->run();
		ACS_SHORT_LOG((LM_INFO,"ORBHelper::runOrbThread the stand alone orb has stopped."));
		execute=false;
		}
	    //well this almost certainly will never be called but just be
	    //on the safe side of things
	    myself_p->sleep();
	    }
	
	if (BACIThread::DoneThread != 0) 
	    {
	    BACIThread::DoneThread();
	    }
	
	delete baciParameter_p;
	myself_p->setStopped();
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR, "ORBHelper::runOrbThread failed!"));
	return static_cast<void *>(0);
	}
    return static_cast<void *>(0);
}
//-----------------------------------------------------------------------------
 }; 
/*___oOo___*/




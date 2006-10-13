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
* "@(#) $Id: acsexmplClientErrorComponent.cpp,v 1.5 2006/10/13 14:04:27 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-10-31 created
*/

/** @file acsexmplClientErrorComponent.cpp
 *  @htmlonly
 *  <br><br>
    @endhtmlonly
 *  @param "component name" Use this required parameter to specify which component
 *  should be activated.
 *  @htmlonly
    <br><hr>
    @endhtmlonly
 *  @param "-ORBEndpoint iiop://yyy:xxxx" Use this optional parameter to specify which host/port SimpleClient
 *  should run on.
 *  @htmlonly
    <br><hr>
    @endhtmlonly
 *  @param "-m corbaloc::yyy:xxxx/Manager" Use this optional parameter to specify where
 *  manager is.
 *  @htmlonly
    <br><hr>
    @endhtmlonly
 */

/** @defgroup ACSEXMPLCLIENTERROROCOMPDOC Client Error Component
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
This example shows a client that:
<ul>
  <li>logs into manager via SimpleClient</li>
  <li>activates the ErrorComponent component specified from the command-line</li>
  <li>calls the methods in this component to show examples of error handling</li>
  <li>releases the component</li>
  <li>logs out of manager</li>
</ul>
<br>
Error handling examples are encapsulated in the ClientErrorComponent class.
Each method in the class shows an example.
<br>
<h2>What can I gain from this example?</h2>
<ul>
  <li>SimpleClient usage.</li>
  <li>Dealing with errors accessing (remote) components.</li>
</ul>
<br>
<br>
<h2>Links</h2>
<ul>
  <li><a href="acsexmplClientErrorComponent_8cpp.html">Hello World Client File Reference</a></li>
</ul>
</div>
   @endhtmlonly
 * @}
 */


#include <maciSimpleClient.h>
#include <acsexmplErrorComponentC.h>
#include <ACSErrTypeCommon.h>
#include <acsutilTimeStamp.h>
#include <string.h>

ACE_RCSID(acsexmpl, acsexmplErrorComponentClient, "$Id: acsexmplClientErrorComponent.cpp,v 1.5 2006/10/13 14:04:27 bjeram Exp $")
using namespace maci;

/*******************************************************************************/

/**
 * This class demonstrates error handling when accessing a (remote)
 * component.
 * 
 * When the class is instantiated, it gets a reference to the 
 * ErrorComponent counterpart throw the manager services provided
 * by the SimpleClient.
 * When the destructor is called, the reference to the component is released.
 * This strategy ensures a clean handling of references to Components.
 *
 * Each of the public methods of the class demonstrates some error handling pattern.
 * Each method is self-contained and it should never throw exceptions in itself,
 * but for ACSErrTypeCommon::CouldntAccessComponentEx, to be used when the
 * reference to the component is not properly initialised.
 * Other errors are always completely handled internally.
 */
class ClientErrorComponent
{
  public:
    ClientErrorComponent(SimpleClient &client, char *errorComponent) 
	throw(CORBA::SystemException, ACSErrTypeCommon::CouldntAccessComponentExImpl);
    virtual ~ClientErrorComponent();

    /**
     * Here everything should go fine.
     */
    void TestOk() throw(ACSErrTypeCommon::CouldntAccessComponentExImpl);

    /**
     * Example 1: Calls a method that throws an exception
     *            with an error trace..
     * <ul>
     *   <li> Catches the exception, 
     *   <li> Adds context information
     *   <li> sends it to the logging system
     * </ul>
     */
    void TestReceiveRemoteException() throw(ACSErrTypeCommon::CouldntAccessComponentExImpl);

    /**
     * Example 2: Calls a method that returns a completion
     *            If the completion contains an error, then
     * <ul>
     *   <li> Catches the exception, 
     *   <li> prints it locally 
     *   <li> sends it to the logging system
     * </ul>
     */
    void TestReceiveRemoteCompletion() throw(ACSErrTypeCommon::CouldntAccessComponentExImpl);

    /**
     * Example 3: Calls a method that throws a CORBA system exception
     *            It:
     * <ul>
     *   <li> Catches the exception, 
     *   <li> prints it locally 
     *   <li> sends it to the logging system
     * </ul>
     */
    void TestReceiveCorbaSystemException() throw(ACSErrTypeCommon::CouldntAccessComponentExImpl);

  private:
    SimpleClient &client_m;
    std::string   errorComponent_m;
    acsexmplErrorComponent::ErrorComponent_var foo_m;
};

ClientErrorComponent::ClientErrorComponent(SimpleClient &client, 
					   char *errorComponent)
    throw(CORBA::SystemException, ACSErrTypeCommon::CouldntAccessComponentExImpl): 
    client_m(client), errorComponent_m(errorComponent)
{
    ACS_TRACE("ClientErrorComponent::ClientErrorComponent");

    try
	{
    /*
     * Get the specific component we have requested
     * Add exception handling.
     */

    foo_m = 
	client_m.getComponent<acsexmplErrorComponent::ErrorComponent>(errorComponent_m.c_str(), 0, true);    
	}
    catch(maciErrType::CannotGetComponentExImpl &_ex)
	{
	throw ACSErrTypeCommon::CouldntAccessComponentExImpl(_ex,
							     __FILE__, __LINE__,
							     "ClientErrorComponent::ClientErrorComponent");
	
	}//try-catch
}//ClientErrorComponent

ClientErrorComponent::~ClientErrorComponent()
{ 
    ACS_TRACE("ClientErrorComponent::~ClientErrorComponent");

    try
	{
   /*
     * Release the component
     */
    client_m.releaseComponent( errorComponent_m.c_str());
	}
    catch(maciErrType::CannotReleaseComponentExImpl &_ex)
	{
	_ex.log();
	}
}//~ClientErrorComponent

void ClientErrorComponent::TestOk() 
    throw(ACSErrTypeCommon::CouldntAccessComponentExImpl)
{
    ACS_TRACE("ClientErrorComponent::TestOk");
    if (CORBA::is_nil(foo_m.in()) == true)
	{
	throw ACSErrTypeCommon::CouldntAccessComponentExImpl(
				   __FILE__, __LINE__,
				   "ClientErrorComponent::TestOk");
	}

    try
	{
	foo_m->displayMessage();
	}
    catch(CORBA::SystemException &ex)
	{
	// Map......
	ACSErrTypeCommon::GenericErrorExImpl displayMessageEx(
				   __FILE__, __LINE__,
				   "ClientErrorComponent::TestReceiveRemoteException");
	displayMessageEx.setErrorDesc("badMethod has thrown a CORBA exception");
	displayMessageEx.log();
	}
    catch(...)
	{
	ACSErrTypeCommon::GenericErrorExImpl displayMessageEx(__FILE__, __LINE__,
		      				 "ClientErrorComponent::TestReceiveRemoteException");
	displayMessageEx.setErrorDesc("badMethod has thrown an UNEXPECTED exception");
	displayMessageEx.log();
	}
}

void ClientErrorComponent::TestReceiveRemoteException() 
    throw(ACSErrTypeCommon::CouldntAccessComponentExImpl)
{
    ACS_TRACE("ClientErrorComponent::TestReceiveRemoteException");

    if (CORBA::is_nil(foo_m.in()) == true)
	{
	throw ACSErrTypeCommon::CouldntAccessComponentExImpl(
				   __FILE__, __LINE__,
				   "ClientErrorComponent::TestReceiveRemoteException");
	}
    ACS_SHORT_LOG((LM_INFO, "Example 1: Calls a method that throws an exception."));
    try
	{
	foo_m->badMethod(3);
	}
    catch(ACSErrTypeCommon::GenericErrorEx &ex)
	{
	ACSErrTypeCommon::GenericErrorExImpl badMethodEx(ex,
				   __FILE__, __LINE__,
				   "ClientErrorComponent::TestReceiveRemoteException");
	badMethodEx.setErrorDesc("badMethod has thrown the expected exception");
	badMethodEx.log();

	ACS::Time timeStamp = badMethodEx.getTimeStamp();
	ACE_CString tString = getStringifiedUTC(timeStamp);
	ACS_DEBUG_PARAM("ClientErrorComponent::TestReceiveRemoteException", 
			"Time of the exception: %s\n", tString.c_str());
	}
    catch(CORBA::SystemException &ex)
	{
	// Map......
	ACSErrTypeCommon::GenericErrorExImpl badMethodEx(
				   __FILE__, __LINE__,
				   "ClientErrorComponent::TestReceiveRemoteException");
	badMethodEx.setErrorDesc("badMethod has thrown a CORBA exception");
	badMethodEx.log();

	ACS::Time timeStamp = badMethodEx.getTimeStamp();
	ACE_CString tString = getStringifiedUTC(timeStamp);
	ACS_DEBUG_PARAM("ClientErrorComponent::TestReceiveRemoteException", 
			"Time of the CORBA exception: %s\n", tString.c_str());
	}
    catch(...)
	{
	ACSErrTypeCommon::GenericErrorExImpl badMethodEx(__FILE__, __LINE__,
		      				 "ClientErrorComponent::TestReceiveRemoteException");
	badMethodEx.setErrorDesc("badMethod has thrown an UNEXPECTED exception");
	badMethodEx.log();

	ACS::Time timeStamp = badMethodEx.getTimeStamp();
	ACE_CString tString = getStringifiedUTC(timeStamp);
	ACS_DEBUG_PARAM("ClientErrorComponent::TestReceiveRemoteException", 
			"Time of the unexpected exception: %s\n", tString.c_str());
	}

}

void ClientErrorComponent::TestReceiveRemoteCompletion() 
    throw(ACSErrTypeCommon::CouldntAccessComponentExImpl)
{
    ACS_TRACE("ClientErrorComponent::TestReceiveRemoteCompletion");

    if (CORBA::is_nil(foo_m.in()) == true)
	{
	throw ACSErrTypeCommon::CouldntAccessComponentExImpl(
				   __FILE__, __LINE__,
				   "ClientErrorComponent::TestReceiveRemoteCompletion");
	}
    try
	{
	CompletionImpl comp;

	// OK Completion
	ACS_SHORT_LOG((LM_INFO, "Example 2a: Calls a method that returns an OK completion."));
	comp = foo_m->completionFromException(0);
	comp.log();

	// ERROR completion with an error trace inside.
	ACS_SHORT_LOG((LM_INFO, "Example 2b: Calls a method that returns an Error completion."));
	comp = foo_m->completionFromException(3);
	comp.log();
	}
    catch(CORBA::SystemException &ex)
	{
	// Map......
	ACSErrTypeCommon::GenericErrorExImpl ex(
				   __FILE__, __LINE__,
				   "ClientErrorComponent::TestReceiveRemoteCompletion");
	ex.setErrorDesc("completionFromException has thrown a CORBA exception");
	ex.log();
	}
    catch(...)
	{
	ACSErrTypeCommon::GenericErrorExImpl ex(__FILE__, __LINE__,
							 "ClientErrorComponent::TestReceiveRemoteCompletion");
	ex.setErrorDesc("completionFromException has thrown an UNEXPECTED exception");
	ex.log();
	}
}

void ClientErrorComponent::TestReceiveCorbaSystemException() 
    throw(ACSErrTypeCommon::CouldntAccessComponentExImpl)
{
    ACS_TRACE("ClientErrorComponent::TestReceiveCorbaSystemException");

    if (CORBA::is_nil(foo_m.in()) == true)
	{
	throw ACSErrTypeCommon::CouldntAccessComponentExImpl(
				   __FILE__, __LINE__,
				   "ClientErrorComponent::TestReceiveRemoteException");
	}
    ACS_SHORT_LOG((LM_INFO, "Example 3: Calls a method that throws a CORBA System Exception."));
    try
	{
	foo_m->corbaSystemException();
	}
    catch(CORBA::SystemException &ex)
	{
	// This show how to map a CORBA System exception from TAO
        // in the ACS wrapper ACSErrTypeCommon::CORBAProblemExImpl.
        /**
	 * @todo Implement a real wrapper exception class, 
	 *        to make the conversion transparent 
	 */
	ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(
				   __FILE__, __LINE__,
				   "ClientErrorComponent::TestReceiveRemoteException");
	corbaProblemEx.setMinor(ex.minor());
	corbaProblemEx.setCompletionStatus(ex.completed());
	corbaProblemEx.setInfo(ex._info().c_str());
	corbaProblemEx.log();

	ACS::Time timeStamp = corbaProblemEx.getTimeStamp();
	ACE_CString tString = getStringifiedUTC(timeStamp);
	ACS_DEBUG_PARAM("ClientErrorComponent::TestReceiveRemoteException", 
			"Time of the CORBA::SystemException exception: %s\n", tString.c_str());
	}
    catch(...)
	{
	ACSErrTypeCommon::GenericErrorExImpl badMethodEx(__FILE__, __LINE__,
		      				 "ClientErrorComponent::TestReceiveRemoteException");
	badMethodEx.setErrorDesc("corbaSystemException has thrown an UNEXPECTED exception");
	badMethodEx.log();

	ACS::Time timeStamp = badMethodEx.getTimeStamp();
	ACE_CString tString = getStringifiedUTC(timeStamp);
	ACS_DEBUG_PARAM("ClientErrorComponent::TestReceiveRemoteException", 
			"Time of the unexpected exception: %s\n", tString.c_str());
	}

}


/*******************************************************************************/

int main(int argc, char *argv[])
{
    // Creates and initializes the SimpleClient object
    SimpleClient client;
    if (client.init(argc,argv) == 0)
	{
	return -1;
	}
    else
	{
	//Must log into manager before we can really do anything
	client.login();
	}

    /**************************************************************************
     * Here we instantiate the object used to show examples of error handling.
     * Each method call demonstrate one aspect of error hanlding.
     * See the class documentation for details.
     */
    try
	{
	ClientErrorComponent clientErrorComponent(client, argv[1]);
	
	//Call the displayMessage() method existing in the interface for ErrorComponent
	clientErrorComponent.TestOk();
	clientErrorComponent.TestReceiveRemoteException();
	clientErrorComponent.TestReceiveRemoteCompletion();
	clientErrorComponent.TestReceiveCorbaSystemException();
	}
    catch(ACSErr::ACSbaseExImpl ex)
	{
	/*
	 * We should never get here, because the methods in the example
	 * should be all self contained and none of them should throw
	 * any exception.
	 */
	ACSErrTypeCommon::GenericErrorExImpl badMethodEx(ex, __FILE__, __LINE__,
							 "main");
	badMethodEx.setErrorDesc("Examples of error handling have thrown an ACS exception");
	badMethodEx.log();
	}
    catch(...)
	{
	/*
	 * We should never get here, because the methods in the example
	 * should be all self contained and none of them should throw
	 * any exception.
	 */
	ACSErrTypeCommon::GenericErrorExImpl badMethodEx(__FILE__, __LINE__,
							 "main");
	badMethodEx.setErrorDesc("Examples of error handling have thrown an UNEXPECTED exception");
	badMethodEx.log();
	}

    /****************************************************
     * We logout from manager
     */
    client.logout();
    
    //Sleep for 3 sec to allow everything to cleanup and stablize
    ACE_OS::sleep(3);
    return 0;
}

/*___oOo___*/




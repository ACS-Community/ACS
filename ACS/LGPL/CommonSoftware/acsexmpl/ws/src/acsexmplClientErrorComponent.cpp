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
* "@(#) $Id: acsexmplClientErrorComponent.cpp,v 1.13 2008/10/09 08:41:11 cparedes Exp $"
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

/** @addtogroup ACSEXMPLTOC
*/
/*@{
*/

/** @addtogroup ACSEXMPLTOCCLIENTS
*/
/*@{
*/

/** @defgroup ACSEXMPLCLIENTERROROCOMPDOC Client Error Component
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
Client that uses the Error Component to catch exception/completion and manage them.
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

ACE_RCSID(acsexmpl, acsexmplErrorComponentClient, "$Id: acsexmplClientErrorComponent.cpp,v 1.13 2008/10/09 08:41:11 cparedes Exp $")
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
    /**
     * @throw ACSErrTypeCommon::CouldntAccessComponentExImpl
     */
    ClientErrorComponent(SimpleClient &client, char *errorComponent) ;
    virtual ~ClientErrorComponent();

    /**
     * Here everything should go fine.
     * @throw ACSErrTypeCommon::CouldntAccessComponentExImpl
     */
    void TestOk();

    /**
     * Example 1: Calls a method that throws an exception
     *            with an error trace..
     * <ul>
     *   <li> Catches the exception, 
     *   <li> Adds context information
     *   <li> sends it to the logging system
     * </ul>
     * @throw ACSErrTypeCommon::CouldntAccessComponentExImpl
     */
    void TestReceiveRemoteException() ;

    /**
     * Example 2: Calls a method that returns a completion
     *            If the completion contains an error, then
     * <ul>
     *   <li> Catches the exception, 
     *   <li> prints it locally 
     *   <li> sends it to the logging system
     * </ul>
     * @throw ACSErrTypeCommon::CouldntAccessComponentExImpl
     */
    void TestReceiveRemoteCompletion();

    /**
     * Example 3: Calls a method that throws an exception
     *            with an error trace..
     * <ul>
     *   <li> Catches the exception, 
     *   <li> Adds context information
     *   <li> sends it to the logging system
     * </ul>
     * @throw ACSErrTypeCommon::CouldntAccessComponentExImpl
     */
    void testExceptionFromCompletion();

    /**
     * Example 4: Calls a method that throws an exception 
     *            with an error trace..
     * <ul>
     *   <li> Catches the exception, 
     *   <li> Adds context information
     *   <li> sends it to the logging system
     * </ul>
     * @throw ACSErrTypeCommon::CouldntAccessComponentExImpl
     */
    void testTypeException();
    /**
     * Example 5: Calls a method that throws a CORBA system exception
     *            It:
     * <ul>
     *   <li> Catches the exception, 
     *   <li> prints it locally 
     *   <li> sends it to the logging system
     * </ul>
     * @throw ACSErrTypeCommon::CouldntAccessComponentExImpl
     */
    void TestReceiveCorbaSystemException();

    /**
     * Example 6: Calls a method that returns a completion
     *            If the completion contains an error, then
     * <ul>
     *   <li> Catches the exception,
     *   <li> prints it locally
     *   <li> sends it to the logging system
     * </ul>
     * @throw ACSErrTypeCommon::CouldntAccessComponentExImpl
     */
    void testCompletionFromCompletion(); 

    /**
     * Example 6: Calls a method that returns a completion created on the stack instead of heap
     *            If the completion contains an error, then
     * <ul>
     *   <li> Catches the exception,
     *   <li> prints it locally
     *   <li> sends it to the logging system
     * </ul>
     * @throw ACSErrTypeCommon::CouldntAccessComponentExImpl
     */
    void testCompletionOnStack(); 


   /**
     * Example 7: Calls a method that returns a completion as an "out" parameter.
     * @throw ACSErrTypeCommon::CouldntAccessComponentExImpl
     */
    void testOutCompletion(); 

  private:
    SimpleClient &client_m;
    std::string   errorComponent_m;
    acsexmplErrorComponent::ErrorComponent_var foo_m;
};

   /**
    * @throw ACSErrTypeCommon::CouldntAccessComponentExImpl
    */
ClientErrorComponent::ClientErrorComponent(SimpleClient &client, 
					   char *errorComponent): 
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

   /**
    * @throw ACSErrTypeCommon::CouldntAccessComponentExImpl
    */
void ClientErrorComponent::TestOk() 
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
	ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(
				   __FILE__, __LINE__,
				   "ClientErrorComponent::TestOk");
	corbaProblemEx.setMinor(ex.minor());
	corbaProblemEx.setCompletionStatus(ex.completed());
	corbaProblemEx.setInfo(ex._info().c_str());
	corbaProblemEx.log();
	}
    catch(...)
	{
	ACSErrTypeCommon::GenericErrorExImpl displayMessageEx(__FILE__, __LINE__,
		      				 "ClientErrorComponent::TestReceiveRemoteException");
	displayMessageEx.setErrorDesc("UNEXPECTED: displayMessage has thrown an UNEXPECTED exception");
	displayMessageEx.log();
	}
}

void ClientErrorComponent::TestReceiveRemoteException() 
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
	foo_m->badMethod(5);
        ACS_SHORT_LOG((LM_INFO, "UNEXPECTED: should have thrown an exception"));
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
	ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(
				   __FILE__, __LINE__,
				   "ClientErrorComponent::TestReceiveRemoteException");
	corbaProblemEx.setMinor(ex.minor());
	corbaProblemEx.setCompletionStatus(ex.completed());
	corbaProblemEx.setInfo(ex._info().c_str());
	corbaProblemEx.log();

	//Is this really necessary? The exceptions already have a timeStamp
	ACS::Time timeStamp = corbaProblemEx.getTimeStamp();
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
void ClientErrorComponent::testTypeException() 
{
    ACS_TRACE("ClientErrorComponent::testTypeException");

    if (CORBA::is_nil(foo_m.in()) == true)
	{
	throw ACSErrTypeCommon::CouldntAccessComponentExImpl(
				   __FILE__, __LINE__,
				   "ClientErrorComponent::testTypeException");
	}
    ACS_SHORT_LOG((LM_INFO, "Example 4a: typeException with depth 0"));
    try
	{
	foo_m->typeException(0);
        ACS_SHORT_LOG((LM_INFO, "OK: No exception thrown"));
	}
    catch(ACSErrTypeCommon::GenericErrorEx &ex)
	{
	ACSErrTypeCommon::GenericErrorExImpl badMethodEx(ex,
				   __FILE__, __LINE__,
				   "ClientErrorComponent::testTypeException");
	badMethodEx.setErrorDesc("UNEXPECTED: shouldn't have thrown an exception");
	badMethodEx.log();

	ACS::Time timeStamp = badMethodEx.getTimeStamp();
	ACE_CString tString = getStringifiedUTC(timeStamp);
	ACS_DEBUG_PARAM("ClientErrorComponent::testTypeException", 
			"Time of the exception: %s\n", tString.c_str());
	}
    catch(CORBA::SystemException &ex)
	{
	ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(
				   __FILE__, __LINE__,
				   "ClientErrorComponent::TestTypeException");
	corbaProblemEx.setMinor(ex.minor());
	corbaProblemEx.setCompletionStatus(ex.completed());
	corbaProblemEx.setInfo(ex._info().c_str());
	corbaProblemEx.log();

	//Is this really necessary? The exceptions already have a timeStamp
	ACS::Time timeStamp = corbaProblemEx.getTimeStamp();
	ACE_CString tString = getStringifiedUTC(timeStamp);
	ACS_DEBUG_PARAM("ClientErrorComponent::TestReceiveRemoteException", 
			"Time of the CORBA exception: %s\n", tString.c_str());
	}
    catch(...)
	{
	ACSErrTypeCommon::GenericErrorExImpl badMethodEx(__FILE__, __LINE__,
		      				 "ClientErrorComponent::testTypeException");
	badMethodEx.setErrorDesc("UNEXPECTED: typeException has thrown an UNKNOWN exception");
	badMethodEx.log();

	ACS::Time timeStamp = badMethodEx.getTimeStamp();
	ACE_CString tString = getStringifiedUTC(timeStamp);
	ACS_DEBUG_PARAM("ClientErrorComponent::testTypeException", 
			"Time of the unexpected exception: %s\n", tString.c_str());
	}
    ACS_SHORT_LOG((LM_INFO, "Example 4b: typeException with depth 5"));
    try
	{
	foo_m->typeException(5);
        ACS_SHORT_LOG((LM_INFO, "UNEXPECTED: should have thrown an exception"));
	}
    catch(ACSErrTypeCommon::ACSErrTypeCommonEx &ex)
	{
	ACSErrTypeCommon::GenericErrorExImpl badMethodEx(ex,
				   __FILE__, __LINE__,
				   "ClientErrorComponent::testTypeException");
	badMethodEx.setErrorDesc("typeException has thrown the expected exception");
	badMethodEx.log();

	ACS::Time timeStamp = badMethodEx.getTimeStamp();
	ACE_CString tString = getStringifiedUTC(timeStamp);
	ACS_DEBUG_PARAM("ClientErrorComponent::testTypeException", 
			"Time of the exception: %s\n", tString.c_str());
	}
    catch(CORBA::SystemException &ex)
	{
	ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(
				   __FILE__, __LINE__,
				   "ClientErrorComponent::TestReceiveRemoteException");
	corbaProblemEx.setMinor(ex.minor());
	corbaProblemEx.setCompletionStatus(ex.completed());
	corbaProblemEx.setInfo(ex._info().c_str());
	corbaProblemEx.log();

	//Is this really necessary? The exceptions already have a timeStamp
	ACS::Time timeStamp = corbaProblemEx.getTimeStamp();
	ACE_CString tString = getStringifiedUTC(timeStamp);
	ACS_DEBUG_PARAM("ClientErrorComponent::testTypeException", 
			"Time of the CORBA exception: %s\n", tString.c_str());
	}
    catch(...)
	{
	ACSErrTypeCommon::GenericErrorExImpl badMethodEx(__FILE__, __LINE__,
		      				 "ClientErrorComponent::testTypeException");
	badMethodEx.setErrorDesc("typeException has thrown an UNEXPECTED exception");
	badMethodEx.log();

	ACS::Time timeStamp = badMethodEx.getTimeStamp();
	ACE_CString tString = getStringifiedUTC(timeStamp);
	ACS_DEBUG_PARAM("ClientErrorComponent::testTypeException", 
			"Time of the unexpected exception: %s\n", tString.c_str());
	}

}
void ClientErrorComponent::testExceptionFromCompletion() 
{
    ACS_TRACE("ClientErrorComponent::testExceptionFromCompletion");

    if (CORBA::is_nil(foo_m.in()) == true)
	{
	throw ACSErrTypeCommon::CouldntAccessComponentExImpl(
				   __FILE__, __LINE__,
				   "ClientErrorComponent::testExceptionFromCompletion");
	}
    ACS_SHORT_LOG((LM_INFO, "Example 3a: ExceptionFromCompletion with depth 0"));
    try
	{
	foo_m->exceptionFromCompletion(0);
        ACS_SHORT_LOG((LM_INFO, "OK: No exception thrown"));
	}
    catch(ACSErrTypeCommon::GenericErrorEx &ex)
	{
	ACSErrTypeCommon::GenericErrorExImpl badMethodEx(ex,
				   __FILE__, __LINE__,
				   "ClientErrorComponent::testExceptionFromCompletion");
	badMethodEx.setErrorDesc("UNEXPECTED: shouldn't have thrown an exception");
	badMethodEx.log();

	ACS::Time timeStamp = badMethodEx.getTimeStamp();
	ACE_CString tString = getStringifiedUTC(timeStamp);
	ACS_DEBUG_PARAM("ClientErrorComponent::testExceptionFromCompletion", 
			"Time of the exception: %s\n", tString.c_str());
	}
    catch(CORBA::SystemException &ex)
	{
	ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(
				   __FILE__, __LINE__,
				   "ClientErrorComponent::TestExceptionFromCompletion");
	corbaProblemEx.setMinor(ex.minor());
	corbaProblemEx.setCompletionStatus(ex.completed());
	corbaProblemEx.setInfo(ex._info().c_str());
	corbaProblemEx.log();

	//Is this really necessary? The exceptions already have a timeStamp
	ACS::Time timeStamp = corbaProblemEx.getTimeStamp();
	ACE_CString tString = getStringifiedUTC(timeStamp);
	ACS_DEBUG_PARAM("ClientErrorComponent::TestReceiveRemoteException", 
			"Time of the CORBA exception: %s\n", tString.c_str());
	}
    catch(...)
	{
	ACSErrTypeCommon::GenericErrorExImpl badMethodEx(__FILE__, __LINE__,
		      				 "ClientErrorComponent::testExceptionFromCompletion");
	badMethodEx.setErrorDesc("UNEXPECTED: exceptionFromCompletion has thrown an UNKNOWN exception");
	badMethodEx.log();

	ACS::Time timeStamp = badMethodEx.getTimeStamp();
	ACE_CString tString = getStringifiedUTC(timeStamp);
	ACS_DEBUG_PARAM("ClientErrorComponent::testExceptionFromCompletion", 
			"Time of the unexpected exception: %s\n", tString.c_str());
	}
    ACS_SHORT_LOG((LM_INFO, "Example 3b: ExceptionFromCompletion with depth 5"));
    try
	{
	foo_m->exceptionFromCompletion(5);
        ACS_SHORT_LOG((LM_INFO, "UNEXPECTED: should have thrown an exception"));
	}
    catch(ACSErrTypeCommon::GenericErrorEx &ex)
	{
	ACSErrTypeCommon::GenericErrorExImpl badMethodEx(ex,
				   __FILE__, __LINE__,
				   "ClientErrorComponent::testExceptionFromCompletion");
	badMethodEx.setErrorDesc("exceptionFromCompletion has thrown the expected exception");
	badMethodEx.log();

	ACS::Time timeStamp = badMethodEx.getTimeStamp();
	ACE_CString tString = getStringifiedUTC(timeStamp);
	ACS_DEBUG_PARAM("ClientErrorComponent::testExceptionFromCompletion", 
			"Time of the exception: %s\n", tString.c_str());
	}
    catch(CORBA::SystemException &ex)
	{
	ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(
				   __FILE__, __LINE__,
				   "ClientErrorComponent::TestExceptionFromCompletion");
	corbaProblemEx.setMinor(ex.minor());
	corbaProblemEx.setCompletionStatus(ex.completed());
	corbaProblemEx.setInfo(ex._info().c_str());
	corbaProblemEx.log();

	//Is this really necessary? The exceptions already have a timeStamp
	ACS::Time timeStamp = corbaProblemEx.getTimeStamp();
	ACE_CString tString = getStringifiedUTC(timeStamp);
	ACS_DEBUG_PARAM("ClientErrorComponent::testExceptionFromCompletion", 
			"Time of the CORBA exception: %s\n", tString.c_str());
	}
    catch(...)
	{
	ACSErrTypeCommon::GenericErrorExImpl badMethodEx(__FILE__, __LINE__,
		      				 "ClientErrorComponent::testExceptionFromCompletion");
	badMethodEx.setErrorDesc("exceptionFromCompletion has thrown an UNEXPECTED exception");
	badMethodEx.log();

	ACS::Time timeStamp = badMethodEx.getTimeStamp();
	ACE_CString tString = getStringifiedUTC(timeStamp);
	ACS_DEBUG_PARAM("ClientErrorComponent::testExceptionFromCompletion", 
			"Time of the unexpected exception: %s\n", tString.c_str());
	}

}

void ClientErrorComponent::TestReceiveRemoteCompletion() 
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
	ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(
				   __FILE__, __LINE__,
				   "ClientErrorComponent::TestReceiveRemoteCompletion");
	corbaProblemEx.setMinor(ex.minor());
	corbaProblemEx.setCompletionStatus(ex.completed());
	corbaProblemEx.setInfo(ex._info().c_str());
	corbaProblemEx.log();

	}
    catch(...)
	{
	ACSErrTypeCommon::GenericErrorExImpl ex(__FILE__, __LINE__,
							 "ClientErrorComponent::TestReceiveRemoteCompletion");
	ex.setErrorDesc("completionFromException has thrown an UNEXPECTED exception");
	ex.log();
	}
}
void ClientErrorComponent::testCompletionOnStack() 
{
    ACS_TRACE("ClientErrorComponent::testCompletionOnStack");

    if (CORBA::is_nil(foo_m.in()) == true)
	{
	throw ACSErrTypeCommon::CouldntAccessComponentExImpl(
				   __FILE__, __LINE__,
				   "ClientErrorComponent::testCompletionOnStack");
	}
    try
	{
	CompletionImpl comp;

	// OK Completion
	ACS_SHORT_LOG((LM_INFO, "Example 7a: completionOnStack with depth 0."));
	comp = foo_m->completionOnStack(0);
	comp.log();

	// ERROR completion with an error trace inside.
	ACS_SHORT_LOG((LM_INFO, "Example 7b: completionOnStack with depth 3."));
	comp = foo_m->completionOnStack(3);
	comp.log();
	}
    catch(CORBA::SystemException &ex)
	{
	ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(
				   __FILE__, __LINE__,
				   "ClientErrorComponent::TestCompletionOnStack");
	corbaProblemEx.setMinor(ex.minor());
	corbaProblemEx.setCompletionStatus(ex.completed());
	corbaProblemEx.setInfo(ex._info().c_str());
	corbaProblemEx.log();
	}
    catch(...)
	{
	ACSErrTypeCommon::GenericErrorExImpl ex(__FILE__, __LINE__,
							 "ClientErrorComponent::testCompletionOnStack");
	ex.setErrorDesc("CompletionOnStack has thrown an UNEXPECTED exception");
	ex.log();
	}
}

void ClientErrorComponent::testCompletionFromCompletion() 
{
    ACS_TRACE("ClientErrorComponent::testCompletionFromCompletion");

    if (CORBA::is_nil(foo_m.in()) == true)
	{
	throw ACSErrTypeCommon::CouldntAccessComponentExImpl(
				   __FILE__, __LINE__,
				   "ClientErrorComponent::testCompletionFromCompletion");
	}
    try
	{
	CompletionImpl comp;

	// OK Completion
	ACS_SHORT_LOG((LM_INFO, "Example 6a: completionFromCompletion with depth 0."));
	comp = foo_m->completionFromException(0);
	comp.log();

	// ERROR completion with an error trace inside.
	ACS_SHORT_LOG((LM_INFO, "Example 6b: completionFromCompletion with depth 3."));
	comp = foo_m->completionFromException(3);
	comp.log();
	}
    catch(CORBA::SystemException &ex)
	{
	ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(
				   __FILE__, __LINE__,
				   "ClientErrorComponent::TestCompletionFromCompletion");
	corbaProblemEx.setMinor(ex.minor());
	corbaProblemEx.setCompletionStatus(ex.completed());
	corbaProblemEx.setInfo(ex._info().c_str());
	corbaProblemEx.log();
	}
    catch(...)
	{
	ACSErrTypeCommon::GenericErrorExImpl ex(__FILE__, __LINE__,
							 "ClientErrorComponent::testCompletionFromCompletion");
	ex.setErrorDesc("completionFromCompletion has thrown an UNEXPECTED exception");
	ex.log();
	}
}

void ClientErrorComponent::TestReceiveCorbaSystemException() 
{
    ACS_TRACE("ClientErrorComponent::TestReceiveCorbaSystemException");

    if (CORBA::is_nil(foo_m.in()) == true)
	{
	throw ACSErrTypeCommon::CouldntAccessComponentExImpl(
				   __FILE__, __LINE__,
				   "ClientErrorComponent::TestReceiveRemoteException");
	}
    ACS_SHORT_LOG((LM_INFO, "Example 5: Calls a method that throws a CORBA System Exception."));
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
void 
ClientErrorComponent::testOutCompletion() 
{
    ACS_TRACE("ClientErrorComponent::testOutCompletion");

    if (CORBA::is_nil(foo_m.in()) == true)
        {
        throw ACSErrTypeCommon::CouldntAccessComponentExImpl(
                                   __FILE__, __LINE__,
                                   "ClientErrorComponent::testCompletionFromCompletion");
        }
    try
        {
        ACSErr::Completion_var comp_var;

        // OK Completion
        ACS_SHORT_LOG((LM_INFO, "Example 8: outCompletion"));
        foo_m->outCompletion(comp_var.out());
        CompletionImpl compImpl(comp_var.in());
        compImpl.log();

        }
    catch(CORBA::SystemException &ex)
        {
       ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(
                                   __FILE__, __LINE__,
                                   "ClientErrorComponent::TestReceiveRemoteException");
        corbaProblemEx.setMinor(ex.minor());
        corbaProblemEx.setCompletionStatus(ex.completed());
        corbaProblemEx.setInfo(ex._info().c_str());
        corbaProblemEx.log();

        }
    catch(...)
        {
        ACSErrTypeCommon::GenericErrorExImpl ex(__FILE__, __LINE__,
                                                         "ClientErrorComponent::outCompletion");
        ex.setErrorDesc("outCompletion has thrown an UNEXPECTED exception");
        ex.log();
        }
    
}
/* @}*/
/* @}*/

/*******************************************************************************/
/** @cond
*/    

int main(int argc, char *argv[])
{
    // Creates and initializes the SimpleClient object
    SimpleClient client;
    if (client.init(argc,argv) == 0)
	{
    	ACS_SHORT_LOG((LM_ERROR, "acsexmplClientErrorComponent::main, Failed to initialize"));
	return -1;
	}
    else
	{
	//Must log into manager before we can really do anything
	client.login();
	}

    	ACS_SHORT_LOG((LM_TRACE, "acsexmplClientErrorComponent::main")); 
    /**************************************************************************
     * Here we instantiate the object used to show examples of error handling.
     * Each method call demonstrate one aspect of error hanlding.
     * See the class documentation for details.
     */
    try
	{
    	ACS_SHORT_LOG((LM_TRACE, "acsexmplClientErrorComponent::main, created instance of ClientErrorComponent"));
	ClientErrorComponent clientErrorComponent(client, argv[1]);
	
	//Call the displayMessage() method existing in the interface for ErrorComponent
    	ACS_SHORT_LOG((LM_TRACE, "acsexmplClientErrorComponent::main, calling TestOk()"));
	clientErrorComponent.TestOk();
    	ACS_SHORT_LOG((LM_TRACE, "acsexmplClientErrorComponent::main, calling TestReceiveRemoteException()"));
	clientErrorComponent.TestReceiveRemoteException();
    	ACS_SHORT_LOG((LM_TRACE, "acsexmplClientErrorComponent::main, calling TestReceiveRemoteCompletion()"));
	clientErrorComponent.TestReceiveRemoteCompletion();
    	ACS_SHORT_LOG((LM_TRACE, "acsexmplClientErrorComponent::main, calling TestReceiveCorbaSystemException()"));
	clientErrorComponent.TestReceiveCorbaSystemException();
    	ACS_SHORT_LOG((LM_TRACE, "acsexmplClientErrorComponent::main, calling testCompletionFromCompletion()"));
	clientErrorComponent.testCompletionFromCompletion();
    	ACS_SHORT_LOG((LM_TRACE, "acsexmplClientErrorComponent::main, calling testExceptionFromCompletion()"));
	clientErrorComponent.testExceptionFromCompletion();
    	ACS_SHORT_LOG((LM_TRACE, "acsexmplClientErrorComponent::main, calling testTypeException()"));
	clientErrorComponent.testTypeException();
    	ACS_SHORT_LOG((LM_TRACE, "acsexmplClientErrorComponent::main, calling testCompletionOnStack()"));
	clientErrorComponent.testCompletionOnStack();
    	ACS_SHORT_LOG((LM_TRACE, "acsexmplClientErrorComponent::main, calling testOutCompletion()"));
	clientErrorComponent.testOutCompletion();
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
/** @endcond
*/    

/*___oOo___*/




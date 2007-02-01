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
* "@(#) $Id: acsexmplClientComponentIOR.cpp,v 1.8 2007/02/01 05:14:26 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-06-17 fixed client.init(argc, argv)
* msekoran  17/02/01  created 
*/

/** @file acsexmplClientComponentIOR.cpp
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

/** @defgroup ACSEXMPLCLIENTCOMPONENTIORDOC Client Component IOR
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
This example shows a client that:
<ul>
  <li>logs into manager</li>
  <li>gets an unnarrowed reference to any given component</li>
  <li>uses SimpleClient's ORB to determine the component's IOR. The IOR is basically a CORBA telephone number</li>
  <li>prints the stringified IOR to standard out</li>
  <li>logs out of manager</li>
</ul>
<br>
<br>
<h2>What can I gain from this example?</h2>
<ul>
  <li>SimpleClient usage.</li>
  <li>Using manager directly through SimpleClient.</li>
  <li>ACS logging mechanisms.</li>
  <li>Accessing (remote) components.</li>
  <li>Using ACS to expose the CORBA-centric attributes of components.</li>
</ul>
<br>
<br>
<h2>Links</h2>
<ul>
  <li><a href="acsexmplClientComponentIOR_8cpp.html">Client Component IOR File Reference</a></li>
</ul>
</div>
   @endhtmlonly
 * @}
 */
/* @}*/
/* @}*/

#include <maciSimpleClient.h>

ACE_RCSID(acsexmpl, acsexmplMaciIOR, "$Id: acsexmplClientComponentIOR.cpp,v 1.8 2007/02/01 05:14:26 cparedes Exp $")
using namespace maci;

/** @cond
*/    
int main(int argc, char *argv[]) 
{
    //Checks command-line arguments
    if (argc < 2)
	{
	ACS_SHORT_LOG((LM_INFO, "Usage: %s <component name> <options>", argv[0]));
	return -1;
	}
    else
	{
	ACS_SHORT_LOG((LM_INFO, "Welcome to %s!", argv[0]));
	}
    
    //Creates and initialyses the SimpleClient object
    SimpleClient client;
    if (client.init(argc,argv) == 0)
	{
	ACE_DEBUG((LM_DEBUG,"Cannot init client"));
	return -1;
	}
    else
	{
	//Must log into manager before we can really do anything
	client.login();
	}
    
    try
	{
	//Gets from manager the reference to the requested component.
	//Pay special attention that this reference is just a generic
	//CORBA object at this point.
	ACS_SHORT_LOG((LM_INFO, "Looking for Object '%s' ", argv[1]));
        CORBA::Object_var obj =  client.getComponent(argv[1], 0 , true);
	
	//Get the stringified IOR of the component.  The IOR of CORBA objects
	//can be considered to be a unique "phone number" used to access CORBA
	//servants.
	ACS_SHORT_LOG((LM_INFO, "Getting stringified IOR"));
	CORBA::String_var mior = client.getORB()->object_to_string(obj.in());    
	
	//Print the IOR to standard out
	u_int result;         
	ACS_SHORT_LOG ((LM_INFO, "IOR for %s is: %s", argv[1], mior.in()));
	result = ACE_OS::printf ("%s", mior.in());
	}
    catch(maciErrType::CannotGetComponentExImpl &_ex)
	{
	_ex.log();
	return -1;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__, 
							"main");
	
	uex.log();
	return -1;
	}

    //Normally you would not want to have separate try sections for releasing
    //the components and logging out from manager.  This is a very special case 
    //since we do not know ahead of time what will be released.  In other words,
    //argv[1] could technically be "manager" which would end up raising a 
    //no-permission exception.  To get around this just use separate try/catch 
    //sections and ignore no-permission exceptions.
    try
	{
	//All clients must cleanly release objects they activate!
	client.releaseComponent(argv[1]);
	}
    catch(maciErrType::CannotReleaseComponentExImpl &_ex)
	{
	_ex.log();
	return -1;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__, 
							"main");
	uex.log();
	return -1;
	}
    
    try
	{
	if (client.logout() == 0)
	    {
	    ACS_SHORT_LOG ((LM_INFO, "Cannot logout"));
	    return -1;
	    }
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR, "Exception caught"));
	return -1;
	}    
    
    ACS_SHORT_LOG((LM_INFO,"The end!"));
    return 0;
}


/** @endcond
*/    


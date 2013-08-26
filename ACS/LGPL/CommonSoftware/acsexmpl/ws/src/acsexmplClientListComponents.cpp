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
* "@(#) $Id: acsexmplClientListComponents.cpp,v 1.8 2007/02/01 05:14:26 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-06-17 fixed client.init(argc,argv)
* gchiozzi 2002-02-13 Cleaned up
* msekoran  17/02/01  created 
*/

/** @file acsexmplClientListComponents.cpp
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

/** @defgroup ACSEXMPLCLIENTLISTCOMPONENTSDOC Client List Components
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
This example shows a client that:
<ul>
  <li>logs into manager</li>
  <li>gets information on all components</li>
  <li>logs out of manager</li>
</ul>
<br>
<br>
<h2>What can I gain from this example?</h2>
<ul>
  <li>SimpleClient usage.</li>
  <li>Using manager directly through SimpleClient.</li>
  <li>The knowledge that a manager reference can be used for more than just getting component references.</li>
  <li>ACS logging mechanisms.</li>
</ul>
<br>
<br>
<h2>Links</h2>
<ul>
  <li><a href="acsexmplClientListComponents_8cpp.html">List Components File Reference</a></li>
</ul>
</div>
   @endhtmlonly
 * @}
 */

/* @}*/
/* @}*/

#include <maciSimpleClient.h>

ACE_RCSID(acsexmpl, acsexmplListCOBS, "$Id: acsexmplClientListComponents.cpp,v 1.8 2007/02/01 05:14:26 cparedes Exp $")
using namespace maci;

/*---------------------------------------------------------------------------------------*/

/** @cond
*/    
int main(int argc, char *argv[]) 
{
    ACS_SHORT_LOG((LM_INFO, "Welcome to %s!", argv[0]));

    //Creates and initializes the SimpleClient object
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
	//Query the manager for a list of all components
	//and print infos for all of them
	maci::HandleSeq seq;
	//Invoke a manager method directly to obtain information on components.  Please see the
	//doxygen-generated documentation for maci.idl for more detailed info.
	maci::ComponentInfoSeq_var devs = client.manager()->get_component_info(client.handle(), 
									       seq, 
									       "*", 
									       "*",
									       false);
	
	//Print some standard information about all components to standard out.
	ACS_SHORT_LOG((LM_INFO, "Listing all components known to manager:"));
	for (unsigned int i = static_cast<unsigned int>(0); i < devs->length (); i++) 
	    {
	    ACS_SHORT_LOG((LM_INFO, "\tComponent: %s (type: %s)", devs[i].name.in(), devs[i].type.in()));
	    }
	
	//Logout from manager cleanly
	if(client.logout() == 0)
	    {
	    ACS_SHORT_LOG ((LM_INFO, "Cannot logout"));
	    return -1;
	    }
	}
    catch( CORBA::SystemException &_ex ) // can be thrown by get_component_infos
	{
	ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__,
							    "main");
	corbaProblemEx.setMinor(_ex.minor());
	corbaProblemEx.setCompletionStatus(_ex.completed());
	corbaProblemEx.setInfo(_ex._info().c_str());
	corbaProblemEx.log();
	return -1;
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"Error!"));
	return -1;
	}

    ACS_SHORT_LOG ((LM_INFO, "The end!"));
    return 0;
}

/** @endcond
*/    



/***************************************************************************
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
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
*    MA 02111-1307  USA
*
* "@(#) $Id: onchangeMonitorTest.cpp,v 1.40 2008/10/01 02:33:31 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram    2002/11/14 created
*/

static char *rcsId="@(#) $Id: onchangeMonitorTest.cpp,v 1.40 2008/10/01 02:33:31 cparedes Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <baciS.h>
#include <baciCORBA.h>
#include <enumpropTestDeviceC.h>
#include <logging.h>

class TestCBpattern: public virtual POA_ACS::CBpattern 
{
private:
  ENUMPROP_TEST::ROStates_var rostates;
  ACE_CString prop;
  ACS::stringSeq_var description;  
public:
  TestCBpattern(ENUMPROP_TEST::ROStates_ptr _rostates) : rostates(_rostates) 
  {
    prop = rostates->description();
    description = rostates->statesDescription();
  }
  
  void working (ACS::pattern value,
		const ACSErr::Completion & c,
		const ACS::CBDescOut & desc
		 )
  {
      if (c.code!=0)
	  ACS_SHORT_LOG ((LM_DEBUG, "(%s::CBStates::working) Value: %s (%llu) Completion type: %d code: %d", prop.c_str(), description[value].in(), value, c.type, c.code));
  }

  void done (	ACS::pattern value,
		const ACSErr::Completion & c,
		const ACS::CBDescOut & desc
		 )
  {
    ACS_SHORT_LOG ((LM_DEBUG, "(%s::CBStates::done) Value: %s (%llu)", prop.c_str(),description[value].in(), value));
  }
    
  CORBA::Boolean negotiate ( ACS::TimeInterval time_to_transmit,
			     const ACS::CBDescOut & desc
			      )
  {
    return 1;
  }
};


int main(int argc, char* argv[]) 
{

    
    
    try
	{
	// create logging proxy
	LoggingProxy *m_logger = new LoggingProxy(0, 0, 31, 0);
	LoggingProxy::init(m_logger);
	LoggingProxy::ProcessName(argv[0]);
	LoggingProxy::ThreadName("main");
	ACS_SHORT_LOG((LM_INFO, "Logging proxy successfully created !")); 

	ACE_CString g_strCmdLn;
	for (int i=argc-1; i>=0; i--)
	    g_strCmdLn = ACE_CString(argv[i])+ " " + g_strCmdLn;

	if (g_strCmdLn.find("-ORBDottedDecimalAddresses")==ACE_CString::npos)
	    g_strCmdLn += " -ORBDottedDecimalAddresses 1";

	ACE_TCHAR **m_argv = argv;
	int m_argc = argc;
	ACE_OS::string_to_argv((ACE_TCHAR*)g_strCmdLn.c_str(),
			 m_argc,
			 m_argv);
	BACI_CORBA::InitCORBA(m_argc, m_argv);
	/**
	 * Get reference to a device
	 */

	char fileName[64];
	sprintf(fileName, "file://%s.ior", argv[1]);
	CORBA::Object_var object = BACI_CORBA::getORB()->string_to_object (fileName);
	
	
	if (CORBA::is_nil(object.in())) 
	    {
	    ACS_SHORT_LOG ((LM_DEBUG, "Cannot get Object"));
	    return -1;
	    }

	// Try to narrow the object reference to a PowerSupply reference.
	ENUMPROP_TEST::enumpropTestDevice_var dev = ENUMPROP_TEST::enumpropTestDevice::_narrow (object.in ());
	
	if (CORBA::is_nil(dev.in())) 
	    {
	    ACS_SHORT_LOG((LM_DEBUG, "Failed to narrow enumnTestDevice"));
	    return 0;
	    }
	ACS_SHORT_LOG((LM_DEBUG, "Device narrowed."));

	// Get current stat
	ENUMPROP_TEST::ROStates_var currentState = dev->currentState();
	
	
	//   get states description	
	ACS::stringSeq_var description = currentState->statesDescription( );
	
        
	TestCBpattern cbStates (currentState.in());
        ACS::CBpattern_var cbStatesObj = cbStates._this();

        ACS::CBDescIn desc;
	desc.id_tag = 1;
	ACS::Monitorpattern_var monitor = currentState->create_monitor(cbStatesObj.in(), desc);
	monitor->set_timer_trigger(50000000); //=disable
	monitor->set_value_trigger(1, true); //=set on change

	/**
	 * Enter main loop and stays there for a 
	 * fixed amount of time
	 */
	ACS_SHORT_LOG((LM_DEBUG, "(BACIClient main thread) Going in main loop sleep..."));
	ACE_Time_Value t(20);
	BACI_CORBA::getORB()->run(t);

	monitor->destroy();

        /* Allow time for the done() callback to come back */
	ACE_Time_Value t5(5);
	BACI_CORBA::getORB()->run(t5);


	BACI_CORBA::DoneCORBA();

	// Delete the logger last.
	delete m_logger;
	}
    catch( CORBA::Exception &ex )
	{
	ACE_PRINT_EXCEPTION (ex,"Error!");
	return -1;
	}
    ACE_CHECK_RETURN (-1);
    
    return 0;
}









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
* "@(#) $Id: alarmTest.cpp,v 1.42 2012/01/24 01:00:04 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram 2001-12-05 removed SimpleClient
* bjeram
*/

static char *rcsId="@(#) $Id: alarmTest.cpp,v 1.42 2012/01/24 01:00:04 tstaig Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <baciCORBA.h>
#include <baciS.h>
#include <enumpropTestDeviceC.h>
#include <ace/SString.h>
#include <logging.h>


class AlarmCBpattern: public virtual POA_ACS::Alarmpattern
{
private:
  ENUMPROP_TEST::ROStates_var rostates;
  ACE_CString prop;
  ACS::stringSeq_var description;  
public:
  AlarmCBpattern(ENUMPROP_TEST::ROStates_ptr _rostates) : rostates(ENUMPROP_TEST::ROStates::_duplicate(_rostates)) 
  {
    prop = rostates->description();
    description = rostates->statesDescription();
  }
  
  void alarm_raised (ACS::pattern value,
		const ACSErr::Completion & c,
		const ACS::CBDescOut & desc
		 ) 
  {
    ACS_SHORT_LOG ((LM_DEBUG, "(%s::AlarmCBStates::raised) Value: %s (%llu). Completion (type=%d, code=%d)", prop.c_str(), description[value].in(), value, c.type, c.code));
  }

  void alarm_cleared (	ACS::pattern value,
		const ACSErr::Completion & c,
		const ACS::CBDescOut & desc
		 ) 
  {
    ACE_DEBUG ((LM_DEBUG, "(%s::AlarmCBStates::cleared) Value: %s (%llu). Completion (type=%d, code=%d)", prop.c_str(),description[value].in(), value, c.type, c.code));
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

    if (argc<2)
    {
      ACE_OS::printf ("usage: %s <object_name>\n", argv[0]);
      return -1;
    }
    
    
    try
	{
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

	LoggingProxy EP_log (0, 0, 31, 0);
	LoggingProxy::init (&EP_log);

	/**
	 * Get reference to a device
	 */

	char fileName[64];
	sprintf(fileName, "file://%s.ior", argv[1]);
	CORBA::Object_var object = BACI_CORBA::getORB()->string_to_object (fileName);
	

	if (CORBA::is_nil(object.in())) 
	    {
	    ACE_DEBUG ((LM_DEBUG, "Cannot get Object"));
	    return -1;
	    }

	// Try to narrow the object reference to a PowerSupply reference.
	ENUMPROP_TEST::enumpropTestDevice_var dev = ENUMPROP_TEST::enumpropTestDevice::_narrow (object.in ());
	
	if (CORBA::is_nil(dev.in())) 
	    {
	    ACS_SHORT_LOG((LM_DEBUG, "Failed to narrow enumnTestDevice "));
	    return 0;
	    }
	ACS_SHORT_LOG((LM_DEBUG, "Device narrowed."));

	// Get current stat
	ENUMPROP_TEST::ROStates_var currentState = dev->currentState();
	

	//   get states description	
	ACS::stringSeq_var description = currentState->statesDescription( );
	

	AlarmCBpattern alarmCB (currentState.in());
        ACS::Alarmpattern_var alarmCBObj = alarmCB._this();

        ACS::CBDescIn desc;
	desc.id_tag = 1;

	ACS::Subscription_var monitor = currentState->new_subscription_AlarmEnum(alarmCBObj.in(), desc);
	//	monitor->set_timer_trigger(10000000);

	ACS_SHORT_LOG((LM_DEBUG, "(main thread) Going in main loop sleep..."));
	ACE_Time_Value tv(5);
	BACI_CORBA::getORB()->run(tv);

	ACS_SHORT_LOG((LM_DEBUG, "(main thread) Exit ... "));
	monitor->destroy();

        /* Allow time for the done() callback to come back */
	BACI_CORBA::getORB()->run(tv);

	BACI_CORBA::DoneCORBA();
	}
    catch( CORBA::Exception &ex )
	{
	ex._tao_print_exception("Error!");
	return -1;
	}


    return 0;
}













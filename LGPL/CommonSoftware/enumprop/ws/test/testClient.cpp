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
* "@(#) $Id: testClient.cpp,v 1.46 2008/10/01 02:33:31 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* gchiozzi 2003-05-26 Added some sleeps after get_async to allow for the callback to come back.
* bjeram 2001-12-05 removeed ci
* bjeram 2001-11-29 added ci.logout
* bgustafs 2001-04-19 created
*/


static char *rcsId="@(#) $Id: testClient.cpp,v 1.46 2008/10/01 02:33:31 cparedes Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <baciCORBA.h>
#include <baciS.h>
#include <enumpropTestDeviceC.h>
#include <logging.h>

NAMESPACE_USE(ENUMPROP_TEST)

class TestCBpattern: public virtual POA_ACS::CBpattern 
{
private:
  ACE_CString prop;
  ACS::stringSeq_var description;  
public:
  TestCBpattern(ENUMPROP_TEST::ROStates_ptr rostates )
  {
    try
      {
	prop = rostates->description ();
	
	description = rostates->statesDescription ();
	
      }
    catch( CORBA::Exception &ex )
	{
	ACE_PRINT_EXCEPTION (ex,"Error!");
	}
  }
  
  void working (ACS::pattern value,
		const ACSErr::Completion & c,
		const ACS::CBDescOut & desc
		 ) 
  {
      ACS_SHORT_LOG((LM_INFO,"(%s::CBStates::working) Value: %s (%d)", prop.c_str(), description[value].in(), value));
  }

  void done (	ACS::pattern value,
		const ACSErr::Completion & c,
		const ACS::CBDescOut & desc
		 )
  {
      ACS_SHORT_LOG((LM_INFO,"(%s::CBStates::done) Value: %s (%d)", prop.c_str(),description[value].in(), value));
  }
    
  CORBA::Boolean negotiate ( ACS::TimeInterval time_to_transmit,
			     const ACS::CBDescOut & desc
			      )
  {
    return 1;
  }
};

/*****************************************************************************************
 *
 *                  main
 *
 *******************************************************************************************/
int main(int argc, char* argv[]) 
{

  if (argc<3)
    {
      ACE_OS::printf ("usage: %s <object_name> <state> \n", argv[0]);
      return -1;
    }

      
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
	CORBA::Object_var testObj = BACI_CORBA::getORB()->string_to_object (fileName);
	
	
	if (CORBA::is_nil(testObj.in())) 
	    {
	    ACS_SHORT_LOG((LM_INFO,"Cannot get Object"));
	    return -1;
	    }

	// Try to narrow the object reference to a PowerSupply reference.
	enumpropTestDevice_var dev = enumpropTestDevice::_narrow (testObj.in ());
	
	if (CORBA::is_nil(dev.in())) 
	    {
	    ACS_SHORT_LOG((LM_INFO,"Failed to narrow enumnTestDevice"));
	    return 0;
	    }
        ACS_SHORT_LOG((LM_INFO,"Device narrowed."));

	// Get current stat
	ENUMPROP_TEST::ROStates_var currentState = dev->currentState();

	
	//   get states description	
	ACS::stringSeq_var description = currentState->statesDescription( );
	
	
	// get all states
	ENUMPROP_TEST::StatesSeq_var states = currentState->allStates ();
	

	//get alram_on/off
        ENUMPROP_TEST::StatesSeq_var alarm_on = currentState->alarm_on ();
	

	ENUMPROP_TEST::StatesSeq_var alarm_off = currentState->alarm_off ();
	

	unsigned int j, len = description->length();
        ACS_SHORT_LOG((LM_INFO,"States (%d) for \"currentState\" are: ", len));
	for(unsigned int i=0; i<len; i++){
	  for(j=0; j<alarm_on->length() && (unsigned int)alarm_on[j]!=i; j++);
	  if(j<alarm_on->length())
	      {
	      ACS_SHORT_LOG((LM_INFO,"%d %s     alarm_on", states[i], description[i].in()));
	      }
          else
	      {
	      ACS_SHORT_LOG((LM_INFO,"%d %s", states[i], description[i].in()));
	      }
	}
	
	ACSErr::Completion_var co = new ACSErr::Completion;
	
        // current state
	ENUMPROP_TEST::States state = currentState->get_sync(co.out());
	

        ACS_SHORT_LOG((LM_INFO,"Current state is (get_sync): %s (%d)", description[(CORBA::ULong)state].in(), state));

        ACS_SHORT_LOG((LM_INFO,"Setting current state to: %s", argv[2]));
	// Set state of device	
	if (strcmp(argv[2],"DISABLED") == 0)
	  {
	    co = dev->disable();
	    
	  }
	else if (strcmp(argv[2],"ENABLED") == 0)
	  {
	    co = dev->enable();
	    
	  }
	else if (strcmp(argv[2],"DIAGNOSE") == 0)
	  {
	    co = dev->diagnose();
	    
	  }
	else if (strcmp(argv[2],"SHUTDOWN") == 0)
	  {
	   co = dev->shutdown( );   
	   
	  }
	else if (strcmp(argv[2],"INITIALIZE") == 0)
	  {
	    co = dev->init( );
	    
	  }
	else if (strcmp(argv[2],"ON") == 0)
	  {
	    co = dev->on();
	    
	  }
	else if (strcmp(argv[2],"OFF") == 0)
	  {
	    co = dev->off();
	    
	  }
	else
	  {
	    ACS_SHORT_LOG((LM_INFO,"No valid state defined"));
	  }
	
	state = currentState->get_sync(co.out());
	

        ACS_SHORT_LOG((LM_INFO,"Current state is (get_sync): %s (%d)", description[(CORBA::ULong)state].in(), state));
	
	
	TestCBpattern cbStates (currentState.in());
	
	ACS::CBpattern_var cbStatesObj = cbStates._this();
	ACS::CBDescIn descin;
	descin.id_tag = 1;
	
	currentState->get_async(cbStatesObj.in(), descin);
	
	ACE_OS::sleep (2);  // Make sure the callback comes before doing other things
	
	ACE_Time_Value tv(40);
	BACI_CORBA::getORB()->run(tv);

#ifndef MAKE_VXWORKS
	if (strcmp(argv[argc-1],"noshutdown")){
	    dev->serverShutdown();
	}
	ACE_OS::sleep(5);  // server nees some time to shut down 
#endif
	BACI_CORBA::DoneCORBA();

	// Delete the logger last.
	delete m_logger;
	}
    catch( CORBA::Exception &ex )
	{
	ACE_PRINT_EXCEPTION (ex,"Error!");
	return -1;
	}
    
    return 0;
}











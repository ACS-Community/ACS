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
* "@(#) $Id: testDriverAcs.cpp,v 1.8 2006/10/16 12:14:46 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring 2006-09-06 created
*/

#include <acsncSimpleConsumer.h>
#include <acscommonC.h>
#include <maciSimpleClient.h>
#include <testCppAlarmSourceComponentsC.h>
#include <AlarmSystemC.h>
#include <stdlib.h>


/****************************************************/

class LoggingConsumer : public nc::Consumer
{
  public:
	
    LoggingConsumer():
	nc::Consumer(acscommon::LOGGING_CHANNEL_NAME)
	{
		 mutex_m = new ACE_Mutex;
       evtsReceived = 0;
	    subscribeAllEvents();
	}
	
    /** 
     * Overridden 
     *  @param publishedEvent The real CORBA event.
     *
     *  @return void
     *  @htmlonly
     <br><hr>
     @endhtmlonly
    */
    virtual void 
    push_structured_event(const CosNotification::StructuredEvent &publishedEvent)
	throw (CORBA::SystemException, CosEventComm::Disconnected)
	{
     // TODO: ascertain length of remainder_of_body rather than use fixed 4k length
     char remainderOfBodyStorage[4096];
     const char* remainderOfBody = (const char*) remainderOfBodyStorage;
     publishedEvent.remainder_of_body >>= remainderOfBody; 
     string remainderOfBodyString(remainderOfBody);

     // check for the messages of interest
     if(remainderOfBodyString.find("<Alert TimeStamp") == 0 && remainderOfBodyString.find("Alarm sent:") != std::string::npos) 
     {
       ACE_Guard<ACE_Mutex> guard(*mutex_m);
       {
         std::cout << "Detected notification channel event: " << ++evtsReceived << std::endl;
         std::cout << "remainder of body was: " << remainderOfBodyString << std::endl;
         std::cout.flush();
       }
     }	
	}
	
	int getEventsReceived() { return evtsReceived; }

  protected:

    //--------------------------------------------------------------
    /**
     * Overridden
    */
    const char* getChannelKind()
	{
	    return acscommon::LOGGING_CHANNEL_KIND;
	}
	
    /**
     * Overridden
    */
    const char* getChannelDomain()
	{
	    return "*";
	}


  private:
	
	int evtsReceived;
	ACE_Mutex * mutex_m;

    //--------------------------------------------------------------
    /**
     * Method used to subscribe to all types of events on the channel.
    */
    void subscribeAllEvents() throw (CORBAProblemEx);

};

void LoggingConsumer::subscribeAllEvents()
    throw (CORBAProblemEx)
{
    ACS_TRACE("LoggingConsumer::subscribeAllEvents");
    init();
	
    // Setup the CA to receive event_type
    CosNotification::EventTypeSeq added(1);
    CosNotification::EventTypeSeq removed (0);
    added.length (1);
    removed.length (0);

    // We will listen only specified domain.
    added[0].domain_name = getChannelDomain();
    added[0].type_name   = "*";

    try
	{
	consumerAdmin_m->subscription_change(added, removed);
	} 
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"LoggingConsumer::subscribeAllEvents failed!"));
	CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::LoggingConsumer::subscribeAllEvents");
	throw err.getCORBAProblemEx();
	}	
}

//---------------------------------------------------------- 

/****************************************************/

using namespace maci;

void printUsageAndExit()
{
	std::cout << "\n\nUsage: \n\n" << "testDriverAcs <NUM_ALARMS_TO_SEND>\n\n" << "where NUM_ALARMS_TO_SEND is how many alarms you wish to send.\n\n";
	exit(-1);	
}

/*******************************************************************************/
int main(int argc, char *argv[])
{
	if(argc < 2)
	{
		printUsageAndExit();
	}
	int numAlarmsToSend = atoi(argv[1]);
	
	// Create and initialize the SimpleClient object
	SimpleClient client;
	if (client.init(argc,argv) == 0)
	{
		return -1;
	}
	else
	{
		// Must log into manager before we can really do anything
		client.login();
	}
   
	// Get the component which will be used to generate alarms
	testalarmsystem::AlarmTestMount_var alarmTestMount = client.get_object<testalarmsystem::AlarmTestMount>("ALARM_SOURCE_MOUNTCPP", 0, true);
    
	/*
	 * This can throw exceptions.
	 * No point catching: just exit, but it would be
	 * nice to add error handling
	 */
	LoggingConsumer *m_simpConsumer_p = new LoggingConsumer;

	m_simpConsumer_p->consumerReady();
	ACE_OS::sleep(10);

	int sentEvtCount = 0;
	ACE_Time_Value tv1(1);

	while ( (sentEvtCount < numAlarmsToSend) ) 
	{
		alarmTestMount->faultMount();
		sentEvtCount++;
		client.run(tv1);
	}

	int timeWaited = 0;
	int MAX_TIME_TO_WAIT = 30;
	while ((m_simpConsumer_p->getEventsReceived() < numAlarmsToSend) && (timeWaited < MAX_TIME_TO_WAIT))
	{
		timeWaited++;
		client.run(tv1);
	}

	if(m_simpConsumer_p->getEventsReceived() >= numAlarmsToSend)
	{
		std::cout << "disconnecting consumer" << std::endl;
		std::cout << "received: " << m_simpConsumer_p->getEventsReceived() 
			<< " events, and sent: " << sentEvtCount << " events" << std::endl;
	}
	else
	{
		std::cout << "ERROR: never detected all the events before the timeout elapsed" << std::endl;
	}

	m_simpConsumer_p->disconnect();   

	// release the component and logout from manager
	client.manager()->release_component(client.handle(), "ALARM_SOURCE_MOUNTCPP");
	client.logout();

	return 0;
}


/*___oOo___*/

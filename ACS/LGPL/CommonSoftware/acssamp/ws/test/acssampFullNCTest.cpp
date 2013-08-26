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
 * "@(#) $Id: acssampFullNCTest.cpp,v 1.18 2008/10/07 06:41:54 cparedes Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * oat 2002-10-31 created
 */

/**
 * This test program is used to demonstrate/test a full example
 * of usage of the sampling system.
 * It does the following step:
 *  - get the reference to the factory SAMP object
 *  - initialize a new sampling object to sample LAMP1:brightness
 *    with a frequncy of 0.1 Hz and a report rate of 1 sec
 *  - starts in a new thread a client to consume the sampled data
 *  - printout the received data 
 */

#include <vltPort.h>
#include <acsutil.h>

#include <maciSimpleClient.h>
#include <acssampC.h>
#include <baciS.h>
#include <acserr.h>

#include <acsncConsumer.h>

using namespace std;
using namespace maci;
using namespace acssamp;
ACE_RCSID(acssampFullNCTest, fullNCTest, "$Id: acssampFullNCTest.cpp,v 1.18 2008/10/07 06:41:54 cparedes Exp $")

    struct ThreadParam
    {
	ACE_CString chName;
	CORBA::ORB *orb;    
    };

// client code; see NC documentation
class SamplerConsumer : public nc::Consumer
{
  public:
    
    SamplerConsumer(const char* channelName, const CORBA::ORB_ptr orb) : Consumer(channelName, orb)
	{
	    ACS_SHORT_LOG((LM_INFO,"::SamplerConsumer::SamplerConsumer(names, orb)"));
	    init();
	}
    
    virtual ~SamplerConsumer()
	{
	    //ACS_SHORT_LOG((LM_INFO,"::SamplerConsumer::~SamplerConsumer~"));
	}

  protected:

    /*
     * @throw CosEventComm::Disconnected
     */
    void push_structured_event(const CosNotification::StructuredEvent &notification)
	{
	    cout << "Event: domain = " << (const char *)notification.header.fixed_header.event_type.domain_name << endl;
    
	  acssamp::SampObj::SampDataBlockSeq *m_SampledData_p, m_SampledData;
	
	    m_SampledData_p=&m_SampledData;

	    notification.filterable_data[0].value >>= m_SampledData_p;

	    cout << "STRUCT_LEN: " << m_SampledData_p->length() << endl;

	    for (CORBA::ULong q = 0; q < m_SampledData_p->length(); q++) {

	    cout << "TIME STAMP: " <<  (*m_SampledData_p)[q].sampTime << endl;
 
	      CORBA::Double extVal;

	    (*m_SampledData_p)[q].sampVal >>=extVal;

	    cout << "VALUE: " << extVal << endl;
	    }
	} 
};



class NCThread : public ACS::Thread
{
  public:
    NCThread(const ACE_CString& name,
	     ThreadParam thParam,
	     const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime, 
	     const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime) :
	ACS::Thread(name)
	{
	    thParam_p = thParam;
	}

    ~NCThread()
	{
	}

    virtual void run()
	{
// initialize thread
	    if (ThreadBase::InitThread)
		ThreadBase::InitThread(getName().c_str());

	    //ACS_SHORT_LOG((LM_INFO,"Starting NC thread %s", getName().c_str()));
  
	    // Create consumer
	    SamplerConsumer *sampConsumer_p = new SamplerConsumer(thParam_p.chName.c_str(),
								  thParam_p.orb);
	    sampConsumer_p->addSubscription<acssamp::SampObj::SampDataBlockSeq>();
	    sampConsumer_p->consumerReady();

	    while(check())
		{
		if(!isSuspended())
		    {
		    sleep();
		    }
		}

	    sampConsumer_p->disconnect();
	    
	    if(sampConsumer_p)
		delete sampConsumer_p;

	    // cleanup thread
	    if (ThreadBase::DoneThread)
		ThreadBase::DoneThread();

	    setStopped();
	}
		
  private:
  
    ThreadParam thParam_p;   
};

/*******************************************************************************/
    
int main(int argc, char *argv[])
{
    /// Creates and initializes the SimpleClient object
    SimpleClient client;
    if (!client.init(argc,argv))
	{
	return -1;
	}
    else
	{
	client.login();
	}
   
    ACS_SHORT_LOG((LM_INFO, "Getting Component"));

    /// Get the specific reference to the factory object
  acssamp::Samp_var samp = client.get_object<acssamp::Samp>("SAMP1", 0, true);
    
    if (!CORBA::is_nil(samp.in()))
	{	
	ACS_SHORT_LOG((LM_DEBUG, "Got samp descriptor()."));

	  acssamp::SampObj_var sampObj = samp->initSampObj("LAMP1","brightness",1000000,10000000);

	ACS_SHORT_LOG((LM_INFO,"*** Start to sample ***"));

	ThreadParam thParam;
	// get the not. channel and pass it to the client thread
	
	ACE_CString channelName(sampObj->getChannelName());
	thParam.chName = channelName;
	ACS_SHORT_LOG((LM_INFO,"Not Channel: %s",channelName.c_str()));
	thParam.orb = client.getORB();

	  ACS::ThreadManager *threadManager_p = 0;
	threadManager_p = new ACS::ThreadManager();
	if (!threadManager_p) 
	    {
	    cout << "Could not create the ThreadManager!" << endl;
	    }

	NCThread *ncThread_p = threadManager_p->create<NCThread,ThreadParam>("NCThread",thParam);
	ncThread_p->resume();

	//delay to enable client connection to the NC channel
	  ACE_OS::sleep(10);

	sampObj->start();
 	
	  ACE_OS::sleep(15);

	sampObj->suspend();
	
	  ACE_OS::sleep(5);
	
	sampObj->resume();
	
	  ACE_OS::sleep(10);

	sampObj->stop();

	  ACE_OS::sleep(2);

      	sampObj->destroy();

	if (threadManager_p)
	    delete threadManager_p;	
	}
    
    client.manager()->release_component(client.handle(), "SAMP1");	
    ACS_SHORT_LOG((LM_INFO, "SAMP1 released"));
    client.logout();
    
    /// sleep for 3 sec to allow everytihng to cleanup and stableize
  ACE_OS::sleep(3);
    
    return 0;
}

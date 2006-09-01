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
* "@(#) $Id: acssampFullNCTest.cpp,v 1.16 2006/09/01 02:20:55 cparedes Exp $"
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
#include <acssampS.h>
#include <baciS.h>
#include <acserr.h>

#include <acsncConsumer.h>

using namespace std;
 using namespace maci;
ACE_RCSID(acssampFullNCTest, fullNCTest, "$Id: acssampFullNCTest.cpp,v 1.16 2006/09/01 02:20:55 cparedes Exp $")
    

  

// client code; see NC documentation
class SamplerConsumer : public nc::Consumer
{

public:
 
  SamplerConsumer(const char* cName, const CORBA::ORB_ptr orb) : Consumer(cName, orb)
  {
    ACS_SHORT_LOG((LM_INFO,"::SamplerConsumer::SamplerConsumer(names, orb)"));
    init();
  }
 
    /**
     * Destructor
     */
  virtual ~SamplerConsumer()
  {
    ACS_SHORT_LOG((LM_INFO,"::SamplerConsumer::SamplerConsumer~"));
  }


  protected:

  void push_structured_event(const CosNotification::StructuredEvent &notification
			     )
    throw (CORBA::SystemException, CosEventComm::Disconnected)
  {
    
    cout << "Event: domain = \n" << (const char *)notification.header.fixed_header.event_type.domain_name << endl;
    

    ACSSamp::SampObj::SampDataBlockSeq *m_SampledData_p, m_SampledData;
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



// client thread class 

class ThreadHandler
{

public:

  ThreadHandler(const char * chName, const CORBA::ORB_ptr orb): m_controlLoop_p(BACIThread::NullBACIThread)
  {


    channelName=CORBA::string_dup(chName);
    intOrb=orb;


    threadManager = new BACIThreadManager();
    if (!threadManager) 
      {
	cout << "ThreadHandler threadManager ERROR !!" << endl;
      }


    if(!m_controlLoop_p)
      {
	m_controlLoop_p = threadManager->create("runConsumer", 
						(void *)runConsumer, 
						(void *)this);
      }
  }
  
  
  ~ThreadHandler()
  {
    if (threadManager)
      delete threadManager;
  }



  static void runConsumer(void *param_p)
  {
    
    
    
    ACE_DEBUG((LM_INFO, "::ThreadHandler::runConsumer(*param_p)"));
    if (!param_p) 
      {
	return;
	}
    
    BACIThreadParameter *baciParameter_p = (BACIThreadParameter *)param_p;
    BACIThread *myself_p = baciParameter_p->getBACIThread();

    ThreadHandler * th_p= (ThreadHandler *)baciParameter_p->getParameter();

    
    if (BACIThread::InitThread) 
      {
	BACIThread::InitThread("runConsumer");
      }


    // Create consumer
    SamplerConsumer *client = new SamplerConsumer(th_p->getChannelName(),th_p->getORBptr());
    client->addSubscription<ACSSamp::SampObj::SampDataBlockSeq>();
    client->consumerReady();
    
    while(myself_p->check())
      {
	if(!myself_p->isSuspended())
	  {
	    myself_p->sleep();
	  }
      }
    
    if (BACIThread::DoneThread) 
      {
	BACIThread::DoneThread();
      }
    delete baciParameter_p;
    
    client->disconnect();
    myself_p->setStopped();
  }
  

  CORBA::ORB_ptr getORBptr() const { return intOrb; }
  const char * getChannelName() const { return channelName.c_str(); } 


private:
  
  BACIThreadManager* threadManager;
  BACIThread *m_controlLoop_p;
  
  ACE_CString channelName;
  CORBA::ORB_ptr intOrb;


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
    ACSSamp::Samp_var foo = client.get_object<ACSSamp::Samp>("SAMP1", 0, true);
    
    if (!CORBA::is_nil(foo.in()))
	{
	
	ACS_SHORT_LOG((LM_DEBUG, "Got samp descriptor()."));

	ACSSamp::SampObj_var fooNew = foo->initSampObj("LAMP1","brightness",1000000,10000000);

	ACS_SHORT_LOG((LM_INFO,"*** Start to sample ***"));

	// get the not. channel and pass it to the client thread
	ACS_SHORT_LOG((LM_INFO,"Not Channel: %s",fooNew->getChannelName()));

	ThreadHandler clientTest(fooNew->getChannelName(),client.getORB());


	//delay to enable client connection to the NC channel
	
	ACE_OS::sleep(10);
	fooNew->start();

 	
	ACE_OS::sleep(15);


	fooNew->suspend();
	ACE_OS::sleep(5);
	fooNew->resume();
	
	ACE_OS::sleep(10);

	fooNew->stop();
	ACE_OS::sleep(2);
      	fooNew->destroy();

	}
    
    client.manager()->release_component(client.handle(), "SAMP1");	
    ACS_SHORT_LOG((LM_INFO, "SAMP1 released"));
    client.logout();
    
    /// sleep for 3 sec to allow everytihng to cleanup and stableize
    ACE_OS::sleep(3);
    
    return 0;
}

/*___oOo___*/






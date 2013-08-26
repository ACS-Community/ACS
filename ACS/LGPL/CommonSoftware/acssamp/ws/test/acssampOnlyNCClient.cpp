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
* "@(#) $Id: acssampOnlyNCClient.cpp,v 1.17 2008/10/07 06:41:54 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* oat 2003-09-07 created
*/

/**
 * This test program is used to demonstrate/test a simple
 * client to get sampling data from the notification channel.
 * It is taken from the NC package.
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
ACE_RCSID(acssampOnlyNCClient, OnlyNCClient, "$Id: acssampOnlyNCClient.cpp,v 1.17 2008/10/07 06:41:54 cparedes Exp $")
    
  
  
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
  {
    
    cout << "Event: domain = \n" << (const char *)notification.header.fixed_header.event_type.domain_name << endl;
    

    acssamp::SampObj::SampDataBlockSeq *m_SampledData_p, m_SampledData;
    m_SampledData_p=&m_SampledData;

    notification.filterable_data[0].value >>= m_SampledData_p;

    cout << "STRUCT_LEN: " << m_SampledData_p->length() << endl;

    for (CORBA::ULong q = 0; q < m_SampledData_p->length(); q++) {

      cout << "TIME STAMP: " <<  (*m_SampledData_p)[q].sampTime << endl;

      CORBA::Double extVal;
      (*m_SampledData_p)[q].sampVal >>= extVal;
      cout << "VALUE: " << extVal << endl;

    }
    
  }
  
  
};






/*******************************************************************************/
    
int main(int argc, char *argv[])
{

    

    ACE_CString fullNCname;

    ACE_CString samplingFrequency;
    ACE_CString reportRate;

    if (argc == 3)
	{
	samplingFrequency=ACE_CString(argv[1]);
	reportRate=ACE_CString(argv[2]);
	}
    else if (argc == 1)
	{
	samplingFrequency="1000000";
	reportRate="10000000";
	}
    else
	{
	ACS_SHORT_LOG((LM_INFO, "usage: acssampOnlyNCServer <sampFrequency> <reportRate>"));
	cout << endl;
	return -1;
	}

    fullNCname="NC_LAMP1_brightness_"+samplingFrequency+"_"+reportRate;

    cout << "used notification channel >> " << fullNCname << endl;
    
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

    SamplerConsumer * myConsumer=0;

    try 
	{

	// the channel name is defined, once the sampling object is activated;
	// see the corresponding acssampTestClient.cpp
	myConsumer = new SamplerConsumer(fullNCname.c_str(),
						       client.getORB());
    
	myConsumer->addSubscription<acssamp::SampObj::SampDataBlockSeq>();
	myConsumer->consumerReady();
	
	ACE_Time_Value time(30,0);
	client.run(time);

	}
    catch(...)
	{
	cerr << "Error when allocating consumer." << endl;
	}

    if (myConsumer)
	delete myConsumer; 
    
    myConsumer = 0;
    
    
    // We release our Component and logout from the Manager
    //    client.manager()->release_component(client.handle(), argv[1]);
    
    client.logout();
	
    
    /// sleep for 3 sec to allow everytihng to cleanup and stableize
    ACE_OS::sleep(3);
    
    return 0;
}

/*___oOo___*/






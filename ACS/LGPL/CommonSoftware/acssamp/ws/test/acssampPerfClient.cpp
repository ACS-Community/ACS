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
* "@(#) $Id: acssampPerfClient.cpp,v 1.10 2008/10/07 06:41:54 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* oat       2004-02-20 created
*/

/**
 * This program is used to evaluate performances of the sampling system.
 * It connects to a server, gets data form the NC and store them in
 * a file for subsequent analysis.
 */

#include <vltPort.h>

#include <fstream>

#include <acsutil.h>

#include <maciSimpleClient.h>
#include <acssampC.h>
#include <acssampS.h>
#include <baciS.h>
#include <acserr.h>

#include <acsncConsumer.h>

using namespace std;
using namespace maci;

ACE_RCSID(acssampPerfClient, PerfClient, "$Id: acssampPerfClient.cpp,v 1.10 2008/10/07 06:41:54 cparedes Exp $")
    

static int endme = 1;
static void stopLoop (int dummy) { 
    endme=0;
}

    
class SamplerConsumer : public nc::Consumer
{

  public:
 
  SamplerConsumer(const char* cName, const char* fName, const CORBA::ORB_ptr orb) : Consumer(cName, orb)
  {
      ACS_SHORT_LOG((LM_INFO,"::SamplerConsumer::SamplerConsumer(names, orb)"));
      init();
      
      file.open(fName);
      if (!file) {
         cerr << "Cannot open file " << fName << " to store data" << endl;
	 throw 1;
      }
      firstTime=0;     
 
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
    
      // cout << "Event: domain = \n" << (const char *)notification.header.fixed_header.event_type.domain_name << endl;

    CORBA::ULongLong diff;


    acssamp::SampObj::SampDataBlockSeq *m_SampledData_p, m_SampledData;
    m_SampledData_p=&m_SampledData;

    notification.filterable_data[0].value >>= m_SampledData_p;

    file << "STRUCT_LEN: " << m_SampledData_p->length() << endl;

    for (CORBA::ULong q = 0; q < m_SampledData_p->length(); q++) {

       CORBA::Double extVal;
       (*m_SampledData_p)[q].sampVal >>= extVal;
    
       if (firstTime == 0)
	   {
	   firstTime= (*m_SampledData_p)[q].sampTime;
	   diff = firstTime;
	   }
       else 
	   {
	   diff= (*m_SampledData_p)[q].sampTime - firstTime;
	   }

       firstTime= (*m_SampledData_p)[q].sampTime;

       file << diff <<  "    " << extVal << endl;

    }
    
  }

  private:

    ofstream file;
    CORBA::ULongLong firstTime; 
};





/*******************************************************************************/
    
int main(int argc, char *argv[])
{

    ACE_CString fullNCname;

    ACE_CString samplingFrequency;
    ACE_CString reportRate;

    ACE_CString fileName;

    if (argc == 4)
	{
	fileName=ACE_CString(argv[1]);
	samplingFrequency=ACE_CString(argv[2]);
	reportRate=ACE_CString(argv[3]);
	}
    else if (argc == 2)
	{
	fileName=ACE_CString(argv[1]);
	samplingFrequency="1000000";
	reportRate="10000000";
	}
    else
	{
	ACS_SHORT_LOG((LM_INFO, "usage: acssampPerfClient <fileName> [<sampFrequency>] [<reportRate>]"));
	cout << endl;
	return -1;
	}

    signal(SIGINT,stopLoop);

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
    // the channel name is defined once the sampling object is activated;
    // see the corresponding acssampTestClient.cpp
	myConsumer = new SamplerConsumer(fullNCname.c_str(),
					 fileName.c_str(),
					 client.getORB());
    

	myConsumer->addSubscription<acssamp::SampObj::SampDataBlockSeq>();
	myConsumer->consumerReady();

	cout << "Infinite loop started; press Ctrl-C to stop it ..." << endl;

	CORBA::ORB_var orb=client.getORB();

	while (endme)
	    {
	    if (orb->work_pending()) 
		orb->perform_work();
	    }
	cout << "... out of the loop!" << endl;

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






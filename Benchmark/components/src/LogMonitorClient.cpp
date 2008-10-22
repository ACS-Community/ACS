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
* "@(#) $Id: LogMonitorClient.cpp,v 1.1 2008/10/22 02:47:49 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
*/


#include <maciSimpleClient.h>
#include <IdlCompilationTimeC.h>

ACE_RCSID(Benchmark,LogSenderClient , "$Id: LogMonitorClient.cpp,v 1.1 2008/10/22 02:47:49 cparedes Exp $")
using namespace maci;
    
int main(int argc, char *argv[])
{
    // Creates and initializes the SimpleClient object
    SimpleClient client;
    if (client.init(argc,argv) == 0)
	{

	return -1;
	}
    else
	{
	//Must log into manager before we can really do anything
	client.login();
	}

ACE_CString centralizedLogger("Log");
      CORBA::Object_var log_obj = client.manager()->get_service(client.handle(), centralizedLogger.c_str(), true);


      if (log_obj.ptr() != CORBA::Object::_nil())
          {
          DsLogAdmin::Log_var logger = DsLogAdmin::Log::_narrow(log_obj.in());
            while (true){
                
                std::cout << "MAX size = " << logger->get_max_size()<<  "N records = " << logger->get_n_records() << " Current size = " << logger->get_current_size() << "\n"; 
                sleep(1);
            }
      }else
            std::cout << "Error\n";    

client.logout();
    
    ACE_OS::sleep(3);
    return 0;
}

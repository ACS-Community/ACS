/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2014 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
*
* who       when      what
* --------  --------  ----------------------------------------------
* almadev  2014-08-28  created 
*/
#include <stdint.h>
#include <stdexcept>
#include <sstream>
#include <orbsvcs/CosEventCommS.h>
#include <orbsvcs/CosEventChannelAdminC.h>
#include <orbsvcs/CosNamingC.h>
#include <orbsvcs/CosNotificationC.h>
#include <orbsvcs/CosNotifyChannelAdminS.h>
#include <orbsvcs/Notify/MonitorControlExt/NotifyMonitoringExtC.h>


#include "corbaNotifyTest_ifC.h"

#include "pDataSupplier.h"


void printUsage() {
	std::cout  << "USAGE: pDataSupplier sendInterval nItems IOR" << std::endl;
	std::cout << "\tsendInterval: interval of time (msec) between 2 sends of a pData" << std::endl;
	std::cout << "\tnItems: number of items to send (0 means forever)" << std::endl;
	std::cout << "\tIOR: the IOR of the notify service" << std::endl << std::endl;
	exit(1);
}

void getParams(int argc,char *argv[],uint32_t &sendInterval,uint32_t &nItems,std::string &iorNS)
{
	if (argc!=4) {
		std::cout << "Wrong command line!" <<std::endl;
		printUsage();
	}

	try {
		sendInterval = atoi(argv[1]);
	} catch(...) {
		std::cout << "Wrong sendInterval value" << std::endl;
		printUsage();
	}
	try {
		nItems = atoi(argv[2]);
	} catch(...) {
		std::cout << "Wrong nItems value" << std::endl;
		printUsage();
	}

	iorNS = argv[3];
}

/**
 * TODOs:
 *    * Add the shutdown hook for a clean exit when CTRL+C is pressed
 */
int main(int argc, char *argv[])
{
	uint32_t sendInterval = 0;
	uint32_t nItems = 0;
	std::string iorNS;
	getParams(argc, argv, sendInterval, nItems, iorNS);

	DataSupplier ds;

        try {
		ds.init_ORB(argc, argv);
		ds.run(sendInterval, nItems, iorNS);
		ds.shutdown();
	} catch(std::exception &ex) {
		std::cout << "Exception: " << ex.what() << std::endl;
		exit(1);
	} catch(...) {
		std::cout << "An unknown exception has been thrown!" << std::endl;
		exit(1);
	}

	std::cout << "The supplier ends ..." << std::endl;

	exit(0);
}

void DataSupplier::init_ORB (int argc,
                      char *argv []
                      )
{
  this->orb = CORBA::ORB_init (argc,  argv, "");


  CORBA::Object_ptr poa_object  =
    this->orb->resolve_initial_references("RootPOA");


  if (CORBA::is_nil (poa_object))
    {
      ACE_ERROR ((LM_ERROR, " (%P|%t) Unable to initialize the POA.\n"));
      return;
    }
  this->root_poa_ =
    PortableServer::POA::_narrow (poa_object);

  PortableServer::POAManager_var poa_manager =
    root_poa_->the_POAManager ();

  poa_manager->activate ();

}


void DataSupplier::run(uint32_t sendInterval,uint32_t nItems,
	const std::string &iorNS)
{
	CORBA::Object_var obj = orb->string_to_object(iorNS.c_str());

	CosNotifyChannelAdmin::EventChannelFactory_var ecf
	  = CosNotifyChannelAdmin::EventChannelFactory::_narrow(obj.in());

	if (CORBA::is_nil(ecf.in()))
		throw std::runtime_error("no event channel factory");
		//std::cout << "Is not an event channel factory!" << std::endl;
        else
		std::cout << "Event channel factory loaded!" << std::endl;

	// Create a channel
	std::cout << "Creating a channel ..." << std::endl;
	CosNotifyChannelAdmin::ChannelID id; 
	CosNotification::QoSProperties init_qos(0); 
	CosNotification::AdminProperties init_admin(0); 
	CosNotifyChannelAdmin::EventChannel_var channel 
	  = ecf->create_channel(init_qos, init_admin, id);
        
	if(CORBA::is_nil(channel.in()))
		throw std::runtime_error("channel cannot be created!");
	else
		std::cout << "Channel created: " << id << std::endl;

	// Get the admin object to the event channel 
	CosEventChannelAdmin::SupplierAdmin_var supplierAdmin 
	  = channel->for_suppliers();
  
	// Obtain a ProxyPushConsumer from the SupplierAdmin.
	CosEventChannelAdmin::ProxyPushConsumer_var consumer
	  = supplierAdmin->obtain_push_consumer();

	// Invoke the connect_push_supplier operation, passing
	// a nil PushSupplier reference to it.
	CosEventComm::PushSupplier_var nilSupplier 
	  = CosEventComm::PushSupplier::_nil();
	consumer->connect_push_supplier(nilSupplier);

	benchmark::MountStatusData data;

	data.antennaName = "DV01";   // The name of the antenna e.g., "DV01"
        //data.timestamp;     // ACS::Time The timestamp of the current value 
        data.onSource = true; // true if the commanded and measured positions are close
        data.azCommanded = 1.3;   // The commanded Az position
        data.elCommanded = 1.5;   // The commanded El position
        data.azCommandedValid = true; // true if there is a az command at this time
        data.elCommandedValid = true; // true if there is a el command at this time
        data.azPrePosition = 1.5; // The measured az position 24 ms before timestamp
        data.azPosition = 1.7;    // The measured Az position at the timestamp
        data.elPrePosition = 1.9; // The measured el position 24 ms before timestamp
        data.elPosition = 2.3;    // The measured el position at teh timestamp
        data.azPositionsValid = true; // true if there is a measured az position at this time
        data.elPositionsValid = true; // true if there is a measured el position at this time
        data.azPointingModelCorrection = 2.5; // The correction in az applied by the pointing model
        data.elPointingModelCorrection = 2.7; // The correction in el applied by the pointing model
        data.pointingModel = true; // true if a pointing model is being used
        data.azAuxPointingModelCorrection = 2.9; // The correction in az applied by the aux pointing model
        data.elAuxPointingModelCorrection = 3.1; // The correction in el applied by the aux pointing model
        data.auxPointingModel = true; // true if a aux pointing model is being used
        data.azEncoder = 3.3;     // The az encoder reading at this time. 
        data.elEncoder = 3.5;     // The el encoder reading at this time
        data.azEncoderValid = true; // true if the az encoder reading is valid
        data.elEncoderValid = true; // true if the el encoder reading is valid
        data.subrefX = 3.7; // Measured X position of the subreflector
        data.subrefY = 3.9; // Measured Y position of the subreflector
        data.subrefZ = 4.1; // Measured Z position of the subreflector
        data.subrefPositionValid = true; // true if the subreflector position was measured
        data.subrefTip = 4.3; // Measured tip of the subreflector
        data.subrefTilt = 4.5; // Measured tilt of the subreflector
        data.subrefRotationValid = true; // true if the subreflector rotation was measured
        data.subrefCmdX = 4.7; // Commanded X position of the subreflector
        data.subrefCmdY = 4.9; // Commanded Y position of the subreflector
        data.subrefCmdZ = 5.1; // Commanded Z position of the subreflector
        data.subrefPositionCmdValid = true; // true if a command was sent to position the subreflector
        data.subrefCmdTip = 5.3; // Commanded tip of the subreflector
        data.subrefCmdTilt = 5.5; // Commanded tilt of the subreflector
        data.subrefRotationCmdValid = true; // true if a command was sent to rotate the subreflector


	for(uint32_t i = 0;nItems == 0 || i < nItems; ++i)
	{

		std::ostringstream oss;
		oss << "AN_" << i;
		data.antennaName = oss.str().c_str();

		std::cout << "Iteration " << data.antennaName << std::endl;

		CORBA::Any any;
		any <<= data;
		consumer->push(any);

		if(sendInterval > 0)
		{
			sleep(sendInterval);
		}
	}

	consumer->disconnect_push_consumer();

	std::cout << "Deleting the channel ..." << std::endl;
	channel->destroy();
}

void DataSupplier::shutdown () {
   // shutdown the ORB.
  if (!CORBA::is_nil (this->orb.in ()))
    {
      this->orb->shutdown(true);
      this->orb->destroy();
    }
}

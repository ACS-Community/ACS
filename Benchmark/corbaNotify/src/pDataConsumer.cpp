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

#include "pDataConsumer.h"

void printUsage() {
	std::cout  << "USAGE: pDataConsummer channelID IOR" << std::endl;
	std::cout << "\tchannelID: the number id of the notification channel" << std::endl;
	std::cout << "\tIOR: the IOR of the notify service" << std::endl << std::endl;
	exit(1);
}

void getParams(int argc,char *argv[],CosNotifyChannelAdmin::ChannelID &channelID,std::string &iorNS)
	 
{
	if (argc!=3) {
		std::cout << "Wrong command line!" <<std::endl;
		printUsage();
	}

	try {
		channelID = atoi(argv[1]);
	} catch(...) {
		std::cout << "Wrong channelID value" << std::endl;
		printUsage();
	}

	iorNS = argv[2];
}

DataConsumer::DataConsumer (CORBA::ORB_ptr orb):
		orb(CORBA::ORB::_duplicate(orb))
{}

void DataConsumer::push (const CORBA::Any& data) {
	std::cout<< "DataConsumer::push received something" << std::endl;
}

void DataConsumer::disconnect_push_consumer(void) {
	/*CORBA::Object_var obj = orb->resolve_initial_references("djb");
	PortableServer::Current_var current = PortableServer::Current::_narrow(obj.in());
	PortableServer::POA_var poa = current->get_POA();
	PortableServer::ObjectId_var objectId = current->get_object_id();
	poa->deactivate_object(objectId.in());
	*/
}

int main(int argc, char *argv[])
{
	CosNotifyChannelAdmin::ChannelID channelID;
	std::string iorNS;
	getParams(argc, argv, channelID, iorNS);

 // Initialize the ORB.
	CORBA::ORB_var orb = CORBA::ORB_init(argc, argv, "");

	CORBA::Object_var poa_object = orb->resolve_initial_references ("RootPOA");

	if (CORBA::is_nil (poa_object))
	{
		ACE_ERROR ((LM_ERROR, " (%P|%t) Unable to initialize the POA.\n"));
		exit(1);
	}

	PortableServer::POA_var root_poa = PortableServer::POA::_narrow (poa_object);

	PortableServer::POAManager_var poa_manager = root_poa->the_POAManager ();
	poa_manager->activate ();


// Get the Notification Channel Factory
	CORBA::Object_var obj = orb->string_to_object(iorNS.c_str());

	CosNotifyChannelAdmin::EventChannelFactory_var ecf
	  = CosNotifyChannelAdmin::EventChannelFactory::_narrow(obj.in());

	if (CORBA::is_nil(ecf.in()))
		throw std::runtime_error("no event channel factory");
	else
		std::cout << "Event channel factory loaded!" << std::endl;

// Get the channel
	try {
		std::cout << "Getting the channel " << channelID << " ..." << std::endl;
		CosNotifyChannelAdmin::EventChannel_var channel 
		  = ecf->get_event_channel(channelID);
        
		if(CORBA::is_nil(channel.in()))
			throw std::runtime_error("channel not exists!");

		DataConsumer dataConsumerservant(orb);
		PortableServer::ObjectId_var oid = root_poa->activate_object(&dataConsumerservant);
		CORBA::Object_var consumer_obj = root_poa->id_to_reference(oid.in());
		CosEventComm::PushConsumer_var consumer = CosEventComm::PushConsumer::_narrow(consumer_obj.in());

		CosEventChannelAdmin::ConsumerAdmin_var consumer_admin =
		      channel->for_consumers ();

		//Get a proxy supplier from the consumer admin
		CosEventChannelAdmin::ProxyPushSupplier_var supplier = consumer_admin->obtain_push_supplier();

		//Connect to the proxy push supplier, passing the push consumer object ref to it
		supplier->connect_push_consumer(consumer.in());

		std::cout << "Running the ORB loop" << std::endl;
		orb->run();

	} catch(CosNotifyChannelAdmin::ChannelNotFound &ex) {
		std::cout << "Exception: channel not found" << std::endl;		
	} catch(std::exception &ex) {
		std::cout << "Exception: " << ex.what() << std::endl;		
	}

	// TODO: handle the clean termination of the process

	// shutdown the ORB.
	std::cout << "Shutting down ORB" << std::endl;
	if (!CORBA::is_nil (orb.in ()))
	{
		orb->shutdown(true);
		orb->destroy();
	}

	std::cout << "The consumer ends ..." << std::endl;
	return EXIT_SUCCESS;
 }

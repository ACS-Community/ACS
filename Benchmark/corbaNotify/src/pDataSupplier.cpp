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
#include "TimespecUtils.h"


void printUsage(const std::string &msgErr="") {

	if(msgErr.size() > 0)
	{
		std::cout << std::endl << "\tERROR: " << msgErr << std::endl << std::endl;
	}

	std::cout << "\tUSAGE: pDataSupplier -i sendInterval -n nItems -r IOR -f channelFile" << std::endl;
	std::cout << "\t\tsendInterval: interval of time (msec) between 2 sends of a pData" << std::endl;
	std::cout << "\t\tnItems: number of items to send (0 means forever)" << std::endl;
	std::cout << "\t\tIOR: the IOR of the notify service" << std::endl;
	std::cout << "\t\tchannelFile: path of the file to store the channel ID used" << std::endl << std::endl;
	exit(1);
}

void getParams(int argc,char *argv[],uint32_t &sendInterval,uint32_t &nItems,std::string &iorNS,std::string &channelFile)
{
	int c;

	sendInterval = 1000;
	nItems = 0;
	iorNS = "";
	channelFile = "";
	std::string str;

	while((c = getopt(argc, argv, "i:n:r:f:")) != -1)
	{
		switch(c)
		{
		case 'i':
			str = optarg;
			if(str.find_first_not_of("0123456789") == std::string::npos)
			{
				sendInterval = atoi(str.c_str());
			} else {
				printUsage("Wrong send interval");
			}
			break;
		case 'n':
			str = optarg;
			if(str.find_first_not_of("0123456789") == std::string::npos)
			{
				nItems = atoi(str.c_str());
			} else {
				printUsage("Wrong number of items");
			}
			break;
		case 'r':
			iorNS = optarg;
			break;
		case 'f':
			channelFile = optarg;
			break;
		default:
			printUsage("Unknown option: " + std::string(1,c));
			break;
		}
	}

	if(iorNS.size() <= 0)
	{
		printUsage("IOR of the Notify Service is required");
	}
}

DataSupplier ds;
void signal_handler(int sig)
{
	ds.stop();
}

/**
 * TODOs:
 *    * Add the shutdown hook for a clean exit when CTRL+C is pressed
 */
int main(int argc, char *argv[])
{
	uint32_t ret = 0;
	uint32_t sendInterval = 0;
	uint32_t nItems = 0;
	std::string iorNS;
	std::string channelFile;

	signal(SIGINT, signal_handler);

	getParams(argc, argv, sendInterval, nItems, iorNS, channelFile);

        try {
		ds.init_ORB(argc, argv);
		ds.run(sendInterval, nItems, iorNS, channelFile);
	} catch(std::exception &ex) {
		ACE_DEBUG((LM_ERROR, "%T Exception: %s\n", ex.what()));
		ret = 1;
	} catch(...) {
		ACE_DEBUG((LM_ERROR, "%T An unknown exception has been thrown!\n"));
		ret = 1;
	}

	ds.shutdown();

	ACE_DEBUG((LM_INFO, "%T Supplier ends ...\n"));

	exit(ret);
}

DataSupplier::DataSupplier()
	: m_stop(false)
{
}

DataSupplier::~DataSupplier()
{
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

void DataSupplier::stop()
{
	m_stop = true;
}

void DataSupplier::run(uint32_t sendInterval,uint32_t nItems,
	const std::string &iorNS,const std::string &channelFile)
{
	CORBA::Object_var obj = orb->string_to_object(iorNS.c_str());

	CosNotifyChannelAdmin::EventChannelFactory_var ecf
	  = CosNotifyChannelAdmin::EventChannelFactory::_narrow(obj.in());

	//NotifyMonitoringExt::EventChannelFactory_var ecf
	//  = NotifyMonitoringExt::EventChannelFactory::_narrow(ecf1.in());

	if (CORBA::is_nil(ecf.in()))
		throw std::runtime_error("no event channel factory");
        else
		std::cout << "Event channel factory loaded!" << std::endl;

	// Create a channel
	ACE_DEBUG((LM_INFO, "%T Creating a channel ...\n"));
	CosNotifyChannelAdmin::ChannelID id; 
	CosNotification::QoSProperties init_qos(0); 
	CosNotification::AdminProperties init_admin(0); 
	CosNotifyChannelAdmin::EventChannel_var channel 
	  = ecf->create_channel(init_qos, init_admin, id);
        
	if(CORBA::is_nil(channel.in()))
		throw std::runtime_error("channel cannot be created!");
	else
		ACE_DEBUG((LM_INFO, "%T Channel created: %d\n", id));

	// Store the channel ID in a file
	if(channelFile.size() > 0)
	{
		std::ofstream f(channelFile.c_str(), ios::out | ios::trunc);
		if(f.is_open() == true)
		{
			f << id;
			f.close();
		}
	}

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

	timespec currTime;
	TimespecUtils::get_current_timespec(currTime);

	data.antennaName = "DV01";   // The name of the antenna e.g., "DV01"
        data.timestamp = TimespecUtils::timespec_2_100ns(currTime);     // ACS::Time The timestamp of the current value 
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

	for(uint32_t i = 0;(nItems == 0 || i < nItems) && m_stop == false; ++i)
	{

		std::ostringstream oss;
		oss << "AN_" << i;
		data.antennaName = oss.str().c_str();

		TimespecUtils::get_current_timespec(currTime);
		data.timestamp = TimespecUtils::timespec_2_100ns(currTime);

		ACE_DEBUG((LM_INFO, "%T Iteration %d in channel %d with timestamp %s\n", i, id, 
			TimespecUtils::timespec_2_str(currTime).c_str()));

		CORBA::Any any;
		any <<= data;
		consumer->push(any);

		if(sendInterval > 0)
		{
			usleep(sendInterval * 1000);
		}
	}

	if(m_stop == true)
	{
		ACE_DEBUG((LM_INFO, "%T Stopping it ...\n"));
	}

	consumer->disconnect_push_consumer();

	ACE_DEBUG((LM_INFO, "%T Deleting the channel %d ...\n", id));
	channel->destroy();
}

void DataSupplier::shutdown () 
{
	// shutdown the ORB.
	if (!CORBA::is_nil (this->orb.in ()))
	{
		this->orb->shutdown(true);
		this->orb->destroy();
	}
}

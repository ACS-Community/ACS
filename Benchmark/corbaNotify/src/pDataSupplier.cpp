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
#include <tao/Messaging/Messaging.h>

#include <ORB_Core.h>
#include <Reactor.h>

#include "corbaNotifyTest_ifC.h"

#include "pDataSupplier.h"
#include "TimevalUtils.h"
#include "SupplierTimer.h"
#include "CorbaNotifyUtils.h"


void printUsage(const std::string &msgErr="") {

	if(msgErr.size() > 0)
	{
		std::cout << std::endl << "\tERROR: " << msgErr << std::endl << std::endl;
	}

	std::cout << "\tUSAGE: pDataSupplier -i sendInterval -l arrayLength -n nItems -r IOR -c channel -f channelFile -o outputDelay -a antennaPrefixName -b ORBOptions -t timeout" << std::endl;
	std::cout << "\t\tsendInterval: interval of time (msec) between 2 sends of a pData" << std::endl;
    std::cout << "\t\tarrayLength: length of the array sent. Default value is 1" << std::endl;
	std::cout << "\t\tnItems: number of items to send (0 means forever)" << std::endl;
	std::cout << "\t\tIOR: the IOR of the notify service" << std::endl;
	std::cout << "\t\tchannel: channel ID. New channel is created when the channel ID is not defined" << std::endl;
	std::cout << "\t\tchannelFile: path of the file to store the channel ID used" << std::endl;
	std::cout << "\t\toutputDelay: time interval in minutes between two consecutive output messages. Default is 1 minute." << std::endl;
	std::cout << "\t\tantennaPrefixName: antenna prefix name. Default value is ANTENNA_" << std::endl;
	std::cout << "\t\tORBOptions: options passed in the initialization of ORB" << std::endl;
    std::cout << "\t\ttimeout: timeout in milliseconds at different levels: \"orb:10,thread:100,proxy:100\"" << std::endl;
	std::cout << std::endl;
	exit(1);
}

static const std::string STR_TIMEOUT_ORB="orb";
static const std::string STR_TIMEOUT_THREAD="thread";
static const std::string STR_TIMEOUT_PROXY="proxy";

void fillTimeout(TimeoutMS &timeout,const std::string &config)
{
    
    std::string substr = config;
    std::string keyValue;
    std::string key;
    std::string value;
    uint32_t iValue;
    std::size_t pos = substr.find_first_of(",");
    if(pos == std::string::npos)
    {
        pos = substr.size();
    }
    std::size_t pos2;
    while(false == substr.empty())
    {
        keyValue = substr.substr(0, pos);
        pos2 = keyValue.find_first_of(":");
        if(pos2 != std::string::npos)
        {
            key = keyValue.substr(0,pos2);
            value = keyValue.substr(pos2+1);
            if(value.find_first_not_of("0123456789") == std::string::npos)
            {
                iValue = atoi(value.c_str());
                if(key == STR_TIMEOUT_ORB)
                {
                    timeout.orb = iValue;
                } else if(key == STR_TIMEOUT_THREAD) {
                    timeout.thread = iValue;
                } else if(key == STR_TIMEOUT_PROXY) {
                    timeout.proxy = iValue;
                }    
            }    
        }
        if(substr.size() > pos + 1)
        {
            substr = substr.substr(pos + 1);
        } else {
            substr = std::string();
        }
        pos = substr.find_first_of(",");
        if(pos == std::string::npos)
        {
            pos = substr.size();
        }
    }
}

/**
 * Get command line parameters
 */
void getParams(int argc,char *argv[],SuppParams &params)
{
	int c;
	std::string str;

	params.sendInterval = DEFAULT_SEND_INTERVAL;
    params.arrayLength = DEFAULT_ARRAY_LENGTH;
	params.nItems = DEFAULT_NUM_ITEMS;
	params.iorNS = DEFAULT_IOR_NS;
	params.channelFile = DEFAULT_CHANNEL_FILE;
	params.outputDelay = DEFAULT_OUTPUT_DELAY;
	params.channelID = DEFAULT_CHANNEL_ID;
	params.antennaPrefixName = DEFAULT_ANTENNA_PREFIX_NAME;
    params.timeout.orb = 0;
    params.timeout.thread = 0;
    params.timeout.proxy = 0;

	while((c = getopt(argc, argv, "a:b:c:i:l:n:r:t:f:o:")) != -1)
	{
		switch(c)
		{
		case 'a':
			params.antennaPrefixName = optarg;
			break;
		case 'b':
			params.ORBOptions = optarg;
			break;
		case 'c':
			str = optarg;
			if(str.find_first_not_of("0123456789") == std::string::npos)
			{
				params.channelID = atoi(str.c_str());
			} else {
				printUsage("Wrong channel ID. Must be an integer");
			}
			break;
		case 'i':
			str = optarg;
			if(str.find_first_not_of("0123456789") == std::string::npos)
			{
				params.sendInterval = atoi(str.c_str());
			} else {
				printUsage("Wrong send interval. Must be an integer");
			}
			break;
        case 'l':
            str = optarg;
			if(str.find_first_not_of("0123456789") == std::string::npos)
			{
				params.arrayLength = atoi(str.c_str());
			} else {
				printUsage("Wrong array length. Must be an integer");
			}
			break;
		case 'n':
			str = optarg;
			if(str.find_first_not_of("0123456789") == std::string::npos)
			{
				params.nItems = atoi(str.c_str());
			} else {
				printUsage("Wrong number of items. Must be an integer");
			}
			break;
		case 'o':
			str = optarg;
			if(str.find_first_not_of("0123456789") == std::string::npos)
			{
				params.outputDelay = atoi(str.c_str());
			} else {
				printUsage("Wrong output delay. Must be an integer");
			}
			break;
		case 'r':
			params.iorNS = optarg;
			break;
        case 't':
            fillTimeout(params.timeout, optarg);
            break;
		case 'f':
			params.channelFile = optarg;
			break;
		default:
			printUsage("Unknown option: " + std::string(1,c));
			break;
		}
	}

	if(params.iorNS.size() <= 0)
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
 *
 */
int main(int argc, char *argv[])
{
	uint32_t ret = 0;
	SuppParams params;

	// Register signal handler for Ctrl + C
	signal(SIGINT, signal_handler);

	// Get command line parameters
	getParams(argc, argv, params);

	int orbArgc = 0;
	ACE_TCHAR **orbArgv = NULL;
	CorbaNotifyUtils::getORBOptions("pDataSupplier", params.ORBOptions, orbArgc, &orbArgv);

    try {
		ds.init_ORB(argc, argv);
		ds.run(params);
    } catch(CORBA::TIMEOUT &ex) {
        ACE_DEBUG((LM_ERROR, "%T CORBA::TIMEOUT Exception: %s\n", ex._info().c_str()));
        ret = 1;
	} catch(std::exception &ex) {
		ACE_DEBUG((LM_ERROR, "%T Exception: %s\n", ex.what()));
		ret = 1;
	} catch(...) {
		ACE_DEBUG((LM_ERROR, "%T An unknown exception has been thrown!\n"));
		ret = 1;
	}

	CorbaNotifyUtils::delORBOptions(orbArgc, &orbArgv);

	ds.shutdown();

	ACE_DEBUG((LM_INFO, "%T Supplier ends ...\n"));

	exit(ret);
}

DataSupplier::DataSupplier()
	: m_stop(false), m_numEventsSent(0), m_numEventsSentOk(0), m_timer(0)
{
	for(uint32_t i = 0;i < NUM_ERR;++i)
	{
		m_numEventsSentErr[i] = 0;
	}
}

DataSupplier::~DataSupplier()
{
    if(m_timer != 0)
    {
        delete m_timer;
    }
}

void DataSupplier::init_ORB (int argc,
                      char *argv []
                      )
{
	this->orb = CORBA::ORB_init (argc,  argv, "");

	CORBA::Object_var poa_object  =
		this->orb->resolve_initial_references("RootPOA");

	if (CORBA::is_nil (poa_object))
	{
		throw std::runtime_error("Unable to initialize the POA");
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

uint64_t DataSupplier::getNumEventsSent() const
{
	return m_numEventsSent;
}

uint64_t DataSupplier::getNumEventsSentOk() const
{
	return m_numEventsSentOk;
}

uint64_t DataSupplier::getNumEventsSentErrTimeout() const
{
	return m_numEventsSentErr[POS_ERR_TIMEOUT];
}

uint64_t DataSupplier::getNumEventsSentErrTransient() const
{
	return m_numEventsSentErr[POS_ERR_TRANSIENT];
}

uint64_t DataSupplier::getNumEventsSentErrObjNotExist() const
{
	return m_numEventsSentErr[POS_ERR_OBJ_NOT_EXIST];
}

uint64_t DataSupplier::getNumEventsSentErrCommFailure() const
{
	return m_numEventsSentErr[POS_ERR_COMM_FAILURE];
}

uint64_t DataSupplier::getNumEventsSentErrUnknown() const
{
	return m_numEventsSentErr[POS_ERR_UNKNOWN];
}


void DataSupplier::run(const SuppParams &params)
{
    // Set timeout at ORB level
    if(params.timeout.orb > 0)
    {
        // Set the policy value
        TimeBase::TimeT relative_rt_timeout = params.timeout.orb * 10000/*1millisecond*/;
        CORBA::Any relative_rt_timeout_as_any;  
        relative_rt_timeout_as_any <<= relative_rt_timeout;  

        // Create the policy and add it to a CORBA::PolicyList.  
        CORBA::PolicyList policy_list;  
        policy_list.length(1);  
        policy_list[0] = orb->create_policy (Messaging::RELATIVE_RT_TIMEOUT_POLICY_TYPE, relative_rt_timeout_as_any);  

        // Apply the policy at the ORB level.  
        CORBA::Object_var obj = orb->resolve_initial_references("ORBPolicyManager");  
        CORBA::PolicyManager_var policy_manager = CORBA::PolicyManager::_narrow(obj.in());  
        policy_manager->set_policy_overrides (policy_list, CORBA::ADD_OVERRIDE);
        ACE_DEBUG((LM_INFO, "%T Changed ORB timeout to be: %dms\n", params.timeout.orb));
    }

    // Set timeout at thread level
    if(params.timeout.thread > 0)
    {
        // Set the policy value
        TimeBase::TimeT relative_rt_timeout = params.timeout.thread * 10000/*1millisecond*/;
        CORBA::Any relative_rt_timeout_as_any;  
        relative_rt_timeout_as_any <<= relative_rt_timeout;  

        // Create the policy and add it to a CORBA::PolicyList.  
        CORBA::PolicyList policy_list;  
        policy_list.length(1);  
        policy_list[0] = orb->create_policy (Messaging::RELATIVE_RT_TIMEOUT_POLICY_TYPE, relative_rt_timeout_as_any);  

        // Apply the policy at the thread level.  
        CORBA::Object_var obj = orb->resolve_initial_references("PolicyCurrent");  
        CORBA::PolicyManager_var policy_manager = CORBA::PolicyManager::_narrow(obj.in());  
        policy_manager->set_policy_overrides (policy_list, CORBA::ADD_OVERRIDE);
        ACE_DEBUG((LM_INFO, "%T Changed thread timeout to be: %dms\n", params.timeout.thread));
    }


	CORBA::Object_var obj = orb->string_to_object(params.iorNS.c_str());

	CosNotifyChannelAdmin::EventChannelFactory_var ecf
	  = CosNotifyChannelAdmin::EventChannelFactory::_narrow(obj.in());

	if (CORBA::is_nil(ecf.in()))
		throw std::runtime_error("no event channel factory");
    else
		ACE_DEBUG((LM_INFO,"%T Event channel factory loaded!\n"));

	CosNotifyChannelAdmin::ChannelID id; 
	CosNotification::QoSProperties init_qos(0); 
	CosNotification::AdminProperties init_admin(0); 
	CosNotifyChannelAdmin::EventChannel_var channel;

	// Create a channel if no channel ID is given
	if(params.channelID < 0)
	{
		ACE_DEBUG((LM_INFO, "%T Creating a channel ...\n"));
		channel = ecf->create_channel(init_qos, init_admin, id);
		if(CORBA::is_nil(channel.in()))
			throw std::runtime_error("channel cannot be created!");
		else
			ACE_DEBUG((LM_INFO, "%T Channel created: %d\n", id));

	// Get the channel associated to the given channel ID
	} else {
		ACE_DEBUG((LM_INFO, "%T Getting the channel ...\n"));
		id = params.channelID;
		channel = ecf->get_event_channel(id);
		if(CORBA::is_nil(channel.in()))
			throw std::runtime_error("channel not found!");
		else
			ACE_DEBUG((LM_INFO, "%T Channel found: %d\n", id));
	}


	// Store the channel ID in a file
	saveChannelId(params.channelFile, id);

	// Get the admin object to the event channel 
	CosEventChannelAdmin::SupplierAdmin_var supplierAdmin 
	  = channel->for_suppliers();
  
	// Obtain a ProxyPushConsumer from the SupplierAdmin.
	CosEventChannelAdmin::ProxyPushConsumer_var consumer
	  = supplierAdmin->obtain_push_consumer();

// Check timeout policies in the Admin
/*
    //ACE_DEBUG((LM_INFO, "Checking policies of admin\n"));
    CORBA::PolicyTypeSeq policies;
    policies.length(1);
    policies[0] = Messaging::RELATIVE_RT_TIMEOUT_POLICY_TYPE;
    CORBA::PolicyList *policy_list = supplierAdmin->_get_policy_overrides(policies);
    for(CORBA::ULong i = 0;i < policy_list->length();++i)
    {
        CORBA::PolicyType type = (*policy_list)[i]->policy_type();
        if(type == Messaging::RELATIVE_RT_TIMEOUT_POLICY_TYPE) 
        {
            Messaging::RelativeRoundtripTimeoutPolicy_var pol
                = Messaging::RelativeRoundtripTimeoutPolicy::_narrow(policies[i]);
            TimeBase::TimeT timeout = pol->relative_expiry();
            ACE_DEBUG((LM_INFO, "Round-trip Timeout: %lu ms\n", (unsigned long)(timeout/10000)));
        }
    }*/

    // Set timeout at proxy level
    if(params.timeout.proxy > 0)
    {
        // Set the policy value
        TimeBase::TimeT relative_rt_timeout = params.timeout.proxy * 10000/*1millisecond*/;
        CORBA::Any relative_rt_timeout_as_any;
        relative_rt_timeout_as_any <<= relative_rt_timeout;

        // Create the policy and add it to a CORBA::PolicyList.  
        CORBA::PolicyList policy_list;  
        policy_list.length(1);  
        policy_list[0] = orb->create_policy (Messaging::RELATIVE_RT_TIMEOUT_POLICY_TYPE, relative_rt_timeout_as_any);  

        // Apply the policy at the proxy level.  
        consumer->_set_policy_overrides(policy_list, CORBA::SET_OVERRIDE);
        ACE_DEBUG((LM_INFO, "%T Changed proxy timeout to be: %dms\n", params.timeout.proxy));
    }

	// Invoke the connect_push_supplier operation, passing
	// a nil PushSupplier reference to it.
	CosEventComm::PushSupplier_var nilSupplier 
	  = CosEventComm::PushSupplier::_nil();
	consumer->connect_push_supplier(nilSupplier);

	// Set a timer to periodically output the number of messages sent
    if(m_timer == 0)
    {
	    m_timer = new SupplierTimer(*this);
    }
	ACE_Time_Value timeout(params.outputDelay * 60, 0);
	this->orb->orb_core()->reactor()->schedule_timer(m_timer, 0, timeout, timeout);

	timeval currTime;
	TimevalUtils::get_current_timeval(currTime);

	// Create the object to be sent through the channel
	benchmark::MountStatusData data;
	data.antennaName = params.antennaPrefixName.c_str();   // The name of the antenna e.g., "DV01"
	data.timestamp = TimevalUtils::timeval_2_ms(currTime);     // ACS::Time The timestamp of the current value
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

	benchmark::MountStatusDataSeq dataSeq;
    dataSeq.length(params.arrayLength);
    for(uint32_t i = 0;i < params.arrayLength;++i)
    {
        dataSeq[i] = data;
    }

    ACE_DEBUG((LM_INFO, "%T Sequence to be sent has been initialized with %d items\n", params.arrayLength));

	for(uint32_t i = 0;(params.nItems == 0 || i < params.nItems) && m_stop == false; ++i)
	{

        for(uint32_t j = 0;j < params.arrayLength;++j)
        {
		    std::ostringstream oss;
		    oss << params.antennaPrefixName << i;
		    dataSeq[j].antennaName = oss.str().c_str();

    		TimevalUtils::get_current_timeval(currTime);
	    	dataSeq[j].timestamp = TimevalUtils::timeval_2_ms(currTime);
        }

		//ACE_DEBUG((LM_INFO, "%T Iteration %d in channel %d with timestamp %q\n", i, id, (int64_t)data.timestamp));

		CORBA::Any any;
		any <<= dataSeq;

		try {
			consumer->push(any);
			++m_numEventsSentOk;
	        } catch(CORBA::TIMEOUT &ex) {
			++m_numEventsSentErr[POS_ERR_TIMEOUT];
		} catch(CORBA::OBJECT_NOT_EXIST &ex) {
			++m_numEventsSentErr[POS_ERR_OBJ_NOT_EXIST];
		} catch(CORBA::TRANSIENT &ex) {
			++m_numEventsSentErr[POS_ERR_TRANSIENT];
		} catch(CORBA::COMM_FAILURE &ex) {
			++m_numEventsSentErr[POS_ERR_COMM_FAILURE];
	        } catch(...) {
			++m_numEventsSentErr[POS_ERR_UNKNOWN];
	        }

		if(params.sendInterval > 0)
		{
			usleep(params.sendInterval * 1000);
		}

		++m_numEventsSent;
	}

	if(m_stop == true)
	{
		ACE_DEBUG((LM_INFO, "%T Stopping it ...\n"));
	}

	consumer->disconnect_push_consumer();

	if(params.channelID < 0)
	{
		ACE_DEBUG((LM_INFO, "%T Deleting channel %d ...\n", id));
		channel->destroy();
	}
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

/**
 * Store the channel ID in a file
 */
void DataSupplier::saveChannelId(const std::string &file,
		CosNotifyChannelAdmin::ChannelID channelID)
{
	if(file.size() > 0)
	{
		std::ofstream f(file.c_str(), ios::out | ios::trunc);
		if(f.is_open() == true)
		{
			f << channelID;
			f.close();
		} else {
			ACE_DEBUG((LM_ERROR, "%T Channel ID file %s cannot be opened\n", file.c_str()));
		}
	}
}

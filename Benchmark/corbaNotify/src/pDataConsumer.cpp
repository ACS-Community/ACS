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
#include "pDataConsumer.h"
#include "TimevalUtils.h"
#include "ORBTask.h"
#include "ConsumerTimer.h"
#include "CorbaNotifyUtils.h"

#include <tao/Messaging/Messaging.h>
#include <ORB_Core.h>
#include <Reactor.h>

#include <stdint.h>
#include <stdexcept>
#include <sstream>
#include <vector>
#include <syslog.h>

#include "corbaNotifyTest_ifC.h"



void printUsage(const std::string &errMsg="") 
{

	if(errMsg.size() > 0)
	{
		std::cout << std::endl << "\tERROR: " << errMsg << std::endl << std::endl;
	}

	std::cout << "\tUSAGE: pDataConsummer -c channel -r IOR -d maxDelaySec -t delayType -i intervalMin -m timeout -o ORBOptions -s sleepTimeRecvEvent" << std::endl;
	std::cout << "\t\tchannel: the ID of the notification channel or the path where the channel ID is stored. If the ID is not set, it creates a new channel" << std::endl;
	std::cout << "\t\tIOR: the IOR of the Notify Service" << std::endl;
	std::cout << "\t\tmaxDelaySec: maximum delay allowed in seconds" << std::endl;
	std::cout << "\t\tdelayType: type of the delay to calculate. Can be:" << std::endl;
	std::cout << "\t\t\tCONSUMER: elapsed time between consecutive events in the consumer" << std::endl; 
	std::cout << "\t\t\tSUPPLIER: elapsed time between consecutive events in the supplier" << std::endl;
	std::cout << "\t\t\tSUPP_CON: delay of one event" << std::endl;
	std::cout << "\t\t\tDefault is SUPP_CON" << std::endl;
	std::cout << "\t\tintervalMin: time interval in minutes between consecutive output messages showing the number of events received" << std::endl;
    std::cout << "\t\ttimeout: timeout in milliseconds at different levels: \"orb:10,thread:100\"" << std::endl;
	std::cout << "\t\tORBOptions: options passed in the initialization of ORB" << std::endl;
	std::cout << "\t\tsleepTimeRecvEvent: sleep time set when receiving an event. Default value is 0." << std::endl;

	std::cout << std::endl;
	exit(1);
}

static const std::string STR_TIMEOUT_ORB="orb";
static const std::string STR_TIMEOUT_THREAD="thread";
static const std::string STR_TIMEOUT_PROXY="proxy";

void getParams(int argc,char *argv[],ConsumerParams &params)
	 
{
	int c;
	std::string str;
	/*std::string channel;
	std::string sdelay;
	std::string sinterval;*/

	params.channelID = DEFAULT_CHANNEL_ID;
	params.iorNS = DEFAULT_IOR_NS;
	params.maxDelaySec = DEFAULT_MAX_DELAY_SEC;
	params.delayType = DEFAULT_DELAY_TYPE;
	params.interval = DEFAULT_INTERVAL;
	params.sleepTimeRecvEvent = DEFAULT_SLEEP_TIME_RECV_EVENT;
    params.timeout.orb = 0;
    params.timeout.thread = 0;
    params.timeout.proxy = 0;

	while((c = getopt(argc, argv, "c:i:o:r:d:m:s:t:")) != -1)
	{
		switch(c)
		{
		case 'c':
			str = optarg;

			if(str.find_first_not_of("0123456789") != std::string::npos)
			{
				std::ifstream f(str.c_str());
				if(f.is_open() == true)
				{
					std::string sch;
					std::getline(f, sch);
					f.close();
					try {
						params.channelID = atoi(sch.c_str());

					} catch(...) {
						printUsage("Wrong channel ID: " + sch);
					}
				} else {
					printUsage("Wrong channel ID file: " + str);
				}
			} else {
				params.channelID = atoi(str.c_str());
			}
			break;
		case 'i':
			str = optarg;
			if(str.find_first_not_of("0123456789") != std::string::npos)
			{
				printUsage("Wrong interval. Must be an integer");
			}
			params.interval = atoi(str.c_str());
			break;
		case 'o':
			params.ORBOptions = optarg;
			break;
		case 'r':
			params.iorNS = optarg;
			break;
		case 'd':
			str = optarg;
			if(str.find_first_not_of("0123456789.") != std::string::npos)
			{
				printUsage("Wrong delay. Must be a number");
			} else {
				params.maxDelaySec = atof(str.c_str());
			}
			break;
        case 'm':
            TimevalUtils::fillTimeout(params.timeout, optarg);
            break;
		case 's':
			str = optarg;
			if(str.find_first_not_of("0123456789.") != std::string::npos)
			{
				printUsage("Wrong sleep time. Must be a number");
			} else {
				params.sleepTimeRecvEvent = atof(str.c_str());
			}
			break;
		case 't':
			str = optarg;
			if(str != DT_CONSUMER && str != DT_SUPPLIER && str != DT_SUPP_CON)
			{
				printUsage("Unknown delay type: " + str);
			} else {
				params.delayType = str;
			}
			break;
		default:
			printUsage("Unknown option: " + std::string(1, c));
			break;
		}
	}

	/*
	if(params.channelID == -1)
	{
		printUsage("A channel ID is required");
	}*/

	if(params.iorNS.size() <= 0)
	{
		printUsage("IOR of the Notify Service is required");
	}
}

Consumer consumer;

void signal_handler(int sig)
{
	ACE_DEBUG((LM_INFO, "%T Stopping it ...\n"));
    try {
	    consumer.disconnect_push_consumer();
    } catch(...) {}
}

int main(int argc, char *argv[])
{
	ConsumerParams params;

	signal(SIGINT, signal_handler);

	getParams(argc, argv, params);

	consumer.run(argc, argv, params);

	ACE_DEBUG((LM_INFO, "Consumer ends ...\n"));
	return EXIT_SUCCESS;
 }

Consumer::Consumer()
{
	TimevalUtils::set_timeval(m_tLastEvent, 0, 0);
	m_maxDelay = 0;
	m_lastEventTimestamp = 0;
	m_delayType = DT_SUPP_CON;
	m_numEventsReceived = 0;
	m_timer = 0;
	m_sleepTimeRecvEvent = 0;
}

Consumer::~Consumer()
{
    if(m_timer != 0)
    {
        delete m_timer;
    }
}


bool Consumer::run (int argc, ACE_TCHAR* argv[],const ConsumerParams &params)
{
	
	m_maxDelay = params.maxDelaySec * 1000;
	m_sleepTimeRecvEvent = params.sleepTimeRecvEvent;
	ACE_DEBUG((LM_INFO, "Max delay: %q ms\n", m_maxDelay));
	m_delayType = params.delayType;

	try {
		int orbArgc = 0;
		ACE_TCHAR **orbArgv = NULL;
		CorbaNotifyUtils::getORBOptions("pDataConsumer", params.ORBOptions, orbArgc, &orbArgv);

		std::string errMsg;
		bool initialized = init_ORB(orbArgc, orbArgv);
		CorbaNotifyUtils::delORBOptions(orbArgc, &orbArgv);

		if(initialized == false)
		{
			return false;	
		}


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
            policy_list[0] = m_orb->create_policy (Messaging::RELATIVE_RT_TIMEOUT_POLICY_TYPE, relative_rt_timeout_as_any);  

            // Apply the policy at the ORB level.  
            CORBA::Object_var obj = m_orb->resolve_initial_references("ORBPolicyManager");  
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
            policy_list[0] = m_orb->create_policy (Messaging::RELATIVE_RT_TIMEOUT_POLICY_TYPE, relative_rt_timeout_as_any);  

            // Apply the policy at the thread level.  
            CORBA::Object_var obj = m_orb->resolve_initial_references("PolicyCurrent");  
            CORBA::PolicyManager_var policy_manager = CORBA::PolicyManager::_narrow(obj.in());  
            policy_manager->set_policy_overrides (policy_list, CORBA::ADD_OVERRIDE);
            ACE_DEBUG((LM_INFO, "%T Changed thread timeout to be: %dms\n", params.timeout.thread));
        }


		CosNotifyChannelAdmin::EventChannel_var channel;
		CosEventChannelAdmin::ConsumerAdmin_var consumer_admin;
		CosEventChannelAdmin::ProxyPushSupplier_var supplier;
		CosEventComm::PushConsumer_var consumer;
		CosNotifyChannelAdmin::ChannelID channelID = params.channelID;

		if(getNotificationChannel(params.iorNS,channelID,channel,errMsg) == false)
		{
			ACE_DEBUG((LM_ERROR, "Error: %s", errMsg.c_str()));
			return false;
		}

		consumer_admin = channel->for_consumers ();
		supplier = consumer_admin->obtain_push_supplier ();
		consumer = this->_this ();
		supplier->connect_push_consumer (consumer.in ());

		ACE_DEBUG((LM_INFO, "Waiting for events in channel %d ...\n", channelID));

		// Create & schedule the timer
        if(0 == m_timer)
        {
		    m_timer = new ConsumerTimer(*this);
        }
		ACE_Time_Value timeout(params.interval * 60, 0);
		m_orb->orb_core()->reactor()->schedule_timer(m_timer, 0, timeout, timeout);

		ORB_Task task(m_orb);

		int retval = task.activate (THR_NEW_LWP | THR_JOINABLE);
		if (retval != 0)
		{
			ACE_ERROR((LM_ERROR,"Failed to activate the ORB task"));
			return false;
		}

		task.wait();

	} catch(CORBA::Exception &ex) {
		ex._tao_print_exception ("Consumer::run");
	} catch(std::exception &stdEx) {
		ACE_ERROR((LM_ERROR, (std::string("Consumer::run exception: ") + stdEx.what() + "\n").c_str()));
	} catch(...) {
		ACE_ERROR((LM_ERROR, "Unknown exception in Consumer::run\n"));
	}

	return true;
}

void Consumer::push (const CORBA::Any &event)
{
	uint64_t currTimestamp;
	int64_t tsSupplierDiff;
	int64_t tSuppConDiff;
	int64_t tConsumerDiff;
    int32_t seqLength;
	//timeval tSupplierDiff;
	//timeval tSuppConDiff;
	timeval tEvent;

    benchmark::MountStatusData *data;
	benchmark::MountStatusDataSeq *dataSeq;
	if(event >>= dataSeq)
	{
		++m_numEventsReceived;

        if(dataSeq->length() > 0)
        {
            seqLength = dataSeq->length();
            data = &(*dataSeq)[0];

            // Calculate current event time
            TimevalUtils::get_current_timeval(tEvent);
            currTimestamp = TimevalUtils::timeval_2_ms(tEvent);

            // Check the delay of the current event
            if(m_tLastEvent.tv_sec > 0 || m_tLastEvent.tv_usec > 0)
            {
                // Calculate delay between consecutive events in the consumer
                tConsumerDiff = TimevalUtils::diff_timeval(tEvent, m_tLastEvent);

                // Calculate delay between consecutive events in the supplier
                tsSupplierDiff = (int64_t)data->timestamp - (int64_t)m_lastEventTimestamp;

                // Calculate delay between supplier and consumer
                tSuppConDiff = (int64_t)currTimestamp - (int64_t)data->timestamp;

                if(m_maxDelay > 0)
                {
                    if(m_maxDelay < tsSupplierDiff && m_delayType == DT_SUPPLIER)
                    {
                        ACE_DEBUG((LM_NOTICE, "%T Event received %s [length=%d] with supplier delay %q ms but maximum allowed is %q ms\n",
                            data->antennaName.in(), seqLength, tsSupplierDiff, m_maxDelay));
                    }

                    if(m_maxDelay < tConsumerDiff && m_delayType == DT_CONSUMER)
                    {
                        ACE_DEBUG((LM_NOTICE, "%T Event received %s [length=%d] with consumer delay %q ms but maximum allowed is %q ms\n",
                            data->antennaName.in(), seqLength, tConsumerDiff, m_maxDelay));
                    }

                    if(m_maxDelay < tSuppConDiff && m_delayType == DT_SUPP_CON)
                    {
                        ACE_DEBUG((LM_NOTICE, "%T Event received %s [length=%d] with delay %q ms but maximum allowed is %q ms\n",
                            data->antennaName.in(), seqLength, tSuppConDiff, m_maxDelay));
                    }

                } else {
                    ACE_DEBUG((LM_NOTICE, "%T Event received %s [length=%d] with delay %q ms, supplier delay %q ms and consumer delay %q ms\n",
                        data->antennaName.in(), seqLength, tSuppConDiff, tsSupplierDiff, tConsumerDiff));
                }
            }

            // Update last event received time
            TimevalUtils::set_timeval(m_tLastEvent, tEvent.tv_sec, tEvent.tv_usec);
            m_lastEventTimestamp = data->timestamp;

        } else {
            ACE_DEBUG((LM_ERROR, "Event received but is an empty sequence!\n"));
        }

	} else {
		ACE_DEBUG((LM_WARNING, "Event received but it's not of type MountStatusDataSeq\n"));
	}

	if(m_sleepTimeRecvEvent > 0)
	{
		ACE_OS::sleep(m_sleepTimeRecvEvent);
	}
}

void Consumer::offer_change(const CosNotification::EventTypeSeq&, const CosNotification::EventTypeSeq&)
{

}

uint64_t Consumer::getNumEventsReceived() const
{
	return m_numEventsReceived;
}

void Consumer::disconnect_push_consumer (void)
{
    m_orb->destroy();
	m_orb->shutdown(0);
}

bool Consumer::init_ORB(int argc, ACE_TCHAR* argv[])
{
	m_orb = CORBA::ORB_init(argc, argv, "");

	CORBA::Object_var poa_object = m_orb->resolve_initial_references ("RootPOA");

	if (CORBA::is_nil (poa_object))
	{
		ACE_ERROR ((LM_ERROR, " (%P|%t) Unable to initialize the POA.\n"));
		return false;
	}

	PortableServer::POA_var root_poa = PortableServer::POA::_narrow (poa_object);

	PortableServer::POAManager_var poa_manager = root_poa->the_POAManager ();
	poa_manager->activate ();

	return true;
}


bool Consumer::getNotificationChannel(const std::string &iorNS,
		CosNotifyChannelAdmin::ChannelID &channelID,
		CosNotifyChannelAdmin::EventChannel_var &channel,
		std::string &errMsg)
{
	try {
// Get the Notification Channel Factory
		CORBA::Object_var obj = m_orb->string_to_object(iorNS.c_str());

		CosNotifyChannelAdmin::EventChannelFactory_var ecf
		  = CosNotifyChannelAdmin::EventChannelFactory::_narrow(obj.in());

		if (CORBA::is_nil(ecf.in()))
		{
			errMsg = "Unable to get the Notification Channel Factory";
			return false;
		}

// Create a channel if no channel ID is given
		if(channelID <= 0)
		{
			ACE_DEBUG((LM_INFO, "%T Creating a channel ...\n"));
			CosNotification::QoSProperties init_qos(0); 
			CosNotification::AdminProperties init_admin(0); 
			channel = ecf->create_channel(init_qos, init_admin, channelID);
			if(CORBA::is_nil(channel.in()))
			{
				throw std::runtime_error("channel cannot be created!");
			} else {
				ACE_DEBUG((LM_INFO, "%T Channel created: %d\n", channelID));
				return true;
			}
		} else {
// Get the channel
			channel = ecf->get_event_channel(channelID);
			if(CORBA::is_nil(channel.in()))
			{
				std::ostringstream oss;
				oss << "Unable to get the channel " << channelID;
				errMsg = oss.str();
				return false;
			}
		}

	} catch(CosNotifyChannelAdmin::ChannelNotFound &ex) {
		std::ostringstream oss;
		oss << "Unable to get the channel " << channelID;
		errMsg = oss.str();
		return false;
	}	

	errMsg = "";
	return true;
}

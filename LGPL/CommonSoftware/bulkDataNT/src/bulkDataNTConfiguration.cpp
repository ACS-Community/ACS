/**************************************************************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2011
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
* "@(#) $Id: bulkDataNTConfiguration.cpp,v 1.24 2012/06/15 10:55:57 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/

#include "bulkDataNTConfiguration.h"
#include <ndds_config_cpp.h>
#include <logging.h>

using namespace AcsBulkdata;

const char* const DDSConfiguration::DEFAULT_LIBRARY = "BulkDataQoSLibrary";
const char* const DDSConfiguration::DEFAULT_SENDER_STREAM_PROFILE = "SenderStreamDefaultQosProfile";
const char* const DDSConfiguration::DEFAULT_SENDER_FLOW_PROFILE= "SenderFlowDefaultQosProfile";
const char* const DDSConfiguration::DEFAULT_RECEIVER_STREAM_PROFILE = "ReceiverStreamDefaultQosProfile";
const char* const DDSConfiguration::DEFAULT_RECEIVER_FLOW_PROFILE = "ReceiverFlowDefaultQosProfile";
const char* const DDSConfiguration::DEFAULT_API_CREATE_PROFILE = "APICreateProfile";

short DDSConfiguration::debugLevel = -1;
unsigned int DDSConfiguration::DDSLogVerbosity = (unsigned int)(NDDS_CONFIG_LOG_VERBOSITY_WARNING);

double SenderFlowConfiguration::DEFAULT_SENDFRAME_TIMEOUT=5.0;  //secs
double SenderFlowConfiguration::DEFAULT_ACKs_TIMEOUT=1.0; //secs

const char* const ReceiverFlowConfiguration::DEFAULT_MULTICAST_ADDRESS="225.3.2.1";
double ReceiverFlowConfiguration::DEFAULT_CBRECEIVE_PROCESS_TIMEOUT=0.01; //sec
bool ReceiverFlowConfiguration::DEFAULT_ENABLE_MULTICAST=true;

bool DDSConfiguration::ignoreUserProfileQoS = true;
bool DDSConfiguration::ignoreEnvironmentProfileQoS = true;

std::string DDSConfiguration::urlProfileQoS;
const char* const DDSConfiguration::DEFAULT_QoS_FILE="/config/bulkDataNTDefaultQosProfiles.xml";

bool AcsBulkdata::isBulkDataNTEnabled()
{
	const char *enableBulkDataNT = getenv("ENABLE_BULKDATA_NT");

	return ( enableBulkDataNT != 0 &&
			(strcmp(enableBulkDataNT, "1")==0 ||
			 strcasecmp(enableBulkDataNT, "true")==0 ||
			 strcasecmp(enableBulkDataNT, "y")==0
			)
		);
}
/**************************************************************************************
 * 			StreamConfiguration
 **************************************************************************************/
DDSConfiguration::DDSConfiguration()
{
	libraryQos=DDSConfiguration::DEFAULT_LIBRARY;
	DDSConfiguration::setDebugLevelFromEnvVar();

	if (urlProfileQoS.empty())
	{
		urlProfileQoS = "[";
		fillUrlProfileQoS("ACSDATA");
		urlProfileQoS+="]";
		// here we are sure that this is executed just once
		char *envVar = getenv("NDDS_DISCOVERY_PEERS");
		if (envVar && *envVar)
		{
			ACS_SHORT_LOG((LM_WARNING, "Env. variable NDDS_DISCOVERY_PEERS has been set to: %s, what could cause a problem to connect sender and receivers if it is not proprly used.", envVar));
		}//if
	}//if
}//DDSConfiguration

void DDSConfiguration::setDebugLevelFromEnvVar()
{
	if (debugLevel<0) //we read and set debug level jsut once
	{
		char *bulkDataNtDebug = getenv("BULKDATA_NT_DEBUG");
		if (bulkDataNtDebug && *bulkDataNtDebug)
		{
			debugLevel = atoi(bulkDataNtDebug);
			DDSConfiguration::setDDSLogVerbosity();
			ACS_SHORT_LOG((LM_INFO, "BulkDataNT debug level read from env. var. BULKDATA_NT_DEBUG and set to %d", debugLevel));
		}
		else
		{
			debugLevel=0;
		}
	}
}//setDebugLevelFromEnvVar

void DDSConfiguration::fillUrlProfileQoS(const char* envVar, const char *dilim)
{
	char *envVarValue = getenv(envVar);
	if (envVarValue != NULL)
	{
		urlProfileQoS += "file://";
		urlProfileQoS += envVarValue;
		urlProfileQoS += DEFAULT_QoS_FILE;
		urlProfileQoS += dilim;
	}
}//fillUrlProfileQoS

void DDSConfiguration::setDDSLogVerbosity()
{
	//RTI logging
	switch (DDSConfiguration::debugLevel)
	{
	case 2:
		DDSConfiguration::DDSLogVerbosity = NDDS_CONFIG_LOG_VERBOSITY_STATUS_LOCAL;
		break;
	case 3:
		DDSConfiguration::DDSLogVerbosity = NDDS_CONFIG_LOG_VERBOSITY_STATUS_REMOTE;
		break;
	case 4:
		DDSConfiguration::DDSLogVerbosity = NDDS_CONFIG_LOG_VERBOSITY_STATUS_ALL;
		break;
	}
	NDDSConfigLogger::get_instance()->set_verbosity_by_category(
			NDDS_CONFIG_LOG_CATEGORY_API,  (NDDS_Config_LogVerbosity)(DDSConfiguration::DDSLogVerbosity));
	/*
	NDDSConfigLogger::get_instance()->set_verbosity_by_category(
				NDDS_CONFIG_LOG_CATEGORY_COMMUNICATION,  (NDDS_Config_LogVerbosity)(DDSConfiguration::DDSLogVerbosity));
	NDDSConfigLogger::get_instance()->set_verbosity_by_category(
					NDDS_CONFIG_LOG_CATEGORY_ENTITIES,  (NDDS_Config_LogVerbosity)(DDSConfiguration::DDSLogVerbosity));
*/
}//setDDSLogVerbosity

void DDSConfiguration::setStringProfileQoS(char *cfg, const char *defaultProfile)
{
	profileQos=DEFAULT_API_CREATE_PROFILE;

	stringProfileQoS="<qos_profile name=\"";
	stringProfileQoS+=profileQos;
	stringProfileQoS+="\" base_name=\"";
	stringProfileQoS+=DDSConfiguration::DEFAULT_LIBRARY;
	stringProfileQoS+="::";
	stringProfileQoS+=defaultProfile;
	stringProfileQoS+="\">";
	stringProfileQoS+=cfg;
	stringProfileQoS+="</qos_profile>";
}//setStringProfileQoS

void DDSConfiguration::setStringProfileQoS(char*  profileName, char* cfg, const char *defaultProfile)
{
	profileQos=profileName;
	stringProfileQoS="<qos_profile name=\"";
	stringProfileQoS+=profileQos;
	stringProfileQoS+="\" base_name=\"";
	stringProfileQoS+=DDSConfiguration::DEFAULT_LIBRARY;
	stringProfileQoS+="::";
	stringProfileQoS+=defaultProfile;
	stringProfileQoS+="\">";
	stringProfileQoS+=cfg;
	stringProfileQoS+="</qos_profile>";
}//setStringProfileQoS

/**************************************************************************************
 * 			StreamConfiguration
 **************************************************************************************/
StreamConfiguration::StreamConfiguration()
{
}//StreamConfiguration


/**************************************************************************************
 * 			SenderStreamConfiguration
 **************************************************************************************/
SenderStreamConfiguration::SenderStreamConfiguration()
{
	profileQos=DEFAULT_SENDER_STREAM_PROFILE;
}//StreamConfiguration

/**************************************************************************************
 * 			ReceiverStreamConfiguration
 **************************************************************************************/
ReceiverStreamConfiguration::ReceiverStreamConfiguration()
{
	profileQos=DEFAULT_RECEIVER_STREAM_PROFILE;
}//ReceiverStreamConfiguration

void ReceiverStreamConfiguration::setDDSReceiverStreamQoS(char *cfg)
{
	setStringProfileQoS(cfg, DDSConfiguration::DEFAULT_RECEIVER_FLOW_PROFILE);
}//setDDSReceiverStreamQoS

void ReceiverStreamConfiguration::setDDSReceiverStreamQoS(char *profileName, char* cfg)
{
	setStringProfileQoS(profileName, cfg, DDSConfiguration::DEFAULT_RECEIVER_FLOW_PROFILE);
}//setDDSReceiverStreamQoS

/**************************************************************************************
 * 			ReceiverFlowConfiguration
 **************************************************************************************/
ReceiverFlowConfiguration::ReceiverFlowConfiguration()
{
	profileQos=DEFAULT_RECEIVER_FLOW_PROFILE;
	cbReceiveProcessTimeout =DEFAULT_CBRECEIVE_PROCESS_TIMEOUT;
	enableMulticast = DEFAULT_ENABLE_MULTICAST;
	multicastAddress = DEFAULT_MULTICAST_ADDRESS;
}//ReceiverFlowConfiguration


double ReceiverFlowConfiguration::getCbReceiveProcessTimeout() const
{
    return cbReceiveProcessTimeout;
}

void ReceiverFlowConfiguration::setCbReceiveProcessTimeout(double cbReceiveProcessTimeout)
{
    this->cbReceiveProcessTimeout = cbReceiveProcessTimeout;
}

std::string ReceiverFlowConfiguration::getMulticastAddress() const
{
    return multicastAddress;
}

bool ReceiverFlowConfiguration::isEnableMulticast() const
{
    return enableMulticast;
}

void ReceiverFlowConfiguration::setEnableMulticast(bool enableMulticast)
{
    this->enableMulticast = enableMulticast;
}

void ReceiverFlowConfiguration::setMulticastAddress(std::string multicastAddress)
{
    this->multicastAddress = multicastAddress;
}

void ReceiverFlowConfiguration::setDDSReceiverFlowQoS(char* cfg)
{
	setStringProfileQoS(cfg, DDSConfiguration::DEFAULT_RECEIVER_FLOW_PROFILE);
}//setDDSReceiverFlowQoS

void ReceiverFlowConfiguration::setDDSReceiverFlowQoS(char* profileName, char* cfg)
{
	setStringProfileQoS(profileName, cfg, DDSConfiguration::DEFAULT_RECEIVER_FLOW_PROFILE);
}//setDDSReceiverFlowQoS

/**************************************************************************************
 * 			SenderFlowConfiguration
 **************************************************************************************/
SenderFlowConfiguration::SenderFlowConfiguration()
{
	profileQos=DEFAULT_SENDER_FLOW_PROFILE;
	sendFrameTimeout = DEFAULT_SENDFRAME_TIMEOUT;
	ACKsTimeout = DEFAULT_ACKs_TIMEOUT;
}

double SenderFlowConfiguration::getACKsTimeout() const
{
    return ACKsTimeout;
}

double SenderFlowConfiguration::getSendFrameTimeout() const
{
    return sendFrameTimeout;
}

void SenderFlowConfiguration::setACKsTimeout(double acKsTimeout)
{
    ACKsTimeout = acKsTimeout;
}

void SenderFlowConfiguration::setSendFrameTimeout(double frameTimeout)
{
    this->sendFrameTimeout = frameTimeout;
}

void SenderFlowConfiguration::setDDSSenderFlowQoS(char* cfg)
{
	setStringProfileQoS(cfg, DDSConfiguration::DEFAULT_SENDER_FLOW_PROFILE);
}//setDDSReceiverFlowQoS

void SenderFlowConfiguration::setDDSSenderFlowQoS(char* profileName, char* cfg)
{
	setStringProfileQoS(profileName, cfg, DDSConfiguration::DEFAULT_SENDER_FLOW_PROFILE);
}//setDDSReceiverFlowQoS






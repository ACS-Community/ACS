/*******************************************************************************
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
* "@(#) $Id: bulkDataNTConfiguration.cpp,v 1.16 2012/01/19 11:41:33 bjeram Exp $"
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

short DDSConfiguration::debugLevel = -1;

unsigned int DDSConfiguration::DDSLogVerbosity = (unsigned int)(NDDS_CONFIG_LOG_VERBOSITY_WARNING);

const char* const StreamConfiguration::DEFAULT_QoS_FILE="/config/bulkDataNTDefaultQosProfiles.xml";

DDSConfiguration::DDSConfiguration()
{
	libraryQos=DDSConfiguration::DEFAULT_LIBRARY;
	ignoreUserProfileQoS = true;
	ignoreUserProfileQoS = true;
	DDSConfiguration::setDebugLevelFromEnvVar();
}

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

}//setDDSLogVerbosity

StreamConfiguration::StreamConfiguration()
{
	char *envVarValue;

	urlProfileQoS = "[";

	envVarValue = getenv("MODPATH");
	if (envVarValue != NULL)
	{
		urlProfileQoS += "file://..";
		urlProfileQoS += DEFAULT_QoS_FILE;
		urlProfileQoS += "|";
	}

	fillUrlProfileQoS("MODROOT", "|");
	fillUrlProfileQoS("INTROOT", "|");

	envVarValue = getenv("INTLIST");
	if (envVarValue != NULL) {
		char *tmpEnvVarValue = strdup(envVarValue); // we have to make copy otherwise next time the INTLIST is corupted
		char* tok = strtok(tmpEnvVarValue,":");
		while (tok != NULL)
		{
			urlProfileQoS += "file://";
			urlProfileQoS += tok;
			urlProfileQoS += DEFAULT_QoS_FILE;
			urlProfileQoS += "|";
			tok = strtok(NULL, ":");
		}//while
		free(tmpEnvVarValue);
	}//if

	fillUrlProfileQoS("ACSROOT"); //for sure we have ACSROOT
	urlProfileQoS+="]";
}//StreamConfiguration

void StreamConfiguration::fillUrlProfileQoS(const char* envVar, const char *dilim)
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

SenderStreamConfiguration::SenderStreamConfiguration()
{
	profileQos=DEFAULT_SENDER_STREAM_PROFILE;
}//StreamConfiguration


ReceiverStreamConfiguration::ReceiverStreamConfiguration()
{
	profileQos=DEFAULT_RECEIVER_STREAM_PROFILE;
}//ReceiverStreamConfiguration


ReceiverFlowConfiguration::ReceiverFlowConfiguration()
{
	profileQos=DEFAULT_RECEIVER_FLOW_PROFILE;
	cbReceiveProcessTimeout = 0.01; //secs
	enableMulticast = true;
	multicastAddress = "225.3.2.1";
}//ReceiverFlowConfiguration

SenderFlowConfiguration::SenderFlowConfiguration()
{
	profileQos=DEFAULT_SENDER_FLOW_PROFILE;
	sendFrameTimeout = 5.0;  //secs
	ACKsTimeout = 1.0; //secs
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

void ReceiverFlowConfiguration::setMulticastAddress(char *multicastAddress)
{
    this->multicastAddress = multicastAddress;
}





//SenderFlowConfiguration



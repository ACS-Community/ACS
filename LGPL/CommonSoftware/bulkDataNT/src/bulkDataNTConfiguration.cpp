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
* "@(#) $Id: bulkDataNTConfiguration.cpp,v 1.7 2011/09/28 16:42:18 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/

#include "bulkDataNTConfiguration.h"
#include <ndds_config_c.h>

using namespace AcsBulkdata;

const char* const DDSConfiguration::DEFAULT_LIBRARY = "BulkDataQoSLibrary";

const char* const DDSConfiguration::DEFAULT_SENDER_STREAM_PROFILE = "SenderStreamDefaultQosProfile";
const char* const DDSConfiguration::DEFAULT_SENDER_FLOW_PROFILE= "SenderFlowDefaultQosProfile";

const char* const DDSConfiguration::DEFAULT_RECEIVER_STREAM_PROFILE = "ReceiverStreamDefaultQosProfile";
const char* const DDSConfiguration::DEFAULT_RECEIVER_FLOW_PROFILE = "ReceiverFlowDefaultQosProfile";

const char* const StreamConfiguration::DEFAULT_QoS_FILE="/config/bulkDataNTDefaultQosProfiles.xml";

DDSConfiguration::DDSConfiguration()
{
	libraryQos=DDSConfiguration::DEFAULT_LIBRARY;
	ignoreUserProfileQoS = true;
	ignoreUserProfileQoS = true;
}

StreamConfiguration::StreamConfiguration()
{
	char *envVarValue;

	DDSLogVerbosity = (unsigned int)(NDDS_CONFIG_LOG_VERBOSITY_WARNING);

	urlProfileQoS = "[";

	envVarValue = getenv("MODPATH");
	if (envVarValue != NULL)
	{
		urlProfileQoS += "file://..";
		urlProfileQoS += DEFAULT_QoS_FILE;
		urlProfileQoS += " | ";
	}

	fillUrlProfileQoS("MODROOT", " | ");
	fillUrlProfileQoS("INTROOT", " | ");

	envVarValue = getenv("INTLIST");
	if (envVarValue != NULL) {
		char *tmpEnvVarValue = strdup(envVarValue); // we have to make copy otherwise next time the INTLIST is corupted
		char* tok = strtok(tmpEnvVarValue,":");
		while (tok != NULL)
		{
			urlProfileQoS += "file://";
			urlProfileQoS += tok;
			urlProfileQoS += DEFAULT_QoS_FILE;
			urlProfileQoS += " | ";
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
}//ReceiverFlowConfiguration

SenderFlowConfiguration::SenderFlowConfiguration()
{
	profileQos=DEFAULT_SENDER_FLOW_PROFILE;
}//SenderFlowConfiguration



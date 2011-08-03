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
* "@(#) $Id: bulkDataNTConfiguration.cpp,v 1.2 2011/08/03 15:06:32 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/

#include "bulkDataNTConfiguration.h"
#include <ndds_config_c.h>

using namespace AcsBulkdata;

StreamConfiguration::StreamConfiguration()
{
	libraryQos="BulkDataQoSLibrary";
	DDSLogVerbosity = (unsigned int)(NDDS_CONFIG_LOG_VERBOSITY_WARNING);
}//StreamConfiguration

SenderStreamConfiguration::SenderStreamConfiguration()
{
	profileQos="SenderDefaultQosProfile";
}//StreamConfiguration


ReceiverStreamConfiguration::ReceiverStreamConfiguration()
{
	profileQos="ReceiverDefaultQosProfile";
}//ReceiverStreamConfiguration




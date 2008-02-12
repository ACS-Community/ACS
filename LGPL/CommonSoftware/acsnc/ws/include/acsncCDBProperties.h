#ifndef acsnc_cdb_properties_H
#define acsnc_cdb_properties_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) UNSPECIFIED - FILL IN, 2005 
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
* "@(#) $Id: acsncCDBProperties.h,v 1.8 2008/02/12 01:10:33 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* almadave  2005-04-24  created
*/

/** @file acsncCDBProperties.h
 *  Header file for classes/functions designed to extract a channel's 
 *  quality of service and admin properties from the ACS CDB.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <orbsvcs/CosNotificationC.h>
#include <cdbDALC.h>
#include <iostream>
#include <string>
#include <map>

namespace nc {

    /**
     * Class which contains static methods used to discover whether or
     * not properties for ACS notification channels have been defined in
     * the ACS CDB. If these properties, are present this class turns
     * the pure XML records into CORBA properties.
     */
    class CDBProperties {

      public:
	/**
	 * Simple function which returns true if the given channel 
	 * has an entry in $ACS_CDB/CDB/MACI/EventChannels/ section 
	 * of the ACS configuration database.
	 * @param channelName name of the channel we want to know about
	 * @return true if an entry exists in the ACS CDB for this channel
	 *         and false otherwise.
	 */
	static bool 
	cdbChannelConfigExists(const std::string& channelName);

	/**
	 * Given a channel name that exists in the ACS CDB
	 * ($ACS_CDB/CDB/MACI/Channels/channelName/channelName.xml), this
	 * function returns the channels administrative properties in their CORBA
	 * format.
	 * @param channelName name of the channel found in $ACS_CDB/CDB/MACI/Channels
	 * @return channel's admin properties
	 */
	static CosNotification::AdminProperties
	getCDBAdminProps(const std::string& channelName);
	
	/**
	 * Given a channel name that exists in the ACS CDB
	 * ($ACS_CDB/CDB/MACI/Channels/channelName/channelName.xml), this
	 * function returns the channel's quality of service properties in their CORBA
	 * format.
	 * @param channelName name of the channel found in $ACS_CDB/CDB/MACI/Channels
	 * @return channel's quality of service properties
	 */
	static CosNotification::QoSProperties
	getCDBQoSProps(const std::string& channelName);

	/**
	 * The following was requested by H. Sommer and is needed for integrations.
	 * It should be removed at some later date.
	 */
	static bool
	getIntegrationLogs(const std::string& channelName);

 
	typedef std::map<std::string, double> EventHandlerTimeoutMap;
	/**
	 * The following returns a map where each key is the
	 * name of an event and the value is the maximum amount of time
	 * an event handler has to process the event before a warning
	 * message is logged.
	 * @param channelName - name of the channel
	 */
	static EventHandlerTimeoutMap
	getEventHandlerTimeoutMap(const std::string& channelName);

	/**
	 * Helper function returns a reference to the ACS CDB.
	 */
	static CDB::DAL_ptr
	getCDB();

    };
};


#endif /*!_H*/

#ifndef ASI_CONFIGURATION_CONSTANTS_H
#define ASI_CONFIGURATION_CONSTANTS_H

/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
// TODO later: use the CDB or the asi-configuration.xml file for this information!
namespace asiConfigurationConstants
{

// The XML file used by the java code for this information looks something like this:
/*
<ASI-configuration>
    <ASI-version>0.9</ASI-version>
   	<alarms-topic>CMW.ALARM_SYSTEM.ALARMS.SOURCES</alarms-topic>
   	<backup-delivery-mode>0</backup-delivery-mode>
   	<backup-priority>9</backup-priority>
   	<backup-time-to-live>60000</backup-time-to-live>
   	<changes-delivery-mode>0</changes-delivery-mode>
   	<changes-priority>9</changes-priority>
   	<changes-time-to-live>60000</changes-time-to-live>
    <source-name-property>SOURCE_NAME</source-name-property>
    <source-hostname-property>SOURCE_HOSTNAME</source-hostname-property>
    <backup-property>BACKUP</backup-property>
    <alarms-number-property>ALARMS_NUMBER</alarms-number-property>
	<channel-pool-size>10</channel-pool-size>
	<channel-property>CHANNEL</channel-property>
</ASI-configuration>
*/
   const char * const ASI_VERSION = "0.9";
   const char * const ALARMS_TOPIC = "CMW.ALARM_SYSTEM.ALARMS.SOURCES";
	const char * const ALARM_SOURCE_NAME = "ALARM_SYSTEM_SOURCES";
	const int BACKUP_DELIVERY_MODE = 0;
   const int BACKUP_PRIORITY = 9;
   const long BACKUP_TIME_TO_LIVE = 60000;
   const int CHANGES_DELIVERY_MODE = 0;
   const int CHANGES_PRIORITY = 9;
   const long CHANGES_TIME_TO_LIVE = 60000;
};
#endif

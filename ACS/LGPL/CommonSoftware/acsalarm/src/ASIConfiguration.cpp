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
#include "ASIConfiguration.h"
#include "asiConfigurationConstants.h"

using acsalarm::ASIConfiguration;
using asiConfigurationConstants::ASI_VERSION;
using asiConfigurationConstants::ALARMS_TOPIC;
using asiConfigurationConstants::BACKUP_DELIVERY_MODE;
using asiConfigurationConstants::BACKUP_PRIORITY;
using asiConfigurationConstants::BACKUP_TIME_TO_LIVE;
using asiConfigurationConstants::CHANGES_DELIVERY_MODE;
using asiConfigurationConstants::CHANGES_PRIORITY;
using asiConfigurationConstants::CHANGES_TIME_TO_LIVE;

/*
 * Constructor.
 */
ASIConfiguration::ASIConfiguration()
{
	// TODO later: get this information from the CDB
	asiVersion = ASI_VERSION;
	alarmsTopic = ALARMS_TOPIC;
	backupDeliveryMode = BACKUP_DELIVERY_MODE;
	backupPriority = BACKUP_PRIORITY;
	backupTimeToLive = BACKUP_TIME_TO_LIVE;
	changesDeliveryMode = CHANGES_DELIVERY_MODE;
	changesPriority = CHANGES_PRIORITY;
	changesTimeToLive = CHANGES_TIME_TO_LIVE;
}

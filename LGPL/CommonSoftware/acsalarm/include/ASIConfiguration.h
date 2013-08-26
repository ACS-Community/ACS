#ifndef ASI_CONFIGURATION_H
#define ASI_CONFIGURATION_H

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
#include <string>

namespace acsalarm
{
	/*
	 * A class to encapsulate various configuration data for the laser alarm system
	 * for use by cpp alarm source clients.
	 */
	class ASIConfiguration
	{
		private:
			std::string asiVersion;
			std::string alarmsTopic;
			int backupDeliveryMode;
			int backupPriority;
			long backupTimeToLive;
			int changesDeliveryMode;
			int changesPriority;
			long changesTimeToLive;

		public:
			ASIConfiguration();
			virtual ~ASIConfiguration() {}

			std::string getASIVersion() { return asiVersion; }
			void setASIVersion(std::string version) { asiVersion = version; }

			std::string getAlarmsTopic() { return alarmsTopic; }
			void setAlarmsTopic(std::string topic) { alarmsTopic = topic; }

			int getBackupDeliveryMode() { return backupDeliveryMode; }
			void setBackupDeliveryMode(int mode) { backupDeliveryMode = mode; }

			int getBackupPriority() { return backupPriority; }
			void setBackupPriority(int priority) { backupPriority = priority; }

			long getBackupTimeToLive() { return backupTimeToLive; }
			void setBackupTimeToLive(long timeToLive) { backupTimeToLive = timeToLive; }
		
			int getChangesDeliveryMode() { return changesDeliveryMode; }
			void setChangesDeliveryMode(int mode) { changesDeliveryMode = mode; }

			int getChangesPriority() { return changesPriority; }
			void setChangesPriority(int priority) { changesPriority = priority; }

			long getChangesTimeToLive() { return changesTimeToLive; }
			void setChangesTimeToLive(long timeToLive) { changesTimeToLive = timeToLive; }
	};
};
#endif

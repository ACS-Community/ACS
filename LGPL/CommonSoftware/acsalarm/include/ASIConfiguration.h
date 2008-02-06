#ifndef ASI_CONFIGURATION_H
#define ASI_CONFIGURATION_H

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

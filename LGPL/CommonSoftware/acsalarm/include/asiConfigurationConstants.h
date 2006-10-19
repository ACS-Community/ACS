#ifndef ASI_CONFIGURATION_CONSTANTS_H
#define ASI_CONFIGURATION_CONSTANTS_H

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

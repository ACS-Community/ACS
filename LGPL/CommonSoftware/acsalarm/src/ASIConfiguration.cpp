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

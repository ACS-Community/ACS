#include "ASIConfiguration.h"
#include "asiConfigurationConstants.h"

using namespace asiConfigurationConstants;
using namespace acsalarm;

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

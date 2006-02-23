#include "ASIConfiguration.h"
#include "asiConfigurationConstants.h"

using namespace asiConfigurationConstants;
using namespace laserSource;

/*
 * Constructor.
 */
ASIConfiguration::ASIConfiguration()
{
	// TODO later: get this information from the CDB
	asiVersion = ASI_VERSION;
	publisherDLLPath = PUBLISHER_DLL_PATH;
	publisherFactoryFunctionName = DLL_PUBLISHER_FACTORY_FUNCTION_NAME;
	alarmsTopic = ALARMS_TOPIC;
	backupDeliveryMode = BACKUP_DELIVERY_MODE;
	backupPriority = BACKUP_PRIORITY;
	backupTimeToLive = BACKUP_TIME_TO_LIVE;
	changesDeliveryMode = CHANGES_DELIVERY_MODE;
	changesPriority = CHANGES_PRIORITY;
	changesTimeToLive = CHANGES_TIME_TO_LIVE;
}

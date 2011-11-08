#include "bulkDataNTCallback.h"

void AcsBulkdata::BulkDataNTCallback::onError(ACSErr::CompletionImpl &error)
{
	error.log();
}//onError


#include "bulkDataNTCallback.h"

void BulkDataCallback::onError(ACSErr::CompletionImpl &error)
{
	error.log();
}//onError


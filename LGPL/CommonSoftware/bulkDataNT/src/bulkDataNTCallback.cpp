// should be removed !!!!

#include "bulkDataNTCallback.h"

void BulkDataCallback::onError(ACSErr::CompletionImpl &error)
{
	error.log();
}//onError

void BulkDataCallback::onSenderConnect()
{
	std::string longName = streamName_m + "#" + flowName_m;
	ACS_LOG(LM_RUNTIME_CONTEXT, __PRETTY_FUNCTION__, (LM_ERROR, "New sender connected to: %s", longName.c_str()));
}

void BulkDataCallback::onSenderDisconnect()
{
	std::string longName = streamName_m + "#" + flowName_m;
	ACS_LOG(LM_RUNTIME_CONTEXT, __PRETTY_FUNCTION__, (LM_ERROR, "New sender connected to: %s", longName.c_str()));
}

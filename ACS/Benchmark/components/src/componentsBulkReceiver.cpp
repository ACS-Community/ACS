#include "componentsBulkReceiver.h"

BulkDataReceiver:: BulkDataReceiver()
{
    ACS_TRACE("BulkDataReceiver::BulkDataReceiver");
}

int BulkDataReceiver::cbStart(ACE_Message_Block * userParam_p)
{
    ACS_TRACE("BulkDataReceiver::handle_start");
    return 0;
}

int BulkDataReceiver::cbReceive(ACE_Message_Block * frame)
{   
    while (frame != 0)
	{
	frame = frame->cont ();
	}

    return 0;
}

int BulkDataReceiver::cbStop()
{   
    ACS_TRACE("BulkDataReceiver::handle_stop");
    return 0;
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(BulkDataReceiverImpl<BulkDataReceiver>)
/* ----------------------------------------------------------------*/

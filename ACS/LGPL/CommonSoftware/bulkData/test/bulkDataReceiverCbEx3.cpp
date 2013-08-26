#include "bulkDataReceiverCbEx3.h"

using namespace ACSBulkDataError;

BulkDataReceiverCbEx3::BulkDataReceiverCbEx3() : count1_m(0), timeout_m(false)
{
    ACS_TRACE("BulkDataReceiverCbEx3::BulkDataReceiverCbEx3"); 

    // internal timeout for this callback: 5 loops for 1 sec = 12 sec
    ACE_Time_Value waitPeriod_m;
    waitPeriod_m.set(1L, 0L);
    setSleepTime(waitPeriod_m);
    setSafeTimeout(5);

}


BulkDataReceiverCbEx3::~BulkDataReceiverCbEx3()
{
    ACS_TRACE("BulkDataReceiverCbEx3::~BulkDataReceiverCbEx3"); 
}


int
BulkDataReceiverCbEx3::cbStart(ACE_Message_Block * userParam_p)
{

    count1_m = 0;
    timeout_m = false;

    char message[256];
    ACE_OS::strcpy(message, userParam_p->rd_ptr());
    ACS_SHORT_LOG((LM_INFO,"BulkDataReceiverCbEx3::cbStart - message: %s",message));  

    if(ACE_OS::strcmp(message,"ERROR") == 0)
	{
	//We simulate that we cannot open a file
	AVCouldNotOpenFileExImpl err = AVCouldNotOpenFileExImpl(__FILE__,__LINE__,"BulkDataReceiverCbEx3::cbStart");
	throw err;
	}

    if(ACE_OS::strcmp(message,"TIMEOUT") == 0)
	{
	ACS_SHORT_LOG((LM_INFO,"BulkDataReceiverCbEx3::cbStart - timeout simulation!"));
	timeout_m = true;
	}

    if(ACE_OS::strcmp(message,"OK") == 0)
	ACS_SHORT_LOG((LM_INFO,"BulkDataReceiverCbEx3::cbStart - no error - everything fine!"));  

    return 0;
}

int
BulkDataReceiverCbEx3::cbReceive(ACE_Message_Block * frame_p)
{

    count1_m += frame_p->length();

    if(timeout_m == true)
	{
	// simulate a very very slow data ingestion
	ACE_OS::sleep(12);
	return 0;
	}


    if(count1_m > 25000)
	{
	//We simulate a generic error after having received certain amount of data
	AVInitErrorExImpl err = AVInitErrorExImpl(__FILE__,__LINE__,"BulkDataReceiverCbEx3::cbReceive");
	throw err;
	}
	


    return 0;
}

int
BulkDataReceiverCbEx3::cbStop()
{

    ACS_SHORT_LOG((LM_INFO, "flow 1 total length: %d", count1_m)); 

    return 0;
}


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(BulkDataReceiverEx3Impl<BulkDataReceiverCbEx3>)
/* ----------------------------------------------------------------*/

    

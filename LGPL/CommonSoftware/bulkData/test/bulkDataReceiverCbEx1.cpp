#include "bulkDataReceiverCbEx1.h"

BulkDataReceiverCbEx1::BulkDataReceiverCbEx1() : count1_m(0), count2_m(0), count4_m(0)
{
    ACS_TRACE("BulkDataReceiverCbEx1::BulkDataReceiverCbEx1"); 

    fp_p = 0;
}


BulkDataReceiverCbEx1::~BulkDataReceiverCbEx1()
{
    ACS_TRACE("BulkDataReceiverCbEx1::~BulkDataReceiverCbEx1"); 
}


int
BulkDataReceiverCbEx1::cbStart(ACE_Message_Block * userParam_p)
{
    ACS_TRACE("BulkDataReceiverCbEx1::cbStart"); 

    if(flowNumber_m == 1)
	{
	ACS_SHORT_LOG((LM_DEBUG, "flowname 1: %s", flowname_m.c_str()));
	ACS_SHORT_LOG((LM_DEBUG, "length param flowname 1: %d", userParam_p->length()));

	count1_m = 0;
	}
    else if(flowNumber_m == 2)
	{
	ACS_SHORT_LOG((LM_DEBUG, "flowname 2: %s", flowname_m.c_str()));
	ACS_SHORT_LOG((LM_DEBUG, "length param flowname 2: %d", userParam_p->length()));

	count2_m = 0;
	}
    else if(flowNumber_m == 3)
	{
	ACS_SHORT_LOG((LM_DEBUG, "flowname 3: %s", flowname_m.c_str()));
	ACS_SHORT_LOG((LM_DEBUG, "length param flowname 3: %d", userParam_p->length()));

	char filename[256];
	ACE_OS::strcpy(filename, userParam_p->rd_ptr());

	fp_p = ACE_OS::fopen (filename,"w");

	if (fp_p == 0)
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiverCbEx1::cbStart %s not open successfully.", filename));
	    return -1;
	    }


	}
    else if(flowNumber_m == 4)
	{
	ACS_SHORT_LOG((LM_DEBUG, "flowname 4: %s", flowname_m.c_str()));

	count4_m = 0;
	}

    return 0;
}

int
BulkDataReceiverCbEx1::cbReceive(ACE_Message_Block * frame_p)
{
    ACS_TRACE("BulkDataReceiverCbEx1::cbReceive"); 

    if(flowNumber_m == 1)
	{
        ACS_SHORT_LOG((LM_DEBUG, "flowname 1: %s", flowname_m.c_str()));
        ACS_SHORT_LOG((LM_DEBUG, "length data flowname 1: %d", frame_p->length()));
	count1_m += frame_p->length();
	}
    else if(flowNumber_m == 2)
	{
        ACS_SHORT_LOG((LM_DEBUG, "flowname 2: %s", flowname_m.c_str()));
        ACS_SHORT_LOG((LM_DEBUG, "length data flowname 2: %d", frame_p->length()));
	count2_m += frame_p->length();
	}
    else if(flowNumber_m == 3)
	{    
	while (frame_p != 0)
	    {
	    int result = ACE_OS::fwrite (frame_p->rd_ptr(),
					 frame_p->length(),
					 1,
					 fp_p);
	    if (result == 0)
		{
		ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiverCbEx1::cbReceive failed"));
		return -1;
		}
	  
	    frame_p = frame_p->cont ();
	    }
	}
    else if(flowNumber_m == 4)
	{
	ACS_SHORT_LOG((LM_DEBUG, "flowname 4: %s", flowname_m.c_str()));
	ACS_SHORT_LOG((LM_DEBUG, "length data flowname 4: %d", frame_p->length()));
	count4_m += frame_p->length();
	}

    return 0;
}

int
BulkDataReceiverCbEx1::cbStop()
{
    ACS_TRACE("BulkDataReceiverCbEx1::cbStop"); 

    if(flowNumber_m == 1)
	ACS_SHORT_LOG((LM_INFO, "flow 1 total length: %d", count1_m)); 
    if(flowNumber_m == 2)
	ACS_SHORT_LOG((LM_INFO, "flow 2 total length: %d", count2_m)); 
    if(flowNumber_m == 4)
	ACS_SHORT_LOG((LM_INFO, "flow 4 total length: %d", count4_m)); 

    if(flowNumber_m == 3)
	ACE_OS::fclose(fp_p);

    return 0;
}

int
BulkDataReceiverCbEx1::myMethod()
{
    ACS_TRACE("BulkDataReceiverCbEx1::myMethod"); 

    return 0;
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(BulkDataReceiverImpl<BulkDataReceiverCbEx1>)
/* ----------------------------------------------------------------*/

    
    

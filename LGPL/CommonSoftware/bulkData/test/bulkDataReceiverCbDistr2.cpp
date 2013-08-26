#include "bulkDataReceiverCbDistr2.h"

BulkDataReceiverCbDistr2::BulkDataReceiverCbDistr2() : count1_m(0)
{
    ACS_TRACE("BulkDataReceiverCbDistr2::BulkDataReceiverCbDistr2"); 
}


BulkDataReceiverCbDistr2::~BulkDataReceiverCbDistr2()
{
    ACS_TRACE("BulkDataReceiverCbDistr2::~BulkDataReceiverCbDistr2"); 
}


int
BulkDataReceiverCbDistr2::cbStart(ACE_Message_Block * userParam_p)
{
    ACS_TRACE("BulkDataReceiverCbDistr2::cbStart"); 

    if(flowNumber_m == 2)
	{

	ACS_SHORT_LOG((LM_DEBUG, "length param flowname 2: %d", userParam_p->length()));

	char filename[256];
	ACE_OS::strcpy(filename, userParam_p->rd_ptr());

	fp_p = ACE_OS::fopen (filename,"w");

	if (fp_p == 0)
	    {
	    ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiverCbEx1::cbStart %s not open successfully.", filename));
	    return -1;
	    }
	
	ACS_SHORT_LOG((LM_INFO, "RECEIVER 2 flowname 2: %s", flowname_m.c_str()));
	ACS_SHORT_LOG((LM_INFO, "RECEIVER 2 successfully opened file: %s", filename));

	}
    
    
    return 0;
}


int
BulkDataReceiverCbDistr2::cbReceive(ACE_Message_Block * frame_p)
{
    ACS_TRACE("BulkDataReceiverCbDistr2::cbReceive"); 
    
//    ACS_SHORT_LOG((LM_INFO, "RECEIVER 2 flowname 2: %d", frame_p->length()));

    if(flowNumber_m == 2)
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


    return 0;
}


int
BulkDataReceiverCbDistr2::cbStop()
{
    ACS_TRACE("BulkDataReceiverCbDistr2::cbStop"); 

    
    if(flowNumber_m == 2)
	{
	ACE_OS::fflush(fp_p);
	ACE_OS::fclose(fp_p);
        ACS_SHORT_LOG((LM_INFO, "RECEIVER 2 successfully saved file!"));
	}

    return 0;
}

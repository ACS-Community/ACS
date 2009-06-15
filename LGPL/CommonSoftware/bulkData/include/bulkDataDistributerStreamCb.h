#ifndef _BULKDATA_DISTRIBUTER_STREAM_CB_H
#define _BULKDATA_DISTRIBUTER_STREAM_CB_H

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "orbsvcs/AV/AVStreams_i.h"
#include "orbsvcs/AV/Endpoint_Strategy.h"
#include "orbsvcs/AV/Protocol_Factory.h"
#include "orbsvcs/AV/Flows_T.h"
#include "orbsvcs/AV/Transport.h"
#include "orbsvcs/AV/Policy.h"

#include <baci.h>

#include "ACSBulkDataError.h"

#include <iostream>

#include "bulkDataDistributerImpl.h"

class BulkDataDistributerStreamCb : public TAO_AV_Callback
{

  public:

    BulkDataDistributerStreamCb();

    BulkDataDistributerStreamCb(TAO_StreamCtrl * stream_p);

    ~BulkDataDistributerStreamCb();

    virtual int handle_start(void);

    virtual int handle_stop (void);

    virtual int handle_destroy (void);

    virtual int receive_frame (ACE_Message_Block *frame_p, TAO_AV_frame_info *frame_info, const ACE_Addr &);

    virtual void setFlowname (const char*);

    virtual void setDistributerImpl(BulkDataDistributerImpl<BulkDataDistributerStreamCb> *distr_p);

    ACE_HANDLE getHandle();

    CORBA::Boolean isFepAlive()
	{
	    return isFepAlive_m;
	}

  protected:

    ACE_CString flowname_m;

    CORBA::ULong flowNumber_m;

  private:

    BulkDataDistributerImpl<BulkDataDistributerStreamCb> *distr_m;

    CORBA::Boolean isFepAlive_m;
};


#endif /*!_BULKDATA_DISTRIBUTER_STREAM_CB_H*/

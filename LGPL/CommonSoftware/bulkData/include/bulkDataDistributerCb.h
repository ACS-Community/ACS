#ifndef _BULKDATA_DISTRIBUTER_CB_H
#define _BULKDATA_DISTRIBUTER_CB_H

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
//#include <acsQoS.h>

#include "ACSBulkDataError.h"

#include <iostream>

#include "bulkDataDistributerImpl.h"

class BulkDataDistributerCb : public TAO_AV_Callback
{

  public:

    enum Cb_State
    {
	CB_UNS,
	CB_SEND_PARAM,
	CB_SEND_DATA
    };

    enum Cb_SubState
    {
	CB_SUB_UNS,
	CB_SUB_INIT
    };


    BulkDataDistributerCb();

    BulkDataDistributerCb(TAO_StreamCtrl * stream_p);

    ~BulkDataDistributerCb();

    virtual int handle_start(void);

    virtual int handle_stop (void);

    virtual int handle_destroy (void);

    virtual int receive_frame (ACE_Message_Block *frame, TAO_AV_frame_info *frame_info, const ACE_Addr &);

    virtual void setFlowname (const char*);

    // The use of setSleepTime is deprecated. It has no effect
    // on the waiting time of the handle_stop method
    virtual void setSleepTime(ACE_Time_Value locWaitPeriod);

    // The use of setSafeTimeout is deprecated. It has no effect
    // on the waiting time of the handle_stop method
    virtual void setSafeTimeout(CORBA::ULong locLoop);

    virtual int cbFwdStart(ACE_Message_Block * userParam_p = 0);

    virtual int cbFwdReceive(ACE_Message_Block * frame_p);

    virtual int cbFwdStop();

    virtual int cbFwdUserStop();

    virtual int cbHandshake(ACE_Message_Block * frame_p);

    virtual void setDistributerImpl(BulkDataDistributerImpl<BulkDataDistributerCb> *distr_p);

    virtual CORBA::Boolean isTimeout();

    virtual CORBA::Boolean isWorking();

    ACE_HANDLE getHandle();

    CORBA::Boolean isFepAlive()
	{
	    return isFepAlive_m;
	}

    // The use of setCbTimeout is deprecated. It has no effect
    // on the waiting time of the handle_stop method
    virtual void setCbTimeout(ACE_Time_Value cbTimeout);


  protected:

    ACE_CString flowname_m;

    CORBA::ULong flowNumber_m;

  private:

    BulkDataDistributerImpl<BulkDataDistributerCb> *distr_m;

    ACE_Time_Value waitPeriod_m;

    CORBA::ULong loop_m;

    Cb_State state_m;
    Cb_SubState substate_m;

    CORBA::Long dim_m;

    CORBA::Long count_m;

    CORBA::Long frameCount_m;

    CORBA::Boolean timeout_m;

    CORBA::Boolean working_m;

    CORBA::Boolean isFepAlive_m;
};


#endif /*!_BULKDATA_DISTRIBUTER_CB_H*/

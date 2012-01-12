#ifndef _BULKDATA_NT_CALLBACK_H
#define _BULKDATA_NT_CALLBACK_H

/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2011
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: bulkDataNTCallback.h,v 1.22 2012/01/12 10:30:41 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/


#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acserr.h>
#include <SString.h>

namespace AcsBulkdata
{


class BulkDataNTCallback
{
public:
	virtual ~BulkDataNTCallback(){};

	// flow and stream names are set in ReceiverFlow ctor, should we keep them public ?
	void setFlowName (const char* name) { flowName_m =name; }
	const char* getFlowName () { return flowName_m.c_str(); }

	void setStreamName (const char* name) { streamName_m =name; }
	const char* getStreamName () { return streamName_m.c_str(); }

	void setReceiverName(ACE_CString &name) { recvName_m=name; }
	void setReceiverName(const char *name) { recvName_m=name; }
	const char* getReceiverName () { return recvName_m.c_str(); }

	/**
	 * setter method for cbReceiveProcessTimeout_m
	 * @param to - maximal cbReceiver process time in sec
	 */
	void setCBReceiveProcessTimeout(double to);

	/**
	 * * getter for cbReceiveProcessTimeout_m
	 * @return timeout in sec
	 */
	double getCBReceiveProcessTimeout() { return cbReceiveProcessTimeout_m; }

	/*
    virtual void setSleepTime(ACE_Time_Value locWaitPeriod);

    virtual void setSafeTimeout(CORBA::ULong locLoop);

    virtual CORBA::Boolean isTimeout();
    virtual CORBA::Boolean isWorking();
    virtual CORBA::Boolean isError();

    virtual CompletionImpl *getErrorCompletion();

    virtual void setFlowTimeout(CORBA::ULong timeout);

    template<class TCallback>
    void setReceiver(AcsBulkdata::BulkDataReceiver<TCallback> *recv)
	{
	    ACE_UNUSED_ARG(recv);
	    //to be defined by the user
	}


    virtual void setCbTimeout(ACE_Time_Value cbTimeout)
	{
	    // empty
	}

    void fwdData2UserCB(CORBA::Boolean enable);
	 */
	/********************* methods that have to be implemented by the user *****************/

	virtual int cbStart(unsigned char* userParam_p = 0, unsigned  int size=0)=0;

	virtual int cbReceive(unsigned char * frame_p, unsigned  int size)=0;

	virtual int cbStop() = 0;

	/*********************  methods that can/should be  implemented by the user */

	/**
	 * This method is called when an error happens in the flow's callback (cbStart/cbReceive/cbStop),
	 * and can be overriden by an user. The default implementation just logs the error completion.
	 * @param error - at the moment possible completion errors are:
	 * #WrongFrameOrderCompletion
	 * #FrameLostCompletion
	 * #UnknownDataTypeCompletion
	 * #DDSReturnErrorCompletion
	 * #CBReceiveProcessTimeoutCompletion
	 * #DDSDeadlineMissedCompletion
	 * #DDSIncompatibleQoSCompletion
	 */
	virtual void onError(ACSErr::CompletionImpl &error);

	/// The method is called when a new sender is connected to the flow
	virtual void onSenderConnect(){};

	/// The method is called when a sender is disconnected for a flow
	virtual void onSenderDisconnect(){};

	//TBD: to be implemented now those error goes to onError
	virtual void onDataLost(unsigned long frmaeCount, unsigned long totalFrames, ACSErr::CompletionImpl &error){};
protected:
	std::string flowName_m;
	std::string streamName_m;

	ACE_CString recvName_m;

	double cbReceiveProcessTimeout_m;
	/*
    bool fwdData2UserCB_m;

  private:

    void cleanRecvBuffer();

    void checkFlowTimeout();

    ACE_Time_Value waitPeriod_m;

    CORBA::ULong loop_m;

    Cb_State state_m;
    Cb_SubState substate_m;

    CORBA::Long dim_m;

    CORBA::Long count_m;

    CORBA::Long frameCount_m;

    ACE_Message_Block *bufParam_p;

    CORBA::Boolean timeout_m;
    CORBA::Boolean working_m;
    CORBA::Boolean error_m;

    ACSBulkDataStatus::AVCbErrorCompletion *errComp_p;

    CORBA::ULong flowTimeout_m;

    ACE_Time_Value startTime_m;    

    CORBA::Boolean isFepAlive_m;
	 */
};//class BulkDataNTCallback

};//namespace AcsBulkdata


#endif /*!_BULKDATA_CALLBACK_H*/

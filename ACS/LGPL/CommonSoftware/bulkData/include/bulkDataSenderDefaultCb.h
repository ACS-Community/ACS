#ifndef _BULKDATA_SENDER_DEFAULTCB_H
#define _BULKDATA_SENDER_DEFAULTCB_H


/** @file bulkDataSenderDefaultCb.h 
 */

/** @defgroup BULKDATASENDERDEFAULTCBDOC Bulk Data Sender Default Cb
 *  @{
 * @htmlonly
 <hr size="2" width="100%">
 <div align="left">
 <h2>Description</h2>
 Data can be send in an asynchronous way using a callback. This file provides a default
 callback. In order to use it, the user must provide his own implementation following this
 template. The get_timeout(...) method sets the time interval between two successive
 handel_timeout(...) calls.
 <br>
 <br>
 <h2>Links</h2>
 <ul>
 <li><a href="classBulkDataSenderDefaultCallback.html">Bulk Data Sender Default Callback Class Reference</a></li>
 </ul>
 </div>
 @endhtmlonly
 * @}
 */

class BulkDataSenderDefaultCallback : public TAO_AV_Callback
{

  public:

    /**
     * Constructor
     * @param stream established stream on which data are sent
     */
    BulkDataSenderDefaultCallback (TAO_StreamCtrl * stream_p)
	{
	    ACS_TRACE("BulkDataSenderDefaultCallback::BulkDataSenderDefaultCallback");

	    isFepAlive_m = true;
	}


    /** 
     *  Method called with a period determined by the the timeout tv parameter
     *  (see method get_timeout(...).
     *  @return void
     *  @htmlonly
     <br><hr>
     @endhtmlonly
    */
    virtual int handle_timeout (void *arg)
	{
	    ACS_TRACE("BulkDataSenderDefaultCallback::handle_timeout");
	    return 0;
	}


    virtual int handle_end_stream (void)
	{
	    ACS_TRACE("BulkDataSenderDefaultCallback::handle_end_stream");
	    return 0;
	}

    /** 
     *  Sets the timeout tv which determines the period of sending data.
     *  (zoer means that the callback is never activated).
     *  @return void
     *  @htmlonly
     <br><hr>
     @endhtmlonly
    */
    virtual void get_timeout (ACE_Time_Value *&tv, void *&arg)
	{
	    ACS_TRACE("BulkDataSenderDefaultCallback::get_timeout");
	    tv = 0;
      
	    /*
	      ACE_Time_Value *timeout;
	      ACE_NEW (timeout,ACE_Time_Value(0,500000));
	      tv = timeout;
	    */
	}

    void set_protocol_object (TAO_AV_Protocol_Object * prtclObject_p) 
	{
	    ACS_TRACE("BulkDataSenderDefaultCallback::set_protocol_object");
	    protocolObject_p = prtclObject_p;
	}

    virtual int handle_destroy (void)
	{
	    ACS_TRACE("BulkDataSenderDefaultCallback::handle_destroy");

	    isFepAlive_m = false;

	    return 0;
	}

    ACE_HANDLE getHandle()
	{
	    ACE_Event_Handler *event_handler = handler_->event_handler();
	    ACE_HANDLE handle = event_handler->get_handle();
	    return handle;
	}


    CORBA::Boolean isFepAlive()
	{
	    return isFepAlive_m;
	}


  private:

    TAO_StreamCtrl * stream_p;
    TAO_AV_Protocol_Object * protocolObject_p;

    CORBA::Boolean isFepAlive_m;
};

#endif /* _BULKDATA_SENDER_DEFAULTCB_H */


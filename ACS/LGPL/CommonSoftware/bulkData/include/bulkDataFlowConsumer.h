#ifndef _BULKDATA_FLOW_CONSUMER_H
#define _BULKDATA_FLOW_CONSUMER_H
/*******************************************************************************
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *
 *
 * "@(#)"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * oat       27/01/05  created 
 */

/** @file bulkDataFlowConsumer.h  
 */

namespace AcsBulkdata
{
  /** @defgroup BULKDATAFLOWCONSUMERDOC Bulk Data Flow Consumer
   *  @{
   * @htmlonly
   <hr size="2" width="100%">
   <div align="left">
   <h2>Description</h2>
   bulkDataFlowConsumer.h implements the Bulk Data Flow Consumer
   <br>
   <br>
   <h2>Links</h2>
   <ul>
   <li><a href="classBulkDataFlowConsumer.html">BulkDataFlowConsumer Class Reference</a></li>
   </ul>
   </div>
   @endhtmlonly
   * @}
   */


  template<class TReceiverCallback>
    class BulkDataFlowConsumer : public virtual TAO_FlowConsumer
    {
    public:

      /** Constructor
       * @param flowname
       * @param AVStreams::protocolSpec
       * @param format
       * @htmlonly
       <br><hr>
       @endhtmlonly
      */
      BulkDataFlowConsumer(const char *flowname, AVStreams::protocolSpec prot, const char *format);

      /**
       * Destructor
       */
      virtual ~BulkDataFlowConsumer();

      /**
       * Creates the application callback and return its handle to
       * AVStreams for further application callbacks.
       * This callback will be used to store data e.g. in the Archive.
       * @param flowname
       * @param TAO_AV_Callback
       * @return int
       *  @htmlonly
       <br><hr>
       @endhtmlonly
      */
      virtual int get_callback (const char *flowname, TAO_AV_Callback *&callback);

      /**
       * Set the protocol object
       * @param flowname
       * @param TAO_AV_Protocol_Object
       * @return int
       * @htmlonly
       <br><hr>
       @endhtmlonly
      */
      virtual int set_protocol_object(const char *flowname, TAO_AV_Protocol_Object *object);

      /**
       * Get the protocol object
       * @return TAO_AV_Protocol_Object *
       * @htmlonly
       <br><hr>
       @endhtmlonly
      */
      virtual TAO_AV_Protocol_Object *getProtocolObject();
    
      virtual TReceiverCallback * getBulkDataCallback();

      virtual void set_flow_handler (const char *flowname,
                                     TAO_AV_Flow_Handler *handler);

      virtual  TAO_AV_Flow_Handler * getFlowHandler();

      virtual void setCbTimeout(ACE_Time_Value cbTimeout)
          {
	      cbTimeout_m = cbTimeout;
          }	

    private:

      TReceiverCallback *cb_p;

      ACE_CString flowname_m;

      TAO_AV_Protocol_Object *protObj_p;

      TAO_AV_Flow_Handler *handler_p;

      ACE_Time_Value cbTimeout_m;	
    };


}


#include "bulkDataFlowConsumer.i"

#endif /* _BULKDATA_FLOW_CONSUMER_H */

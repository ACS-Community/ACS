template<class TReceiverCallback>
AcsBulkdata::BulkDataFlowConsumer<TReceiverCallback>::BulkDataFlowConsumer(const char *flowname, AVStreams::protocolSpec prot, const char *format) : TAO_FlowConsumer(flowname,prot,format)
{
  ACS_TRACE("BulkDataFlowConsumer<>::BulkDataFlowConsumer");

  flowname_m = flowname;

  AVStreams::protocolSpec protocols(2);
  protocols.length(3);
  protocols[0] = CORBA::string_dup("TCP");
  protocols[1] = CORBA::string_dup("UDP");
  protocols[2] = CORBA::string_dup("RTP/UDP");
    
  cb_p = 0;

  cbTimeout_m.set(0,10); // default value, 0 sec, 10 usec

  try    
    {
      set_protocol_restriction(protocols);
    }
  catch(...)
    {
      ACS_SHORT_LOG((LM_INFO,"BulkDataFlowConsumer<>::BulkDataFlowConsumer protocol not istantiated"));
    }
}


template<class TReceiverCallback>
AcsBulkdata::BulkDataFlowConsumer<TReceiverCallback>::~BulkDataFlowConsumer()
{
  ACS_TRACE("BulkDataFlowConsumer<>::~BulkDataFlowConsumer");

//  TAO_AV_CORE::instance ()->remove_acceptor(m_flowName.c_str());
//  TAO_AV_CORE::instance ()->remove_connector(m_flowName.c_str());

  if(cb_p)
      delete cb_p;
}


template<class TReceiverCallback>
int AcsBulkdata::BulkDataFlowConsumer<TReceiverCallback>::get_callback(const char *flowname, TAO_AV_Callback *&callback)
{
  ACS_TRACE("BulkDataFlowConsumer<>::get_callback");

  TReceiverCallback *callback_p = new TReceiverCallback();
  if(callback_p == NULL)
    {
      ACE_ERROR_RETURN((LM_ERROR,"BulkDataFlowConsumer<>::get_callback not initialized."),-1);
    }

  callback_p->setCbTimeout(cbTimeout_m);

  callback = callback_p;

  callback_p->setFlowname(flowname);

  cb_p = callback_p;
    
  return 0;
}


template<class TReceiverCallback>
int AcsBulkdata::BulkDataFlowConsumer<TReceiverCallback>::set_protocol_object(const char *flowname, TAO_AV_Protocol_Object *object)
{
  ACS_TRACE("BulkDataFlowConsumer<>::set_protocol_object");

  protObj_p = object;

  return 0;
}


template<class TReceiverCallback>
TAO_AV_Protocol_Object * AcsBulkdata::BulkDataFlowConsumer<TReceiverCallback>::getProtocolObject()
{
  ACS_TRACE("BulkDataFlowConsumer<>::get_protocol_object");

  return protObj_p;
}


template<class TReceiverCallback>
TReceiverCallback* AcsBulkdata::BulkDataFlowConsumer<TReceiverCallback>::getBulkDataCallback()
{
  ACS_TRACE("BulkDataFlowConsumer<>::getBulkDataCallback");
/*
  if(cb_p == 0)
      {
      AVCallbackErrorExImpl err = AVCallbackErrorExImpl(__FILE__,__LINE__,"BulkDataFlowConsumer::getBulkDataCallback");
      throw err;
      }
*/
  return cb_p;
}


template<class TReceiverCallback>
void AcsBulkdata::BulkDataFlowConsumer<TReceiverCallback>::set_flow_handler(const char *flowname,TAO_AV_Flow_Handler *handler )
{
  ACS_TRACE("BulkDataFlowConsumer<>::set_flow_handler");

  ACE_UNUSED_ARG(flowname);

  handler_p = handler;
}


template<class TReceiverCallback>
TAO_AV_Flow_Handler * AcsBulkdata::BulkDataFlowConsumer<TReceiverCallback>::getFlowHandler()
{
  ACS_TRACE("BulkDataFlowConsumer<>::getFlowHandler");

  return handler_p;
}


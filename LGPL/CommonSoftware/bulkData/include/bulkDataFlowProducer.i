template<class TSenderCallback>
AcsBulkdata::BulkDataFlowProducer<TSenderCallback>::BulkDataFlowProducer(const char *flowname, AVStreams::protocolSpec prot, const char *format, TAO_StreamCtrl *strCtrl) : TAO_FlowProducer(flowname, prot, format)
{
  ACS_TRACE("BulkDataFlowProducer<>::BulkDataFlowProducer");

  flowname_m = flowname;
  protObj_p = 0;
  callback_p = 0;
  strCtrl_p = strCtrl;
}


template<class TSenderCallback>
AcsBulkdata::BulkDataFlowProducer<TSenderCallback>::~BulkDataFlowProducer()
{
  ACS_TRACE("BulkDataFlowProducer<>::~BulkDataFlowProducer");

//  TAO_AV_CORE::instance ()->remove_acceptor(m_flowName.c_str());
//  TAO_AV_CORE::instance ()->remove_connector(m_flowName.c_str());

  if(callback_p)
      delete callback_p;
}


template<class TSenderCallback>
int AcsBulkdata::BulkDataFlowProducer<TSenderCallback>::get_callback(const char *flowname, TAO_AV_Callback *&callback)
{
  ACS_TRACE("BulkDataFlowProducer<>::get_callback");

  callback_p = new TSenderCallback(strCtrl_p);
  if(callback_p == NULL)
    {
      ACE_ERROR_RETURN((LM_ERROR,"BulkDataFlowProducer<>::get_callback not initialized."),-1);
    }

  callback = callback_p;

  //callback_p->flowname(flowname);
    
  return 0;
}


template<class TSenderCallback>
int AcsBulkdata::BulkDataFlowProducer<TSenderCallback>::set_protocol_object(const char *flowname, TAO_AV_Protocol_Object *object)
{
  ACS_TRACE("BulkDataFlowProducer<>::set_protocol_object");

  protObj_p = object;

  return 0;
}


template<class TSenderCallback>
TAO_AV_Protocol_Object * AcsBulkdata::BulkDataFlowProducer<TSenderCallback>::getProtocolObject()
{
//  ACS_TRACE("BulkDataFlowProducer<>::getProtocolObject");

  return protObj_p;
}


template<class TSenderCallback>
TSenderCallback* AcsBulkdata::BulkDataFlowProducer<TSenderCallback>::getBulkDataCallback()
{
  ACS_TRACE("BulkDataFlowProducer<>::getBulkDataCallback");

  return callback_p;
}


template<class TSenderCallback>
void AcsBulkdata::BulkDataFlowProducer<TSenderCallback>::set_flow_handler(const char *flowname,TAO_AV_Flow_Handler *handler )
{
  ACS_TRACE("BulkDataFlowProducer<>::set_flow_handler");

  handler_p = handler;
}


template<class TSenderCallback>
TAO_AV_Flow_Handler * AcsBulkdata::BulkDataFlowProducer<TSenderCallback>::getFlowHandler()
{
  ACS_TRACE("BulkDataFlowProducer<>::getFlowHandler");

  return handler_p;
}


/*
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2003 
*
*This library is free software; you can redistribute it and/or
*modify it under the terms of the GNU Lesser General Public
*License as published by the Free Software Foundation; either
*version 2.1 of the License, or (at your option) any later version.
*
*This library is distributed in the hope that it will be useful,
*but WITHOUT ANY WARRANTY; without even the implied warranty of
*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*Lesser General Public License for more details.
*
*You should have received a copy of the GNU Lesser General Public
*License along with this library; if not, write to the Free Software
*Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */

#include "baciRWcontImpl_T.h"
#include "baciPcontImpl_T.i"
#include "baciRWcommonImpl_T.i"

template<ACS_RW_C> 
RWcontImpl<ACS_RW_TL>::RWcontImpl(const ACE_CString& name, BACIComponent* component_p, DevIO<TM>* devIO, bool flagdeldevIO) : 
    RWcommonImpl<ACS_RW_TL>(name, component_p, devIO, flagdeldevIO),
    PcontImpl<ACS_P_TL>(name, this->getProperty(), component_p, devIO, flagdeldevIO)
{
  ACS_TRACE("baci::RWcontImpl&lt;&gt;::RWcontImpl");

  // read static data
  if (readCharacteristics()==false) 
      {
      std::string procName="RWcontImpl::RWcontImpl(";
      procName+=name.c_str();
      procName+=",...)";
      ACS_LOG(LM_RUNTIME_CONTEXT, "baci::RWcontImpl&lt;&gt;::RWcontImpl",
	      (LM_ERROR, "Failed to read static data for '%s'", name.c_str()));
      baciErrTypeProperty::PropertyStaticDataExImpl ex(__FILE__,__LINE__,procName.c_str());
      ex.addData("Property",name.c_str());
      throw ex;
    }

  this->initialization_m = 0;
  ACS_DEBUG("baci::RWcontImpl&lt;&gt;::RWcontImpl", "Successfully created.");
}

template<ACS_RW_C> 
RWcontImpl<ACS_RW_TL>::RWcontImpl(bool init, const ACE_CString& name, BACIComponent* component_p, DevIO<TM>* devIO, bool flagdeldevIO):
    RWcommonImpl<ACS_RW_TL>(init, name, component_p, devIO, flagdeldevIO),
    PcontImpl<ACS_P_TL>(name, this->getProperty(), component_p, devIO, flagdeldevIO)
{
  ACS_TRACE("baci::RWcontImpl&lt;&gt;::RWcontImpl");

  // read static data
  if (readCharacteristics()==false) 
      {
      std::string procName="RWcontImpl::RWcontImpl(";
      procName+=name.c_str();
      procName+=",...)";
      ACS_LOG(LM_RUNTIME_CONTEXT, "baci::RWcontImpl&lt;&gt;::RWcontImpl",
	      (LM_ERROR, "Failed to read static data for '%s'", name.c_str()));
      baciErrTypeProperty::PropertyStaticDataExImpl ex(__FILE__,__LINE__,procName.c_str());
      ex.addData("Property",name.c_str());
      throw ex;
    }
  
  this->initialization_m = 0;
  ACS_DEBUG("baci::RWcontImpl&lt;&gt;::RWcontImpl", "Successfully created.");
}


template<ACS_RW_C> RWcontImpl<ACS_RW_TL>::~RWcontImpl()
{
  ACS_TRACE("baci::RWcontImpl&lt;&gt;::~RWcontImpl");
}

/* --------------- [ Action implementator interface ] -------------- */

template<ACS_RW_C>
ActionRequest RWcontImpl<ACS_RW_TL>::invokeAction(int function,
						  BACIComponent* component_p, 
						  const int &callbackID, 
						  const CBDescIn& descIn, 
						  BACIValue* value, 
						  Completion& completion, 
						  CBDescOut& descOut)
{
  // better implementation with array is possible
  ActionRequest req;
  switch (function) 
    {
    case GET_ACTION:
      req = this->getValueAction(component_p, callbackID, descIn, value, completion, descOut);
      break;
    case SET_ACTION:
      req = this->setValueAction(component_p, callbackID, descIn, value, completion, descOut);
      break;
    case INC_ACTION:
      req = incrementAction(component_p, callbackID, descIn, value, completion, descOut);
      break;
    case DEC_ACTION:
      req = decrementAction(component_p, callbackID, descIn, value, completion, descOut);
      break;
    default:
      return reqDestroy;
    }

  ACS_COMPLETION(completion, "baci::RWcontImpl&lt;&gt;::invokeAction");
  return req;
}

// async. set value action implementation
template<ACS_RW_C>
void RWcontImpl<ACS_RW_TL>::setValue(BACIProperty* property,
		   BACIValue* value, 
		   Completion &completion,
		   CBDescOut& descOut)
{
  ACE_UNUSED_ARG(property);
  ACE_UNUSED_ARG(descOut);

  TSM v =  value->getValue(static_cast<TSM*>(0));

  if (min_value()>v || max_value()<v)
      { 
      completion.timeStamp=getTimeStamp();

//      char msg[100];
//      sprintf(msg, "%d < %d < %d", min_value(), value->longValue(), max_value());
//      ACS_COMPLETION(completion, "baci::RWcontImpl<ACS_RW_TL>::setValue", ACSErr::ACSErrTypeCommon, ACSErrTypeCommon::OutOfBounds, "Error out of bounds");

      completion = ACSErrTypeCommon::OutOfBoundsCompletion( __FILE__, __LINE__,
							   "RWcommonImpl<ACS_RW_TL>::setValueAction");

      return;
    } 

  completion.timeStamp = getTimeStamp();
  TM t =  value->getValue(static_cast<TM*>(0));
  try 
      {
      this->devIO_mp->write(t, completion.timeStamp);
      }
  catch (ACSErr::ACSbaseExImpl& ex)
      {
      completion = baciErrTypeDevIO::WriteErrorCompletion (ex, __FILE__, __LINE__,"RWcontImpl::setValue(...)");
      return;
      }
  
  completion.type = ACSErr::ACSErrTypeOK;		// no error
  completion.code = ACSErrTypeOK::ACSErrOK;
}//setVale

/// async. increment value action implementation
template<ACS_RW_C>
ActionRequest RWcontImpl<ACS_RW_TL>::incrementAction(BACIComponent* component_p, int callbackID,
			  const CBDescIn& descIn, BACIValue* val,
			  Completion& completion, CBDescOut& descOut)
{
  ACE_UNUSED_ARG(val);

  BACIValue value(0.0);
  this->getValueAction(component_p, callbackID, descIn, &value, completion, descOut);
  
  // if there is no error
  if (completion.type==ACSErr::ACSErrTypeOK)
  {
	 TSM newValue = value.getValue(static_cast<TSM*>(0));
	 newValue += this->min_step();
	 value.setValue(newValue);

	 this->setValueAction(component_p, callbackID, descIn, &value, completion, descOut);
  }

  ACS_COMPLETION(completion, "baci::RWcontImpl&lt;&gt;::incrementAction");

  // complete action requesting done invokation, 
  // otherwise return reqInvokeWorking and set descOut.estimated_timeout
  return reqInvokeDone;
}


/// async. decrement value action implementation
template<ACS_RW_C>
ActionRequest RWcontImpl<ACS_RW_TL>::decrementAction(BACIComponent* component_p, int callbackID,
						     const CBDescIn& descIn, BACIValue* val,
						     Completion& completion, CBDescOut& descOut)
{
  ACE_UNUSED_ARG(val);

  BACIValue value(0.0);
  this->getValueAction(component_p, callbackID, descIn, &value, completion, descOut);

  // if there is no error
  if (completion.type==ACSErr::ACSErrTypeOK)
  {
  	 TSM newValue = value.getValue(static_cast<TSM*>(0));
	 newValue -= this->min_step();
	 value.setValue(newValue);

	 this->setValueAction(component_p, callbackID, descIn, &value, completion, descOut);
  }

  ACS_COMPLETION(completion, "baci::RWcontImpl&lt;&gt;::decrementAction");

  // complete action requesting done invokation, 
  // otherwise return reqInvokeWorking and set descOut.estimated_timeout
  return reqInvokeDone;
}

template<ACS_RW_C>
bool RWcontImpl<ACS_RW_TL>::readCharacteristics()
{

  DAONode* dao = this->getDAONode();
  if (!dao)
      return false;
  
  try
      {

      // NOTE: var is always a scalar value
#define READ_VALUE(name, var) \
      { \
      CORBA::String_var str = dao->get_string( name ); \
      std::istringstream is(str.in()); \
      (istream&) is >> var ; \
      if (!is) \
	  throw CDB::WrongDataType(); \
      }

      READ_VALUE("min_value", min_value_m);
      READ_VALUE("max_value", max_value_m);

#undef READ_VALUE

      return true;
      }
  catch (ACSErr::ACSbaseExImpl& ex)
      {
      ex.log();
      return false;
      }
  catch (...)
      {
      return false;
      }

}

/* ---------------------- [ CORBA interface ] ---------------------- */

template<ACS_RW_C>
void RWcontImpl<ACS_RW_TL>::increment (ACS::CBvoid_ptr cb,
		     const ACS::CBDescIn & desc)
  throw (CORBA::SystemException)
{
  this->property_mp->getComponent()->registerAction(BACIValue::type_null, cb, 
				       desc, this, INC_ACTION);
}

template<ACS_RW_C>
void RWcontImpl<ACS_RW_TL>::decrement (ACS::CBvoid_ptr cb,
		     const ACS::CBDescIn & desc)
  throw (CORBA::SystemException)
{
  this->property_mp->getComponent()->registerAction(BACIValue::type_null, cb, 
				       desc, this, DEC_ACTION);
}

template<ACS_RW_C>
TS RWcontImpl<ACS_RW_TL>::min_value ()
  throw (CORBA::SystemException)
{
  return min_value_m;
}

template<ACS_RW_C>
TS RWcontImpl<ACS_RW_TL>::max_value ()
  throw (CORBA::SystemException) 
{
  return max_value_m;
}
















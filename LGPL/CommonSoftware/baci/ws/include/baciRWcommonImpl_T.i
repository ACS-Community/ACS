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

#include "baciRWcommonImpl_T.h"

template<ACS_RW_C> 
baci::RWcommonImpl<ACS_RW_TL>::RWcommonImpl(const ACE_CString& name, BACIComponent* component_p, DevIO<TM>* devIO, bool flagdeldevIO) : 
    PcommonImpl<ACS_P_TL>(name, component_p, devIO, flagdeldevIO) 
{
  ACS_TRACE("baci::RWcommonImpl&lt;&gt;::RWcommonImpl");

  if (this->devIO_mp->initializeValue()==true) 
      {
      ACS::Time timeStamp = getTimeStamp();
      try 
	  {
	  this->devIO_mp->write(this->defaultValue_m, timeStamp);
	  } 
      catch (ACSErr::ACSbaseExImpl& ex) 
	  {
	  std::string procName="RWcommonImpl::RWcommonImpl(";
	  procName+=name.c_str();
	  procName+=",...)";
	  baciErrTypeProperty::PropertySetInitValueExImpl newEx(ex.getErrorTrace(),__FILE__,__LINE__,procName.c_str());
	  newEx.addData("Property",name.c_str());
	  throw newEx;
	  } 
      catch (...) // can not happend but ...
	  {
	  std::string procName="RWcommonImpl::RWcommonImpl(";
	  procName+=name.c_str();
	  procName+=",...)";
	  baciErrTypeProperty::PropertySetInitValueExImpl newEx(__FILE__,__LINE__,procName.c_str());
	  newEx.addData("Property",name.c_str());
	  throw newEx;
	  }
      ACS_DEBUG("baci::RWcommonImpl&lt;&gt;::RWcommonImpl", "DevIO initial value set to the default value.");
      }

//  initialization_m = 0;
  ACS_DEBUG("baci::RWcommonImpl&lt;&gt;::RWcommonImpl", "Successfully created.");
}

template<ACS_RW_C> 
baci::RWcommonImpl<ACS_RW_TL>::RWcommonImpl(bool init, const ACE_CString& name, BACIComponent* component_p, DevIO<TM>* devIO, bool flagdeldevIO) : 
    PcommonImpl<ACS_P_TL>(name, component_p, devIO, flagdeldevIO) 
{
  ACS_TRACE("baci::RWcommonImpl&lt;&gt;::RWcommonImpl");
  
  ACS_DEBUG("baci::RWcommonImpl&lt;&gt;::RWcommonImpl", "Successfully created.");
}

template<ACS_RW_C> baci::RWcommonImpl<ACS_RW_TL>::~RWcommonImpl()
{
    ACS_TRACE("baci::RWcommonImpl&lt;&gt;::~RWcommonImpl");
}

/* --------------- [ Action implementator interface ] -------------- */

template<ACS_RW_C>
baci::ActionRequest baci::RWcommonImpl<ACS_RW_TL>::invokeAction(int function,
						    BACIComponent* component_p, 
						    const int &callbackID, 
						    const CBDescIn& descIn, 
						    BACIValue* value, 
						    Completion& completion, 
						    CBDescOut& descOut)
{
  // better implementation with array is possible
  ActionRequest req;
  CompletionImpl c;

  switch (function) 
    {
    case GET_ACTION:
      req = this->getValueAction(component_p, callbackID, descIn, value, c, descOut);
      break;
    case SET_ACTION:
      req = setValueAction(component_p, callbackID, descIn, value, c, descOut);
      break;
    default:
      return reqDestroy;
    }
  
  if (c.isErrorFree())
      {
      completion = c;
      }
  else
      {
      completion = baciErrTypeProperty::InvokeActionErrorCompletion(c,
					       __FILE__,
					       __LINE__,
					       "baci::RWcommonImpl&lt;&gt;::invokeAction");
	}

  return req;
}//invokeAction

/* -------------- [ Other interfaces ] -------------- */

/// async. set value action implementation
template<ACS_RW_C>
baci::ActionRequest baci::RWcommonImpl<ACS_RW_TL>::setValueAction(BACIComponent* component_p, 
						      int callbackID,
						      const CBDescIn& descIn, 
						      BACIValue* value,
						      Completion& completion, 
						      CBDescOut& descOut)
{
  ACE_UNUSED_ARG(component_p);
  ACE_UNUSED_ARG(callbackID);
  ACE_UNUSED_ARG(descIn);

  CompletionImpl co;

  this->setValue(this->property_mp, value, co, descOut);

  if (co.isErrorFree())
      {
      completion = co;
      }
  else
      {
      baciErrTypeProperty::CanNotSetValueCompletion  errComp(co, 
					__FILE__, 
					__LINE__, 
					"baci::RWcommonImpl&lt;&gt;::setValueAction");
      errComp.setProperty((this->getProperty()->getName()));
      completion = errComp;
      }

  // complete action requesting done invokation, 
  // otherwise return reqInvokeWorking and set descOut.estimated_timeout
  return reqInvokeDone;
}

// async. set value action implementation
template<ACS_RW_C>
void baci::RWcommonImpl<ACS_RW_TL>::setValue(BACIProperty* property,
		   BACIValue* value, 
		   Completion &completion,
		   CBDescOut& descOut)
{
  ACE_UNUSED_ARG(property);
  ACE_UNUSED_ARG(descOut);

  ACS::Time ts  = getTimeStamp();
  TM v =  value->getValue(static_cast<TM*>(0));
  try
      {
      this->devIO_mp->write(v, ts);
      }
  catch (ACSErr::ACSbaseExImpl& ex) 
      {
      completion = baciErrTypeDevIO::WriteErrorCompletion (ex, __FILE__, __LINE__,"RWcommonImpl::setValue(...)");
      return;
      } 
  
  completion = ACSErrTypeOK::ACSErrOKCompletion();
  completion.timeStamp = ts; // update timestamp with value set in DevIO
}//setValue

/* ---------------------- [ CORBA interface ] ---------------------- */

template<ACS_RW_C>
ACSErr::Completion * baci::RWcommonImpl<ACS_RW_TL>::set_sync (TIN val
		    )
{
  CompletionImpl co;
  BACIValue value(val);
  ACS::CBDescOut descOut;

  this->setValue(this->property_mp, &value, co, descOut);
  if (co.isErrorFree())
      {
      return co.returnCompletion(false);
      }
  else
      {
      baciErrTypeProperty::CanNotSetValueCompletion completion(co, 
					  __FILE__, 
					  __LINE__, 
					  "baci::RWcommonImpl&lt;&gt;::set_sync");
      completion.setProperty((this->getProperty()->getName()));
      return completion.returnCompletion(false);
      }
}//set_sync

template<ACS_RW_C>
void baci::RWcommonImpl<ACS_RW_TL>::set_async (TIN value,
		     ACS::CBvoid_ptr cb,
		     const ACS::CBDescIn & desc
		     )
{
  this->property_mp->getComponent()->registerAction(BACIValue::type_null, cb, 
				       desc, this, SET_ACTION, BACIValue(value));
}//set_async

template<ACS_RW_C>
void baci::RWcommonImpl<ACS_RW_TL>::set_nonblocking (TIN val
			   )
{
  ACSErr::CompletionImpl c;
  BACIValue value(val);
  ACS::CBDescOut descOut;

  this->setValue(this->property_mp, &value, c, descOut);
  
  if (!c.isErrorFree())
      {
      baciErrTypeProperty::CanNotSetValueCompletion completion(c, 
				 __FILE__, 
				 __LINE__, 
				 "baci::RWcommonImpl&lt;&gt;::set_nonblocking");
      completion.setProperty(this->property_mp->getName());
      completion.log();
      }
}//set_nonblocking















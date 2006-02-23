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

#include "baciROcommonImpl_T.h"

template<ACS_RO_C> 
ROcommonImpl<ACS_RO_TL>::ROcommonImpl(const ACE_CString& name, BACIComponent* component_p, DevIO<TM>* devIO, bool flagdeldevIO) : 
    PcommonImpl<ACS_P_TL>(name, component_p, devIO, flagdeldevIO),
    monitorEventDispatcher_mp(0)
{
  ACS_TRACE("baci::ROcommonImpl&lt;&gt;::ROcommonImpl");
  
  // read static data
  if (readCharacteristics()==false) 
    {
		ACS_LOG(LM_RUNTIME_CONTEXT, "baci::ROcommonImpl&lt;&gt;::ROcommonImpl",
			(LM_ERROR, "Failed to read static data for '%s'", name.c_str()));
		return;
    }

 if (this->devIO_mp->initializeValue()==true) 
      {
      ACS::Time timeStamp = getTimeStamp();

      try 
	  {
	  this->devIO_mp->write(this->defaultValue_m, timeStamp);
	  } 
      catch (ACSErr::ACSbaseExImpl& ex) 
	  {
	  std::string procName="ROcommonImpl::ROcommonImpl(";
	  procName+=name.c_str();
	  procName+=",...)";
	  baciErrTypeProperty::PropertySetInitValueExImpl newEx(ex.getErrorTrace(),__FILE__,__LINE__,procName.c_str());
	  newEx.addData("Property",name.c_str());
	  throw newEx;
	  } 
      catch (...) 
	  {
	  std::string procName="ROcommonImpl::ROcommonImpl(";
	  procName+=name.c_str();
	  procName+=",...)";
	  baciErrTypeProperty::PropertySetInitValueExImpl newEx(__FILE__,__LINE__,procName.c_str());
	  newEx.addData("Property",name.c_str());
	  throw newEx;
	  }
      ACS_DEBUG("baci::ROcommonImpl&lt;&gt;::ROcommonImpl", "DevIO initial value set to the default value.");
      }


  ACS_DEBUG("baci::ROcommonImpl&lt;&gt;::ROcommonImpl", "Successfully created.");
// TMP uncment
//initialization_m = 0;
}

template<ACS_RO_C> 
ROcommonImpl<ACS_RO_TL>::ROcommonImpl(bool init, const ACE_CString& name, BACIComponent *component_p, DevIO<TM> *devIO, bool flagdeldevIO ) : 
    PcommonImpl<ACS_P_TL>(name, component_p, devIO, flagdeldevIO),
    monitorEventDispatcher_mp(0)
{
  ACS_TRACE("baci::ROcommonImpl&lt;&gt;::ROcommonImpl");
  ACE_UNUSED_ARG(init);
  
  // read static data
  if (readCharacteristics()==false) 
    {
		ACS_LOG(LM_RUNTIME_CONTEXT, "baci::ROcommonImpl&lt;&gt;::ROcommonImpl",
			(LM_ERROR, "Failed to read static data for '%s'", name.c_str()));
		return;
    }

  ACS_DEBUG("baci::ROcommonImpl&lt;&gt;::ROcommonImpl", "Successfully created.");

  // property successfuly initialized
// TMP uncment
//initialization_m = 0;
}

template<ACS_RO_C> ROcommonImpl<ACS_RO_TL>::~ROcommonImpl()
{
  ACS_TRACE("baci::ROcommonImpl&lt;&gt;::~ROcommonImpl");
  

   // destroy event dispatcher (including event subscribers)
  if (monitorEventDispatcher_mp!=0) 
  {
    delete monitorEventDispatcher_mp;
    monitorEventDispatcher_mp = 0;
  }
}



template<ACS_RO_C> 
bool ROcommonImpl<ACS_RO_TL>::readCharacteristics()
{
  DAONode* dao = this->getDAONode();
  if (!dao)
      return false;
  
  try
      {

      CORBA::Double dbl;

      dbl = dao->get_double("alarm_timer_trig");
      dbl = dbl * static_cast<CORBA::Double>(10000000.0);
      alarmTimerTrig_m = static_cast<CORBA::ULong>(dbl);

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

/*___oOo___*/

















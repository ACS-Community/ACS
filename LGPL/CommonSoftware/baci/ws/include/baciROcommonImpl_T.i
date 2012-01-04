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

#include <Basic_Types.h> // for ACE_UINT64_FORMAT_SPECIFIER_ASCII
#include "baciROcommonImpl_T.h"

template<ACS_RO_C> 
baci::ROcommonImpl<ACS_RO_TL>::ROcommonImpl(const ACE_CString& name, BACIComponent* component_p, DevIO<TM>* devIO, bool flagdeldevIO) : 
    PcommonImpl<ACS_P_TL>(name, component_p, devIO, flagdeldevIO),
    monitorEventDispatcher_mp(0), alarmSystemMonitor_mp(0)
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

 if (this->monitorEventDispatcher_mp==0 && this->alarmTimerTrig_m!=0)
     {
     CBDescIn descIn;
     descIn.id_tag = 0;
     this->monitorEventDispatcher_mp = new MonitorEventDispatcher<TIN, TCB, POA_CB>(descIn, this->alarmTimerTrig_m, this->property_mp);
     }  //if

  ACS_DEBUG("baci::ROcommonImpl&lt;&gt;::ROcommonImpl", "Successfully created.");
// TMP uncment
//initialization_m = 0;
}

template<ACS_RO_C> 
baci::ROcommonImpl<ACS_RO_TL>::ROcommonImpl(bool init, const ACE_CString& name, BACIComponent *component_p, DevIO<TM> *devIO, bool flagdeldevIO ) : 
    PcommonImpl<ACS_P_TL>(name, component_p, devIO, flagdeldevIO),
    monitorEventDispatcher_mp(0), alarmSystemMonitor_mp(0)
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

  if (this->monitorEventDispatcher_mp==0)
      {
      CBDescIn descIn;
      descIn.id_tag = 0;
      this->monitorEventDispatcher_mp = new MonitorEventDispatcher<TIN, TCB, POA_CB>(descIn, this->alarmTimerTrig_m, this->property_mp);
      }  //if

  ACS_DEBUG("baci::ROcommonImpl&lt;&gt;::ROcommonImpl", "Successfully created.");

  // property successfuly initialized
// TMP uncment
//initialization_m = 0;
}//ROcommonImpl

template<ACS_RO_C> baci::ROcommonImpl<ACS_RO_TL>::~ROcommonImpl()
{
  ACS_TRACE("baci::ROcommonImpl&lt;&gt;::~ROcommonImpl");
  

   // destroy event dispatcher (including event subscribers)
  if (monitorEventDispatcher_mp!=0) 
      {
      delete monitorEventDispatcher_mp;
      monitorEventDispatcher_mp = 0;
      }
}//~ROcommonImpl

template<ACS_RO_C>
void baci::ROcommonImpl<ACS_RO_TL>::setAlarmFaultFamily(const char* ff)
{
	ACS_TRACE("baci::ROcommonImpl&lt;&gt;::setAlarmFaultFamily");
	if (ff==0)
		throw ACSErrTypeCommon::NullPointerExImpl (__FILE__, __LINE__, "baci::ROcommonImpl&lt;&gt;::setAlarmFaultFamily");

	if (this->alarmSystemMonitor_mp!=0)
	{
		this->alarmSystemMonitor_mp->setFaultFamily(ff);
	}
	else
	{
		if (this->alarmTimerTrig_m!=0)
		{
			//TBD: better exception should be thrown
			ACSErrTypeCommon::NullPointerExImpl ex(__FILE__, __LINE__, "baci::ROcommonImpl&lt;&gt;::setAlarmFaultFamily");
			ex.setVariable("alarmSystemMonitor_mp");
			throw ex;
		}
		else
		{
			ACS_LOG(LM_RUNTIME_CONTEXT, "baci::ROcommonImpl&lt;&gt;::setAlarmFaultFamily", (LM_DEBUG, "Failed to set alarm fault family for property: %s to: %s. alarmTimerTrig="ACE_UINT64_FORMAT_SPECIFIER_ASCII, this->property_mp->getName(), ff, alarmTimerTrig_m));
		}//if-else
	}//if-else
}//setAlarmFaultFamily

template<ACS_RO_C>
void baci::ROcommonImpl<ACS_RO_TL>::setAlarmFaultMember(const char* fm)
{
	ACS_TRACE("baci::ROcommonImpl&lt;&gt;::setAlarmFaultMember");
	if (fm==0)
		throw ACSErrTypeCommon::NullPointerExImpl (__FILE__, __LINE__, "baci::ROcommonImpl&lt;&gt;::setAlarmFaultMember");

	if (this->alarmSystemMonitor_mp!=0)
	{
		this->alarmSystemMonitor_mp->setFaultMember(fm);
	}
	else
	{
		if (this->alarmTimerTrig_m!=0)
		{
			//TBD: better exception should be thrown
			ACSErrTypeCommon::NullPointerExImpl ex(__FILE__, __LINE__, "baci::ROcommonImpl&lt;&gt;::setAlarmFaultMember");
			ex.setVariable("alarmSystemMonitor_mp");
			throw ex;
		}
		else
		{
			ACS_LOG(LM_RUNTIME_CONTEXT, "baci::ROcommonImpl&lt;&gt;::setAlarmFaultMember", (LM_DEBUG, "Failed to set alarm fault member for property: %s to: %s. alarmTimerTrig="ACE_UINT64_FORMAT_SPECIFIER_ASCII, this->property_mp->getName(), fm, alarmTimerTrig_m));
		}//if-else
	}//if-else

}//setAlarmFaultMember



template<ACS_RO_C> 
bool baci::ROcommonImpl<ACS_RO_TL>::readCharacteristics()
{
  cdb::DAONode* dao = this->getDAONode();
  if (!dao)
      return false;
  
  try
      {

      CORBA::Double dbl;
      CORBA::String_var str;

      dbl = dao->get_double("alarm_timer_trig");
      dbl = dbl * static_cast<CORBA::Double>(10000000.0);
      alarmTimerTrig_m = static_cast<CORBA::ULong>(dbl);


      try {
         str = dao->get_string("alarm_fault_family");
      } catch (ACSErr::ACSbaseExImpl& ex) { }

      if (strlen(str)!=0) //if FF is not specified in the CDB
    	  alarmFaultFamily_m = str.in();
      else {
        ACE_CString compType = this->property_mp->getComponent()->getType();
        if( compType.length() == 0 ) {
    	    alarmFaultFamily_m = "BACIProperty"; //default
        }
        else {

          size_t pos;

          // strip out "IDL:" and ":1.0" from the type string, if necessary
          if( compType.find("IDL:") == 0 )
            alarmFaultFamily_m = compType.substr(4, -1);
          if( (pos = alarmFaultFamily_m.find(":1.0")) == (alarmFaultFamily_m.length() - 4) )
            alarmFaultFamily_m = alarmFaultFamily_m.substr(0, pos);

          // keep just the interface name, throw away the pragma and module names
          while( (pos = alarmFaultFamily_m.find('/')) != ACE_CString::npos )
            alarmFaultFamily_m = alarmFaultFamily_m.substr(pos + 1);

          ACE_CString fm(this->property_mp->getName());     // property name is "CompName:PropName
          alarmFaultFamily_m += '#';
          alarmFaultFamily_m += fm.substr(fm.find(':') + 1);
        }
      }

      try {
         str = dao->get_string("alarm_fault_member");
      } catch (ACSErr::ACSbaseExImpl& ex) { }

      if (strlen(str)!=0) //if FF is not specified in the CDB
    	  alarmFaultMember_m = str.in();
      else {
        ACE_CString fm(this->property_mp->getName());     // property name is "CompName:PropName"
    	  alarmFaultMember_m = fm.substr(0, fm.find(':')); //
      }

       alarmLevel_m = dao->get_long("alarm_level");

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

















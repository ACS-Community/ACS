/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration),
*    All rights reserved
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
* "@(#) $Id: baciTestClassImpl.cpp,v 1.116 2008/10/01 02:26:45 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran 2002-05-26 Error system w/ simulated errors added
* bjeram 2002-04-03 change index from 8 to 9 and change the length of desc->properties from 12 to 13
* msekoran 2002-02-11 New priperties added.
* gchiozzi 2002-01-18 Removed maci support functions.
* msekoran 2002-01-03 minor fixes, new priperties
* rlemke   2001-08-05 integrated withe new BACI 
* rlemke   2001-03-20 created from BaciTestClass
* gchiozzi 2001-02-15 Added real implementation for method descriptor()
* gchiozzi 2001-02-15 Added body of get_interface() method for Object Explorer
* gchiozzi 2001-02-15 created standard header 
*/

#include <vltPort.h>

static char *rcsId="@(#) $Id: baciTestClassImpl.cpp,v 1.116 2008/10/01 02:26:45 cparedes Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);


#include <baciDB.h>
#include <logging.h>
#include <baciTestClassImpl.h>

#include <baciTestUtils.h>
#include <baciTestDevIO.h>
#include <baciTestErrorDevIO.h>

#include <ACSErrTypeOK.h>
#include <ACSErrTypeCommon.h>

#define ON_ACTION 0
#define OFF_ACTION 1
#define RESET_ACTION 2

#include <iostream>

using namespace baciErrTypeProperty;
using namespace baci;     

/////////////////////////////////////////////////
// BaciTestClassImpl
/////////////////////////////////////////////////


BaciTestClassImpl::BaciTestClassImpl(const ACE_CString& name,
				     maci::ContainerServices *cs,
				     bool monitoring) :
    CharacteristicComponentImpl(name, cs, monitoring),
    m_shutdown(0),
    m_RWdoubleWithErrorDevIOProp_sp(new RWdouble(name+":RWdoubleWithErrorDevIOProp", getComponent(), new TestErrorDevIO()),this),
    m_RWdoubleWithDevIOProp_sp(new RWdouble(name+":RWdoubleWithDevIOProp", getComponent(), new TestDevIO()),this), 
    m_ROdoubleProp_sp(new ROdouble(name+":ROdoubleProp", getComponent()),this), 
    m_RWdoubleProp_sp(new RWdouble(name+":RWdoubleProp", getComponent()),this), 
    m_ROfloatProp_sp(new ROfloat(name+":ROfloatProp", getComponent()),this), 
    m_RWfloatProp_sp(new RWfloat(name+":RWfloatProp", getComponent()),this), 
    m_ROlongProp_sp(new ROlong(name+":ROlongProp", getComponent()),this), 
    m_RWlongProp_sp(new RWlong(name+":RWlongProp", getComponent()),this), 
    m_ROpatternProp_sp(new ROpattern(name+":ROpatternProp", getComponent()),this), 
    m_RWpatternProp_sp(new RWpattern(name+":RWpatternProp", getComponent()),this),
    m_ROstringProp_sp(new ROstring(name+":ROstringProp", getComponent()),this), 
    m_RWstringProp_sp(new RWstring(name+":RWstringProp", getComponent()),this),
    m_ROdoubleSeqProp_sp(new ROdoubleSeq(name+":ROdoubleSeqProp", getComponent()),this),
    m_RWdoubleSeqProp_sp(new RWdoubleSeq(name+":RWdoubleSeqProp", getComponent()),this), 
    m_ROfloatSeqProp_sp(new ROfloatSeq(name+":ROfloatSeqProp", getComponent()),this),
    m_RWfloatSeqProp_sp(new RWfloatSeq(name+":RWfloatSeqProp", getComponent()),this), 
    m_ROlongSeqProp_sp(new ROlongSeq(name+":ROlongSeqProp", getComponent()),this), 
    m_RWlongSeqProp_sp(new RWlongSeq(name+":RWlongSeqProp", getComponent()),this)
{
  
  ACS_TRACE("::BaciTestClassImpl::BaciTestClassImpl");
  
  // register actions
  m_actions[0] = &BaciTestClassImpl::onAction;
  m_actions[1] = &BaciTestClassImpl::offAction;
  m_actions[2] = &BaciTestClassImpl::resetAction;

 /*
  * Here we are at the top of the hierarchy and we do not have
  * a container to use for the test.
  * Therefore we call by hand __initialize() and __execute()
  * to "activate" the component
  * These are the methods that would be called by the Container
  * when managing the lifecycle of the component.
  */
  __initialize();
  __execute();
}

BaciTestClassImpl::~BaciTestClassImpl()
{
  
  ACS_TRACE("::BaciTestClassImpl::~BaciTestClassImpl");

  if (getComponent())
      ACS_DEBUG_PARAM("::BaciTestClassImpl::~BaciTestClassImpl", "Destroying %s...", getComponent()->getName());

  // stop threads
  if (getComponent())
      getComponent()->stopAllThreads();

  ACS_DEBUG("::BaciTestClassImpl::~BaciTestClassImpl", "Properties destroyed");
  
}

/* --------------- [ Action implementator interface ] -------------- */

ActionRequest 
BaciTestClassImpl::invokeAction(int function,
			  BACIComponent* component_p, const int &callbackID, 
			  const CBDescIn& descIn, BACIValue* value, 
			  Completion& completion, CBDescOut& descOut) 
{

  // better implementation with array is possible
  ActionRequest req;
  CompletionImpl co;

  switch (function) 
    {
      case ON_ACTION:
        req = onAction(component_p, callbackID, descIn, value, co, descOut);
	break;
      case OFF_ACTION:
        req = offAction(component_p, callbackID, descIn, value, co, descOut);
	break;
      case RESET_ACTION:
        req = resetAction(component_p, callbackID, descIn, value, co, descOut);
	break;
      default:
        return reqDestroy;
    }

  if (co.isErrorFree())
      {
      completion = co;
      }
  else
      {
      completion = InvokeActionErrorCompletion(co,
					       __FILE__,
					       __LINE__,
					       "::BaciTestClassImpl::invokeAction");
      }//if-else

    return req;
}

/* ------------------ [ Action implementations ] ----------------- */

/// implementation of async. on() method
ActionRequest 
BaciTestClassImpl::onAction(BACIComponent* component_p, int callbackID,
		      const CBDescIn& descIn, BACIValue* value,
		      Completion& completion, CBDescOut& descOut)
{
  ACS_DEBUG_PARAM("::BaciTestClassImpl::onAction", "%s", getComponent()->getName());

  completion.timeStamp = getTimeStamp();
  
  // simulate errors
  if (descIn.id_tag == 666)
    {
    ACSErrTypeCommon::IOErrorCompletion comp(__FILE__, __LINE__, "::BaciTestClassImpl::onAction");
    comp.addData("Reason", "Simulation error");
    completion = comp;

    return reqInvokeDone;
    }

  DBConnector::writeCommand(getComponent()->getName(), "on", getStringifiedTimeStamp());

  completion = ACSErrTypeOK::ACSErrOKCompletion();

  // complete action requesting done invokation, 
  // otherwise return reqInvokeWorking and set descOut.estimated_timeout
  return reqInvokeDone;
}



/// implementation of async. off() method
ActionRequest 
BaciTestClassImpl::offAction(BACIComponent* component_p, int callbackID,
		       const CBDescIn& descIn, BACIValue* value,
		       Completion& completion, CBDescOut& descOut)
{
  ACS_DEBUG_PARAM("::BaciTestClassImpl::offAction", "%s", getComponent()->getName());
  
  completion.timeStamp = getTimeStamp();
  
  // simulate errors
  if (descIn.id_tag == 666)
    {
    ACSErrTypeCommon::IOErrorCompletion c(__FILE__, __LINE__, "::BaciTestClassImpl::offAction");
    c.addData("Reason", "Simulation error");
    completion = c;

    return reqInvokeDone;
    }

  DBConnector::writeCommand(getComponent()->getName(), "off", getStringifiedTimeStamp());

  completion = ACSErrTypeOK::ACSErrOKCompletion();

  // complete action requesting done invokation, 
  // otherwise return reqInvokeWorking and set descOut.estimated_timeout
  return reqInvokeDone;
}

/// implementation of async. reset() method
ActionRequest 
BaciTestClassImpl::resetAction(BACIComponent* component_p, int callbackID,
			 const CBDescIn& descIn, BACIValue* value,
			 Completion& completion, CBDescOut& descOut)
{
  ACS_DEBUG_PARAM("::BaciTestClassImpl::resetAction", "%s", getComponent()->getName());
  
  completion.timeStamp = getTimeStamp();
  
  // simulate errors
  if (descIn.id_tag == 666)
    {
    ACSErrTypeCommon::IOErrorCompletion comp(__FILE__, __LINE__, "::BaciTestClassImpl::resetAction");
    comp.addData("Reason", "Simulation error");
    completion = comp;
      return reqInvokeDone;
    }

  DBConnector::writeCommand(getComponent()->getName(), "reset", getStringifiedTimeStamp());

  completion = ACSErrTypeOK::ACSErrOKCompletion();


  // complete action requesting done invokation, 
  // otherwise return reqInvokeWorking and set descOut.estimated_timeout
  return reqInvokeDone;
}

/* ----------------------------------------------------------------*/
/* --------------------- [ CORBA interface ] ----------------------*/
/* ----------------------------------------------------------------*/
void
BaciTestClassImpl::shutdown (
			 
			 )
{
  
  ACS_TRACE("::BaciTestClassImpl::shutdown");
  
  if (m_shutdown) 
    return;
  else
    m_shutdown=true;

  CORBAShutdown::shutdown();
}

void
BaciTestClassImpl::on (ACS::CBvoid_ptr cb,
		 const ACS::CBDescIn & desc
		 )
{
  ACS_DEBUG("::BaciTestClassImpl::on", "Registering ON Action");
  getComponent()->registerAction(BACIValue::type_null, cb, 
			   desc, this, ON_ACTION);
}

void
BaciTestClassImpl::off (ACS::CBvoid_ptr cb,
		 const ACS::CBDescIn & desc
		 )
{
  ACS_DEBUG("::BaciTestClassImpl::off", "Registering OFF Action");
  getComponent()->registerAction(BACIValue::type_null, cb, 
			   desc, this, OFF_ACTION);
}

void
BaciTestClassImpl::reset (ACS::CBvoid_ptr cb,
		 const ACS::CBDescIn & desc
		 )
{
  ACS_DEBUG("::BaciTestClassImpl::reset", "Registering RESET Action");
  getComponent()->registerAction(BACIValue::type_null, cb, 
			   desc, this, RESET_ACTION);
}

CORBA::Boolean
BaciTestClassImpl::isPropertiesMonitoringActive()
{
    CORBA::Boolean retVal = CharacteristicComponentImpl::isPropertiesMonitoringActive();
    ACS_SHORT_LOG((LM_INFO,"Retrieved monitoring active status: %d", retVal));
 
    return retVal;  
} //isPropertiesMonitoringActive

ACS::RWdouble_ptr
BaciTestClassImpl::RWdoubleWithErrorDevIOProp ()
{
  if (m_RWdoubleWithErrorDevIOProp_sp==0)
	  return ACS::RWdouble::_nil();

  ACS::RWdouble_var prop = ACS::RWdouble::_narrow(m_RWdoubleWithErrorDevIOProp_sp->getCORBAReference()
						  );
  return prop._retn();
}

ACS::RWdouble_ptr
BaciTestClassImpl::RWdoubleWithDevIOProp ()
{
  if (m_RWdoubleWithDevIOProp_sp==0)
	  return ACS::RWdouble::_nil();

  ACS::RWdouble_var prop = ACS::RWdouble::_narrow(m_RWdoubleWithDevIOProp_sp->getCORBAReference()
						  );
  return prop._retn();
}

ACS::RWdouble_ptr
BaciTestClassImpl::RWdoubleProp ()
{
  if (m_RWdoubleProp_sp==0)
	  return ACS::RWdouble::_nil();

  ACS::RWdouble_var prop = ACS::RWdouble::_narrow(m_RWdoubleProp_sp->getCORBAReference()
						  );
  return prop._retn();
}

ACS::ROdouble_ptr
BaciTestClassImpl::ROdoubleProp ()
{
  if (m_ROdoubleProp_sp==0)
	  return ACS::ROdouble::_nil();

  ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_ROdoubleProp_sp->getCORBAReference()
						  );
  return prop._retn();
}

ACS::RWfloat_ptr
BaciTestClassImpl::RWfloatProp ()
{
  if (m_RWfloatProp_sp==0)
	  return ACS::RWfloat::_nil();

  ACS::RWfloat_var prop = ACS::RWfloat::_narrow(m_RWfloatProp_sp->getCORBAReference()
						  );
  return prop._retn();
}

ACS::ROfloat_ptr
BaciTestClassImpl::ROfloatProp ()
{
  if (m_ROfloatProp_sp==0)
	  return ACS::ROfloat::_nil();

  ACS::ROfloat_var prop = ACS::ROfloat::_narrow(m_ROfloatProp_sp->getCORBAReference()
						  );
  return prop._retn();
}


ACS::RWlong_ptr
BaciTestClassImpl::RWlongProp ()
{
  if (m_RWlongProp_sp==0)
	  return ACS::RWlong::_nil();

  ACS::RWlong_var prop = ACS::RWlong::_narrow(m_RWlongProp_sp->getCORBAReference()
						  );
  return prop._retn();
}

ACS::ROlong_ptr
BaciTestClassImpl::ROlongProp ()
{
  if (m_ROlongProp_sp==0)
	  return ACS::ROlong::_nil();

  ACS::ROlong_var prop = ACS::ROlong::_narrow(m_ROlongProp_sp->getCORBAReference()
						  );
  return prop._retn();
}


ACS::ROpattern_ptr
BaciTestClassImpl::ROpatternProp ()
{
  if (m_ROpatternProp_sp==0)
	  return ACS::ROpattern::_nil();

  ACS::ROpattern_var prop = ACS::ROpattern::_narrow(m_ROpatternProp_sp->getCORBAReference()
						    );
  return prop._retn();
}

ACS::RWpattern_ptr
BaciTestClassImpl::RWpatternProp ()
{
  if (m_RWpatternProp_sp==0)
	  return ACS::RWpattern::_nil();

  ACS::RWpattern_var prop = ACS::RWpattern::_narrow(m_RWpatternProp_sp->getCORBAReference()
						    );
  return prop._retn();
}

ACS::ROstring_ptr
BaciTestClassImpl::ROstringProp()
{
  if (m_ROstringProp_sp==0)
	  return ACS::ROstring::_nil();

  ACS::ROstring_var prop = ACS::ROstring::_narrow(m_ROstringProp_sp->getCORBAReference()
						    );
  return prop._retn();
}

ACS::RWstring_ptr
BaciTestClassImpl::RWstringProp()
{
  if (m_RWstringProp_sp==0)
	  return ACS::RWstring::_nil();

  ACS::RWstring_var prop = ACS::RWstring::_narrow(m_RWstringProp_sp->getCORBAReference()
						    );
  return prop._retn();
}


ACS::RWdoubleSeq_ptr
BaciTestClassImpl::RWdoubleSeqProp ()
{
  if (m_RWdoubleSeqProp_sp==0)
	  return ACS::RWdoubleSeq::_nil();

  ACS::RWdoubleSeq_var prop = ACS::RWdoubleSeq::_narrow(m_RWdoubleSeqProp_sp->getCORBAReference()
							);
  return prop._retn();
}

ACS::ROdoubleSeq_ptr
BaciTestClassImpl::ROdoubleSeqProp ()
{
  if (m_ROdoubleSeqProp_sp==0)
	  return ACS::ROdoubleSeq::_nil();

  ACS::ROdoubleSeq_var prop = ACS::ROdoubleSeq::_narrow(m_ROdoubleSeqProp_sp->getCORBAReference()
							);
  return prop._retn();
}

ACS::RWfloatSeq_ptr
BaciTestClassImpl::RWfloatSeqProp ()
{
  if (m_RWfloatSeqProp_sp==0)
	  return ACS::RWfloatSeq::_nil();

  ACS::RWfloatSeq_var prop = ACS::RWfloatSeq::_narrow(m_RWfloatSeqProp_sp->getCORBAReference()
							);
  return prop._retn();
}

ACS::ROfloatSeq_ptr
BaciTestClassImpl::ROfloatSeqProp ()
{
  if (m_ROfloatSeqProp_sp==0)
	  return ACS::ROfloatSeq::_nil();

  ACS::ROfloatSeq_var prop = ACS::ROfloatSeq::_narrow(m_ROfloatSeqProp_sp->getCORBAReference()
							);
  return prop._retn();
}


ACS::RWlongSeq_ptr
BaciTestClassImpl::RWlongSeqProp ()
{
  if (m_RWlongSeqProp_sp==0)
	  return ACS::RWlongSeq::_nil();

  ACS::RWlongSeq_var prop = ACS::RWlongSeq::_narrow(m_RWlongSeqProp_sp->getCORBAReference()
						    );
  return prop._retn();
}

ACS::ROlongSeq_ptr
BaciTestClassImpl::ROlongSeqProp ()
{
  if (m_ROlongSeqProp_sp==0)
	  return ACS::ROlongSeq::_nil();

  ACS::ROlongSeq_var prop = ACS::ROlongSeq::_narrow(m_ROlongSeqProp_sp->getCORBAReference()
						    );
  return prop._retn();

}












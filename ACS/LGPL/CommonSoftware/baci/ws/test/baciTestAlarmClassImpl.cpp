////#include <baciTestAlarmClassImpl.h>
#include <vltPort.h>

static char *rcsId="@(#) $Id: baciTestAlarmClassImpl.cpp,v 1.8 2009/09/25 13:58:59 bjeram Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <baciROpattern.h>
#include <baciDB.h>
#include <logging.h>
#include <baciTestAlarmClassImpl.h>

#include <baciTestUtils.h>
#include <baciTestDevIO.h>
#include <baciTestErrorDevIO.h>

#include <ACSErrTypeOK.h>
#include <ACSErrTypeCommon.h>

using namespace baci;

/////////////////////////////////////////////////
// MyROPatternProperty
/////////////////////////////////////////////////

/// just setting the member property to point roPatternProperty_p
/// also using the constructor inherited from RWdouble
MyROPatternProperty::MyROPatternProperty(const ACE_CString &name, 
					 BACIComponent *cob_p,
					 ROpattern *roPatternProperty_p) :
    baci::RWpattern(name, cob_p),
    m_roPatternProperty_p(roPatternProperty_p)
{
    ACS_TRACE("MyROPatternProperty::MyROPatternProperty");   
}


/// async. set value action implementation
void 
MyROPatternProperty::setValue(BACIProperty *property_p,
			      BACIValue *value_p, 
			      Completion &completion,
			      CBDescOut &descOut)
{
    ACS::Time timestamp;
    
    /* Calls parent class method */
    baci::RWpattern::setValue(property_p, value_p, completion, descOut);
    
    /* Calls getDevIO()->writePattern() for associated roPatternProperty property */
    ACS::pattern value = value_p->patternValue();
    m_roPatternProperty_p->getDevIO()->write(value, timestamp);
}







BaciTestAlarmClassImpl::BaciTestAlarmClassImpl(const ACE_CString& name,
					       maci::ContainerServices* containerServices,
					       bool monitoring) :
    CharacteristicComponentImpl(name,containerServices, monitoring),
    m_shutdown(0),
    m_roPatternProperty_sp(new ROpattern(name+":roPatternProperty", getComponent()),this),
    m_rwPatternProperty_sp(this)
{
    ACS_TRACE("BaciTestAlarmClassImpl::BaciTestAlarmClassImpl");
    m_rwPatternProperty_sp = new MyROPatternProperty(name+":rwPatternProperty", getComponent(), m_roPatternProperty_sp);
    //m_roPatternProperty_sp->setAlarmFaultFamily("UserDefinedFF");
    //m_roPatternProperty_sp->setAlarmFaultMember("UserDefinedFM");

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


BaciTestAlarmClassImpl::~BaciTestAlarmClassImpl()
{
    ACS_TRACE("BaciTestAlarmClassImpl::~BaciTestAlarmClassImpl");
}


void
BaciTestAlarmClassImpl::shutdown()
{
    ACS_TRACE("BaciTestAlarmClassImpl::shutdown");
  
    if (m_shutdown) 
	return;
    else
	m_shutdown=true;

    CORBAShutdown::shutdown();
}

void BaciTestAlarmClassImpl::changeAlarmFFFM(const char* ff, const char *fm)
{
	m_roPatternProperty_sp->setAlarmFaultFamily(ff);
	m_roPatternProperty_sp->setAlarmFaultMember(fm);
}//changeAlarmFFFM

CORBA::Boolean
BaciTestAlarmClassImpl::isPropertiesMonitoringActive() 
{
    CORBA::Boolean retVal = CharacteristicComponentImpl::isPropertiesMonitoringActive();
    ACS_SHORT_LOG((LM_INFO,"Retrieved monitoring active status: %d", retVal));
 
    return retVal;  
} //isPropertiesMonitoringActive


ACS::ROpattern_ptr
BaciTestAlarmClassImpl::roPatternProperty()
{
    if (m_roPatternProperty_sp == 0)
	{
	return ACS::ROpattern::_nil();
	}
    
    ACS::ROpattern_var prop = ACS::ROpattern::_narrow(m_roPatternProperty_sp->getCORBAReference());
    return prop._retn();
}


ACS::RWpattern_ptr
BaciTestAlarmClassImpl::rwPatternProperty()
{
    if (m_rwPatternProperty_sp == 0)
	{
	return ACS::RWpattern::_nil();
	}
    
    ACS::RWpattern_var prop = ACS::RWpattern::_narrow(m_rwPatternProperty_sp->getCORBAReference());
    return prop._retn();
}


/* --------------- [ MACI DLL support functions ] -----------------*/
//////////////#include <maciACSComponentDefines.h>
////////////////MACI_DLL_SUPPORT_FUNCTIONS(BaciTestAlarmClassImpl)
/* ----------------------------------------------------------------*/

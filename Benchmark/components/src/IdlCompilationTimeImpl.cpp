#include "IdlCompilationTimeImpl.h"

ACE_RCSID(acstests, SimpleBACIComponentImpl, "$Id: IdlCompilationTimeImpl.cpp,v 1.2 2005/04/29 21:45:13 dfugate Exp $");

SimpleBACIComponent::SimpleBACIComponent(const ACE_CString& name,
    maci::ContainerServices *containerServices) :
    CharacteristicComponentImpl(name, containerServices),
    m_property(0)
{
    ACS_TRACE("::SimpleBACIComponent::SimpleBACIComponent");
    m_property = new RWlong(name+":property", getComponent());
    CHARACTERISTIC_COMPONENT_PROPERTY(property, m_property);
}

SimpleBACIComponent::~SimpleBACIComponent()
{
    ACS_TRACE("::SimpleBACIComponent::~SimpleBACIComponent");

    if (getComponent() != 0)
    {
	ACS_DEBUG_PARAM("::SimpleBACIComponent::~SimpleBACIComponent", "Destroying %s...", getComponent()->getName());
	getComponent()->stopAllThreads();
    }
    
    if (m_property != 0) { m_property->destroy(); m_property = 0; }
    
    ACS_DEBUG("::SimpleBACIComponent::~SimpleBACIComponent", "Properties destroyed");
}



ActionRequest SimpleBACIComponent::invokeAction(int function,
						BACIComponent *cob_p, 
						const int &callbackID, 
						const CBDescIn &descIn, 
						BACIValue *value_p, 
						Completion &completion, 
						CBDescOut &descOut) 
{
    switch(function) 
    {
	case ACTION:
	{
	    ACS_DEBUG_PARAM("::SimpleBACIComponent::Action", "%s", getComponent()->getName());
    
	    completion.timeStamp = getTimeStamp();
	    completion.type=ACSErr::ACSErrTypeOK;
	    completion.code = ACSErrTypeOK::ACSErrOK;

	    return reqInvokeDone;
	}
	default:
	    return reqDestroy;
    }
}

void SimpleBACIComponent::method() throw (CORBA::SystemException)
{
    
}

void SimpleBACIComponent::action(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc) throw (CORBA::SystemException)
{
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, ACTION);
}

ACS::RWlong_ptr SimpleBACIComponent::property() throw (CORBA::SystemException)
{
    if (m_property == 0) return ACS::RWlong::_nil();
    
    ACS::RWlong_var prop = ACS::RWlong::_narrow(m_property->getCORBAReference());
    return prop._retn();
}

#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(SimpleBACIComponent)

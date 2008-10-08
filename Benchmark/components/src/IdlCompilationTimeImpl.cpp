#include "IdlCompilationTimeImpl.h"

ACE_RCSID(acstests, IdlCompilationTimeComponentImpl, "$Id: IdlCompilationTimeImpl.cpp,v 1.4 2008/10/08 01:57:23 cparedes Exp $");

IdlCompilationTimeComponent::IdlCompilationTimeComponent(const ACE_CString& name,
    maci::ContainerServices *containerServices) :
    CharacteristicComponentImpl(name, containerServices),
    m_property(0)
{
    ACS_TRACE("::IdlCompilationTimeComponent::IdlCompilationTimeComponent");
    m_property = new RWlong(name+":property", getComponent());
    CHARACTERISTIC_COMPONENT_PROPERTY(property, m_property);
}

IdlCompilationTimeComponent::~IdlCompilationTimeComponent()
{
    ACS_TRACE("::IdlCompilationTimeComponent::~IdlCompilationTimeComponent");

    if (getComponent() != 0)
    {
	ACS_DEBUG_PARAM("::IdlCompilationTimeComponent::~IdlCompilationTimeComponent", "Destroying %s...", getComponent()->getName());
	getComponent()->stopAllThreads();
    }
    
    if (m_property != 0) { m_property->destroy(); m_property = 0; }
    
    ACS_DEBUG("::IdlCompilationTimeComponent::~IdlCompilationTimeComponent", "Properties destroyed");
}



ActionRequest IdlCompilationTimeComponent::invokeAction(int function,
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
	    ACS_DEBUG_PARAM("::IdlCompilationTimeComponent::Action", "%s", getComponent()->getName());
    
	    completion.timeStamp = getTimeStamp();
	    completion.type=ACSErr::ACSErrTypeOK;
	    completion.code = ACSErrTypeOK::ACSErrOK;

	    return reqInvokeDone;
	}
	default:
	    return reqDestroy;
    }
}

void IdlCompilationTimeComponent::method()
{
    
}

void IdlCompilationTimeComponent::action(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc)
{
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, ACTION);
}

ACS::RWlong_ptr IdlCompilationTimeComponent::property()
{
    if (m_property == 0) return ACS::RWlong::_nil();
    
    ACS::RWlong_var prop = ACS::RWlong::_narrow(m_property->getCORBAReference());
    return prop._retn();
}

#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(IdlCompilationTimeComponent)

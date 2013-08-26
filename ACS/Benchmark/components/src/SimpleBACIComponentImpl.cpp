#include <SimpleBACIComponentImpl.h>

ACE_RCSID(acstests, SimpleBACIComponentImpl, "$Id: SimpleBACIComponentImpl.cpp,v 1.6 2008/10/08 01:57:23 cparedes Exp $")

/////////////////////////////////////////////////
// SimpleBACIComponent
/////////////////////////////////////////////////

SimpleBACIComponent::SimpleBACIComponent(const ACE_CString& name,
    maci::ContainerServices *containerServices) :
    BasePerfCompImpl(name, containerServices),
    m_property_sp(new baci::RWlong(name+":property", getComponent()), this)
{
    ACS_TRACE("::SimpleBACIComponent::SimpleBACIComponent");
}

SimpleBACIComponent::~SimpleBACIComponent()
{
    ACS_TRACE("::SimpleBACIComponent::~SimpleBACIComponent");
}

/* --------------- [ Action implementator interface ] -------------- */

baci::ActionRequest 
SimpleBACIComponent::invokeAction(int function,
				  baci::BACIComponent *cob_p, 
				  const int &callbackID, 
				  const CBDescIn &descIn, 
				  baci::BACIValue *value_p, 
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

	    return baci::reqInvokeDone;
	}
	default:
	    return baci::reqDestroy;
	}
}

/* --------------------- [ CORBA interface ] ----------------------*/
void SimpleBACIComponent::action(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc)
{
    getComponent()->registerAction(baci::BACIValue::type_null, cb, desc, this, ACTION);
}

ACS::RWlong_ptr SimpleBACIComponent::property()
{
    if (m_property_sp == 0) return ACS::RWlong::_nil();
    
    ACS::RWlong_var prop = ACS::RWlong::_narrow(m_property_sp->getCORBAReference());
    return prop._retn();
}

/* --------------- [ MACI DLL support functions ] -----------------*/

#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(SimpleBACIComponent)

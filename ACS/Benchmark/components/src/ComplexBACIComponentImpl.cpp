#include <ComplexBACIComponentImpl.h>

using namespace baci;

ACE_RCSID(acstests, ComplexBACIComponentImpl, "$Id: ComplexBACIComponentImpl.cpp,v 1.6 2008/10/08 01:57:23 cparedes Exp $")

/////////////////////////////////////////////////
// ComplexBACIComponent
/////////////////////////////////////////////////

ComplexBACIComponent::ComplexBACIComponent(const ACE_CString& name,
    maci::ContainerServices *containerServices) :
    BasePerfCompImpl(name, containerServices),
    m_property01(new RWlong(name+":property01", getComponent()), this),
    m_property02(new RWlong(name+":property02", getComponent()), this),
    m_property03(new RWlong(name+":property03", getComponent()), this),
    m_property04(new RWlong(name+":property04", getComponent()), this),
    m_property05(new RWlong(name+":property05", getComponent()), this),
    m_property06(new RWlong(name+":property06", getComponent()), this),
    m_property07(new RWlong(name+":property07", getComponent()), this),
    m_property08(new RWlong(name+":property08", getComponent()), this),
    m_property09(new RWlong(name+":property09", getComponent()), this),
    m_property10(new RWlong(name+":property10", getComponent()), this),
    m_property11(new RWlong(name+":property11", getComponent()), this),
    m_property12(new RWlong(name+":property12", getComponent()), this),
    m_property13(new RWlong(name+":property13", getComponent()), this),
    m_property14(new RWlong(name+":property14", getComponent()), this),
    m_property15(new RWlong(name+":property15", getComponent()), this),
    m_property16(new RWlong(name+":property16", getComponent()), this)
{
    ACS_TRACE("::ComplexBACIComponent::ComplexBACIComponent");
}

ComplexBACIComponent::~ComplexBACIComponent()
{
    ACS_TRACE("::ComplexBACIComponent::~ComplexBACIComponent");
}

/* --------------- [ Action implementator interface ] -------------- */

ActionRequest ComplexBACIComponent::invokeAction(int function,
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
	    ACS_DEBUG_PARAM("::ComplexBACIComponent::ActionXX", "%s", getComponent()->getName());
    
	    // simulate something in hardware...
	    //ACE_OS::sleep(5);
	    // since this method is only simulated, we just change it's state  
	    completion.timeStamp = getTimeStamp();
	    completion.type=ACSErr::ACSErrTypeOK;
	    completion.code = ACSErrTypeOK::ACSErrOK;

	    // complete action requesting done invakation,
	    // otherwise return reqInvokeWorking and set descOut.estimated_timeout
	    return reqInvokeDone;
	}
	default:
	    return reqDestroy;
    }
}

/* --------------------- [ CORBA interface ] ----------------------*/
 
void ComplexBACIComponent::action01(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc)
{
    //registers the action to the BACI and returns control immediately
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, ACTION);
}

void ComplexBACIComponent::action02(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc)
{
    //registers the action to the BACI and returns control immediately
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, ACTION);
}

void ComplexBACIComponent::action03(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc)
{
    //registers the action to the BACI and returns control immediately
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, ACTION);
}

void ComplexBACIComponent::action04(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc)
{
    //registers the action to the BACI and returns control immediately
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, ACTION);
}

void ComplexBACIComponent::action05(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc)
{
    //registers the action to the BACI and returns control immediately
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, ACTION);
}

void ComplexBACIComponent::action06(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc)
{
    //registers the action to the BACI and returns control immediately
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, ACTION);
}

void ComplexBACIComponent::action07(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc)
{
    //registers the action to the BACI and returns control immediately
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, ACTION);
}

void ComplexBACIComponent::action08(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc)
{
    //registers the action to the BACI and returns control immediately
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, ACTION);
}

void ComplexBACIComponent::action09(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc)
{
    //registers the action to the BACI and returns control immediately
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, ACTION);
}

void ComplexBACIComponent::action10(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc)
{
    //registers the action to the BACI and returns control immediately
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, ACTION);
}

void ComplexBACIComponent::action11(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc)
{
    //registers the action to the BACI and returns control immediately
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, ACTION);
}

void ComplexBACIComponent::action12(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc)
{
    //registers the action to the BACI and returns control immediately
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, ACTION);
}

void ComplexBACIComponent::action13(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc)
{
    //registers the action to the BACI and returns control immediately
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, ACTION);
}

void ComplexBACIComponent::action14(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc)
{
    //registers the action to the BACI and returns control immediately
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, ACTION);
}

void ComplexBACIComponent::action15(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc)
{
    //registers the action to the BACI and returns control immediately
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, ACTION);
}

void ComplexBACIComponent::action16(ACS::CBvoid_ptr cb, const ACS::CBDescIn &desc)
{
    //registers the action to the BACI and returns control immediately
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, ACTION);
}

ACS::RWlong_ptr ComplexBACIComponent::property01()
{
    if (m_property01 == 0) return ACS::RWlong::_nil();
    
    ACS::RWlong_var prop = ACS::RWlong::_narrow(m_property01->getCORBAReference());
    return prop._retn();
}

ACS::RWlong_ptr ComplexBACIComponent::property02()
{
    if (m_property02 == 0) return ACS::RWlong::_nil();
    
    ACS::RWlong_var prop = ACS::RWlong::_narrow(m_property02->getCORBAReference());
    return prop._retn();
}

ACS::RWlong_ptr ComplexBACIComponent::property03()
{
    if (m_property03 == 0) return ACS::RWlong::_nil();
    
    ACS::RWlong_var prop = ACS::RWlong::_narrow(m_property03->getCORBAReference());
    return prop._retn();
}

ACS::RWlong_ptr ComplexBACIComponent::property04()
{
    if (m_property04 == 0) return ACS::RWlong::_nil();
    
    ACS::RWlong_var prop = ACS::RWlong::_narrow(m_property04->getCORBAReference());
    return prop._retn();
}

ACS::RWlong_ptr ComplexBACIComponent::property05()
{
    if (m_property05 == 0) return ACS::RWlong::_nil();
    
    ACS::RWlong_var prop = ACS::RWlong::_narrow(m_property05->getCORBAReference());
    return prop._retn();
}

ACS::RWlong_ptr ComplexBACIComponent::property06()
{
    if (m_property06 == 0) return ACS::RWlong::_nil();
    
    ACS::RWlong_var prop = ACS::RWlong::_narrow(m_property06->getCORBAReference());
    return prop._retn();
}

ACS::RWlong_ptr ComplexBACIComponent::property07()
{
    if (m_property07 == 0) return ACS::RWlong::_nil();
    
    ACS::RWlong_var prop = ACS::RWlong::_narrow(m_property07->getCORBAReference());
    return prop._retn();
}

ACS::RWlong_ptr ComplexBACIComponent::property08()
{
    if (m_property08 == 0) return ACS::RWlong::_nil();
    
    ACS::RWlong_var prop = ACS::RWlong::_narrow(m_property08->getCORBAReference());
    return prop._retn();
}

ACS::RWlong_ptr ComplexBACIComponent::property09()
{
    if (m_property09 == 0) return ACS::RWlong::_nil();
    
    ACS::RWlong_var prop = ACS::RWlong::_narrow(m_property09->getCORBAReference());
    return prop._retn();
}

ACS::RWlong_ptr ComplexBACIComponent::property10()
{
    if (m_property10 == 0) return ACS::RWlong::_nil();
    
    ACS::RWlong_var prop = ACS::RWlong::_narrow(m_property10->getCORBAReference());
    return prop._retn();
}

ACS::RWlong_ptr ComplexBACIComponent::property11()
{
    if (m_property11 == 0) return ACS::RWlong::_nil();
    
    ACS::RWlong_var prop = ACS::RWlong::_narrow(m_property11->getCORBAReference());
    return prop._retn();
}

ACS::RWlong_ptr ComplexBACIComponent::property12()
{
    if (m_property12 == 0) return ACS::RWlong::_nil();
    
    ACS::RWlong_var prop = ACS::RWlong::_narrow(m_property12->getCORBAReference());
    return prop._retn();
}

ACS::RWlong_ptr ComplexBACIComponent::property13()
{
    if (m_property13 == 0) return ACS::RWlong::_nil();
    
    ACS::RWlong_var prop = ACS::RWlong::_narrow(m_property13->getCORBAReference());
    return prop._retn();
}

ACS::RWlong_ptr ComplexBACIComponent::property14()
{
    if (m_property14 == 0) return ACS::RWlong::_nil();
    
    ACS::RWlong_var prop = ACS::RWlong::_narrow(m_property14->getCORBAReference());
    return prop._retn();
}

ACS::RWlong_ptr ComplexBACIComponent::property15()
{
    if (m_property15 == 0) return ACS::RWlong::_nil();
    
    ACS::RWlong_var prop = ACS::RWlong::_narrow(m_property15->getCORBAReference());
    return prop._retn();
}

ACS::RWlong_ptr ComplexBACIComponent::property16()
{
    if (m_property16 == 0) return ACS::RWlong::_nil();
    
    ACS::RWlong_var prop = ACS::RWlong::_narrow(m_property16->getCORBAReference());
    return prop._retn();
}

/* --------------- [ MACI DLL support functions ] -----------------*/

#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(ComplexBACIComponent)

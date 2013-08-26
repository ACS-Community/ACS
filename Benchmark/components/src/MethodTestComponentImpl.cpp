#include <MethodTestComponentImpl.h>

ACE_RCSID(acstests, MethodTestComponentImpl, "$Id: MethodTestComponentImpl.cpp,v 1.6 2008/10/08 01:57:23 cparedes Exp $")

/////////////////////////////////////////////////
// MethodTestComponent
/////////////////////////////////////////////////
    MethodTestComponent::MethodTestComponent(const ACE_CString& name,
					     maci::ContainerServices *containerServices) :
    BasePerfCompImpl(name, containerServices),
    m_charSeq(0)
{
    ACS_TRACE("::MethodTestComponent::MethodTestComponent");
    
    m_charSeq = new perftest::charSeq(0);
    m_charSeq->length(0);
}

perftest::charSeq *
MethodTestComponent::testReturnSize()
{
    perftest::charSeq_var tCharSeq = m_charSeq;

    //Not the best way in the world to return this but it should work.
    return tCharSeq._retn();
}

void 
MethodTestComponent::setup(CORBA::ULong count, CORBA::ULong size, ACS::TimeInterval waitTime) 
{
    BasePerfCompImpl::setup(count, size, waitTime);
    
    m_charSeq = new perftest::charSeq(m_size);
    m_charSeq->length(m_size);
    
    for (CORBA::ULong i=0; i<m_charSeq->length(); i++)
	{
	m_charSeq[i] = '*';
	}
}

/* --------------- [ MACI DLL support functions ] -----------------*/

#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(MethodTestComponent)

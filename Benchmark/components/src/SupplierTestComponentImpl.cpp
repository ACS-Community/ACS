#include <SupplierTestComponentImpl.h>
#include <string.h>

ACE_RCSID(acstests, SupplierTestComponentImpl, "$Id: SupplierTestComponentImpl.cpp,v 1.6 2008/10/08 01:57:23 cparedes Exp $")

SupplierTestComponentImpl::SupplierTestComponentImpl(const ACE_CString& name,
    maci::ContainerServices *containerServices) :
    BasePerfCompImpl(name, containerServices),
    m_perfSupplier_p(0)
{
    ACS_TRACE("::SupplierTestComponentImpl::SupplierTestComponentImpl");
    m_perfSupplier_p = new nc::SimpleSupplier("perf channel", this);
}


void 
SupplierTestComponentImpl::cleanUp()
{
    BasePerfCompImpl::cleanUp();

    if (m_perfSupplier_p != 0)
	{
	m_perfSupplier_p->disconnect();
	m_perfSupplier_p=0;
	}
}

void 
SupplierTestComponentImpl::method() 
{
    perftest::charSeqStruct joe;
    joe.aSequence.length(m_size);

    for (unsigned long i = 0; i< m_size; i++)
	{
	joe.aSequence[i] = 'a';
	}

    //reset the timer
    m_profiler->reset();

    
    for (unsigned long i = 0; i < m_count; i++)
	{
	m_profiler->start();
	m_perfSupplier_p->publishData<perftest::charSeqStruct>(joe);
	m_profiler->stop();
	waitAwhile();
	}

    std::ostringstream ostr;
    ostr << "Event Channel Event of Size '" << m_size << "' Bytes from within a CharacteristicComponent" << std::ends;
    m_profiler->fullDescription(const_cast<char *>(ostr.str().c_str()));
}

#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(SupplierTestComponentImpl)

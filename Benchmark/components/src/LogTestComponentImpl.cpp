#include <LogTestComponentImpl.h>
#include <string.h>
#include <iomanip>

ACE_RCSID(acstests, LogTestComponentImpl, "$Id: LogTestComponentImpl.cpp,v 1.7 2008/10/08 01:57:23 cparedes Exp $")

LogTestComponentImpl::LogTestComponentImpl(const ACE_CString& name,
					   maci::ContainerServices *containerServices) :
    BasePerfCompImpl(name, containerServices)
{
    ACS_TRACE("::LogTestComponentImpl::LogTestComponentImpl");
}

void LogTestComponentImpl::method()
{
    //setup the data
    char *str = new char[m_size+1];
    memset(str, '*', m_size);
    str[m_size] = '\0';
    
    //reset the timer
    m_profiler->reset();

    
    for (unsigned long i = 0; i < m_count; i++)
	{
	m_profiler->start();
	ACS_LOG(LM_SOURCE_INFO, "LogTestComponentImpl::testLogging", (LM_INFO, str));
	m_profiler->stop();
	waitAwhile();
	}
    delete []str;

    std::ostringstream ostr;

    ostr << "ACS Log of Size '" << m_size << "' Bytes from within a CharacteristicComponent" << std::ends;

    const char *acsSTDIO = getenv("ACS_LOG_STDOUT");
    if (acsSTDIO && *acsSTDIO)
	{
	m_profiler->addData("ACS_LOG_STDOUT", acsSTDIO);
	}
    else
	{
	m_profiler->addData("ACS_LOG_STDOUT", "None");
	}

    m_profiler->fullDescription(const_cast<char *>(ostr.str().c_str()));
}

#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(LogTestComponentImpl)

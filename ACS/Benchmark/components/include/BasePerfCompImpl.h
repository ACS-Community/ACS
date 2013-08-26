#ifndef BasePerfCompImpl_h
#define BasePerfCompImpl_h

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciCharacteristicComponentImpl.h>
#include "perftestS.h"

#include <acstimeProfiler.h>

class BasePerfCompImpl: public baci::CharacteristicComponentImpl,
			public virtual POA_perftest::BasePerfComp
{
  public:

    BasePerfCompImpl(const ACE_CString& name,
		     maci::ContainerServices *containerServices) : 
	CharacteristicComponentImpl(name, containerServices),
	m_profiler(0)
	{
	    m_profiler = new Profiler();
	}

    virtual ~BasePerfCompImpl() {}

    virtual void cleanUp()
	{
	    CharacteristicComponentImpl::cleanUp();
	    delete m_profiler;
	}

    /* --------------------- [ CORBA interface ] ----------------------*/
    virtual void method()
	{
	}

    virtual void setup(CORBA::ULong count, CORBA::ULong size, ACS::TimeInterval waitTime)
	{
	    m_count = count;
	    m_size = size;
	    if(waitTime < 10)
		{
		m_waitTime.set(0, 0);
		}
	    else
		{
		m_waitTime.set(0, waitTime/10);
		}
	}
    
  protected:

    unsigned long m_count;
    unsigned long m_size;
    ACE_Time_Value m_waitTime;

    Profiler *m_profiler;

    virtual void waitAwhile()
	{
	    if(m_waitTime.usec()!=0L)
		{
		ACE_OS::sleep(m_waitTime);
		}
	}
    
  private:

    void operator=(const BasePerfCompImpl&);
};

#endif

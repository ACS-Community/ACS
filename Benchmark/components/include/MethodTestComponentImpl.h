#ifndef MethodTestComponentImpl_h
#define MethodTestComponentImpl_h

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "BasePerfCompImpl.h"
#include "perftestS.h"

class MethodTestComponent: public virtual BasePerfCompImpl,
			   public virtual POA_perftest::MethodTestComponent
{
  public:
    
    MethodTestComponent(const ACE_CString& name,
    maci::ContainerServices *containerServices);
    virtual ~MethodTestComponent(){};
    
    virtual perftest::charSeq * 
    testReturnSize();

    virtual void 
    testInParam(const perftest::charSeq &chars)
	{
	    return;
	}

    virtual void 
    setup(CORBA::ULong count, CORBA::ULong size, ACS::TimeInterval waitTime);

  private:
    perftest::charSeq_var	m_charSeq;

    void operator=(const MethodTestComponent&);
};

#endif   /* MethodTestComponentImpl_h */

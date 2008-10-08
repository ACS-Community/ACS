#ifndef LogTestComponentImpl_h
#define LogTestComponentImpl_h

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "BasePerfCompImpl.h"
#include "perftestS.h"

class LogTestComponentImpl: public virtual BasePerfCompImpl
{
  public:
    LogTestComponentImpl(const ACE_CString& name,
    maci::ContainerServices *containerServices);
    virtual ~LogTestComponentImpl(){};

    virtual void method();
};

#endif

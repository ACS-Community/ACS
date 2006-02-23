#ifndef ErrTestComponentImpl_h
#define ErrTestComponentImpl_h

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "BasePerfCompImpl.h"
#include "perftestS.h"
#include <acserr.h>
#include <BenchmarkErrType.h>

using namespace acscomponent;

class ErrTestComponentImpl: public virtual BasePerfCompImpl,
			    public POA_perftest::ErrTestComponent
{
  private:
    BenchmarkErrType::BenchmarkErr0ExImpl getException(int depth, BenchmarkErrType::BenchmarkErr0ExImpl err);
  public:
    ErrTestComponentImpl(const ACE_CString& name,
			 maci::ContainerServices *containerServices);
    virtual ~ErrTestComponentImpl(){};
    
    virtual void testExceptions(CORBA::Long depth, CORBA::Boolean err) throw (CORBA::SystemException, ACSErr::ACSException, BenchmarkErrType::BenchmarkErr0Ex);
};

#endif

#ifndef SupplierTestComponentImpl_h
#define SupplierTestComponentImpl_h

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "BasePerfCompImpl.h"

#include <acsncSimpleSupplier.h>

class SupplierTestComponentImpl: public virtual BasePerfCompImpl
{
  public:
    SupplierTestComponentImpl(const ACE_CString& name,
    maci::ContainerServices *containerServices);
    virtual ~SupplierTestComponentImpl() {};
    
    virtual void method();
    virtual void cleanUp();

  private:
    nc::SimpleSupplier *m_perfSupplier_p;
};

#endif

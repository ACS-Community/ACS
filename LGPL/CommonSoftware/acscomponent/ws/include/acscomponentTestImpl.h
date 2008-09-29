#ifndef acscomponentTestImpl_h
#define acscomponentTestImpl_h


#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

//#include <maciACSComponentDefines.h>
#include <acscomponentImpl.h>

#include <acscomponentTestS.h>

class ACSComponentTestClassImpl: public virtual acscomponent::ACSComponentImpl,
				 public virtual POA_ACSCOMPONENT_TEST::ACSComponentTestClass
{
    
  public:
     
    ACSComponentTestClassImpl(const ACE_CString& name, 
			      maci::ContainerServices *);
    
    virtual ~ACSComponentTestClassImpl();
            
    void shutdown(); 
    
};

#endif






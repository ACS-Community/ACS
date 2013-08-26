#ifndef componentGetterImpl_h
#define componentGetterImpl_h

#include <acscomponentImpl.h>
#include "componentGetterS.h"

class ComponentGetterImpl:
	public virtual acscomponent::ACSComponentImpl,
	public POA_demo::ComponentGetter
{

  public:
    ComponentGetterImpl(
	       const ACE_CString& name,
	       maci::ContainerServices * containerServices);

    virtual ~ComponentGetterImpl();

    virtual void getOtherComponent();

};

#endif

#include "componentGetterImpl.h"

/* Cons + dest */
ComponentGetterImpl::ComponentGetterImpl(
		       const ACE_CString &name,
		       maci::ContainerServices * containerServices) :
    ACSComponentImpl(name, containerServices) { }
ComponentGetterImpl::~ComponentGetterImpl() {}

void ComponentGetterImpl::getOtherComponent() {
	getContainerServices()->getComponent<demo::ComponentGetter>("COMP_TO_GET");
	ACS_TRACE("Got component without problems :)");
}

#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(ComponentGetterImpl);

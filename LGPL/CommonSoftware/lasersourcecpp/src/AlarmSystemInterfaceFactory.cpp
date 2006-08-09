#include "AlarmSystemInterfaceFactory.h"

// The manager
maci::Manager_ptr laserSource::AlarmSystemInterfaceFactory::m_manager=maci::Manager::_nil();

                        // The naming service
CosNaming::NamingContext_ptr laserSource::AlarmSystemInterfaceFactory::m_naming_p=CosNaming::NamingContext::_nil();

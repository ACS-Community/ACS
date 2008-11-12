#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#ifndef _SIMPLE_EXAMPLE_DDS_H_
#define _SIMPLE_EXAMPLE_DDS_H

#include <acscomponentImpl.h>

#include "SimpleExampleS.h"

class SimpleExampleNCImpl: 
	virtual public POA_DDS_SIMPLE_EXAMPLE::simpleExample,
	virtual public acscomponent::ACSComponentImpl
{
	public:
		SimpleExampleNCImpl(const ACE_CString &name, 
				maci::ContainerServices *containerServices);

		virtual void sendMessage() 
			throw (CORBA::SystemException);

		virtual ~SimpleExampleNCImpl();
};

#endif

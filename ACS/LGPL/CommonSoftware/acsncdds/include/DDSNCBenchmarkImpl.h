#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#ifndef _DDSNCBENCHMARKIMPL_H_
#define _DDSNCBENCHMARKIMPL_H_

#include <acscomponentImpl.h>

#include "NCBenchmarkS.h"

class DDSNCBenchmarkSupplierImpl : 
	virtual public POA_NC_BENCHMARK::testNCSupplier,
	virtual public acscomponent::ACSComponentImpl
{
	public:
		DDSNCBenchmarkSupplierImpl(const ACE_CString &name, 
				maci::ContainerServices *containerServices);

		virtual void runTest(::CORBA::ULong freq, ::CORBA::ULong duration) 
			throw (CORBA::SystemException);

		virtual void cleanUp();

		virtual ~DDSNCBenchmarkSupplierImpl();
};

#endif

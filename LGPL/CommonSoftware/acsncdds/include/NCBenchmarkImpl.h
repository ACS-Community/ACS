#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#ifndef _NCBENCHMARKIMPL_H_
#define _NCBENCHMARKIMPL_H_

#include <acscomponentImpl.h>

#include "NCBenchmarkS.h"

class NCBenchmarkSupplierImpl : 
	virtual public POA_NC_BENCHMARK::testNCSupplier,
	virtual public acscomponent::ACSComponentImpl
{
	public:
		NCBenchmarkSupplierImpl(const ACE_CString &name, 
				maci::ContainerServices *containerServices);

		virtual void runTest(::CORBA::ULong freq, ::CORBA::ULong duration) 
			throw (CORBA::SystemException);

		virtual ~NCBenchmarkSupplierImpl();
};

#endif

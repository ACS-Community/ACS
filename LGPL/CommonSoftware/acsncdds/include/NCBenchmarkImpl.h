#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#ifndef _NCBENCHMARKIMPL_H_
#define _NCBENCHMARKIMPL_H_

#include <acscomponentImpl.h>

#include "NCBenchmarkS.h"

class NCBenchmarkImpl : virtual public POA_NC_BENCHMARK::testNcComponent,
								virtual public acscomponent::ACSComponentImpl
{
	public:
		NCBenchmarkImpl(const ACE_CString &name, maci::ContainerServices *containerServices);

		virtual void runTest() throw (CORBA::SystemException);

		virtual ~NCBenchmarkImpl();
};

#endif

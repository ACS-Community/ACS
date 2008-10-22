#include <vltPort.h>
#include <iostream>
#include "NCBenchmarkImpl.h"

NCBenchmarkImpl::NCBenchmarkImpl(const ACE_CString &name, 
			maci::ContainerServices *containerServices) : 
		ACSComponentImpl(name, containerServices)
{

}

void NCBenchmarkImpl::runTest() throw (::CORBA::SystemException)
{
	std::cout << "Hola" << std::endl;
}

NCBenchmarkImpl::~NCBenchmarkImpl()
{

}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(NCBenchmarkImpl)
/* ----------------------------------------------------------------*/


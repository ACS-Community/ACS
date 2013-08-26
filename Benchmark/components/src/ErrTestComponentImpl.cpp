#include "ErrTestComponentImpl.h"
#include <string.h>

ACE_RCSID(acstests, ErrTestComponentImpl, "$Id: ErrTestComponentImpl.cpp,v 1.6 2008/10/08 01:57:23 cparedes Exp $")

ErrTestComponentImpl::ErrTestComponentImpl(const ACE_CString& name,
    maci::ContainerServices *containerServices) :
    BasePerfCompImpl(name, containerServices)
{
    ACS_TRACE("::ErrTestComponentImpl::ErrTestComponentImpl");
}

void ErrTestComponentImpl::genException(int depth, BenchmarkErrType::BenchmarkErr0ExImpl excep, CORBA::Boolean err) 
	throw (BenchmarkErrType::BenchmarkErr0Ex)
{
	if (depth==0) 
	{
		if(err == true)
		{
			throw excep.getBenchmarkErr0Ex();
		}
		return;
	}
	else 
	{
		BenchmarkErrType::BenchmarkErr0ExImpl tExcept(excep.getErrorTrace(), __FILE__, __LINE__, "ErrTestComponentImpl::genException");
		genException(depth-1, tExcept, err);
	}
}

void 
ErrTestComponentImpl::testExceptions(CORBA::Long depth, CORBA::Boolean err) 
{
	if (depth < 1)
	{
		BenchmarkErrType::BenchmarkErr0ExImpl myException(__FILE__, __LINE__, "ErrTestComponentImpl::testExceptions");
		throw myException.getBenchmarkErr0Ex();
	}
	genException(depth-1, 
		BenchmarkErrType::BenchmarkErr0ExImpl(__FILE__,  __LINE__, "ErrTestComponentImpl::testExceptions"), err);
}

#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(ErrTestComponentImpl)

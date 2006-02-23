#include "ErrTestComponentImpl.h"
#include <string.h>

ACE_RCSID(acstests, ErrTestComponentImpl, "$Id: ErrTestComponentImpl.cpp,v 1.4 2005/04/29 21:45:13 dfugate Exp $")

ErrTestComponentImpl::ErrTestComponentImpl(const ACE_CString& name,
    maci::ContainerServices *containerServices) :
    BasePerfCompImpl(name, containerServices)
{
    ACS_TRACE("::ErrTestComponentImpl::ErrTestComponentImpl");
}

BenchmarkErrType::BenchmarkErr0ExImpl 
ErrTestComponentImpl::getException(int depth, BenchmarkErrType::BenchmarkErr0ExImpl err) 
{
    if (depth==0) 
	{
	return err;
	}
    
    else 
	{
	BenchmarkErrType::BenchmarkErr0ExImpl tExcept = BenchmarkErrType::BenchmarkErr0ExImpl(err.getErrorTrace(), 
											      __FILE__, __LINE__, "ErrTestComponentImpl::getException");
	return getException(depth-1, tExcept);
	}
}

void 
ErrTestComponentImpl::testExceptions(CORBA::Long depth, CORBA::Boolean err) 
    throw (CORBA::SystemException, ACSErr::ACSException, BenchmarkErrType::BenchmarkErr0Ex)
{
    if (depth < 1)
	{
	throw ACSErr::ACSException();
	}

    BenchmarkErrType::BenchmarkErr0ExImpl tExcept = getException(depth-1, 
								 BenchmarkErrType::BenchmarkErr0ExImpl(__FILE__,  
												       __LINE__, 
												       "ErrTestComponentImpl::testExceptions"));
    if (err==true)
	{
	throw tExcept.getBenchmarkErr0Ex();
	}
}

#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(ErrTestComponentImpl)

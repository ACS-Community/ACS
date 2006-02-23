/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: acserrHandlers.cpp,v 1.3 2005/06/17 08:20:50 bjeram Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram  yyyy-mm-dd  created 
*/

#define _POSIX_SOURCE 1
#include "vltPort.h"
#include "acserrHandlers.h"

static char *rcsId="@(#) $Id: acserrHandlers.cpp,v 1.3 2005/06/17 08:20:50 bjeram Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);


using namespace acserrHandlersErr;

// this class it is used just to inform acserrUncaughtExHandler
// from UnspecifiedEhHandler, in other words to hide ACSbaseExImpl 
// = to prevent that exception is porpagated through the call chain where unspecified exception was thrown
class UnspecException {
  public:
    UnspecException(acserrHandlersErrExImpl& _ex) : unspecEx(_ex){}
    acserrHandlersErrExImpl unspecEx;
};

/**
 ACS Handler functions for exceptions:
*/
void acserrUnspecifiedExHandler()
{

    try
	{
	throw ;
	}
    catch (ACSErr::ACSbaseExImpl &_ex)
	{
	UnspecifiedACSBasedExceptionExImpl ex(_ex, __FILE__, __LINE__, 
					      "acserrUnspecifiedExHandler", ACSErr::Emergency);
	throw UnspecException(ex); // pass to the acserrUncaughtExHandler where it be loged
	}
    catch (CORBA::Exception &ce)
	{
	UnspecifiedCORBAExceptionExImpl ex(__FILE__, __LINE__,
					   "acserrUnspecifiedExHandler", ACSErr::Emergency);
	ex.setCORBAExName(ce._name());
	throw UnspecException(ex); // pass to the acserrUncaughtExHandler
	}
    catch (...)
	{
	UnspecifiedUnknownExceptionExImpl ex(__FILE__, __LINE__,
					     "acserrUnspecifiedExHandler", ACSErr::Emergency);
	throw UnspecException(ex); // pass to the acserrUncaughtExHandler where it be loged
	}
}//acserrUnexpected


void acserrUncaughtExHandler()
{

    try
	{
	throw ;
	}
    catch(UnspecException &_ex)
	{
	UncaughtUnspecifiedExceptionExImpl ex(_ex.unspecEx,
					      __FILE__, __LINE__, "acserrUncaughtExHandler", ACSErr::Emergency);
	ex.log();
//	ACS_LOG(LM_RUNTIME_CONTEXT, "acserrUncaughtExHandler", (LM_EMERGENCY, "Program terminated because unspecified exception was thrown"));
	}
    catch (ACSErr::ACSbaseExImpl &_ex)
	{
	UncaughtACSBasedExceptionExImpl ex(_ex, __FILE__, __LINE__,
					   "acserrUncaughtExHandler", ACSErr::Emergency);
	ex.log();
	}
    catch (CORBA::Exception &ce)
	{
	UncaughtCORBAExceptionExImpl ex(__FILE__, __LINE__,
					"acserrUncaughtExHandler", ACSErr::Emergency);
	ex.setCORBAExName(ce._name());
  	ex.log();
	}
    catch (...)
	{
	UncaughtUnknownExceptionExImpl ex(__FILE__, __LINE__,
					  "acserrUncaughtExHandler", ACSErr::Emergency);
  	ex.log();
	}
}//acserrTerminate


/*___oOo___*/

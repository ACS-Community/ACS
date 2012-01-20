/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: acserrHandlers.cpp,v 1.13 2012/01/20 22:07:43 tstaig Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram  yyyy-mm-dd  created
*/

#include "vltPort.h"
#include "acserrHandlers.h"
#include <cstring>
#if !defined (MAKE_VXWORKS) && !defined(__CYGWIN__)
#include <execinfo.h>
#endif

static char *rcsId="@(#) $Id: acserrHandlers.cpp,v 1.13 2012/01/20 22:07:43 tstaig Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#ifndef MAKE_VXWORKS
const unsigned int ACSErrmaxBackTraceDepth=255;
#endif

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
 * helper function that adds back trace info to the exception
 * and releases the array of strings that contains the back trace
 */
#if !defined (MAKE_VXWORKS) && !defined(__CYGWIN__)

void addBackTrace2Exception(ACSErr::ACSbaseExImpl &ex, char **backTraceString, size_t backTraceSize)
{
	char backTraceLevel[4];

	if (backTraceString!=NULL)
	{
		for(size_t i=0; i<backTraceSize; i++)
		{
			sprintf(backTraceLevel, "#%d", i);
			ex.addData(backTraceLevel, backTraceString[i]);
		}//for
		free(backTraceString);
	}//if
};//addBackTrace2Exception
#endif

/**
 ACS Handler functions for exceptions:
*/
void acserrUnspecifiedExHandler()
{
#if !defined (MAKE_VXWORKS) && !defined(__CYGWIN__)
	void *backTrace[ACSErrmaxBackTraceDepth];
	size_t backTraceSize;
	char **backTraceString;

	backTraceSize = backtrace (backTrace, 255);
	backTraceString = backtrace_symbols (backTrace, backTraceSize);
#endif
    try
	{

	throw ;
	}
    catch (ACSErr::ACSbaseExImpl &_ex)
	{
	UnspecifiedACSBasedExceptionExImpl ex(_ex, __FILE__, __LINE__,
					      "acserrUnspecifiedExHandler", ACSErr::Emergency);
#if !defined (MAKE_VXWORKS) && !defined(__CYGWIN__)
	addBackTrace2Exception(ex, backTraceString, backTraceSize);
#endif

	throw UnspecException(ex); // pass to the acserrUncaughtExHandler where it be logged
	}
    catch (CORBA::Exception &ce)
	{
	UnspecifiedCORBAExceptionExImpl ex(__FILE__, __LINE__,
					   "acserrUnspecifiedExHandler", ACSErr::Emergency);
	ex.setCORBAExName(ce._name());
#if !defined (MAKE_VXWORKS) && !defined(__CYGWIN__)
	addBackTrace2Exception(ex, backTraceString, backTraceSize);
#endif

	throw UnspecException(ex); // pass to the acserrUncaughtExHandler
	}
    catch(std::exception &stdex)
      {
        UnspecifiedSTDExceptionExImpl ex(__FILE__, __LINE__,
            "acserrUnspecifiedExHandler", ACSErr::Emergency);
        ex.setWhat(stdex.what());
#if !defined (MAKE_VXWORKS) && !defined(__CYGWIN__)
        addBackTrace2Exception(ex, backTraceString, backTraceSize);
#endif
        throw UnspecException(ex); // pass to the acserrUncaughtExHandler
      }
    catch (...)
	{
	UnspecifiedUnknownExceptionExImpl ex(__FILE__, __LINE__,
					     "acserrUnspecifiedExHandler", ACSErr::Emergency);
#if !defined (MAKE_VXWORKS) && !defined(__CYGWIN__)
	addBackTrace2Exception(ex, backTraceString, backTraceSize);
#endif
	throw UnspecException(ex); // pass to the acserrUncaughtExHandler where it be loged
	}
}//acserrUnexpected


void acserrUncaughtExHandler()
{
#if !defined (MAKE_VXWORKS) && !defined(__CYGWIN__)
	void *backTrace[ACSErrmaxBackTraceDepth];
	size_t backTraceSize;
	char **backTraceString;
	//here we set the handler to verbose in case an exception is generated in this function
	std::set_terminate(__gnu_cxx::__verbose_terminate_handler);

	backTraceSize = backtrace (backTrace, 255);
	backTraceString = backtrace_symbols (backTrace, backTraceSize);
#endif

    try
	{
	throw ;
	}
    catch(UnspecException &_ex)
	{
	UncaughtUnspecifiedExceptionExImpl ex(_ex.unspecEx,
					      __FILE__, __LINE__, "acserrUncaughtExHandler", ACSErr::Emergency);
#if !defined (MAKE_VXWORKS) && !defined(__CYGWIN__)
	// here we already have back trace from acserrUnspecifiedExHandler, but we have to free backTraceString
	free(backTraceString);
#endif
	ex.log();
//	ACS_LOG(LM_RUNTIME_CONTEXT, "acserrUncaughtExHandler", (LM_EMERGENCY, "Program terminated because unspecified exception was thrown"));
	}
    catch (ACSErr::ACSbaseExImpl &_ex)
	{
	UncaughtACSBasedExceptionExImpl ex(_ex, __FILE__, __LINE__,
					   "acserrUncaughtExHandler", ACSErr::Emergency);
#if !defined (MAKE_VXWORKS) && !defined(__CYGWIN__)
	addBackTrace2Exception(ex, backTraceString, backTraceSize);
#endif
	ex.log();
	}
    catch (CORBA::Exception &ce)
	{
	UncaughtCORBAExceptionExImpl ex(__FILE__, __LINE__,
					"acserrUncaughtExHandler", ACSErr::Emergency);
	ex.setCORBAExName(ce._name());
#if !defined (MAKE_VXWORKS) && !defined(__CYGWIN__)
	addBackTrace2Exception(ex, backTraceString, backTraceSize);
#endif
  	ex.log();
	}
    catch(std::exception &stdex)
    {
      UncaughtSTDExceptionExImpl ex(__FILE__, __LINE__,
                                    "acserrUncaughtExHandler", ACSErr::Emergency);
      ex.setWhat(stdex.what());
#if !defined (MAKE_VXWORKS) && !defined(__CYGWIN__)
      addBackTrace2Exception(ex, backTraceString, backTraceSize);
#endif

      ex.log();
    }
    catch (...)
	{
	UncaughtUnknownExceptionExImpl ex(__FILE__, __LINE__,
					  "acserrUncaughtExHandler", ACSErr::Emergency);
#if !defined (MAKE_VXWORKS) && !defined(__CYGWIN__)
	addBackTrace2Exception(ex, backTraceString, backTraceSize);
#endif
  	ex.log();
	}
}//acserrTerminate


#if !defined (MAKE_VXWORKS) && !defined(__CYGWIN__)

void acserrSigSegvHandler(int signal, siginfo_t* info, void* data)
{
    const std::string myTimeStamp(::getStringifiedTimeStamp().c_str());

    std::cout << "Caught signal "
        << std::strerror(info->si_errno)
        << ", code = "
        << info->si_code
        << ".  Code interpretation = ";
    if(info->si_code == SEGV_MAPERR)
    {
        std::cout << "address not mapped to object";
    }
    else if(info->si_code == SEGV_ACCERR)
    {
        std::cout << "invalid permissions for mapped object";
    }
    else
    {
        std::cout << "unknown code";
    }

    std::cout << ".  Date/Time = "
        << myTimeStamp
        << "\n";
}
#endif //!MAKE_VXWORKS


/*___oOo___*/

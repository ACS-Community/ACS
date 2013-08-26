/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciManagerShutdown.cpp,v 1.85 2008/07/14 13:41:20 bjeram Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2001/12/24  cleaned
* msekoran  2001/02/21  created
*/

/** @file maciManagerShutdown.cpp
 *  maciManagerShutdown is used to stop a Manager on the workstation.  There are many different
 *  parameters which can be passed to maciManagerShutdown, but none are required.
 *  @htmlonly
 *  <br><br>
 *  @endhtmlonly

 *  @param "-ORBInitRef NameService=corbaloc::yyy:xxxx/NameService" Use this optional parameter to specify which
 *  host/port to get a reference to the naming service from.
 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly

 *  @param "-ORBInitRef NotifyEventChannelFactory=corbaloc::yyy:xxxx/NotifyEventChannelFactory" Use this optional
 *  parameter to specify which host/port to get a reference to the notification service from.
 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly
 */


#include <vltPort.h>
#include <acsutil.h>

#include <logging.h>
#include <maciHelper.h>

 using namespace maci;

int
main (int argc, char *argv[])
{

    LoggingProxy * logger = new LoggingProxy(0, 0, 31);
    if (logger)
    {
	LoggingProxy::init(logger);
	LoggingProxy::ProcessName(argv[0]);
	LoggingProxy::ThreadName("main");
    }
    else
	ACS_SHORT_LOG((LM_INFO, "Failed to initialize logging."));


    try
    {
      // Initialize the ORB.
      CORBA::ORB_var orb = CORBA::ORB_init (argc,
                                            argv,
                                            "TAO"
                                            );



      maci::Manager_var mgr = MACIHelper::resolveManager(orb.ptr(), argc, argv, 0, 0);
      if (CORBA::is_nil(mgr.ptr()))
      {
	  ACS_SHORT_LOG((LM_ERROR, "Failed to resolve Manager reference."));
	  return -1;
      }

      ACS_SHORT_LOG((LM_INFO, "Calling maci::Manager::shutdown()..."));


      CORBA::ULong code = 0;
      if (argc > 1)
	code = atol(argv[1]);

      // a little hack to manager (this class should implement administrator interface, etc..)
      mgr->shutdown(0x05000000, code);


      ACS_SHORT_LOG((LM_INFO, "Done."));

    }
    catch( CORBA::Exception &ex )
    {

	ACS_SHORT_LOG((LM_INFO, "Failed."));
	ACE_PRINT_EXCEPTION (ex,
			     ACE_TEXT ("Caught unexpected exception:"));

	return -1;
    }

    return 0;
}






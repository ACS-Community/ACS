/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciContainerShutdown.cpp,v 1.9 2008/07/14 13:41:20 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bgustafs 2002-04-15 corrected shutdown action
* msekoran  2001/12/23  created
*/


/** @file maciContainerShutdown.cpp
 *  maciContainerShutdown is used to stop an Container on the workstation.  There are many different
 *  parameters which can be passed to maciContainerShutdown, but only the container we want to stop is required.
 *  @htmlonly
 *  <br><br>
 *  @endhtmlonly

 *  @param container This is simply the name of the container in the CDB that we want to stop.
 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly

 *  @param "-ORBInitRef NameService=corbaloc::yyy:xxxx/NameService" Use this optional parameter to specify an
 *  instance of the NameService that "knows about" the container we want to stop.
 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly
 */



#include <vltPort.h>
#include <acsutil.h>

#include <logging.h>
#include <maciHelper.h>
#include <maciContainerImpl.h>

 using namespace maci;

int
main (int argc, char *argv[])
{

    if (argc < 2)
    {
	ACE_OS::printf("\n\tusage: %s <container name wildcard> [<ORB options>]\n\n", argv[0]);
	return -1;
    }

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

	maci::HandleSeq handles;
	handles.length(0);

	// a little hack to manager (this clas should implement administrator interface, etc..)
	maci::Handle h = 0x05000000;
	maci::ContainerInfoSeq_var containers = mgr->get_container_info(h, handles, argv[1]);


	if (!containers.ptr() || containers->length()==0)
	{
	    ACS_SHORT_LOG((LM_INFO, "No containers matching name %s found.", argv[1]));
	    return -1;
	}

	CORBA::ULong action = CONTAINER_EXIT << 8;  // exit

	for (CORBA::ULong i = 0; i < containers->length(); i++)
	{
	    try
	    {
	    //ACS_SHORT_LOG((LM_INFO, "Calling maci::Container::shutdown() on '%s'", containers[i].name.in()));
	    //containers[i].reference->shutdown(action);
	        ACS_SHORT_LOG((LM_INFO, "Calling maci::Manager::shutdown_container(%s, %d).", containers[i].name.in(), action));
		mgr->shutdown_container(h, containers[i].name.in(), action);

		ACS_SHORT_LOG((LM_INFO, "Done."));
	    }
	    catch( CORBA::Exception &ex )
	    {
	        ACS_SHORT_LOG((LM_INFO, "Failed."));
		//ACE_PRINT_EXCEPTION (ex, ACE_TEXT ("Exception caught while calling Container::shutdown() method."));
		ACE_PRINT_EXCEPTION (ACE_ANY_EXCEPTION, ACE_TEXT ("Exception caught while calling Manager::shutdown_container() method."));
	    }

	}

	ACS_SHORT_LOG((LM_INFO, "Done all."));

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






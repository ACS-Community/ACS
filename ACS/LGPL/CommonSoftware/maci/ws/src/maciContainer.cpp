/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciContainer.cpp,v 1.19 2011/03/18 17:02:40 rtobar Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-08-15 changed argUnpack.h to acsutilArgUnpack.h
* bgustafs 2002-04-15 corrected activator shutdown on VxWorks
* bgustafs 2001-07-12 added namespace declarations
* msekoran  2001/02/21  created 
*/

/** @file maciContainer.cpp
 *  maciContainer is used to start an Container on the workstation or LCU.  There are many different
 *  parameters which can be passed to maciContainer, but only the container we want to start is required.
 *  @htmlonly
 *  <br><br>
 *  @endhtmlonly

 *  @param container This is simply the name of the container in the CDB that we want to start.
 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly

 *  @param "-r" Use this optional parameter to run container in recovery mode.
 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly

 *  @param "-p filename" Use this optional parameter to write out the process ID of this container to filename.
 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly

 *  @param "-m corbaloc::yyy:xxxx/Manager" Use this optional parameter to run this container with a 
 *  manager running on a port other than 3000 or for a manager on a different PC.
 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly

 *  @param "-ORBEndpoint iiop://yyy:xxxx" Use this optional parameter to specify which host/port container
 *  should run on.
 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly

 *  @param "-ORBInitRef NameService=corbaloc::yyy:xxxx/NameService" Use this optional parameter to specify which 
 *  host/port container should get a reference to the naming service from.
 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly

 *  @param "-ORBInitRef NotifyEventChannelFactory=corbaloc::yyy:xxxx/NotifyEventChannelFactory" Use this optional 
 *  parameter to specify which host/port container should get a reference to the notification service from.
 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly
 */   


#include <vltPort.h>
#include <maciContainer.h>
#include <maciHelper.h>

#ifdef MAKE_VXWORKS
#	include "rebootLib.h"
#	include "acsutilArgUnpack.h"

int g_containerShutdownAction = 0;

#endif

 using namespace maci;

ACE_RCSID(maci, maciContainer, "$Id: maciContainer.cpp,v 1.19 2011/03/18 17:02:40 rtobar Exp $")

volatile bool shutting_down = false;

void TerminationSignalHandler(int)
{
  if (shutting_down) return;
  shutting_down=true;
                                                                                                                                
  // initialize logger (TTS has to be initialized)
  if (ContainerImpl::getLoggerProxy())
    {
      LoggingProxy::init(ContainerImpl::getLoggerProxy());
      LoggingProxy::ThreadName("termination");
    }

  ACS_LOG(0, "termination", (LM_INFO, "Termination signal detected."));

  MACIHelper::terminateResolving();
  
  try
    {
      if (ContainerImpl::getContainer() && 
	  ContainerImpl::getContainer()->getContainerCORBAProxy() != maci::Container::_nil())
        {
	  ContainerImpl::getContainer()->getContainerCORBAProxy()->shutdown(CONTAINER_EXIT << 8);
	  //ContainerImpl::getContainer()->shutdown(CONTAINER_EXIT << 8);
	  
	}
    }
  catch( CORBA::Exception &ex )
    {
      ACE_PRINT_EXCEPTION(ex, "TerminationSignalHandler");
    }
}




int main(int argc, char *argv[])
{
  ACS_SHORT_LOG((LM_INFO, "Starting Container..."));

  ACE_OS::signal(SIGINT,  TerminationSignalHandler);  // Ctrl+C
  ACE_OS::signal(SIGTERM, TerminationSignalHandler);  // termination request

  // This call is necessary for proper handling of wide characters, so the
  // wchar_t* <-> char* conversions work depending on the current value of
  // the LANG (and family) environment variables. This is necessary since
  // xmlEntities are defined as wstring in IDL, and represented as
  // CORBA::WString in C++
  //
  // In ALMA, the standard machines have LANG=en_US.UTF-8, which will use
  // UTF-8 as the character encoding. Other ALMA tools, such as the ALMA-OT,
  // also use UTF-8 to encode/decode strings
  //
  // For more information, please visit, for example
  //    http://www.cl.cam.ac.uk/~mgk25/unicode.html#c
  setlocale(LC_ALL, "");

//  ContainerImpl &container = *ACE_Singleton<ContainerImpl, ACE_Null_Mutex>::instance();
//  ContainerImpl &container = Loki::SingletonHolder<ContainerImpl,
//					 Loki::CreateUsingNew,
//					 Loki::PhoenixSingleton>::Instance();
	 ContainerImpl &container = *ACE_Unmanaged_Singleton<ContainerImpl, ACE_Null_Mutex>::instance();

  // while (container.getShutdownAction() == CONTAINER_RELOAD)

  if (container.init(argc, argv))
  {
      // reinitialization of signal handlers is needed, because of CCS
      // the initialization above is needed to stop resolving, etc..
      ACE_OS::signal(SIGINT,  TerminationSignalHandler);  // Ctrl+C
      ACE_OS::signal(SIGTERM, TerminationSignalHandler);  // termination request

      if (container.connect())
	  container.run();
  }

  ACS_SHORT_LOG((LM_INFO, "Stopping container... "));
  
  container.done();

  ACS_SHORT_LOG((LM_INFO, "Container stopped."));

  // reboot/reload action !!!

  if (ContainerImpl::getLoggerProxy())
    {
      ContainerImpl::getLoggerProxy()->flush();
//      ContainerImpl::getLoggerProxy()->done();
    }

#ifdef MAKE_VXWORKS
  g_containerShutdownAction = container.getShutdownAction();
#endif

  return container.getStatus();
}//main



#ifdef MAKE_VXWORKS

int ContainerStart(char *szCmdLn)
{
  int  argc;
  char *argv[100];

  ACE_OS_Object_Manager ace_os_object_manager;
  ACE_Object_Manager ace_object_manager;
//  ACE_MAIN_OBJECT_MANAGER

    //ACE_OS::string_to_argv(szCmdLn, argc, argv);

  argc = argUnpack(szCmdLn, argv);
  argv[0] = "Container";

  int retval = ace_main_i(argc, argv);

  //  1 -- reboot the computer, 2 -- exit the container - both reboot
  if(g_containerShutdownAction == CONTAINER_REBOOT)
    reboot(0);

  // @@ Deallocate argv as required (cf. ContainerImpl)

  return retval;
}

void ContainerStop(int retval)
{
  ContainerImpl * container = ContainerImpl::getContainer();
  
  //  ACE_TRY
  //    {
      if(container != 0)
	{
	container->shutdown(retval);
	//	
	}
      //    }
}

template ACE_Singleton<ContainerImpl, ACE_Null_Mutex> *
         ACE_Singleton<ContainerImpl, ACE_Null_Mutex>::singleton_;

#endif // defined( MAKE_VXWORKS )






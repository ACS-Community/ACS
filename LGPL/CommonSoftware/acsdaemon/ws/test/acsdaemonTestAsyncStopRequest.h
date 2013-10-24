#ifndef __TestAcsDaemonAsyncStopRequest_h
#define __TestAcsDaemonAsyncStopRequest_h
/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: 
*
* who       when        what
* --------  ----------  ----------------------------------------------
* rbourtem  2013-10-11
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#include <Task.h>
#include <string>
#include <vector>

#include "acsdaemonC.h"

/**
 * Class allowing a CORBA method to be invoked in a seperate thread
 * trapping any exceptions thrown
 */
class TestAcsDaemonAsyncStopRequest : public ACE_Task_Base
{
public:
  TestAcsDaemonAsyncStopRequest();
  ~TestAcsDaemonAsyncStopRequest();

  // run the CORBA method in a seperate task
  void run(const std::string &container, const std::string &host);

  // wait for thread completion
  int wait();

  // Return the exception
  std::string &exceptionString() {return exceptionString_m;};
  //SPARTA::ErrorEvent &exception() { return exception_m;};
  bool commandOK() {return commandOK_m;};

protected:
private:
  // Overloaded ACE_Task_Base method
  int svc();
  bool commandOK_m;
  bool completed_m;

  std::string host_m;
  std::string container_m;
  
  /**
   * ACS instance to be handled by this DEPL object
   */
  int m_acsInstance;
  /**
   * ACS Manager corbaloc URL
   */
  std::string m_managerCorbaloc;
  /**
   * Additional command used to give the manager reference to
   * the acscontainerdaemon when start_container() or stop_container() is invoked
   */
  std::string m_additionalCmdLine; // Additional command used to give the manager reference to
                                   // the acscontainerdaemon when start_container() or stop_container() is invoked

  int stopContainerOnHost(std::string &host, std::string &container) throw (CORBA::SystemException,acsdaemonErrType::FailedToStopContainerEx, ACSErrTypeCommon::BadParameterEx);
  
  acsdaemon::ContainerDaemon_ptr getContainerDaemonRef_(const std::string & host);
  /**
   * Get the corbaloc URL of the ACS manager
   *
   * Ex: "corbaloc::134.171.24.124:3000/Manager"
   * @return the corbaloc URL of the ACS manager
   */
  std::string getManagerCorbaloc_();

  // Store the error string if an exception occurs
  std::string exceptionString_m;
  //SPARTA::ErrorEvent exception_m;
};

int shutdownContainers(std::vector<std::string> containers, std::string) throw (CORBA::SystemException,acsdaemonErrType::FailedToStopContainerEx,ACSErrTypeCommon::BadParameterEx);
int main(int argc, char* argv[]);

#endif  /* __TestAcsDaemonAsyncStopRequest_h */

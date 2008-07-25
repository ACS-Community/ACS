#ifndef maciResolveHelper_H_
#define maciResolveHelper_H_

/*******************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciHelper.h,v 1.89 2008/07/25 07:32:36 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2001/09/21  added getManagerHostname()
* msekoran  2001/08/29  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutil.h>
#include <maciExport.h>

#include <maciS.h>

#include <orbsvcs/CosNamingC.h>

#include <ace/SString.h>

#include <cdb.h>

#include <tao/IFR_Client/IFR_BasicC.h>

namespace maci {

/**
 * MACIHelper class is a class helping to handle commonly used operations.
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 * @version "@(#) $Id: maciHelper.h,v 1.89 2008/07/25 07:32:36 cparedes Exp $"
 */

class maci_EXPORT MACIHelper
{

public:

  /**
   * Extract hostname from corbaloc address.
   * @param corbaloc corbaloc address
   * @return hostname or empty string on failure
   */
  static ACE_CString extractHostnameFromCorbaloc(const ACE_TCHAR *corbaloc);

  /**
   * <i>getManagerHostname</i> is a method helping to retrieve Manager's hostname.
   * The Manager hostame is extraced using the first valid of the following options:
   *    # Command line option <i>-m</i> or <i>-managerReference</i>
   *    # ManagerReference value in configuration database
   *    # Environment variable MANAGER_REFERENCE
   *    # hostname
   * 
   * @param argv command line parameter count
   * @param argc command line array of strings
   * @return Manager's hostname
   */
  static ACE_CString getManagerHostname(int argc, ACE_TCHAR **argv);

  /**
   * <i>resolveManager</i> method is a method helping to resolve Manager's reference.
   * The Manager reference is defined by the first valid of the following options:
   *    # Command line option <i>-m</i> or <i>-managerReference</i>
   *    # ManagerReference value in configuration database
   *    # Environment variable MANAGER_REFERENCE
   *    # corbaloc::<hostname>:xxxx/Manager
   * 
   * @param orb CORBA ORB
   * @param argv command line parameter count
   * @param argc command line array of strings
   * @param retries number of retries resolving Manager's reference, <= 0 means infinite
   * @param secTimeout timeout expressed in seconds, == 0 means inifinite (dt between retries is 3 secs)
   * @return CORBA reference to the Manager or maci::Manager::_nil() if unable to resolve Manager's reference
   */
  static maci::Manager_ptr resolveManager(CORBA::ORB_ptr orb,
					  int argc, ACE_TCHAR **argv,
					  int retries = 3, unsigned int secTimeout = 0);

  /**
   * <i>resolveManager</i> method resolve given stringified CORBA reference to the Maneger.
   * @param orb CORBA ORB
   * @param reference stringified CORBA reference to the Manager
   * @param retries number of retries resolving Manager reference, <= 0 means infinite
   * @param secTimeout timeout expressed in seconds, == 0 means inifinite (dt between retries is 3secs)
   * @return CORBA reference to the Manager or maci::Manager::_nil() if unable to resolve Manager reference
   */
  static maci::Manager_ptr resolveManager(CORBA::ORB_ptr orb,
					  const ACE_TCHAR * reference, 
					  int retries = 3, unsigned int secTimeout = 0);

  /**
   * <i>resolveNameService</i> method is a method helping to resolve CORBA NameService's reference.
   * The NameService reference is defined by the first valid of the following options:
   *    # Command line option <i>-ORBInitRef NameService=<corbaloc reference> (e.g. corbaloc::te1.hq.eso.org:xxxx)</i>
   *      using CORBA::ORB::resolve_initial_references("NameService"), ORB has to be already initialized with the command line
   *    # Environment variable NAMESERVICE_REFERENCE
   *    # corbaloc::<hostname>:xxxx/NameService
   * 
   * @param orb CORBA ORB
   * @param retries number of retries resolving NameService reference, <= 0 means infinite
   * @param secTimeout timeout expressed in seconds, == 0 means inifinite (dt between retries is 3secs)
   * @return CORBA reference to the NameService's root CosNaming::NamingContext
   */
  static CosNaming::NamingContext_ptr resolveNameService(CORBA::ORB_ptr orb,
							 int retries = 3, unsigned int secTimeout = 0);
  /**
   * <i>resolveNameService</i> method resolve given stringified CORBA reference to the NameService.
   * @param orb CORBA ORB
   * @param reference stringified CORBA reference to the NameService
   * @param retries number of retries resolving NameService reference, <= 0 means infinite
   * @param secTimeout timeout expressed in seconds, == 0 means inifinite (dt between retries is 3secs)
   * @return CORBA reference to the NameService's root CosNaming::NamingContext or CosNaming::NamingContext::_nil() if unable to resolve NameService reference
   */
  static CosNaming::NamingContext_ptr resolveNameService(CORBA::ORB_ptr orb,
							 const ACE_TCHAR * reference, 
							 int retries = 3, unsigned int secTimeout = 0);


  /**
   * <i>resolveInterfaceRepository</i> method is a method helping to resolve CORBA InterfaceRepository's reference.
   * The InterfaceRepository reference is defined by the first valid of the following options:
   *    # Command line option <i>-ORBInitRef InterfaceRepository=<corbaloc reference> (e.g. corbaloc::te1.hq.eso.org:xxxx)</i>
   *      using CORBA::ORB::resolve_initial_references("InterfaceRepository"), ORB has to be already initialized with the command line
   *    # Manager get_service("InterfaceRepository")
   *    # corbaloc::<manager's hostname>:xxxx/NameService
   *    # corbaloc::<hostname>:xxxx/NameService
   * 
   * @param orb CORBA ORB
   * @param manager Manager's reference.
   * @param argv command line parameter count
   * @param argc command line array of strings
   * @param retries number of retries resolving NameService reference, <= 0 means infinite
   * @param secTimeout timeout expressed in seconds, == 0 means inifinite (dt between retries is 3secs).   
   * @return CORBA reference to the InterfaceRepository or CORBA::Object::_nil()
   */
  static CORBA::Repository_ptr resolveInterfaceRepository(CORBA::ORB_ptr orb, maci::Manager_ptr manager,
							 int argc, ACE_TCHAR **argv,
							 int retries = 3, unsigned int secTimeout = 0);

  /**
   * <i>resolveInterfaceRepository</i> method resolve given stringified CORBA reference to the InterfaceRepository.
   * @param orb CORBA ORB
   * @param reference stringified CORBA reference to the InterfaceRepository
   * @param retries number of retries resolving InterfaceRepository reference, <= 0 means infinite
   * @param secTimeout timeout expressed in seconds, == 0 means inifinite (dt between retries is 3secs)
   * @return CORBA reference to the InterfaceRepository or CORBA::Object::_nil()
   */
  static CORBA::Repository_ptr resolveInterfaceRepository(CORBA::ORB_ptr orb,
							 const ACE_TCHAR * reference, 
							 int retries = 3, unsigned int secTimeout = 0);

  /**
   * Terminates all resolving processes
   * @param terminate true is all resolving processes are to be cancelled (default), false to set termiante flag to false
   */
  static void terminateResolving(bool terminate = true);

private:

  static bool m_terminate;


};

 }; 

#endif  /* maciResolveHelper_H_ */


#ifndef baciCORBA_h
#define baciCORBA_h

/*******************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2003
*
*This library is free software; you can redistribute it and/or
*modify it under the terms of the GNU Lesser General Public
*License as published by the Free Software Foundation; either
*version 2.1 of the License, or (at your option) any later version.
*
*This library is distributed in the hope that it will be useful,
*but WITHOUT ANY WARRANTY; without even the implied warranty of
*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*Lesser General Public License for more details.
*
*You should have received a copy of the GNU Lesser General Public
*License along with this library; if not, write to the Free Software
*Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: baciCORBA.h,v 1.95 2008/07/14 12:50:19 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* gchiozzi 2001-12-20 Added getPOAManager() and getPOARoot() methods.
* msekoran  2001/03/07 modified
* msekoran  2001/08/26 added InitCORBA, DoneCORBA for BACI modular tests
*/

/**
 * @file
 * Header file BACI CORBA access.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutil.h>
#include <baciExport.h>

#include <tao/corba.h>
#include <tao/PortableServer/PortableServer.h>

/**
 * Class providing CORBA access to BACI (Singleton pattern)
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 */

class baci_EXPORT BACI_CORBA
{

protected:

  /**
   * Contructor
   * @param orb_m
   * @param poaRoot_m
   * @param poaPersistent_m
   */
  BACI_CORBA(
	     CORBA::ORB_ptr orb_m,
	     PortableServer::POAManager_ptr poaManager,
	     PortableServer::POA_ptr poaRoot_m,
	     PortableServer::POA_ptr poaPersistent_m,
	     PortableServer::POA_ptr poaTransient_m);

  /// Destructor
  ~BACI_CORBA();

public:

  /**
   * Get instance of BACI_CORBA class
   * @return reference to BACI_CORBA class
   */
  static BACI_CORBA * getInstance();

  /**
   * Create instance of BACI_CORBA class
   * @param orb_m
   * @param poaRoot_m
   * @param poaPersistent_m
   * @return reference to BACI_CORBA class, 0 if failed
   */
  static BACI_CORBA * createInstance(
				     CORBA::ORB_ptr orb_m,
				     PortableServer::POAManager_ptr poaManager,
				     PortableServer::POA_ptr poaRoot_m,
				     PortableServer::POA_ptr poaPersistent_m,
				     PortableServer::POA_ptr poaTransient_m);

  /**
   * Destroys instance of BACI_CORBA class
   */
  static void destroyInstance();


  /**
   * Get ORB
   * @return reference to ORB
   */
  static CORBA::ORB_ptr getORB();

  /**
   * Get POA Manager
   * @return reference to POA manager
   */
  static PortableServer::POAManager_ptr getPOAManager();

  /**
   * Get root POA
   * @return reference to POARoot
   */
  static PortableServer::POA_ptr getPOARoot();

  /**
   * Get POA
   * @return reference to POA
   */
  static PortableServer::POA_ptr getPOA();

  /**
   * Activate servant with name (id)
   * @return true on success, false on failure
   */
  static CORBA::Object_ptr ActivateCORBAObject(PortableServer::Servant srvnt, const ACE_CString &id);
  static CORBA::Object_ptr ActivateCORBAObject(PortableServer::Servant srvnt, const char * id);

  /**
   * Activates a transient CORBA object.
   * @return Refererence to the narrowed CORBA object
   */
    template<class T> static T* ActivateTransientCORBAObject(PortableServer::Servant servant)
	{
	    if ((instance_mp==0) ||
	    		CORBA::is_nil(instance_mp->poaTransient_m.ptr()))
		{
		return T::_nil();
		}
	    else
		{
		try
		    {
		    PortableServer::ObjectId_var tID = instance_mp->poaTransient_m->activate_object(servant);
		    CORBA::Object_var corbRef = instance_mp->poaTransient_m->id_to_reference(tID.in());
		    return T::_narrow(corbRef.in());
		    }
		catch(...)
		    {
		    return T::_nil();
		    }
		}
	}

  /**
   * Destroy (deactivate) CORBA object
   * @return true on success, false on failure
   */
  static bool DestroyCORBAObject(CORBA::Object_ptr obj);

  /**
   * Destroy (deactivate) servant
   * @return true on success, false on failure
   */
  static bool DestroyCORBAObject(PortableServer::Servant srvnt);


  /**
   * Destroy (deactivate) Transient CORBA object
   * @return true on success, false on failure
   */
  static bool DestroyTransientCORBAObject(CORBA::Object_ptr obj);

  /**
   * Destroy (deactivate) servant
   * @return true on success, false on failure
   */
  static bool DestroyTransientCORBAObject(PortableServer::Servant srvnt);

  /**
   * Initialize CORBA
   * @param argc argument count
   * @param argv string array of arguments
   * @return true on succes, false on failure
   */
  static bool InitCORBA(int argc, char* argv[]);

  /**
   * Finalize CORBA
   * @return true on succes, false on failure
   */
  static bool DoneCORBA();

private:

  static BACI_CORBA * instance_mp;

  PortableServer::POAManager_var poaManager_m;
  PortableServer::POA_var poaRoot_m;
  PortableServer::POA_var poaPersistent_m;
  PortableServer::POA_var poaTransient_m;
  CORBA::ORB_var orb_m;

    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const BACI_CORBA&);

    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    BACI_CORBA(const BACI_CORBA&);

};

#endif  /* baciCORBA_h */









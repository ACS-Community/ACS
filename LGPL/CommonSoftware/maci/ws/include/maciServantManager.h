#ifndef maciServantManager_h
#define maciServantManager_h

/*******************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciServantManager.h,v 1.86 2006/09/01 02:20:54 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2001/05/19  redesigned
*/

#include <acsutil.h>
#include <tao/PortableServer/PortableServer.h>
#include <tao/PortableServer/ServantActivatorC.h>

namespace maci {

/**
 * Servant manager class
 */

class /*maci_EXPORT*/ MACIServantManager :
  public PortableServer::ServantActivator
{

public:
  /// Contructor
  MACIServantManager () {};

  //MACIServantManager (ContainerImpl * container);

  virtual PortableServer::Servant incarnate (const PortableServer::ObjectId &oid,
                                             PortableServer::POA_ptr poa) 
#if (TAO_HAS_MINIMUM_CORBA == 0)
      throw (
	 CORBA::SystemException
        , PortableServer::ForwardRequest
      );
#else
      throw (
        CORBA::SystemException
      );
#endif /* TAO_HAS_MINIMUM_CORBA == 0 */

  virtual void etherealize (const PortableServer::ObjectId &oid,
                            PortableServer::POA_ptr adapter,
                            PortableServer::Servant servant,
                            CORBA::Boolean cleanup_in_progress,
                            CORBA::Boolean remaining_activations) 
      throw (
	CORBA::SystemException
			  );

private:
  //  ContainerImpl * m_container;

};

 }; 

#endif /*maciServantManager_h*/ 



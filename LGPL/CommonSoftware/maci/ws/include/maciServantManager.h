#ifndef maciServantManager_h
#define maciServantManager_h

/*******************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciServantManager.h,v 1.84 2004/09/02 15:25:26 bjeram Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2001/05/19  redesigned
*/

#include <acsutil.h>
#include <tao/PortableServer/PortableServer.h>

NAMESPACE_BEGIN(maci);

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

NAMESPACE_END(maci);

#endif /*maciServantManager_h*/ 



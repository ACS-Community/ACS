#ifndef maciServantManager_h
#define maciServantManager_h

/*******************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciServantManager.h,v 1.87 2008/10/01 02:40:28 cparedes Exp $"
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

  /*
  * @throw PortableServer::ForwardRequest
  */
  virtual PortableServer::Servant incarnate (const PortableServer::ObjectId &oid,
                                             PortableServer::POA_ptr poa); 

  virtual void etherealize (const PortableServer::ObjectId &oid,
                            PortableServer::POA_ptr adapter,
                            PortableServer::Servant servant,
                            CORBA::Boolean cleanup_in_progress,
                            CORBA::Boolean remaining_activations); 

private:
  //  ContainerImpl * m_container;

};

 }; 

#endif /*maciServantManager_h*/ 



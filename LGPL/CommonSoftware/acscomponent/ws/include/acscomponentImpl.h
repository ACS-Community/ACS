#ifndef acscomponentImpl_H
#define acscomponentImpl_H

/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
/************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: acscomponentImpl.h,v 1.33 2011/11/18 15:10:42 rtobar Exp $"
*
* who       when        what
* --------  ----------  -------------------------------------------------
* rcirami   2003/08/28  created

*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "acsutil.h"
#include "logging.h"
#include "loggingLoggable.h"

#include <acscomponentS.h>
#include <acscomponentC.h>
#include <acsContainerServices.h>
#include <acsErrTypeLifeCycle.h>
#include "lokiSmartPtr.h"

namespace acscomponent {

/**
 * This class implements the ACS dynamic component, without any entry in the CDB,
 * and provides life cycle methods.
 * Characteristic components derive from this class.
 */
class ACSComponentImpl : public virtual PortableServer::RefCountServantBase,
			 public virtual POA_ACS::ACSComponent,
			 public Logging::Loggable
{
 public:

  /**
   * Constructor.
   * The ACSComponent shall be considered an abstract class
   * and at the end of the constructor the state of the
   * ACSComponent is set to COMPSTATE_NEW (m_componentState = ACS::COMPSTATE_NEW).
   * @param poa poa which will activate this and also all other Components
   * @param name ACSComponent name
   * @param containerServices  pointer to services provided by the container
   */
  ACSComponentImpl(
    const ACE_CString& name,
    maci::ContainerServices *containerServices);

  /**
   * Destructor
   *
   * Note: ContainerServices is not availble in the destructor
   */
  virtual ~ACSComponentImpl();

  /**
   * Get POA reference
   * This function is used to return m_poa because inherited classes would not
   * have access to it otherwise.
   *
   * @return POA reference
   * @htmlonly
   * <br><hr>
   * @endhtmlonly
   */
  PortableServer::POA_var getPOA() { return m_containerServices_p->getPOA(); }

/* ------------------ [ ACSComponent interface ] ------------------ */

  /**
   * Property for the name of the ACSComponent
   * The string returned by this method is the actual name of this instance
   * which the manager can convert to a DO reference if a client has the right
   * access level.
   *
   * @return Name of DO
   * @htmlonly
   * <br><hr>
   * @endhtmlonly
   */
  virtual char * name ();

  /**
   * Property for the state of the ACSComponent
   * @return state of the ACSComponent
   *
   * @htmlonly
   * <br><hr>
   * @endhtmlonly
   */
  virtual ::ACS::ComponentStates componentState ();

    /*************** Life Cycle methods ***************/


  /**
   * Called to give the component time to initialize itself.
   * For instance, the component could retrieve connections, read in
   * configuration files/parameters, build up in-memory tables, ...
   * Called before {@link #execute}.
   * In fact, this method might be called quite some time before
   * functional requests can be sent to the component.
   * Must be implemented as a synchronous (blocking) call.
   *
   * @return void
   * @htmlonly
   * <br><hr>
   * @endhtmlonly
   */
    virtual void initialize();

    /**
     * Called after {@link #initialize} to tell the
     * component that it has to be ready to accept
     * incoming functional calls any time.
     * Must be implemented as a synchronous (blocking) call
     * (can spawn threads though).
     *
     * @return void
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */
    virtual void execute();

    /**
     * Called after the last functional call to the component has finished.
     * The component should then orderly release resources etc.
     * If this method is overwritten in a subclass,
     * the developer has to make sure that all cleanup performed
     * by the implementation of the base class take place.
     * The best way to do this is to call the implementation of the base
     * itself explicitly, as would be done implicitly in a destructor chain.
     *
     * @return void
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */
    virtual void cleanUp();

    /**
     * Called when due to some error condition the component is about
     * to be forcefully removed
     * some unknown amount of time later (usually not very much...).
     * The component should make an effort to die as neatly as possible.
     * Because of its urgency, this method will be called asynchronously
     * to the execution of any other method of the component.
     *
     * @return void
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */
    virtual void aboutToAbort();

    /**
     * The function starts the thread before calling  execute
     *
     * @return void
     */
    virtual void __execute();

    /**
     * The function stops the threads before calling  aboutToAbort()
     *
     * @return void
     */
    virtual void __aboutToAbort();

    /**
     * The function stops the threads before calling  cleanUp()
     *
     * @return void
     */
    virtual void __cleanUp();

    /**
     * The __initialize simply calls initialize
     * (added for uniformity with the other life cycle function)
     *
     * @return void
     */
    virtual void __initialize();

    /**
     * Get a pointer to the services provided by the container which
     * hosts the component
     *
     * @return A pointer to the container services
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */
    maci::ContainerServices *getContainerServices();

 private:

    /** Name of the component
     */
    ACE_CString m_name;

    /** Smart pointer of the container services
     */
    //std::auto_ptr<maci::ContainerServices> m_containerServices_p;
    Loki::SmartPtr<maci::ContainerServices> m_containerServices_p;

};

} // namespace acscomponent


#endif


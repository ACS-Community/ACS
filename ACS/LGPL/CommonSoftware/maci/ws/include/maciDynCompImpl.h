#ifndef MACIDYNCOMPIMPL_H
#define MACIDYNCOMPIMPL_H
/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: maciDynCompImpl.h,v 1.11 2008/10/09 07:05:37 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* acaproni  2005-02-28  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acscomponentImpl.h>
#include <acsContainerServices.h>
#include <acsErrTypeLifeCycle.h>
#include <maciTestS.h>

/**
 * The component loaded from the component that tests the ContainerServices
 * It is also used to test the container against the exceptions in the
 * constructor and the life cycle methods (see the launchException_m
 * variable below)
 */
class DynamicTestClassImpl: public virtual acscomponent::ACSComponentImpl,
                     public virtual POA_MACI_TEST::DynamicTestClass
{
	public:
	/// Constructors
  DynamicTestClassImpl(
    const ACE_CString& name,
    maci::ContainerServices* containerServices);

  /**
   * Destructor
   */
  virtual ~DynamicTestClassImpl();
  
  /**
    * LifeCycle
    * @throw acsErrTypeLifeCycle::LifeCycleExImpl
    */
  virtual void initialize();

    /**
     * LifeCycle
    * @throw acsErrTypeLifeCycle::LifeCycleExImpl
     */
    virtual void execute();

    /**
     * LifeCycle
     */
    virtual void cleanUp();

    /**
     * LifeCycle
     */
    virtual void aboutToAbort();

  /* ----------------------------------------------------------------*/
  /* --------------------- [ CORBA interface ] ----------------------*/
  /* ----------------------------------------------------------------*/

  // Log the name of the component
  virtual void whoami() ; 
  
  // The following variable (read from CDB and with a default value of 0
  // set in the schema) is used to signal that we whish the component
  // to throw an exception in one method.
  // It is used by the life cycle test to check the robustness of the
  // container
  //
  // launchException_m has the following meaning:
  // 0: no exception (default)
  // 1: exception in the constructor
  // 2: exception in initialize
  // 3: exception in execute
  // 4: exception in cleanUp
  // 5: exception in aboutToAbort
  int launchException_m;

};

#endif /*!MACIDYNCOMPIMPL_H*/


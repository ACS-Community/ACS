#ifndef maciContainerServicesTestClassImpl_h
#define maciContainerServicesTestClassImpl_h

/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciContainerServicesTestClassImpl.h,v 1.13 2007/09/03 06:07:12 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni   2005-02-28 created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutil.h>
#include <acscomponentImpl.h>
#include <acsContainerServices.h>
#include <maciTestS.h>
#include <string>

// The IDL of the dynamic components
#define IDLTYPE "IDL:alma/MACI_TEST/DynamicTestClass:1.0"
// The name of 2 remote components to get
# define COMPNAME "MACI_DYN_TEST1"
# define COMPNAME2 "MACI_DYN_TEST2"

class MaciContainerServicesTestClassImpl: public virtual acscomponent::ACSComponentImpl,
                     public virtual POA_MACI_TEST::ContainerServicesTestClass
{
			
public:

  /// Constructors
  MaciContainerServicesTestClassImpl(
    const ACE_CString& name, 
    maci::ContainerServices* containerServices);

  /**
   * Destructor
   */
  virtual ~MaciContainerServicesTestClassImpl();
  
  /**
    * LifeCycle
    */
  virtual void initialize()
      throw (acsErrTypeLifeCycle::LifeCycleExImpl);

    /**
     * LifeCycle
     */
    virtual void execute()
      throw (acsErrTypeLifeCycle::LifeCycleExImpl);

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

  // Test the defaultComponent activation
  virtual void getComponentTest() throw (CORBA::SystemException);
  
  // Test the getting a Component as Non Sticky 
  virtual void getComponentNonStickyTest() throw (CORBA::SystemException);

  // Test the dynamic component activation
  virtual void dynamicComponentTest() throw (CORBA::SystemException); 
  
  // Test the collocated component activation
  virtual void collocatedComponentTest() throw (CORBA::SystemException); 
  
  // Test the defaultComponent activation
  virtual void defaultComponentTest() throw (CORBA::SystemException);
  
  // Test the request of a component descriptor
  virtual void componentDescriptorTest() throw (CORBA::SystemException);

  // Test the relase of all components
  virtual void releaseResourcesTest() throw (CORBA::SystemException);

  // Test the component listener
  virtual void componentListenerTest() throw (CORBA::SystemException);

};

#endif   /* maciContainerServicesTestClassImpl_h */






#ifndef maciContainerServicesTestClassImpl_h
#define maciContainerServicesTestClassImpl_h

/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciContainerServicesTestClassImpl.h,v 1.16 2008/10/09 07:05:37 cparedes Exp $"
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

  // Test the defaultComponent activation
  virtual void getComponentTest() ;
  
  // Test the getting a Component as Non Sticky 
  virtual void getComponentNonStickyTest() ;

  // Test the dynamic component activation
  virtual void dynamicComponentTest(); 
  
  // Test the collocated component activation
  virtual void collocatedComponentTest(); 
  
  // Test the defaultComponent activation
  virtual void defaultComponentTest();
  
  // Test the defaultComponentSmartPtr activation
  virtual void getComponentSmartPtrTest();
  
  // Test the getting a Component smart pointer as Non Sticky 
  virtual void getComponentNonStickySmartPtrTest();

  // Test the dynamic component smart pointer activation
  virtual void dynamicComponentSmartPtrTest(); 
  
  // Test the collocated component smart pointer activation
  virtual void collocatedComponentSmartPtrTest(); 
  
  // Test the defaultComponent smart pointer activation
  virtual void defaultComponentSmartPtrTest();
  
  // Test the request of a component descriptor
  virtual void componentDescriptorTest();

  // Test the relase of all components
  virtual void releaseResourcesTest();

  // Test the component listener
  virtual void componentListenerTest();

};

#endif   /* maciContainerServicesTestClassImpl_h */






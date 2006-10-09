#ifndef maciTestClassImpl_h
#define maciTestClassImpl_h

/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciTestClassImpl.h,v 1.88 2006/10/09 06:13:39 gchiozzi Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran 2002-07-05 added hierarchical COB
* msekoran 2002-05-17 MaciTestConstructableClass fixed
* kzagar   2002-02-15 created
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
#include <maciTestS.h>
#include <acsContainerServices.h>

/**
 * The class MaciTestClass implements a basic component object with only one method
 * whose purpose is to test all server-side MACI functionality.
 *
 * @author <a href=mailto:klemen.zagar@ijs.si>Klemen Zagar</a>,
 * Jozef Stefan Institute, Slovenia<br>
 * @version "@(#) $Id: maciTestClassImpl.h,v 1.88 2006/10/09 06:13:39 gchiozzi Exp $"
 */

class MaciTestClass: public virtual acscomponent::ACSComponentImpl,
                     public virtual POA_MACI_TEST::MaciTestClass
{
public:

  /// Constructors
  MaciTestClass (
    const ACE_CString& name,
    maci::ContainerServices* containerServices);

  /**
   * Destructor
   */
  virtual ~MaciTestClass();

  /**
   * Initialization status
   * @return initialization status
   */
  int initialization() { return m_initialization; }

  /* ----------------------------------------------------------------*/
  /* --------------------- [ CORBA interface ] ----------------------*/
  /* ----------------------------------------------------------------*/

    virtual CORBA::Boolean test ()               
	throw (CORBA::SystemException);

    virtual CORBA::Object_ptr get_component (const char *cob_url,
					     CORBA::Boolean activate)
	throw (CORBA::SystemException);

    virtual CORBA::Long release_component (const char *cob_url)                                    
	throw (CORBA::SystemException);

protected:

    /// Initialization status
    int m_initialization;

    /// The name of the component
    ACE_CString m_name;
};



class MaciHierarchicalTestClass : public MaciTestClass
{
public:

  /// Constructor
  MaciHierarchicalTestClass (
    const ACE_CString& name,
    maci::ContainerServices* containerServices);

  /**
   * Destructor
   */
  virtual ~MaciHierarchicalTestClass();

 virtual void execute()
      throw (acsErrTypeLifeCycle::LifeCycleExImpl);
      

};

#endif   /* maciTestClassImpl_h */






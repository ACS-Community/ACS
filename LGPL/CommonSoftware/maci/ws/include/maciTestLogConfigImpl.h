#ifndef maciTestLogConfigImpl_h
#define maciTestLogConfigImpl_h

/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciTestLogConfigImpl.h,v 1.2 2008/10/01 02:40:28 cparedes Exp $"
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
 * The class LogConfigTestClass implements a basic component object with only one method
 * whose purpose is to test all server-side MACI functionality.
 *
 * @author <a href=mailto:klemen.zagar@ijs.si>Klemen Zagar</a>,
 * Jozef Stefan Institute, Slovenia<br>
 * @version "@(#) $Id: maciTestLogConfigImpl.h,v 1.2 2008/10/01 02:40:28 cparedes Exp $"
 */

class LogConfigTestClass: public virtual acscomponent::ACSComponentImpl,
                     public virtual POA_MACI_TEST::LogConfigTestClass
{
public:

  /// Constructors
  LogConfigTestClass (
    const ACE_CString& name,
    maci::ContainerServices* containerServices);

  /**
   * Destructor
   */
  virtual ~LogConfigTestClass();

  /**
   * Initialization status
   * @return initialization status
   */

  /* ----------------------------------------------------------------*/
  /* --------------------- [ CORBA interface ] ----------------------*/
  /* ----------------------------------------------------------------*/

    virtual void LogConfigTestClass::log_all ();

};

#endif   /* maciTestLogConfigImpl_h */


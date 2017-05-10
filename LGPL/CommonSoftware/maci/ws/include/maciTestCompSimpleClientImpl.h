#ifndef maciTestCompSimpleClientImpl_h
#define maciTestCompSimpleClientImpl_h

/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciTestCompSimpleClientImpl.h,v 1.92 2011/06/07 23:56:38 javarias Exp $"
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
 * The class MaciTestCompSimpleClient implements a basic component object with only one method
 * whose purpose is to test all server-side MACI functionality.
 *
 * @author <a href=mailto:klemen.zagar@ijs.si>Klemen Zagar</a>,
 * Jozef Stefan Institute, Slovenia<br>
 * @version "@(#) $Id: maciTestCompSimpleClientImpl.h,v 1.92 2011/06/07 23:56:38 javarias Exp $"
 */

class MaciTestCompSimpleClient: public virtual acscomponent::ACSComponentImpl,
                     public virtual POA_MACI_TEST::MaciTestCompSimpleClient
{
public:

  /// Constructors
  MaciTestCompSimpleClient (
    const ACE_CString& name,
    maci::ContainerServices* containerServices);

  /**
   * Destructor
   */
  virtual ~MaciTestCompSimpleClient();


  /* ----------------------------------------------------------------*/
  /* --------------------- [ CORBA interface ] ----------------------*/
  /* ----------------------------------------------------------------*/
	
    virtual void createSimpleClient();

protected:

};





#endif   /* maciTestCompSimpleClientImpl_h */






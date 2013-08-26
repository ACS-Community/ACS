#ifndef loggingHelper_H_
#define loggingHelper_H_

/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration)
*    and Cosylab 2002, All rights reserved
*
*    This library is free software; you can redistribute it and/or
*    modify it under the terms of the GNU Lesser General Public
*    License as published by the Free Software Foundation; either
*    version 2.1 of the License, or (at your option) any later version.
*
*    This library is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*    Lesser General Public License for more details.
*
*    You should have received a copy of the GNU Lesser General Public
*    License along with this library; if not, write to the Free Software
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: loggingHelper.h,v 1.37 2012/01/20 22:07:44 tstaig Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2001/12/23  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutil.h>

#include <orbsvcs/CosNamingC.h>

/**
 * LoggingHelper class is a class helping to handle commonly used operations.
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 * @version "@(#) $Id: loggingHelper.h,v 1.37 2012/01/20 22:07:44 tstaig Exp $"
 */

class LoggingHelper
{

public:

  /**
   * <i>resolveNameService</i> method is a method helping to resolve CORBA NameService's reference.
   * The NameService reference is defined by the first valid of the following options:
   *    # Command line option <i>-ORBInitRef NameService=<corbaloc reference> (e.g. corbaloc::te1.hq.eso.org:xxxx)</i>
   *      using CORBA::ORB::resolve_initial_references("NameService"), ORB has to be already initialized with the command line
   *    # Environment variable NAMESERVICE_REFERENCE
   *    # corbaloc::<hostname>:xxxx/NameService
   * 
   * @param orb CORBA ORB
   * @param retries number of retries resolving NameService reference, <= 0 means infinite
   * @param secTimeout timeout expressed in seconds, == 0 means inifinite (dt between retries is 3secs)
   * @return CORBA reference to the NameService's root CosNaming::NamingContext
   */
  static CosNaming::NamingContext_ptr resolveNameService(CORBA::ORB_ptr orb,
							 int retries = 3, unsigned int secTimeout = 0);
  /**
   * <i>resolveNameService</i> method resolve given stringified CORBA reference to the NameService.
   * @param orb CORBA ORB
   * @param reference stringified CORBA reference to the NameService
   * @param retries number of retries resolving NameService reference, <= 0 means infinite
   * @param secTimeout timeout expressed in seconds, == 0 means inifinite (dt between retries is 3secs)
   * @return CORBA reference to the NameService's root CosNaming::NamingContext or CosNaming::NamingContext::_nil() if unable to resolve NameService reference
   */
  static CosNaming::NamingContext_ptr resolveNameService(CORBA::ORB_ptr orb,
							 const ACE_TCHAR * reference, 
							 int retries = 3, unsigned int secTimeout = 0);

  /**
   * Terminates all resolving processes
   * @param terminate true is all resolving processes are to be cancelled (default), false to set termiante flag to false
   */
  static void terminateResolving(bool terminate = true);

private:

  static bool m_terminate;


};

#endif  /* loggingHelper_H_ */

// ************************************************************************
//
// REVISION HISTORY:
//
// $Log: loggingHelper.h,v $
// Revision 1.37  2012/01/20 22:07:44  tstaig
// Backport from branches ACS-9_0_0-windows-B and ACS-9_1_0-windows-B to support
// ACS on Windows under Cygwin. This commit corresponds to the folowing
// CommonSoftware modules:
// jacsutil acsEclipseUtils xmljbind xmlpybind acserridl acsidlcommon acsutil
// acsutilpy acsstartup loggingidl logging acserr acserrTypes acsQoS
// Along with adding dependencies for some libraries in acsdaemon and acstime
// modules so they would be built correctly.
//
// Revision 1.36  2003/10/15 20:17:26  dfugate
// Naming Service port is now dynamic.
//
// Revision 1.35  2003/05/23 09:18:47  msekoran
// Replaced exponential backoff with 3s retries.
//
// Revision 1.34  2003/03/14 10:24:37  rgeorgie
// LGPL
//
// Revision 1.33  2002/09/23 12:43:06  vltsccm
// msekoran: loggingXMLParser fixed, memory leak removed and tat test added.
//
// Revision 1.32  2002/04/10 14:41:39  vltsccm
// logging1.32
//
// Revision 1.31  2002/03/27 16:44:24  vltsccm
// logging1.31
//
// Revision 1.30  2002/02/13 12:55:32  vltsccm
// logging1.30
//
// Revision 1.29  2002/02/08 13:40:55  vltsccm
// logging1.29
//
// Revision 1.28  2002/02/05 17:51:55  vltsccm
// logging1.28
//
// Revision 1.27  2002/02/04 08:26:29  vltsccm
// logging1.27
//
// Revision 1.26  2002/01/18 09:42:59  vltsccm
// logging1.26
//
// Revision 1.25  2002/01/16 10:41:29  vltsccm
// logging1.25
//
// Revision 1.24  2002/01/15 12:42:20  vltsccm
// logging1.24
//
// Revision 1.23  2002/01/14 21:10:49  vltsccm
// logging1.23
//
// Revision 1.22  2001/12/27 19:04:10  vltsccm
// logging1.22
//
// Revision 1.21  2001/12/24 13:31:33  vltsccm
// logging1.21
//
// Revision 1.20  2001/12/24 13:31:33  vltsccm
// logging1.20
//
// Revision 1.19  2001/12/24 13:31:32  vltsccm
// logging1.19
//
// Revision 1.18  2001/12/24 13:31:32  vltsccm
// logging1.18
//
// Revision 1.17  2001/12/24 13:31:32  vltsccm
// logging1.17
//
// Revision 1.16  2001/12/24 13:31:31  vltsccm
// logging1.16
//
// Revision 1.15  2001/12/24 13:31:31  vltsccm
// logging1.15
//
// Revision 1.14  2001/12/24 13:31:31  vltsccm
// logging1.14
//
// Revision 1.13  2001/12/24 13:31:30  vltsccm
// logging1.13
//
// Revision 1.12  2001/12/24 13:31:30  vltsccm
// logging1.12
//
// Revision 1.11  2001/12/24 13:31:30  vltsccm
// logging1.11
//
// Revision 1.10  2001/12/24 13:31:30  vltsccm
// logging1.10
//
// Revision 1.9  2001/12/24 13:31:29  vltsccm
// logging1.9
//
// Revision 1.8  2001/12/24 13:31:29  vltsccm
// logging1.8
//
// Revision 1.7  2001/12/24 13:31:29  vltsccm
// logging1.7
//
// Revision 1.6  2001/12/24 13:31:28  vltsccm
// logging1.6
//
// Revision 1.5  2001/12/24 13:31:28  vltsccm
// logging1.5
//
// Revision 1.4  2001/12/24 13:31:28  vltsccm
// logging1.4
//
// Revision 1.3  2001/12/24 13:31:27  vltsccm
// logging1.3
//
// Revision 1.2  2001/12/24 13:31:27  vltsccm
// logging1.2
//
// Revision 1.1  2001/12/24 13:31:27  vltsccm
// logging1.1
//
// Revision 1.0  2001/12/24 13:31:27  vltsccm
// logging1.0
//
//
// ************************************************************************

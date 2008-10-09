#ifndef acsncServiceChecker_H
#define acsncServiceChecker_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) Associated Universities Inc., 2007 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: acsncServiceChecker.h,v 1.3 2008/10/09 07:57:41 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* nbarriga  2007-10-12  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

/* The following piece of code alternates the linkage type to C for all
functions declared within the braces, which is necessary to use the
functions in C++-code.
*/


#include <acsncHelper.h>
namespace nc{
    class ServiceChecker: public Helper{
	public:
	    ServiceChecker(CORBA::ORB_ptr orb);
	    bool check(const std::string domain);
	private:
        /*
        * @throw ACSErrTypeCommon::CORBAProblemEx
        */
	    void resolveNotificationFactory(const std::string factoryName);
    };
}




#endif 

#ifndef _timeout_testImpl_H_
#define _timeout_testImpl_H_
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2004 
*
*This library is free software; you can redistribute it and/or
*modify it under the terms of the GNU Lesser General Public
*License as published by the Free Software Foundation; either
*version 2.1 of the License, or (at your option) any later version.
*
*This library is distributed in the hope that it will be useful,
*but WITHOUT ANY WARRANTY; without even the implied warranty of
*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*Lesser General Public License for more details.
*
*You should have received a copy of the GNU Lesser General Public
*License along with this library; if not, write to the Free Software
*Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: timeoutTestImpl.h,v 1.5 2008/09/29 09:42:48 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2004-08-24  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "timeoutTestS.h"

class TimeOutTestImpl : public POA_TimeOutTest
{
	public:
	
		/**
		 * Constructor.
		 *
		 * @param orb the ptr to the orb to use for the test.
		 */
		TimeOutTestImpl (CORBA::ORB_ptr orb);

		/**
		 * Method to test the acsQoS capabilities for timeouts.
		 *
		 * @param x parameter accepted as input, which will also be "echoed" back as output.
		 * @param msecs the number of milliseconds that this method will delay/sleep before returning. This
		 *        can be used to trigger, or not trigger, a timeout for testing purposes, depending on the value
		 *        of the timeout in relation to this value.
		 * @return the value passed in as the input parameter, x, "echoed" back as output.
		 */
		CORBA::Long echo (CORBA::Long x, CORBA::Long msecs) ;

		/**
		 * Used to gracefully exit the test after the client is finished. The client must call this method when 
		 * finished testing the servant, in order to prevent the servant process from hanging around indefinitely.
		 */
		void shutdownOrb();

	private:

		CORBA::ORB_var orb_;
};

#endif /*!_H*/

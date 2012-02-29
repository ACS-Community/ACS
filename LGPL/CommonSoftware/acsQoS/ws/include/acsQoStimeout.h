#ifndef _acsQoS_TimeOut_H_
#define _acsQoS_TimeOut_H_
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
* "@(#) $Id: acsQoStimeout.h,v 1.9 2012/02/29 12:50:09 tstaig Exp $"
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

#include "tao/Messaging/Messaging.h"
#include "acsQoSErrType.h" // error stuff
#include "acsQoSExport.h"

namespace acsQoS
{

class acsQoS_EXPORT Timeout
{
	public:

		/**
		 * Constructor.
		 * 
		 * @param timeout the timeout expressed in milliseconds.
		 */
		Timeout(unsigned long timeout);
    
		/**
		 * Destructor.
		 */
		~Timeout();
    
		/**
		 * Method to get the timeout, in milliseconds.
		 * 
		 * @return the timeout expressed in milliseconds.
		 */
		unsigned long get() { return timeout_m; }
    
		/**
		 * Method to set the timeout, in milliseconds.
		 * 
		 * @param timeout the timeout expressed in milliseconds.
		 */
		void set(unsigned long timeout) { timeout_m = timeout; set(); }
  
		/**
		 * Sets the ORB level timeout. The timeout can be set either on the orb that was passed 
		 * (previously) to the init method, or on the orb that is passed as a second parameter.
		 *
		 * @param timeout the timeout value desired expressed in milliseconds.
		 * @param _orb the orb on which to set the timeout.
		 */
		static void setORBTimeout(unsigned long timeout, CORBA::ORB_ptr _orb =  CORBA::ORB::_nil());

		/**
		 * Sets the timeout on the level of an individual CORBA object. Note that the user
		 * must use the returned object reference for invoking calls in order for the timeout
		 * be used.
		 *
		 * @param timeout the timeout in milliseconds.
		 * @param obj a CORBA object on which to set the timeout.
		 * @return pointer to the new CORBA object which can be used to 
		 * invoke methods with the assigned timeout.
		 */
		template <class T>
		static T* setObjectTimeout(unsigned long timeout, T *obj)
		{
			try
			{
				//convert to 100's of nsecs (needed by TimeT). This calculation may seem 
				// confusing at first, but think about it this way: one millisecond equals how many
				// "hundreds of nanoseconds". The answer is: ten thousand milliseconds is equivalent
				// to a single hundred of nanosecs, so we multiply by 10000 to convert from milliseconds to 100 of ns.
				// Still confused? Then grab a pencil and paper to convince yourself.
				TimeBase::TimeT to = timeout * 10000;    
				CORBA::Any anyTimeOut;
				anyTimeOut <<= to;
    
				CORBA::PolicyList policyList;
				policyList.length (1);
				policyList[0] = orb_m->create_policy (Messaging::RELATIVE_RT_TIMEOUT_POLICY_TYPE, anyTimeOut);

				// we will set timeout at object level
				CORBA::Object_var newObj = obj->_set_policy_overrides (policyList, CORBA::SET_OVERRIDE);

				policyList[0]->destroy();
				return T::_narrow(newObj.in());
			}
			catch (CORBA::Exception &cex)
			{
				acsQoSErrType::CanNotSetTimeoutExImpl ex(__FILE__, __LINE__, "Timeout::setObjectTimeout");
				ex.addData("Caused by CORBA exception", cex._name());
				throw ex;
			}
		}

		/**
		 * Initializes the acsQoS functionality. In order to use acsQoS features such as timeouts, 
		 * things must first be initialised. This is normally done automatically in the container 
		 * and in the simple client, so the user does not have to take care 
		 * of this (i.e. the user does not normally need to call this method explicitly).  
		 *
		 * @param _orb a reference to the ORB to be "prepared" or initialized in order to use QoS features.
		 */
		static void init(CORBA::ORB_ptr _orb);

		/**
		 * Method to check to see if the acsQoS functionality has been initialized.
		 *
		 * @return boolean indicating whether the acsQoS functionality has been initialized (true) or not (false).
		 */
		static bool isInitialized();

		/**
		 * Method to "cleanup" the acsQoS functionality. In order to use acsQoS features such as timeouts, 
		 * things must first be initialised and should also be cleaned up when such features are not needed any 
		 * longer. This is normally done automatically during shutdown of the container 
		 * and/or the simple client, so the user does not have to take care of this 
		 * (i.e. the user does not normally need to call this method explicitly).  
		 *
		 */
		static void done();

	private:

		void set();
	 
		unsigned long timeout_m;

		CORBA::PolicyList policyList_m;    // policy list for setting timeout

		CORBA::PolicyCurrent_var policyCurrent_m;

		CORBA::PolicyList previousPolicy_m;    // here we'll store previously set timeout
    
		static CORBA::ORB_var orb_m;
		static bool initialized_m;
};//class Timeout

};//namespace acsQoS 

#endif /*!_H*/

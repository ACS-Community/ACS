#ifndef _acsQoS_H_
#define _acsQoS_H_
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
* "@(#) $Id: acsQoS.h,v 1.4 2006/02/10 20:41:03 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2004-08-25  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "acsQoStimeout.h"

namespace  acsQoS
{
		/**
		 * Initializes the acsQoS functionality. In order to use acsQoS features such as timeouts, 
		 * things must first be initialised. This is normally done automatically in the container 
		 * and in the simple client, so the user does not have to take care 
		 * of this (i.e. the user does not normally need to call this method explicitly).  
		 *
		 * @param _orb a reference to the ORB to be "prepared" or initialized in order to use QoS features.
		 */
		bool init (CORBA::ORB_ptr _orb);

		/**
		 * Method to check to see if the acsQoS functionality has been initialized.
		 *
		 * @return boolean indicating whether the acsQoS functionality has been initialized (true) or not (false).
		 */
		bool isInitialized();

		/**
		 * Method to "cleanup" the acsQoS functionality. In order to use acsQoS features such as timeouts, 
		 * things must first be initialised and should also be cleaned up when such features are not needed any 
		 * longer. This is normally done automatically during shutdown of the container 
		 * and/or the simple client, so the user does not have to take care of this 
		 * (i.e. the user does not normally need to call this method explicitly).  
		 *
		 */
		void done();
};

#endif 

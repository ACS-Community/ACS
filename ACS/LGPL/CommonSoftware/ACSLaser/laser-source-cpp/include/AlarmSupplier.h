#ifndef alarm_supplier_H
#define alarm_supplier_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) National Research Council of Canada, 2005 
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
* "@(#) $Id: AlarmSupplier.h,v 1.7 2009/10/11 08:17:23 acaproni Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-11-15  created
* sharring 2005-11-22  documented
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <basencSupplier.h>
#include <acscommonC.h>
#include <logging.h>
#include "ASIMessage.h"

/**
 * This class is used to encapsulate functionality related to the
 * CORBA Notification Service to send alarms to the laser alarm server 
 * over a CORBA notification channel. It extends the BaseSupplier class 
 * from the basenc module in ACS.
 */
class AlarmSupplier : public BaseSupplier
{
	public:
    
		/**
		 * Constructor.
		 * @param channelName the name of the notification channel to use 
		 *        when sending events to the laser alarm server.
		 */
		AlarmSupplier(const char* channelName);

		/**
		 * Destructor.
		 */
		virtual ~AlarmSupplier();

		/**
		 * Method to publish an event to the LASER alarm server.
		 * @throw acsncErrType::PublishEventFailureExImpl
		 * 		 Cannot publish the event in the Notification Channel
		 */
		void publishEvent(acsalarm::ASIMessage &msg);
	
	protected:

		/**
		 * Overrides method from BaseSupplier.
		 */
		virtual const char* getChannelKind() { return acscommon::NC_KIND; }

		/**
		 * Overrides method from BaseSupplier.
		 */
		virtual const char* getEventType() { return "ACSJMSMessageEntity"; }

};


#endif /*!alarm_supplier_H*/

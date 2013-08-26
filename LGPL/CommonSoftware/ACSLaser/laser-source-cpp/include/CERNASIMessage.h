#ifndef CERNASIMESSAGE_H
#define CERNASIMESSAGE_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2012 
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
* "@(#) $Id: CERNASIMessage.h,v 1.1 2012/04/05 13:21:00 acaproni Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2012-04-02  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <string>

#include "ASIMessage.h"

namespace laserSource
{
	/**
	 * CERNASIMessage holds a reference to ASIMessage to build the XML of the source alarms
	 * that will be published into the NC.
	 *
	 * The reason to have this method here is that in ACS/LGPL/CommonSoftware the
	 * ASI-Message name space is not yet known neither needed because the ACS
	 * implementation only logs a message for each alarm.
	 */
	class CERNASIMessage
	{
	private:
		/**
		 * The (not NULL) ASIMessage to produce the XML
		 */
		acsalarm::ASIMessage& m_message;
	public:

		/**
		 * Constructor
		 *
		 * @param message The (not NULL) ASIMessage to produce the XML
		 */
		CERNASIMessage(acsalarm::ASIMessage& message);

		/**
		 * builds the xml representation of the message which will be sent to the alarm server
		 */
		 std::string toXML();

	public:
		 /**
		  * Returns an XML fragment (NOT a complete document) representing the timestamp, for
		  * use in the message that is transported from alarm source to alarm server.
		  *
		  * @param elementName the element name when generating the XML fragment,
		  *        for instance in the example below the elementName is "source-timestamp"
		  *
		  * For example:
		  *
		  * <source-timestamp seconds="1129902763" microseconds="132000"/>
		  *
		  *
		  * @param amountToIndent - used to specify a level of indentation (in spaces) for readability
		  */
		 std::string timestampToXML(long secs, long usecs, std::string elementName, int amountToIndent);

		 /**
		  * Returns an XML representation of the fault state. NOTE: this
		  * will not be a complete XML document, but just a fragment.
		  *
		  * @param state The FaultState to format
		  * @param amountToIndent the amount (in spaces) to indent for readability
		  *
		  *
		  * For example:
		  *
		  * <fault-state family="AlarmSource" member="ALARM_SOURCE_ANTENNA" code="1">
		  *     <descriptor>TERMINATE</descriptor>
		  *     <user-properties>
		  *        <property name="ASI_PREFIX" value="prefix"/>
		  *        <property name="TEST_PROPERTY" value="TEST_VALUE"/>
		  *        <property name="ASI_SUFFIX" value="suffix"/>
		  *     </user-properties>
		  *     <user-timestamp seconds="1129902763" microseconds="105000"/>
		  *  </fault-state>
		  */
		 std::string faultStateToXML(acsalarm::FaultState* state, int amountToIndent=3);

		 /**
		 *  Returns an XML fragment (NOT a complete document) representing all of
		 *  the properties contained in this table, for use in the message transported
		 *  from an alarm source to the alarm server.
		 *  @param amountToIndent - used to specify a level of indentation (in spaces) for readability
		 */
		std::string propertiesToXML(acsalarm::Properties props,int amountToIndent = 6);
	};
};

#endif /*!CERNASIMESSAGE_H*/

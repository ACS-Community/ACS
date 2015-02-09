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
* "@(#) $Id: CERNASIMessage.cpp,v 1.1 2012/04/05 13:21:00 acaproni Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2012-04-02  created 
*/

#include "CERNASIMessage.h"
#include <sstream>
#include <vector>
#include <memory>
#include "utilConstants.h"

using laserSource::CERNASIMessage;

CERNASIMessage::CERNASIMessage(acsalarm::ASIMessage& message):
		m_message(message)
{

}

std::string CERNASIMessage::toXML()
{
	std::stringstream ret;

	// create the XML header
	// e.g. <?xml version="1.0" encoding="ISO-8859-1"?>
	ret << XML_HEADER << std::endl;

	// create the opening tag for the ASI-Message element
	// e.g. <ASI-message xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" backup="false" version="0.9" xsi:type="ASI-message">
	ret <<LESS_THAN_SIGN << ASI_MESSAGE_ELEMENT_NAME <<SPACE<<XML_NAMESPACE_PREFIX<<EQUALS_SIGN<<DOUBLE_QUOTE;
	ret <<XML_SCHEMA_URI<<DOUBLE_QUOTE<<SPACE<<ASI_MESSAGE_BACKUP_ATTRIBUTE_NAME<<EQUALS_SIGN<<DOUBLE_QUOTE;

	// output the backup flag using character representation (e.g. "true" and "false")
	// instead of numerical representation (0, 1) for boolean value.
	ret << std::boolalpha << m_message.getBackup();

	ret << DOUBLE_QUOTE<<SPACE<<ASI_MESSAGE_VERSION_ATTRIBUTE_NAME<<EQUALS_SIGN<<DOUBLE_QUOTE<<m_message.getVersion()<<DOUBLE_QUOTE;
	ret << SPACE<<XSI_TYPE_PREFIX<<EQUALS_SIGN<<DOUBLE_QUOTE<<ASI_MESSAGE_TYPE_NAME<<DOUBLE_QUOTE<<GREATER_THAN_SIGN<<std::endl;

	// create the source name element
	// e.g. <source-name>ALARM_SYSTEM_SOURCES</source-name>
	ret << SPACE << SPACE<<SPACE;
	ret << LESS_THAN_SIGN << SOURCE_NAME_ELEMENT_NAME <<GREATER_THAN_SIGN<<m_message.getSourceName()<<LESS_THAN_SIGN;
	ret << FORWARD_SLASH<<SOURCE_NAME_ELEMENT_NAME<<GREATER_THAN_SIGN<<std::endl;

	// create the source hostname element
	// e.g. <source-hostname>NEWBIE.AOC.NRAO.EDU</source-hostname>
	ret << SPACE << SPACE<<SPACE;
	ret << LESS_THAN_SIGN << SOURCE_HOSTNAME_ELEMENT_NAME<<GREATER_THAN_SIGN<<m_message.getSourceHostname()<<LESS_THAN_SIGN;
	ret << FORWARD_SLASH<<SOURCE_HOSTNAME_ELEMENT_NAME<<GREATER_THAN_SIGN<<std::endl;

	// create the source timestamp element
	// e.g. <source-timestamp seconds="1130356783" microseconds="610000"/>
	ret << timestampToXML(
			&m_message.getSourceTimestamp(),
			SOURCE_TIMESTAMP_ELEMENT_NAME,
			3);

	// create the opening tag for the fault-states element
	// e.g. <fault-states>
	ret << SPACE << SPACE<<SPACE;
	ret << LESS_THAN_SIGN<<FAULT_STATES_ELEMENT_NAME<<GREATER_THAN_SIGN<<std::endl;

	// create the fault-state element for each fault state to be sent
	//
	// e.g.
	//
	// <fault-state family="AlarmSource" member="ALARM_SOURCE_ANTENNA" code="1">
	// 	<descriptor>ACTIVE</descriptor>
	// 	<user-properties>
	// 		<property name="ASI_PREFIX" value="prefix"/>
	// 		<property name="TEST_PROPERTY" value="TEST_VALUE"/>
	// 		<property name="ASI_SUFFIX" value="suffix"/>
	// 		</user-properties>
	// 	<user-timestamp seconds="1130356783" microseconds="608000"/>
	// </fault-state>
	std::vector<acsalarm::FaultState>& faultStates = m_message.getFaultStates();
	for(unsigned int i = 0; i < faultStates.size(); i++)
	{
		ret << faultStateToXML(&faultStates[i],6);
	}

	// create the closing tag for the fault-states element
	// e.g. </fault-states>
	ret << SPACE << SPACE<<SPACE;
	ret <<LESS_THAN_SIGN<<FORWARD_SLASH<<FAULT_STATES_ELEMENT_NAME<<GREATER_THAN_SIGN<<std::endl;

	// create the closing tag for the ASI-Message element
	// e.g. </ASI-message>
	ret << LESS_THAN_SIGN<<FORWARD_SLASH<<ASI_MESSAGE_ELEMENT_NAME<<GREATER_THAN_SIGN<<std::endl;
	return ret.str();
}

std::string CERNASIMessage::timestampToXML(const acsalarm::Timestamp* timestamp, std::string elementName, int amountToIndent) const
{
	std::stringstream ret;

	for(int x = 0; x < amountToIndent; x++)
	{
		ret<<SPACE;
	}

	ret<<LESS_THAN_SIGN<<elementName<<GREATER_THAN_SIGN;
	ret<<timestamp->toISOFormat();
	ret<<LESS_THAN_SIGN<<FORWARD_SLASH<<elementName<<GREATER_THAN_SIGN<<std::endl;

	return ret.str();
}

std::string CERNASIMessage::faultStateToXML(acsalarm::FaultState* state, int amountToIndent)
{
	std::stringstream ret;

	// generate the fault-state opening element
	// e.g. <fault-state family="AlarmSource" member="ALARM_SOURCE_ANTENNA" code="1">
	for(int x = 0; x < amountToIndent; x++)
	{
		ret << SPACE;
	}

	ret << LESS_THAN_SIGN<<FAULT_STATE_ELEMENT_NAME<<SPACE;

	// output the fault's family
	ret << FAULT_STATE_FAMILY_ATTRIBUTE_NAME<<EQUALS_SIGN<<DOUBLE_QUOTE<<state->getFamily()<<DOUBLE_QUOTE<<SPACE;

	// output the fault's member
	ret << FAULT_STATE_MEMBER_ATTRIBUTE_NAME<<EQUALS_SIGN<<DOUBLE_QUOTE<<state->getMember()<<DOUBLE_QUOTE<<SPACE;

	// output the fault's code
	ret << FAULT_STATE_CODE_ATTRIBUTE_NAME<<EQUALS_SIGN<<DOUBLE_QUOTE<<state->getCode()<<DOUBLE_QUOTE<<GREATER_THAN_SIGN<<std::endl;

	// indent for readability
	for(int x = 0; x < amountToIndent+3; x++)
	{
		ret<<SPACE;
	}

	// generate the descriptor element
	// e.g. <descriptor>TERMINATE</descriptor>
	ret<< LESS_THAN_SIGN<<FAULT_STATE_DESCRIPTOR_ELEMENT_NAME<<GREATER_THAN_SIGN;
	ret<< state->getDescriptor();
	ret<< LESS_THAN_SIGN<<FORWARD_SLASH<<FAULT_STATE_DESCRIPTOR_ELEMENT_NAME<<GREATER_THAN_SIGN<<std::endl;

	// generate the properties element
	// e.g.
	//
	// <user-properties>
	// 	<property name="ASI_PREFIX" value="prefix"/>
	// 	<property name="TEST_PROPERTY" value="TEST_VALUE"/>
	//		<property name="ASI_SUFFIX" value="suffix"/>
	// </user-properties>
	acsalarm::Properties userProperties = state->getUserProperties();
	if(userProperties.getSize()>0)
	{
		ret<< propertiesToXML(userProperties,amountToIndent+3);
	}

	// generate the user timestamp element
	// e.g. <user-timestamp seconds="1129902763" microseconds="105000"/>
	ret<<timestampToXML(
			&state->getUserTimestamp(),
			std::string(USER_TIMESTAMP_ELEMENT_NAME),
			amountToIndent+3);

	// generate the fault-state closing element
	// e.g. </fault-state>
	for(int x = 0; x < amountToIndent; x++)
	{
		ret<<SPACE;
	}
	ret << LESS_THAN_SIGN<<FORWARD_SLASH<<FAULT_STATE_ELEMENT_NAME<<GREATER_THAN_SIGN<<std::endl;

	return ret.str();
}

std::string CERNASIMessage::propertiesToXML(acsalarm::Properties props, int amountToIndent)
{
	std::stringstream ret;

	std::auto_ptr<std::vector<std::string> > propNames=props.propertyNames();
	int propNamesSize = propNames->size();

	if(propNamesSize>0)
	{
		// Generate the user properties opening element tag
		// e.g. <user-properties>
		for(int x = 0; x < amountToIndent; x++)
		{
			ret<<SPACE;
		}
		ret<<LESS_THAN_SIGN<<USER_PROPERTIES_ELEMENT_NAME<<GREATER_THAN_SIGN<<std::endl;

		// For each property, generate the appropriate XML,
		// e.g. <property name="ASI_PREFIX" value="prefix"/>
		for (int t=0; t<propNamesSize; t++) {
			std::string propName =propNames->at(t);
			std::string propValue=props.getProperty(propName);

			for(int x = 0; x < amountToIndent + 3; x++)
			{
				ret<<SPACE;
			}
			ret <<LESS_THAN_SIGN<<USER_PROPERTIES_PROPERTY_ELEMENT_NAME<<SPACE<<USER_PROPERTIES_NAME_ATTRIBUTE_NAME;
			ret << EQUALS_SIGN<<DOUBLE_QUOTE<<propName<<DOUBLE_QUOTE<<SPACE;
			ret << USER_PROPERTIES_VALUE_ATTRIBUTE_NAME<<EQUALS_SIGN<<DOUBLE_QUOTE<<propValue<<DOUBLE_QUOTE;
			ret << FORWARD_SLASH<<GREATER_THAN_SIGN<<std::endl;
		}
		// Generate the user properties closing element tag
		// e.g. </user-properties>
		for(int x = 0; x < amountToIndent; x++)
		{
			ret<<SPACE;
		}
		ret << LESS_THAN_SIGN<<FORWARD_SLASH<<USER_PROPERTIES_ELEMENT_NAME<<GREATER_THAN_SIGN<<std::endl;
	}

	return ret.str();
}
/*___oOo___*/

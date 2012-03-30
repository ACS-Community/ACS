/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
#include "ASIMessage.h"
#include "utilConstants.h"
#include <sstream>

using std::stringstream;
using std::auto_ptr;
using std::vector;
using std::string;
using acsalarm::ASIMessage;
using acsalarm::FaultState;

/*
 * Default no-args constructor.
 */
ASIMessage::ASIMessage()
{
}

/*
 * Constructor.
 * @param states a vector of FaultState's to be sent to the laser alarm server.
 */
ASIMessage::ASIMessage(auto_ptr<vector<FaultState> > & states)
{
	setFaultStates(states);
}

/*
 * Destructor.
 */
ASIMessage::~ASIMessage()
{
}

/*
 *Builds the xml representation of the message which will be sent to the alarm server
 */ 
string ASIMessage::toXML()
{
	stringstream ret;

	// create the XML header
	// e.g. <?xml version="1.0" encoding="ISO-8859-1"?>
	ret << XML_HEADER << std::endl;

	// create the opening tag for the ASI-Message element
	// e.g. <ASI-message xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" backup="false" version="0.9" xsi:type="ASI-message">
	ret <<LESS_THAN_SIGN << ASI_MESSAGE_ELEMENT_NAME <<SPACE<<XML_NAMESPACE_PREFIX<<EQUALS_SIGN<<DOUBLE_QUOTE;
	ret <<XML_SCHEMA_URI<<DOUBLE_QUOTE<<SPACE<<ASI_MESSAGE_BACKUP_ATTRIBUTE_NAME<<EQUALS_SIGN<<DOUBLE_QUOTE;

	// output the backup flag using character representation (e.g. "true" and "false") 
	// instead of numerical representation (0, 1) for boolean value.
	ret << std::boolalpha << getBackup();

	ret << DOUBLE_QUOTE<<SPACE<<ASI_MESSAGE_VERSION_ATTRIBUTE_NAME<<EQUALS_SIGN<<DOUBLE_QUOTE<<getVersion()<<DOUBLE_QUOTE;
	ret << SPACE<<XSI_TYPE_PREFIX<<EQUALS_SIGN<<DOUBLE_QUOTE<<ASI_MESSAGE_TYPE_NAME<<DOUBLE_QUOTE<<GREATER_THAN_SIGN<<std::endl;

	// create the source name element
	// e.g. <source-name>ALARM_SYSTEM_SOURCES</source-name>
	ret << SPACE << SPACE<<SPACE;
	ret << LESS_THAN_SIGN << SOURCE_NAME_ELEMENT_NAME <<GREATER_THAN_SIGN<<getSourceName()<<LESS_THAN_SIGN;
	ret << FORWARD_SLASH<<SOURCE_NAME_ELEMENT_NAME<<GREATER_THAN_SIGN<<std::endl;

	// create the source hostname element
	// e.g. <source-hostname>NEWBIE.AOC.NRAO.EDU</source-hostname>
	ret << SPACE << SPACE<<SPACE;
	ret << LESS_THAN_SIGN << SOURCE_HOSTNAME_ELEMENT_NAME<<GREATER_THAN_SIGN<<getSourceHostname()<<LESS_THAN_SIGN;
	ret << FORWARD_SLASH<<SOURCE_HOSTNAME_ELEMENT_NAME<<GREATER_THAN_SIGN<<std::endl;

	// create the source timestamp element
	// e.g. <source-timestamp seconds="1130356783" microseconds="610000"/>
	ret << sourceTimestamp->toXML(SOURCE_TIMESTAMP_ELEMENT_NAME, 3);

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
	for(unsigned int i = 0; i < faultStates->size(); i++)
	{
		ret << (*faultStates)[i].toXML(6);
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

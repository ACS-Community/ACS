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
	string retVal;

	// create the XML header
	// e.g. <?xml version="1.0" encoding="ISO-8859-1"?>
	retVal += XML_HEADER;
	retVal += NEWLINE;

	// create the opening tag for the ASI-Message element
	// e.g. <ASI-message xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" backup="false" version="0.9" xsi:type="ASI-message">
	retVal += LESS_THAN_SIGN;
	retVal += ASI_MESSAGE_ELEMENT_NAME;
	retVal += SPACE;
	retVal += XML_NAMESPACE_PREFIX;
	retVal += EQUALS_SIGN;
	retVal += DOUBLE_QUOTE;
	retVal += XML_SCHEMA_URI;
	retVal += DOUBLE_QUOTE;
	retVal += SPACE;
	retVal += ASI_MESSAGE_BACKUP_ATTRIBUTE_NAME;
	retVal += EQUALS_SIGN;
	retVal += DOUBLE_QUOTE;

	// output the backup flag using character representation (e.g. "true" and "false") 
	// instead of numerical representation (0, 1) for boolean value.
	stringstream strStream;
	strStream << std::boolalpha << getBackup();	
	retVal.append(strStream.str());

	retVal += DOUBLE_QUOTE;
	retVal += SPACE;
	retVal += ASI_MESSAGE_VERSION_ATTRIBUTE_NAME;
	retVal += EQUALS_SIGN;
	retVal += DOUBLE_QUOTE;
	retVal += getVersion();
	retVal += DOUBLE_QUOTE;
	retVal += SPACE;
	retVal += XSI_TYPE_PREFIX;
	retVal += EQUALS_SIGN;
	retVal += DOUBLE_QUOTE;
	retVal += ASI_MESSAGE_TYPE_NAME;
	retVal += DOUBLE_QUOTE;
	retVal += GREATER_THAN_SIGN;
	retVal += NEWLINE;

	// create the source name element
	// e.g. <source-name>ALARM_SYSTEM_SOURCES</source-name>
	retVal += SPACE;
	retVal += SPACE;
	retVal += SPACE;
	retVal += LESS_THAN_SIGN;
	retVal += SOURCE_NAME_ELEMENT_NAME;
	retVal += GREATER_THAN_SIGN;
	retVal += getSourceName();
	retVal += LESS_THAN_SIGN;
	retVal += FORWARD_SLASH;
	retVal += SOURCE_NAME_ELEMENT_NAME;
	retVal += GREATER_THAN_SIGN;
	retVal += NEWLINE;

	// create the source hostname element
	// e.g. <source-hostname>NEWBIE.AOC.NRAO.EDU</source-hostname>
	retVal += SPACE;
	retVal += SPACE;
	retVal += SPACE;
	retVal += LESS_THAN_SIGN;
	retVal += SOURCE_HOSTNAME_ELEMENT_NAME;
	retVal += GREATER_THAN_SIGN;
	retVal += getSourceHostname(); 
	retVal += LESS_THAN_SIGN;
	retVal += FORWARD_SLASH;
	retVal += SOURCE_HOSTNAME_ELEMENT_NAME;
	retVal += GREATER_THAN_SIGN;
	retVal += NEWLINE;

	// create the source timestamp element
	// e.g. <source-timestamp seconds="1130356783" microseconds="610000"/>
	retVal += sourceTimestamp->toXML(SOURCE_TIMESTAMP_ELEMENT_NAME, 3);	

	// create the opening tag for the fault-states element
	// e.g. <fault-states>
	retVal += SPACE;
	retVal += SPACE;
	retVal += SPACE;
	retVal += LESS_THAN_SIGN;
	retVal += FAULT_STATES_ELEMENT_NAME;
	retVal += GREATER_THAN_SIGN;
	retVal += NEWLINE;

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
		retVal += (*faultStates)[i].toXML(6);
	}

	// create the closing tag for the fault-states element
	// e.g. </fault-states>
	retVal += SPACE;
	retVal += SPACE;
	retVal += SPACE;
	retVal += LESS_THAN_SIGN;
	retVal += FORWARD_SLASH;
	retVal += FAULT_STATES_ELEMENT_NAME;
	retVal += GREATER_THAN_SIGN;
	retVal += NEWLINE;
	
	// create the closing tag for the ASI-Message element
	// e.g. </ASI-message>
	retVal += LESS_THAN_SIGN;
	retVal += FORWARD_SLASH;
	retVal += ASI_MESSAGE_ELEMENT_NAME;
	retVal += GREATER_THAN_SIGN;
	retVal += NEWLINE;

	return retVal;
}

#ifndef UTIL_CONSTANTS_H
#define UTIL_CONSTANTS_H

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
namespace acsalarm 
{
	// SOME USEFUL CONSTANTS
	const char* const LESS_THAN_SIGN = "<";
	const char* const SPACE = " ";
	const char* const EQUALS_SIGN = "=";
	const char* const DOUBLE_QUOTE = "\"";
	const char* const FORWARD_SLASH = "/";
	const char* const GREATER_THAN_SIGN = ">";
	const char* const NEWLINE = "\n";

	const char* const USER_TIMESTAMP_ELEMENT_NAME = "user-timestamp";
	const char* const USER_TIMESTAMP_SECONDS_ATTRIBUTE_NAME = "seconds";
	const char* const USER_TIMESTAMP_MICROSECONDS_ATTRIBUTE_NAME = "microseconds";

	const char* const SOURCE_TIMESTAMP_ELEMENT_NAME = "source-timestamp";
	const char* const SOURCE_TIMESTAMP_SECONDS_ATTRIBUTE_NAME = USER_TIMESTAMP_SECONDS_ATTRIBUTE_NAME;
	const char* const SOURCE_TIMESTAMP_MICROSECONDS_ATTRIBUTE_NAME = USER_TIMESTAMP_MICROSECONDS_ATTRIBUTE_NAME;

	const char* const USER_PROPERTIES_ELEMENT_NAME = "user-properties";
	const char* const USER_PROPERTIES_PROPERTY_ELEMENT_NAME = "property";
	const char* const USER_PROPERTIES_NAME_ATTRIBUTE_NAME = "name";
	const char* const USER_PROPERTIES_VALUE_ATTRIBUTE_NAME = "value";

	const char* const FAULT_STATE_ELEMENT_NAME = "fault-state";
	const char* const FAULT_STATE_FAMILY_ATTRIBUTE_NAME = "family";
	const char* const FAULT_STATE_MEMBER_ATTRIBUTE_NAME = "member";
	const char* const FAULT_STATE_CODE_ATTRIBUTE_NAME = "code";
	const char* const FAULT_STATE_DESCRIPTOR_ELEMENT_NAME = "descriptor";

	const char* const XML_HEADER = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>";
	const char* const ASI_MESSAGE_ELEMENT_NAME = "ASI-message";
	const char* const XML_NAMESPACE_PREFIX =  "xmlns:xsi";
	const char* const XML_SCHEMA_URI = "http://www.w3.org/2001/XMLSchema-instance";
	const char* const ASI_MESSAGE_BACKUP_ATTRIBUTE_NAME = "backup";
	const char* const ASI_MESSAGE_VERSION_ATTRIBUTE_NAME = "version";
	const char* const XSI_TYPE_PREFIX = "xsi:type";
	const char* const ASI_MESSAGE_TYPE_NAME = "ASI-message";
	const char* const SOURCE_NAME_ELEMENT_NAME = "source-name";
	const char* const SOURCE_HOSTNAME_ELEMENT_NAME = "source-hostname";
	const char* const FAULT_STATES_ELEMENT_NAME = "fault-states";

#ifdef __CYGWIN__
   const char * const CERN_ALARM_SYSTEM_DLL_PATH = "liblaserSourceAcsSpecific.dll";
#else
   const char * const CERN_ALARM_SYSTEM_DLL_PATH = "liblaserSourceAcsSpecific.so";
#endif
   const char * const CERN_ALARM_SYSTEM_DLL_FUNCTION_NAME = "getAlarmSystemInterfaceFactory";
}
#endif

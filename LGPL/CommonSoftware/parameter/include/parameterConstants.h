#ifndef _PARAM_CONSTANTS_H
#define _PARAM_CONSTANTS_H
/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) Associated Universities Inc., 2002 
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration)
*    and Cosylab 2002, All rights reserved
*
*    This library is free software; you can redistribute it and/or
*    modify it under the terms of the GNU Lesser General Public
*    License as published by the Free Software Foundation; either
*    version 2.1 of the License, or (at your option) any later version.
*
*    This library is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*    Lesser General Public License for more details.
*
*    You should have received a copy of the GNU Lesser General Public
*    License along with this library; if not, write to the Free Software
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
namespace Parameters {
	// SOME USEFUL CONSTANTS

	// ParameterSet constants
	const char* const PARAMETERSET_NAMESPACE_PREFIX = "pset";
	const char* const PARAMETERSET_SCHEMA_NAME = "pset.xsd";
	const char* const PSETDEF_STRING = "psetdef";
	const char* const PARAMETER_STRING = "param";
	const char* const PARAMETERSET_STRING = "paramset";
	const char* const TYPE_STRING = "type"; 
	const char* const NAME_STRING = "name"; 
	const char* const VALUE_STRING = "value"; 
	const char* const UNITS_STRING = "units"; 
	const char* const INT_PARAM_STRING = "int"; 
	const char* const DOUBLE_PARAM_STRING = "double"; 
	const char* const STRING_PARAM_STRING = "string"; 
	const char* const BOOL_PARAM_STRING = "bool"; 
	const char* const INT_ARRAY_PARAM_STRING = "intArray"; 
	const char* const DOUBLE_ARRAY_PARAM_STRING = "doubleArray"; 
	const char* const STRING_ARRAY_PARAM_STRING = "stringArray"; 
	const char* const PSET_SCHEMA_FILE_NAME = "pset.xsd";
	const char* const PSET_NAMESPACE_URI = "urn:schemas-cosylab-com:pset:1.0";
	const char* const XML_NAMESPACE_STRING = "xmlns";
	const char* const XML_SCHEMA_INSTANCE_PREFIX = "xsi";
	const char* const XML_SCHEMA_URI = "http://www.w3.org/2001/XMLSchema-instance";
	const char* const XML_SCHEMA_LOCATION_HINT_STRING = "schemaLocation";
	const char* const XML_STANDARD_HEADER = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>";
	const char* const QUOTE = "\"";
	const char* const SPACE = " ";
	const char* const EQUALS = "=";
	const char* const COLON = ":";
	const char* const NEWLINE = "\n";
	const char* const TAB = "\t";
	const char* const LESS_THAN_SIGN = "<";
	const char* const GREATER_THAN_SIGN = ">";

	// ParamSetDef constants
	const char* const PSETDEF_SCHEMA_FILE_NAME = "psetdef.xsd";
	const char* const PSETDEF_NAMESPACE_URI = "urn:schemas-cosylab-com:psetdef:1.0";
	const char* const INSTANCE_DOC_SUBDIR_NAME = "xml";
	const char* const PARAMETER_TAG_NAME_STR = "param";
	const char* const NAME_TAG_NAME_STR = "name";
	const char* const REQUIRED_TAG_NAME_STR = "required";
	const char* const PROMPT_TAG_NAME_STR = "prompt";
	const char* const HELP_TAG_NAME_STR = "help";
	const char* const DEFAULT_TAG_NAME_STR = "default";
	const char* const STRING_DEFAULT_TAG_NAME_STR = "stringdefault";
	const char* const LENGTH_TAG_NAME_STR = "length";
	const char* const VALID_VALUES_TAG_NAME_STR = "validValues";
	const char* const VALUE_TAG_NAME_STR = "value";
	const char* const MAX_TAG_NAME_STR = "max";
	const char* const MIN_TAG_NAME_STR = "min";
	const char* const UNITS_TAG_NAME_STR = "units";
	const char* const MAXLEN_TAG_NAME_STR = "maxlen";
	const char* const INT_PARAM_TYPE_STR = "int";
	const char* const DOUBLE_PARAM_TYPE_STR = "double";
	const char* const STRING_PARAM_TYPE_STR = "string";
	const char* const BOOL_PARAM_TYPE_STR = "bool";
	const char* const INT_ARRAY_PARAM_TYPE_STR = "intArray";
	const char* const DOUBLE_ARRAY_PARAM_TYPE_STR = "doubleArray";
	const char* const STRING_ARRAY_PARAM_TYPE_STR = "stringArray";
	const char* const TRUE_STRING = "true";
	const char* const FALSE_STRING = "false";
	const char* const ONE_STRING = "1";

	// Miscellaneous/shared constants
	const char* const TASK_DIR_ENV_VAR_NAME = "TASK_DIR"; 
	const char* const ACSROOT_ENV_VAR_NAME = "ACSROOT"; 
	const char* const INTROOT_ENV_VAR_NAME = "INTROOT"; 
	const char* const SPACE_STRING = " ";
	const char* const SCHEMA_SUBDIR_NAME = "xsd";
	const char* const CONFIG_SUBDIR_NAME = "idl";
	const char* const SLASH_STRING = "/"; // TODO - portability issue for forward slash?
}
#endif /*!_PARAM_CONSTANTS_H */



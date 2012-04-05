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
#include "Properties.h"
#include "utilConstants.h"
#include <logging.h>
#include <string>
#include <sstream>

using std::string;
using std::stringstream;
using std::map;
using std::vector;
using std::invalid_argument;
using acsalarm::Properties;

/*
 * Default no-args constructor.
 */
Properties::Properties()
{
}

/* 
 * Copy constructor
 */
Properties::Properties(const Properties & rhs)
{
	*this = rhs;
}

/*
 * Destructor.
 */
Properties::~Properties()
{
}

/* 
 * Assignment operator
 */
Properties & Properties::operator=(const Properties & rhs)
{
	propertiesMap = rhs.propertiesMap;
	return *this;
}

/*
 * Equality operator.
 */
bool Properties::operator==(const Properties &rhs)
{
	// Check the number of items in the containers
	if (rhs.getSize()!=getSize()) {
		return false;
	}

	// Check if both maps contain the same pairs <key,value>
	std::map<std::string, std::string>::iterator mapIter;
	for(mapIter= propertiesMap.begin(); mapIter != propertiesMap.end(); mapIter++ )
	{
		// The key of the present item
		string key = mapIter->first;
		string val = mapIter->second;

		string rhsVal = rhs.getProperty(key);

		if (val.compare(rhsVal)!=0) {
			// The value of the same keys differ
			return false;
		}
	}
	return true;
}

bool Properties::operator!=(const Properties &rhs) {
	return !((*this == rhs));
}

/*
 * Searches for the property with the specified key in this property list.
 *
 * @key the key to search for in the map.
 */
string Properties::getProperty(string key) const
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	if(myLoggerSmartPtr != NULL) {
		myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "Properties::getProperty(): entering");
	}

	string retVal;
	map<string, string >::const_iterator mapEntry = propertiesMap.find(key);
	if(mapEntry != propertiesMap.end())
	{
		string logStr = "Properties::getProperty(): entry for: " + key + " was found"; 
		if(myLoggerSmartPtr != NULL) {
			myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, logStr); 
		}
		retVal = mapEntry->second;
	}
	else 
	{ 
		string logStr = "Properties::getProperty(): entry for: " + key + " was not found"; 
		if(myLoggerSmartPtr != NULL) {
			myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, logStr); 
		}
	}

	if(myLoggerSmartPtr != NULL) {
		myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "Properties::getProperty(): exiting");
	}
	return retVal;
} 

/*
 * Returns an enumeration of all the keys in this property list, 
 * including distinct keys in the default property list if a key 
 * of the same name has not already been found from the main properties list.
 */
auto_ptr<vector<string> > Properties::propertyNames()
{
	auto_ptr<vector<string> > retVal;
	retVal.reset(new vector<string>);
	map<string, string>::iterator mapIter;
	for(mapIter = propertiesMap.begin(); mapIter != propertiesMap.end(); mapIter++ )
	{
		retVal->push_back(mapIter->first);
	}
	return retVal;
}

/* 
 * Calls the map method insert.
 * @param key the key for the map entry to be inserted
 * @value the value for the map entry to be inserted
 */
void Properties::setProperty(string key, string value) throw(invalid_argument)
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	if(myLoggerSmartPtr != NULL) {
		myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "Properties::setProperty(): entering.");
	}
	if(0 == key.length())
	{
		// NULL passed in for key; not allowed
		throw invalid_argument("zero-length key not allowed");
	}
	if(0 == value.length())
	{
		// NULL passed in for value; not allowed
		throw invalid_argument("zero-length value not allowed");
	}
	string logStr = "Properties::setProperty(): inserting key: " + key + " and value: " + value;
	if(myLoggerSmartPtr != NULL) {
		myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, logStr);
	}
	propertiesMap.insert(PropertyMapEntryType( key, value ) );
	if(myLoggerSmartPtr != NULL) {
		myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "Properties::setProperty(): exiting.");
	}
}

#ifndef PROPERTIES_H
#define PROPERTIES_H

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
#include <map>
#include <string>
#include <vector>
#include <memory>
#include <stdexcept>

namespace acsalarm
{
	typedef std::map< std::string, std::string >::value_type PropertyMapEntryType;

	/*
	 * Utility class containing a collection of properties, which are name/value pairs of strings.
	 */
	class Properties
	{
		private:

			std::map<std::string, std::string> propertiesMap;

		public:

			// constructors
			Properties();
			Properties(const Properties &);

			// destructor
			virtual ~Properties();

			// assignment operator
			Properties & operator=(const Properties & rhs);

			// equality operator
			bool operator==(const Properties &rhs) ;

			// operator !=
			bool operator!=(const Properties &rhs) ;

			/**
			 * Searches for the property with the specified key in this property list.
			 *
			 * @return the value of the property with the given key;
			 * 			the returned string is empty if a pair with the given key does not exist
			 *
			 */
			std::string getProperty(std::string key) const;

 			// Returns an enumeration of all the keys in this property list, 
			// including distinct keys in the default property list if a key 
			// of the same name has not already been found from the main properties list.
			std::auto_ptr<std::vector<std::string> > propertyNames();

			/**
			 * Add a pair <key, value> to the property
			 *
			 * @param key The key (not empty)
			 * @param value The value (not empty)
			 */
			void setProperty(std::string key, std::string value) throw(std::invalid_argument);

			// Returns an XML fragment (NOT a complete document) representing all of 
			// the properties contained in this table, for use in the message transported
			// from an alarm source to the alarm server.
			// @param amountToIndent - used to specify a level of indentation (in spaces) for readability
			std::string toXML(int amountToIndent = 6);

			/**
			 * @return the size of the properties
			 */
			unsigned int getSize() const { return propertiesMap.size(); }
	};
}
#endif
